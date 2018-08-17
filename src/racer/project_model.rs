use std::path::{Path, PathBuf};

pub trait ProjectModelProvider {
    fn discover_project_manifest(&self, path: &Path) -> Option<PathBuf>;
    fn resolve_dependency(&self, manifest: &Path, dep_name: &str) -> Option<PathBuf>;
}

#[cfg(feature = "cargo")]
pub(crate) mod cargo {
    extern crate cargo;

    use std::{
        cell::RefCell,
        collections::{
            hash_map::{self, HashMap},
            HashSet,
        },
        path::{Path, PathBuf},
        rc::Rc,
    };
    use {FileCache, ProjectModelProvider};

    use self::cargo::{
        core::{
            registry::PackageRegistry,
            resolver::{EncodableResolve, Method, Resolve},
            PackageId, PackageSet, Workspace,
        },
        ops,
        util::{errors::CargoResult, important_paths::find_root_manifest_for_wd, toml},
        Config,
    };

    pub fn cargo_project_model<'c>(file_cache: &'c FileCache) -> Box<ProjectModelProvider + 'c> {
        Box::new(CargoProjectModel {
            file_cache,
            cached_lockfile: Default::default(),
            cached_deps: Default::default(),
        })
    }

    struct CargoProjectModel<'c> {
        file_cache: &'c FileCache,
        /// Cached lockfiles (path to Cargo.lock -> Resolve)
        cached_lockfile: RefCell<HashMap<PathBuf, Rc<Resolve>>>,
        /// Cached dependencie (path to Cargo.toml -> Depedencies)
        cached_deps: RefCell<HashMap<PathBuf, Rc<Dependencies>>>,
    }

    macro_rules! cargo_try {
        ($r:expr) => {
            match $r {
                Ok(val) => val,
                Err(err) => {
                    warn!("Error in cargo: {}", err);
                    return None;
                }
            }
        };
    }

    impl<'c> ProjectModelProvider for CargoProjectModel<'c> {
        fn discover_project_manifest(&self, path: &Path) -> Option<PathBuf> {
            let path = cargo_try!(find_root_manifest_for_wd(path));
            Some(path)
        }
        fn resolve_dependency(&self, manifest: &Path, libname: &str) -> Option<PathBuf> {
            let deps = match self.get_deps(manifest) {
                Some(deps) => {
                    debug!("[resolve_dependency] cache exists for manifest");
                    deps
                }
                None => {
                    // cache doesn't exist
                    // calculating depedencies can be bottleneck we use info! here(kngwyu)
                    info!("[resolve_dependency] cache doesn't exist");
                    let deps_map = self.resolve_dependencies(&manifest)?;
                    self.cache_deps(manifest, deps_map)
                }
            };
            deps.get_src_path(libname)
        }
    }

    impl<'c> CargoProjectModel<'c> {
        /// load `Cargo.lock` file using fileloader
        // TODO: use result
        pub(crate) fn load_lockfile(
            &self,
            path: &Path,
            resolver: &Fn(&str) -> Option<Resolve>,
        ) -> Option<Rc<Resolve>> {
            match self.cached_lockfile.borrow_mut().entry(path.to_owned()) {
                hash_map::Entry::Occupied(occupied) => Some(Rc::clone(occupied.get())),
                hash_map::Entry::Vacant(vacant) => {
                    let contents = match self.file_cache.loader.load_file(path) {
                        Ok(f) => f,
                        Err(e) => {
                            debug!(
                                "[Session::load_lock_file] Failed to load {}: {}",
                                path.display(),
                                e
                            );
                            return None;
                        }
                    };
                    resolver(&contents).map(|res| Rc::clone(vacant.insert(Rc::new(res))))
                }
            }
        }

        fn resolve_dependencies(&self, manifest: &Path) -> Option<HashMap<String, PathBuf>> {
            let mut config = cargo_try!(Config::default());
            // frozen=true, locked=true
            config
                .configure(0, Some(true), &None, true, true, &None, &[])
                .ok()?;
            let ws = cargo_try!(Workspace::new(&manifest, &config));
            // get resolve from lock file
            let lock_path = ws.root().to_owned().join("Cargo.lock");
            let lock_file = self.load_lockfile(&lock_path, &|lockfile| {
                let resolve = cargo_try!(toml::parse(&lockfile, &lock_path, ws.config()));
                let v: EncodableResolve = cargo_try!(resolve.try_into());
                Some(cargo_try!(v.into_resolve(&ws)))
            });
            // then resolve precisely and add overrides
            let mut registry = cargo_try!(PackageRegistry::new(ws.config()));
            let resolve = cargo_try!(match lock_file {
                Some(prev) => resolve_with_prev(&mut registry, &ws, Some(&*prev)),
                None => resolve_with_prev(&mut registry, &ws, None),
            });
            let packages = get_resolved_packages(&resolve, registry);
            // we have caches for each crates, so only need depth1 depedencies(= dependencies in Cargo.toml)
            let depth1_dependencies = match ws.current_opt() {
                Some(cur) => cur.dependencies().iter().map(|p| p.name()).collect(),
                None => HashSet::new(),
            };
            let current_pkg = ws.current().map(|pkg| pkg.name());
            let is_current_pkg = |name| {
                if let Ok(n) = current_pkg {
                    n == name
                } else {
                    false
                }
            };
            let deps_map = packages
                .package_ids()
                .filter_map(|package_id| {
                    let pkg = packages.get(package_id).ok()?;
                    let pkg_name = pkg.name();
                    // for examples/ or tests/ dir, we have to handle current package specially
                    if !is_current_pkg(pkg_name) && !depth1_dependencies.contains(&pkg.name()) {
                        return None;
                    }
                    let targets = pkg.manifest().targets();
                    // we only need library target
                    let lib_target = targets.into_iter().find(|target| target.is_lib())?;
                    // crate_name returns target.name.replace("-", "_")
                    let crate_name = lib_target.crate_name();
                    let src_path = lib_target.src_path().to_owned();
                    Some((crate_name, src_path))
                }).collect();
            Some(deps_map)
        }

        /// Get cached dependencies from manifest path(abs path of Cargo.toml) if they exist.
        fn get_deps(&self, manifest: &Path) -> Option<Rc<Dependencies>> {
            let deps = self.cached_deps.borrow();
            deps.get(manifest).map(|rc| Rc::clone(&rc))
        }

        /// Cache dependencies into session.
        fn cache_deps(&self, manifest: &Path, cache: HashMap<String, PathBuf>) -> Rc<Dependencies> {
            let manifest = manifest.to_owned();
            let deps = Rc::new(Dependencies { inner: cache });
            self.cached_deps.borrow_mut().insert(manifest, deps.clone());
            deps
        }
    }

    /// dependencies info of a package
    #[derive(Clone, Debug, Default)]
    struct Dependencies {
        /// dependencies of a package(library name -> src_path)
        inner: HashMap<String, PathBuf>,
    }

    impl Dependencies {
        /// Get src path from a library name.
        /// e.g. from query string `bit_set` it returns
        /// `~/.cargo/registry/src/github.com-1ecc6299db9ec823/bit-set-0.4.0`
        pub fn get_src_path(&self, query: &str) -> Option<PathBuf> {
            let p = self.inner.get(query)?;
            Some(p.to_owned())
        }
    }

    // wrapper of resolve_with_previous
    fn resolve_with_prev<'cfg>(
        registry: &mut PackageRegistry<'cfg>,
        ws: &Workspace<'cfg>,
        prev: Option<&Resolve>,
    ) -> CargoResult<Resolve> {
        ops::resolve_with_previous(
            registry,
            ws,
            Method::Everything,
            prev,
            None,
            &[],
            true,
            false,
        )
    }

    // until cargo 0.30 is released
    fn get_resolved_packages<'a>(
        resolve: &Resolve,
        registry: PackageRegistry<'a>,
    ) -> PackageSet<'a> {
        let ids: Vec<PackageId> = resolve.iter().cloned().collect();
        registry.get(&ids)
    }
}
