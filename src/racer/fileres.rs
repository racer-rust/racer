use cargo::core::{registry::PackageRegistry,
                  resolver::{EncodableResolve, Method, Resolve},
                  PackageId,
                  PackageSet,
                  Source,
                  SourceId,
                  Workspace};
use cargo::ops::resolve_with_previous;
use cargo::util::{errors::CargoResult, important_paths::find_root_manifest_for_wd, toml};
use cargo::{sources::PathSource, Config};
use core::Session;
use nameres::RUST_SRC_PATH;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// get crate file from current path & crate name
pub fn get_crate_file(name: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!("get_crate_file {}, {:?}", name, from_path);

    if let Some(path) = get_outer_crates(name, from_path, session) {
        debug!("get_outer_crates returned {:?} for {}", path, name);
        return Some(path);
    } else {
        debug!("get_outer_crates returned None");
    }

    // TODO: cache std libs
    let srcpath = &*RUST_SRC_PATH;
    {
        // try lib<name>/lib.rs, like in the rust source dir
        let cratelibname = format!("lib{}", name);
        let filepath = srcpath.join(cratelibname).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    {
        // try <name>/lib.rs
        let filepath = srcpath.join(name).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    None
}

/// get module file from current path & crate name
pub fn get_module_file(name: &str, parentdir: &Path, session: &Session) -> Option<PathBuf> {
    {
        // try just <name>.rs
        let filepath = parentdir.join(format!("{}.rs", name));
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    {
        // try <name>/mod.rs
        let filepath = parentdir.join(name).join("mod.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    None
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

macro_rules! cargo_warn {
    ($r:expr) => {
        match $r {
            Ok(_) => {}
            Err(err) => {
                warn!("Error in cargo: {}", err);
            }
        }
    };
}

/// try to get outer crates
/// if we have dependencies in cache, use it.
/// else, call cargo's function to resolve depndencies.
fn get_outer_crates(libname: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!(
        "[get_outer_crates] lib name: {:?}, from_path: {:?}",
        libname, from_path
    );

    // we have to try 2 names(e.g. crate a-b has a target name a_b by rustc)
    let libname_hyphenated = if libname.contains('_') {
        Some(libname.replace('_', "-"))
    } else {
        None
    };
    let manifest = cargo_try!(find_root_manifest_for_wd(from_path));
    if let Some(deps_info) = session.get_deps(&manifest) {
        // cache exists
        debug!("[get_outer_crates] cache exists for manifest",);
        if let Some(p) = deps_info.get_src_path(libname) {
            Some(p)
        } else if let Some(p) = deps_info.get_src_path(&libname_hyphenated?) {
            Some(p)
        } else {
            None
        }
    } else {
        let manifest = cargo_try!(find_root_manifest_for_wd(from_path));
        debug!("[get_outer_crates] cache doesn't exist");
        resolve_dependencies(&manifest, session, |name| {
            if libname == name {
                return true;
            }
            if let Some(ref hyphnated) = libname_hyphenated {
                if name == hyphnated {
                    return true;
                }
            }
            false
        })
    }
}

fn resolve_dependencies(
    manifest: &Path,
    session: &Session,
    matches_libname: impl Fn(&str) -> bool,
) -> Option<PathBuf> {
    let config = cargo_try!(Config::default());
    let ws = cargo_try!(Workspace::new(&manifest, &config));
    // get resolve from lock file
    let lock_path = ws.root().to_owned().join("Cargo.lock");
    let lock_file = session.load_lock_file(&lock_path, |lock_file| {
        let resolve = cargo_try!(toml::parse(&lock_file, &lock_path, ws.config()));
        let v: EncodableResolve = cargo_try!(resolve.try_into());
        Some(cargo_try!(v.into_resolve(&ws)))
    });

    // then resolve precisely and add overrides
    let mut registry = cargo_try!(PackageRegistry::new(ws.config()));
    let resolve = cargo_try!(match lock_file {
        Some(prev) => resolve_with_prev(&mut registry, &ws, Some(&*prev)),
        None => resolve_with_prev(&mut registry, &ws, None),
    });
    cargo_warn!(add_overrides(&mut registry, &ws));
    // get depedency with overrides
    let resolved_with_overrides = cargo_try!(resolve_with_previous(
        &mut registry,
        &ws,
        Method::Everything,
        Some(&resolve),
        None,
        &[],
        false,
        false,
    ));
    let packages = get_resolved_packages(&resolved_with_overrides, registry);
    // cache depedencies and get the src_path we're searching, if it exists
    let mut res = None;
    // we have caches for each packages, so only depth1 depedencies are necessary
    let depth1_dependencies = match ws.current_opt() {
        Some(cur) => cur.dependencies().iter().map(|p| p.name()).collect(),
        None => HashSet::new(),
    };
    let deps_map = packages
        .package_ids()
        .filter_map(|package_id| {
            let pkg = packages.get(package_id).ok()?;
            if !depth1_dependencies.contains(&pkg.name()) {
                return None;
            }
            let targets = pkg.manifest().targets();
            // we only need library target
            let lib_target = targets.into_iter().find(|target| target.is_lib())?;
            let name = lib_target.name();
            let src_path = lib_target.src_path().to_owned();
            if matches_libname(name) {
                res = Some(src_path.clone());
            }
            Some((name.to_owned(), src_path))
        })
        .collect();
    session.cache_deps(manifest, deps_map);
    res
}

// wrapper of resolve_with_previous
fn resolve_with_prev<'cfg>(
    registry: &mut PackageRegistry<'cfg>,
    ws: &Workspace<'cfg>,
    prev: Option<&Resolve>,
) -> CargoResult<Resolve> {
    resolve_with_previous(
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

// almost same as cargo::ops::resolve::add_overrides
fn add_overrides<'cfg>(
    registry: &mut PackageRegistry<'cfg>,
    ws: &Workspace<'cfg>,
) -> CargoResult<()> {
    let paths = match ws.config().get_list("paths")? {
        Some(list) => list,
        None => return Ok(()),
    };

    let paths = paths.val.iter().map(|&(ref s, ref p)| {
        // The path listed next to the string is the config file in which the
        // key was located, so we want to pop off the `.cargo/config` component
        // to get the directory containing the `.cargo` folder.
        (p.parent().unwrap().parent().unwrap().join(s), p)
    });

    for (path, definition) in paths {
        let id = SourceId::for_path(&path)?;
        let mut source = PathSource::new_recursive(&path, &id, ws.config());
        source.update().map_err(|e| {
            warn!(
                "failed to update path override `{}` \
                 (defined in `{}`)",
                path.display(),
                definition.display()
            );
            e
        })?;
        registry.add_override(Box::new(source));
    }
    Ok(())
}

// same as cargo::ops::resolve::get_resolved_packages
fn get_resolved_packages<'a>(resolve: &Resolve, registry: PackageRegistry<'a>) -> PackageSet<'a> {
    let ids: Vec<PackageId> = resolve.iter().cloned().collect();
    registry.get(&ids)
}
