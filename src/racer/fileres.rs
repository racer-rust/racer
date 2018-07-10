use cargo::core::{resolver::Method, Workspace};
use cargo::ops;
use cargo::util::important_paths::find_root_manifest_for_wd;
use cargo::Config;
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
        debug!("get_outer_crates returned None, try RUST_SRC_PATH");
    }

    // TODO: cache std libs
    if let Some(ref std_path) = *RUST_SRC_PATH {
        // try lib<name>/lib.rs, like in the rust source dir
        let cratelibname = format!("lib{}", name);
        let filepath = std_path.join(cratelibname).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }

        // try <name>/lib.rs
        let filepath = std_path.join(name).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    None
}

/// get module file from current path & crate name
pub fn get_module_file(name: &str, parentdir: &Path, session: &Session) -> Option<PathBuf> {
    // try just <name>.rs
    let filepath = parentdir.join(format!("{}.rs", name));
    if filepath.exists() || session.contains_file(&filepath) {
        return Some(filepath);
    }
    // try <name>/mod.rs
    let filepath = parentdir.join(name).join("mod.rs");
    if filepath.exists() || session.contains_file(&filepath) {
        return Some(filepath);
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

/// try to get outer crates
/// if we have dependencies in cache, use it.
/// else, call cargo's function to resolve depndencies.
fn get_outer_crates(libname: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!(
        "[get_outer_crates] lib name: {:?}, from_path: {:?}",
        libname, from_path
    );

    let manifest = cargo_try!(find_root_manifest_for_wd(from_path));
    if let Some(deps_info) = session.get_deps(&manifest) {
        // cache exists
        debug!("[get_outer_crates] cache exists for manifest",);
        deps_info.get_src_path(libname)
    } else {
        // cache doesn't exist
        let manifest = cargo_try!(find_root_manifest_for_wd(from_path));
        // calucurating depedencies can be bottleneck so used info! here(kngwyu)
        info!("[get_outer_crates] cache doesn't exist");
        resolve_dependencies(&manifest, session, libname)
    }
}

fn resolve_dependencies(manifest: &Path, session: &Session, libname: &str) -> Option<PathBuf> {
    let mut config = cargo_try!(Config::default());
    // verbose=0, quiet=true, frozen=true, locked=true
    config.configure(0, Some(true), &None, true, true, &None, &[]).ok()?;
    let ws = cargo_try!(Workspace::new(&manifest, &config));
    // resolve dependencies
    let (packages, _) = cargo_try!(ops::resolve_ws_with_method(&ws, None, Method::Everything, &[]));
    let depth1_dependencies = match ws.current_opt() {
        Some(cur) => cur.dependencies().iter().map(|p| p.name()).collect(),
        None => HashSet::new(),
    };
    let mut res = None;
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
            // crate_name returns target.name.replace("-", "_")
            let crate_name = lib_target.crate_name();
            let src_path = lib_target.src_path().to_owned();
            if crate_name == libname {
                res = Some(src_path.clone());
            }
            Some((crate_name, src_path))
        })
        .collect();
    session.cache_deps(manifest, deps_map);
    res
}
