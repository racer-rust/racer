use cargo::core::Workspace;
use cargo::ops::{resolve_ws_precisely, Packages};
use cargo::util::important_paths::find_root_manifest_for_wd;
use cargo::Config;
use core::Session;
use nameres::RUST_SRC_PATH;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

/// get crate file from current path & crate name
pub fn get_crate_file(name: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!("get_crate_file {}, {:?}", name, from_path);

    if let Some(path) = get_outer_crates(name, from_path, session) {
        debug!("get_outer_crates returned {:?} for {}", path, name);
        return Some(path);
    } else {
        warn!("get_outer_crates returned None");
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

/// try to get outer crates
fn get_outer_crates(libname: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    macro_rules! cargo_res {
        ($r:expr) => {
            match $r {
                Ok(val) => val,
                Err(err) => {
                    warn!("[get_outer_crates]: {}", err);
                    return None;
                }
            }
        };
    }
    debug!(
        "[get_outer_crates] lib name: {:?}, from_path: {:?}",
        libname, from_path
    );
    // we have to try 2 names(e.g. crate a-b has a target name a_b by rustc)
    let libname_hyphened = {
        let tmp_str = libname.to_owned();
        tmp_str.replace("_", "-")
    };
    let manifest = cargo_res!(find_root_manifest_for_wd(from_path));

    if let Some(deps_info) = session.get_deps(&manifest) {
        debug!("[get_outer_crates] cache exists");
        if let Some(p) = deps_info.get_src_path(libname) {
            Some(p)
        } else if let Some(p) = deps_info.get_src_path(&libname_hyphened) {
            Some(p)
        } else {
            None
        }
    } else {
        debug!("[get_outer_crates] cache doesn't exist");
        let config = cargo_res!(Config::default());
        let ws = cargo_res!(Workspace::new(&manifest, &config));
        let pkg_cur = ws.current_opt()?;
        // what we need is only packages in Cargo.toml
        // so, we cache only those packages
        let toml_deps: HashSet<_> = pkg_cur.dependencies().iter().map(|d| d.name()).collect();
        let specs = cargo_res!(Packages::All.into_package_id_specs(&ws));
        // now we resolve dependncies with 'all_features=true'
        let (packages, _) = cargo_res!(resolve_ws_precisely(&ws, None, &[], true, false, &specs));
        let mut deps_map = HashMap::new();
        let mut res = None;
        for package_id in packages.package_ids() {
            let pkg = match packages.get(package_id) {
                Ok(p) => p,
                Err(_) => continue,
            };
            if !toml_deps.contains(&pkg.name()) {
                continue;
            }
            let targets = pkg.manifest().targets();
            let lib_target = targets.into_iter().find(|target| target.is_lib());
            if let Some(target) = lib_target {
                let name = target.name();
                let src_path = target.src_path().to_owned();
                if name == libname || name == libname_hyphened {
                    res = Some(src_path.clone());
                }
                deps_map.insert(name.to_owned(), src_path);
            }
        }
        session.cache_deps(manifest, deps_map);
        res
    }
}
