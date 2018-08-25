use core::Session;
use nameres::RUST_SRC_PATH;
use std::path::{Path, PathBuf};

/// get crate file from current path & crate name
pub fn get_crate_file(name: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!("get_crate_file {}, {:?}", name, from_path);
    get_std_file(name, session).or_else(|| get_outer_crates(name, from_path, session))
}

pub fn get_std_file(name: &str, session: &Session) -> Option<PathBuf> {
    if let Some(ref std_path) = *RUST_SRC_PATH {
        // try lib<name>/lib.rs, like in the rust source dir
        let cratelibname = format!("lib{}", name);
        let filepath = std_path.join(cratelibname).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    return None;
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

/// try to get outer crates
/// if we have dependencies in cache, use it.
/// else, call cargo-metadata(default) or fall back to rls
fn get_outer_crates(libname: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!(
        "[get_outer_crates] lib name: {:?}, from_path: {:?}",
        libname, from_path
    );

    let manifest = session.project_model.discover_project_manifest(from_path)?;
    let res = session.project_model.resolve_dependency(&manifest, libname);
    println!("{:?}", res);
    res
}
