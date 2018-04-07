use std::fs::{self, File};
use std::rc::Rc;
use std::cell::RefCell;
use std::io::Read;
use std::env;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::collections::HashMap;
use toml;

// otry is 'option try'
macro_rules! otry {
    ($e:expr) => {
        match $e {
            Some(e) => e,
            None => return None
        }
    }
}

// converts Err into None, while logging an error
macro_rules! otry2 {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(e) => { error!("ERROR!: {:?} {} {}", e, file!(), line!()); return None }
        }
    }
}

// converts errors into message + empty vec
macro_rules! vectry {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(e) => { error!("ERROR!: {:?} {} {}", e, file!(), line!()); return Vec::new() }
        }
    }
}

thread_local! {
    /// Caches parsed Cargo.toml/Cargo.lock files, together with the mtime for
    /// invalidation.
    ///
    /// This cache is not tied to the session because it has its own invalidation
    /// strategy, and can be reused across Racer sessions.
    static TOML_CACHE: RefCell<HashMap<PathBuf, (SystemTime, Option<Rc<toml::Value>>)>> =
        Default::default();
}

/// Parse and cache a TOML file (Cargo.toml or Cargo.lock).
fn parse_toml_file(toml_file: &Path) -> Option<Rc<toml::Value>> {
    TOML_CACHE.with(|cache| {
        let mtime = otry!(fs::metadata(toml_file).ok().and_then(|m| m.modified().ok()));
        if let Some(val) = cache.borrow().get(toml_file) {
            if mtime == val.0 {
                return val.1.clone();
            }
        }
        trace!("parse_toml_file parsing: {:?}", toml_file);
        // TODO: use fs::read when stabilized
        let parsed = File::open(toml_file).ok()
            .and_then(|mut f| {
                let mut content = Vec::new();
                f.read_to_end(&mut content).ok().map(|_| content)
            })
            .and_then(|content| toml::from_slice(&content).ok())
            .map(Rc::new);
        cache.borrow_mut().insert(toml_file.into(), (mtime, parsed.clone()));
        parsed
    })
}

#[derive(Debug)]
struct PackageInfo {
    name: String,
    version: Option<String>,
    source: Option<PathBuf>
}

/// Gets the branch from a git source string if one is present.
fn get_branch_from_source(source: &str) -> Option<&str> {
    debug!("get_branch_from_source - Finding branch from {:?}", source);
    source.find("branch=").and_then(|idx| {
        let branch = &source[idx+7..];
        branch.find('#').map(|idx| &branch[..idx]).or(Some(branch))
    })
}

/// Gets the repository_name from a git source string if one is present.
fn get_repository_name_from_source(source: &str) -> Option<&str> {
    debug!("get_repository_name_from_source - Finding repository name from {:?}", source);
    source.rfind('/').and_then(|idx| {
        let repository_name = &source[idx+1..];
        repository_name.find(&['?', '#'][..])
            .map(|idx| repository_name[0..idx].trim_right_matches(".git"))
    })
}

/// Find the main package file for a given crate, given a Cargo.lock file location.
fn find_src_via_lockfile(cratename: &str, lockfile: &Path) -> Option<PathBuf> {
    trace!("find_src_via_lockfile searching for {} in {:?}", cratename, lockfile);
    if let Some(packages) = get_cargo_packages(lockfile) {
        trace!("find_src_via_lockfile got packages");
        for package in packages {
            trace!("find_src_via_lockfile examining {:?}", package);
            if let Some(package_source) = package.source {
                trace!("find_src_via_lockfile package_source {:?}", package_source);
                if let Some(tomlfile) = find_cargo_tomlfile(&package_source) {
                    trace!("find_src_via_lockfile tomlfile {:?}", tomlfile);
                    let package_name = get_package_name(&tomlfile);

                    debug!("find_src_via_lockfile package_name: {}", package_name);

                    if package_name == cratename {
                        return Some(package_source);
                    }
                }
            }
        }
    }

    trace!("find_src_via_lockfile returning None");
    None
}

/// Extract all dependency packages from a Cargo.lock file.
fn get_cargo_packages(lockfile: &Path) -> Option<Vec<PackageInfo>> {
    let lock_table = otry!(parse_toml_file(lockfile));

    debug!("get_cargo_packages found lock_table {}: {:?}", lockfile.display(), lock_table);

    let packages_array = match lock_table.get("package") {
        Some(&toml::Value::Array(ref t1)) => t1,
        _ => return None
    };

    let mut result = Vec::new();

    for package_element in packages_array {
        if let toml::Value::Table(ref package_table) = *package_element {
            if let Some(&toml::Value::String(ref package_name)) = package_table.get("name") {
                trace!("get_cargo_packages processing {}", package_name);

                macro_rules! unwrap_or_continue {
                    ($opt:expr) => {
                        match $opt {
                            Some(v) => v,
                            _ => {
                                debug!("get_cargo_packages skipping {}", package_name);
                                continue;
                            }
                        }
                    }
                }

                let package_version = unwrap_or_continue!(getstr(package_table, "version"));
                let package_source = unwrap_or_continue!(getstr(package_table, "source"));

                let package_source = match package_source.split('+').nth(0) {
                    Some("registry") => {
                        get_versioned_cratefile(package_name, &package_version)
                    },
                    Some("git") => {
                        let sha1 = unwrap_or_continue!(package_source.split('#').last());
                        let branch = get_branch_from_source(&package_source);

                        // use repository name instead of package name
                        let mut dir = match get_repository_name_from_source(&package_source) {
                            Some(repo_name) => {
                                unwrap_or_continue!(find_git_src_dir(repo_name, &sha1, branch))
                            }
                            None => {
                                unwrap_or_continue!(find_git_src_dir(package_name, &sha1, branch))
                            }
                        };

                        // TODO: this is not necessarily correct
                        dir.push("src");
                        dir.push("lib.rs");
                        Some(dir)
                    },
                    _ => return None
                };

                result.push(PackageInfo {
                    name: package_name.clone(),
                    version: Some(package_version),
                    source: package_source,
                });
            }
        }
    }
    Some(result)
}

/// Extract the package name from a Cargo.toml file.
fn get_package_name(cargotomlfile: &Path) -> String {
    if let Some(cargo_table) = parse_toml_file(cargotomlfile) {
        debug!("get_package_name found cargo_table {}: {:?}", cargotomlfile.display(), cargo_table);

        if let Some(&toml::Value::Table(ref lib_table)) = cargo_table.get("lib") {
            if let Some(&toml::Value::String(ref package_name)) = lib_table.get("name") {
                return package_name.clone();
            }
        }

        if let Some(&toml::Value::Table(ref package_table)) = cargo_table.get("package") {
            if let Some(&toml::Value::String(ref package_name)) = package_table.get("name") {
                return package_name.replace('-', "_");
            }
        }
    }

    String::new()
}

/// Find the Cargo "root" directory, where Cargo dependencies are unpacked/checked out.
fn get_cargo_rootdir() -> Option<PathBuf> {
    let dir = if let Some(cargo_home) = env::var_os("CARGO_HOME") {
        debug!("get_cargo_rootdir: CARGO_HOME is set: {:?}", cargo_home);
        PathBuf::from(cargo_home)
    } else {
        debug!("get_cargo_rootdir: no CARGO_HOME");
        otry!(env::home_dir()).join(".cargo")
    };
    if dir.exists() {
        Some(dir)
    } else {
        None
    }
}

/// Find the main package file for a crate with given name/version from CARGO_HOME.
fn get_versioned_cratefile(cratename: &str, version: &str) -> Option<PathBuf> {
    trace!("get_versioned_cratefile searching for {}", cratename);
    let mut dir = otry!(get_cargo_rootdir());

    debug!("get_versioned_cratefile: cargo rootdir is {:?}", dir);
    dir.push("registry");
    dir.push("src");

    for mut src in find_cratesio_src_dirs(&dir) {
        // if version = * then search for the first matching folder
        if version == "*" {
            let start = format!("{}-", cratename);

            if let Some(entry) = fs::read_dir(&src).ok().and_then(|reader| {
                reader
                    .filter_map(|entry| entry.ok())
                    .find(|e| e.file_name().to_str().unwrap().starts_with(&start))
            }) {
                src = entry.path();
            } else {
                continue;
            }
        } else {
            src.push(format!("{}-{}", cratename, version));
        }

        debug!("crate path {:?}", src);

        // TODO: this doesn't catch all possible cases

        // First, check for package name at root (src/cratename/lib.rs)
        src.push("src");
        src.push(cratename);
        src.push("lib.rs");
        if !src.exists() {
            // It doesn't exist, so assume src/lib.rs
            src.pop();
            src.pop();
            src.push("lib.rs");
        }
        debug!("crate path with lib.rs {:?}", src);

        if !src.exists() {
            trace!("failed to open crate path {:?}", src);
            // It doesn't exist, so try /lib.rs
            src.pop();
            src.pop();
            src.push("lib.rs");
        }

        if !src.exists() {
            trace!("failed to open crate path {:?}", src);
            continue;
        }

        return Some(src);
    }
    None
 }

/// Return path to library source if the Cargo.toml name matches cratename.
fn path_if_desired_lib(cratename: &str, path: &Path, cargo_toml: &toml::Value) -> Option<PathBuf> {
    let parent = otry!(path.parent());

    // is it this lib?  (e.g. you're searching from tests to find the main library crate)
    let package_name = otry!(cargo_toml.get("package")
        .and_then(|package| package.as_table())
        .and_then(|package| package.get("name"))
        .and_then(|name| name.as_str()));

    let mut lib_name = package_name.replace('-', "_");
    let mut lib_path = parent.join("src").join("lib.rs");

    if let Some(&toml::Value::Table(ref t)) = cargo_toml.get("lib") {
        if let Some(&toml::Value::String(ref name)) = t.get("name") {
            lib_name = name.clone();
        }

        if let Some(&toml::Value::String(ref pathstr)) = t.get("path") {
            let p = Path::new(pathstr);
            lib_path = parent.join(p);
        }
    }

    if lib_name == cratename {
        debug!("found {} as lib entry in Cargo.toml", cratename);

        if fs::metadata(&lib_path).ok().map_or(false, |m| m.is_file()) {
            return Some(lib_path);
        }
    }

    None
}

/// Find the main package file for a given crate, given a Cargo.toml file location.
fn find_src_via_tomlfile(cratename: &str, cargotomlfile: &Path) -> Option<PathBuf> {
    trace!("find_src_via_tomlfile looking for {}", cratename);
    // only look for 'path' references here.
    // We find the git and crates.io stuff via the lockfile
    let table = otry!(parse_toml_file(cargotomlfile));

    // check if current cargo file is for requested library
    if let Some(lib_path) = path_if_desired_lib(cratename, cargotomlfile, &table) {
        return Some(lib_path);
    }

    // otherwise search the dependencies
    let local_packages = get_local_packages(&table, cargotomlfile, "dependencies").unwrap_or_default();
    let local_packages_dev = get_local_packages(&table, cargotomlfile, "dev-dependencies").unwrap_or_default();

    // if no dependencies are found
    if local_packages.is_empty() && local_packages_dev.is_empty() {
        trace!("find_src_via_tomlfile didn't find local packages");
        return None;
    }

    debug!("find_src_via_tomlfile found local packages: {:?}", local_packages);
    debug!("find_src_via_tomlfile found local packages dev: {:?}", local_packages_dev);

    for package in local_packages.into_iter().chain(local_packages_dev) {
        if let Some(package_source) = package.source {
            if let Some(tomlfile) = find_cargo_tomlfile(package_source) {
                let package_name = get_package_name(&tomlfile);

                debug!("find_src_via_tomlfile package_name: {}", package_name);

                if package_name == cratename {
                    return find_src_via_tomlfile(cratename, &tomlfile)
                }
            }
        }
    }
    None
}

/// Find dependencies given a Cargo.toml section (dependencies or dev-dependencies).
fn get_local_packages(table: &toml::Value, cargotomlfile: &Path, section_name: &str) -> Option<Vec<PackageInfo>> {
    debug!("get_local_packages found table {:?}; \
            getting packages for section '{}'", table, section_name);

    let dep_table = match table.get(section_name) {
        Some(&toml::Value::Table(ref t)) => t,
        _ => {
            trace!("get_local_packages didn't find section {}", section_name);
            return None
        }
    };

    let mut result = Vec::new();
    let parent = otry!(cargotomlfile.parent());

    for (package_name, value) in dep_table {
        let mut package_version = None;

        let package_source = match *value {
            toml::Value::Table(ref t) => {
                if let Some(relative_path) = getstr(t, "path") {
                    // TODO: this path isn't necessarily correct
                    Some(parent.join(relative_path).join("src").join("lib.rs"))
                } else {
                    // TODO: can also contain "git" references checked out
                    // in CARGO_HOME
                    continue
                }
            },
            toml::Value::String(ref version) => {
                // versioned crate
                package_version = Some(version.to_owned());
                get_versioned_cratefile(package_name, version)
            },
            _ => {
                trace!("get_local_packages couldn't find package_source for {}", package_name);
                continue
            },
        };

        result.push(PackageInfo {
            name: package_name.to_owned(),
            version: package_version,
            source: package_source,
        });
    }

    Some(result)
}

/// Find all directories with unpacked sources from crates.io.
fn find_cratesio_src_dirs(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in vectry!(fs::read_dir(dir)) {
        let path = vectry!(entry).path();
        if path.is_dir() {
            if path.file_name()
                .and_then(|s| s.to_str())
                .map_or(false, |fname| fname.starts_with("github.com-"))
            {
                out.push(path);
            }
        }
    }
    out
}

/// Find a checked out Git repository under CARGO_HOME.
fn find_git_src_dir(name: &str, sha1: &str, branch: Option<&str>) -> Option<PathBuf> {
    let mut dir = otry!(get_cargo_rootdir());
    dir.push("git");
    dir.push("checkouts");
    for entry in otry2!(fs::read_dir(&dir)) {
        let mut dir = otry2!(entry).path();
        if !dir.is_dir() {
            continue;
        }
        if dir.file_name().and_then(|s| s.to_str()).map_or(false, |fname| fname.starts_with(name)) {
            // dirname can be the sha1 or master.
            dir.push(sha1);

            if !dir.is_dir() {
                dir.pop();
                dir.push(&sha1[0..7]);
            }

            if !dir.is_dir() && branch.is_some() {
                dir.pop();
                dir.push(branch.unwrap());
            }

            if !dir.is_dir() {
                dir.pop();
                dir.push("master");
            }

            // check that the checkout matches the commit sha1
            let mut check_file = dir.clone();
            check_file.push(".git");
            check_file.push("refs");
            check_file.push("heads");
            check_file.push("master");

            let mut headref = String::new();
            // TODO: use fs::read_to_string when available
            otry2!(otry2!(File::open(&check_file)).read_to_string(&mut headref));

            debug!("git headref is {:?}", headref);

            if sha1 == headref.trim_right() {
                return Some(dir);
            }
        }
    }
    None
}

/// Get and clone a string from a TOML table.
fn getstr(t: &toml::value::Table, k: &str) -> Option<String> {
    match t.get(k) {
        Some(&toml::Value::String(ref s)) => Some(s.clone()),
        _ => None
    }
}

/// Find Cargo.toml in crate root by traversing up the directory tree searching.
///
/// Any directory that contains a Cargo.toml is considered to be a crate root.
/// If the provided `path` contains a Cargo.toml, that path is returned.
fn find_cargo_tomlfile<P>(path: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    let mut path = path.into();
    path.push("Cargo.toml");
    if path.exists() {
        Some(path)
    } else if path.pop() && path.pop() {
        find_cargo_tomlfile(path)
    } else {
        None
    }
}

/// Find workspace root by traversing up the directory tree
///
/// Checks every Cargo.toml found while searching up the directory tree to see if it has a
/// [workspace] section in its top level.
fn find_workspace_root<P>(path: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    let mut path = path.into();
    path.push("Cargo.toml");
    trace!("find_workspace_root checking {:?}", path);
    if path.exists() &&
       parse_toml_file(&path).as_ref().and_then(|toml| toml.get("workspace")).is_some() {
        trace!("find_workspace_root found workspace section in {:?}", path);
        path.pop();
        Some(path)
    } else if path.pop() && path.pop() {
        // If we haven't found the root and there is still more to search, discard the current
        // Cargo.toml, move up a directory, and try again.
        find_workspace_root(path)
    } else {
        None
    }
}

/// Retrieve the Cargo override paths from a TOML config file at the given path.
fn get_override_paths(path: &Path) -> Vec<String> {
    parse_toml_file(path)
        .as_ref()
        .and_then(|config| config.get("paths"))
        .and_then(|paths| paths.as_array())
        .map(|paths| paths.iter()
                          .filter_map(|v| v.as_str())
                          .map(String::from)
                          .collect())
        .unwrap_or_default()
}

/// An iterator yielding PathBuf for cargo override files
///
/// The iterator starts in the provided path and works its way up to the root
/// directory.
struct CargoOverrides(pub PathBuf);

impl Iterator for CargoOverrides {
    type Item = PathBuf;

    fn next(&mut self) -> Option<PathBuf> {
        // Is this the end?
        if self.0.file_name().is_none() {
            trace!("file_name() is none");
            return None;
        }

        // Append .cargo/config to current path
        let mut path = self.0.clone();
        path.push(".cargo");
        path.push("config");

        // Traverse current path up a level for `next()`
        self.0.pop();

        trace!("trying path={:?}", path);

        if path.exists() {
            Some(path)
        } else {
            self.next()
        }
    }
}

/// Attempt to resolve `crate_name` as an override in `path`'s package
///
/// This resolves the crate root for `path`, loads any overrides in
/// `.cargo/config`, and checks if any of those paths are a crate matching
/// `crate_name`.
///
/// The path to `crate_name`'s library root will be returned if a match is
/// found.
fn get_crate_file_from_overrides<P>(cratename: &str, path: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    let path = path.into();
    trace!("get_crate_file_from_overrides; cratename={:?}, path={:?}", cratename, path);

    // For each .cargo/config file
    for cargo_config in CargoOverrides(path) {
        // For each path in .cargo/config `paths`
        for override_path in get_override_paths(&cargo_config) {
            trace!("examining override_path={:?}", override_path);
            let mut path = PathBuf::from(override_path);

            // If path is relative, convert it to an absolute path relative to
            // cargo_config parent directory.
            if path.is_relative() {
                // the cargo config path is minimally /.cargo/config, so
                // popping parent() twice should always work (hence the unwraps
                // are fine).
                let mut tmp = cargo_config
                    .parent().expect("config in path")
                    .parent().expect(".cargo in path")
                    .to_path_buf();

                tmp.push(path);
                path = tmp;
            }

            // Continue to next override if current directory doesn't contain
            // a Cargo.toml.
            path.push("Cargo.toml");
            if !path.exists() {
                trace!("override_path={:?} does not have Cargo.toml", path);
                continue;
            }

            // Read cargo file and see if it's the crate we need.
            if let Some(toml) = parse_toml_file(path.as_path()) {
                if let Some(lib_path) = path_if_desired_lib(cratename, path.as_path(), &toml) {
                    return Some(lib_path);
                } else {
                    trace!("not desired lib");
                }
            } else {
                trace!("failed parsing toml");
            }
        }
    }

    None
}

/// Main public entry point: Retrieve the main crate file for a given crate,
/// in the context of the crate at from_path.
pub fn get_crate_file(cratename: &str, from_path: &Path) -> Option<PathBuf> {
    debug!("get_crate_file: from_path={:?}", from_path);

    let from_path = from_path.canonicalize().unwrap_or_else(|_| from_path.into());

    if let Some(src) = get_crate_file_from_overrides(cratename, &from_path) {
        return Some(src);
    }

    if let Some(cargotomlfile) = find_cargo_tomlfile(&from_path) {
        // look in the lockfile first, if there is one
        // also search workspaces for a lockfile
        trace!("get_crate_file tomlfile is {:?}", cargotomlfile);

        let mut lockfile = find_workspace_root(&from_path).unwrap_or_else(|| {
            let mut dir = cargotomlfile.clone();
            dir.pop();
            dir
        });
        lockfile.push("Cargo.lock");

        if lockfile.exists() {
            if let Some(f) = find_src_via_lockfile(cratename, &lockfile) {
                return Some(f);
            }
        } else {
            trace!("did not find lock file at {:?}", lockfile);
        }

        // oh, no luck with the lockfile. Try the tomlfile
        return find_src_via_tomlfile(cratename, &cargotomlfile);
    }
    None
}

#[cfg(test)]
mod tests {
    extern crate env_logger;
    use std::path::PathBuf;

    #[test]
    fn get_crate_file_from_overrides() {
        let _ = env_logger::init();

        let mut start_from = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        start_from.push("fixtures");

        let mut expected = start_from.clone();
        expected.push("arst");
        expected.push("src");
        expected.push("lib.rs");

        let actual = super::get_crate_file_from_overrides("arst", start_from)
            .expect("finds arst/src/lib.rs");
        assert_eq!(actual.canonicalize().expect("canonicalize path"),
         expected.canonicalize().expect("canonicalize path"));
    }

    #[test]
    fn gets_repository_name_from_source() {
        let source = "git+https://github.com/phildawes/racer.git?branch=dev#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
        let repository_name = super::get_repository_name_from_source(source);
        assert_eq!(repository_name, Some("racer"));
    }

    #[test]
    fn gets_branch_from_git_source_with_hash() {
        let source = "git+https://github.com/phildawes/racer.git?branch=dev#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
        let branch = super::get_branch_from_source(source);
        assert_eq!(branch, Some("dev"));
    }

    #[test]
    fn gets_branch_from_git_source_without_hash() {
        let source = "git+https://github.com/phildawes/racer.git?branch=dev";
        let branch = super::get_branch_from_source(source);
        assert_eq!(branch, Some("dev"));
    }

    #[test]
    fn empty_if_no_branch() {
        let source = "git+https://github.com/phildawes/racer.git#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
        let branch = super::get_branch_from_source(source);
        assert_eq!(branch, None);
    }
}
