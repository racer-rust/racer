use std::fs::File;
use std::io::Read;
use std::env;
use std::path::{Path,PathBuf};
use std::collections::BTreeMap;
use std::fs::{read_dir};
use toml;

#[derive(Debug)]
struct PackageInfo {
    name: String,
    version: Option<String>,
    source: Option<PathBuf>
}

// otry is 'option try'
macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None })
}

// converts errors into None
macro_rules! otry2 {
    ($e:expr) => (match $e { Ok(e) => e, Err(e) => { error!("ERROR!: {:?} {} {}", e, file!(), line!()); return None } })
}

// converts errors into message + empty vec
macro_rules! vectry {
    ($e:expr) => (match $e { Ok(e) => e, Err(e) => { error!("ERROR!: {:?} {} {}", e, file!(), line!()); return Vec::new() } })
}

/// Gets the branch from a git source string if one is present.
fn get_branch_from_source(source: &str) -> Option<&str> {
    debug!("get_branch_from_source - Finding branch from {:?}", source);
    match source.find("branch=") {
        Some(idx) => {
            let (_start, branch) = source.split_at(idx+7);
            match branch.find('#') {
                Some(idx) => {
                    let (branch, _end) = branch.split_at(idx);
                    Some(branch)
                },
                None => Some(branch),
            }
        },
        None => None
    }
}

/// Gets the repository_name from a git source string if one is present.
fn get_repository_name_from_source(source: &str) -> Option<&str> {
    debug!("get_repository_name_from_source - Finding repository name from {:?}", source);
    match source.rfind("/") {
        Some(idx) => {
            let (_, mut repository_name) = source.split_at(idx + 1);
            let mut idx =  repository_name.find("?").unwrap_or_else(|| {
                repository_name.find("#").unwrap_or(0)
            });
            if idx == 0 {
                return None;
            }
            repository_name = &repository_name[0..idx];
            idx = repository_name.find(".git").unwrap_or(0);
            if idx == 0 {
                return Some(repository_name);
            }
            repository_name = &repository_name[0..idx];
            Some(repository_name)
        },
        None => None
    }
}

#[test]
fn gets_repository_name_from_source() {
    let source = "git+https://github.com/phildawes/racer.git?branch=dev#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
    let repository_name = get_repository_name_from_source(source);
    assert_eq!(repository_name, Some("racer"));
}

#[test]
fn gets_branch_from_git_source_with_hash() {
    let source = "git+https://github.com/phildawes/racer.git?branch=dev#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
    let branch = get_branch_from_source(source);
    assert_eq!(branch, Some("dev"));
}

#[test]
fn gets_branch_from_git_source_without_hash() {
    let source = "git+https://github.com/phildawes/racer.git?branch=dev";
    let branch = get_branch_from_source(source);
    assert_eq!(branch, Some("dev"));
}

#[test]
fn empty_if_no_branch() {
    let source = "git+https://github.com/phildawes/racer.git#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1";
    let branch = get_branch_from_source(source);
    assert_eq!(branch, None);
}

fn find_src_via_lockfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    trace!("find_src_via_lockfile searching for {} in {:?}", kratename, cargofile);
    if let Some(packages) = get_cargo_packages(cargofile) {
        trace!("find_src_via_lockfile got packages");
        for package in packages {
            trace!("find_src_via_lockfile examining {:?}", package);
            if let Some(package_source) = package.source {
                trace!("find_src_via_lockfile package_source {:?}", package_source);
                if let Some(tomlfile) = find_cargo_tomlfile(package_source.clone()) {
                    trace!("find_src_via_lockfile tomlfile {:?}", tomlfile);
                    let package_name = get_package_name(tomlfile.as_path());

                    debug!("find_src_via_lockfile package_name: {}", package_name);

                    if package_name == kratename {
                        return Some(package_source);
                    }
                }
            }
        }
    }

    trace!("find_src_via_lockfile returning None");
    None
}

fn parse_toml_file(toml_file: &Path) -> Option<BTreeMap<String, toml::Value>> {
    trace!("parse_toml_file: {:?}", toml_file);
    let mut file = otry2!(File::open(toml_file));
    let mut string = String::new();
    otry2!(file.read_to_string(&mut string));
    let mut parser = toml::Parser::new(&string);

    parser.parse()
}

fn get_cargo_packages(cargofile: &Path) -> Option<Vec<PackageInfo>> {
    let lock_table = parse_toml_file(cargofile).unwrap();

    debug!("get_cargo_packages found lock_table {:?}", lock_table);

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
                        get_versioned_cratefile(package_name, &package_version, cargofile)
                    },
                    Some("git") => {
                        let sha1 = unwrap_or_continue!(package_source.split('#').last());
                        let mut d = unwrap_or_continue!(get_cargo_rootdir(cargofile));
                        let branch = get_branch_from_source(&package_source);
                        d.push("git");
                        d.push("checkouts");

                        //use repository name instead of package name                        
                        let repository_name = get_repository_name_from_source(&package_source);
                        if repository_name.is_some() {
                            d = unwrap_or_continue!(find_git_src_dir(d, repository_name.unwrap(), &sha1, branch));
                        } else {
                            d = unwrap_or_continue!(find_git_src_dir(d, package_name, &sha1, branch));
                        }

                        d.push("src");
                        d.push("lib.rs");

                        Some(d)
                    },
                    _ => return None
                };

                result.push(PackageInfo{
                    name: package_name.to_owned(),
                    version: Some(package_version),
                    source: package_source
                });
            }
        }
    }
    Some(result)
}

fn get_package_name(cargofile: &Path) -> String {
    let lock_table = match parse_toml_file(cargofile) {
        Some(t) => t,
        None => return String::new(),
    };

    debug!("get_package_name found lock_table {:?}", lock_table);

    if let Some(&toml::Value::Table(ref lib_table)) = lock_table.get("lib") {
        if let Some(&toml::Value::String(ref package_name)) = lib_table.get("name") {
            return package_name.clone();
        }
    }

    if let Some(&toml::Value::Table(ref package_table)) = lock_table.get("package") {
        if let Some(&toml::Value::String(ref package_name)) = package_table.get("name") {
            return package_name.replace("-","_");
        }
    }

    String::new()
}

fn get_cargo_rootdir(cargofile: &Path) -> Option<PathBuf> {
    debug!("get_cargo_rootdir. {:?}",cargofile);

    if let Some(cargohome) = env::var_os("CARGO_HOME") {
        debug!("get_cargo_rootdir. CARGO_HOME is set: {:?}",cargohome);

        let d = PathBuf::from(cargohome);

        if d.exists() {
            return Some(d)
        } else {
            return None
        };
    }

    let mut d = otry!(env::home_dir());

    d.push(".cargo");
    if d.exists() {
        Some(d)
    } else {
        None
    }
}

fn get_versioned_cratefile(kratename: &str, version: &str, cargofile: &Path) -> Option<PathBuf> {
    trace!("get_versioned_cratefile searching for {}", kratename);
    let mut d = otry!(get_cargo_rootdir(cargofile));

    debug!("get_versioned_cratefile: cargo rootdir is {:?}",d);
    d.push("registry");
    d.push("src");

    for mut d in find_cratesio_src_dirs(d) {

        // if version=* then search for the first matching folder
        if version == "*" {
            use std::fs::read_dir;
            let mut start_path = d.clone();
            start_path.push(kratename);
            let start_name = start_path.to_str().unwrap();

            if let Ok(reader) = read_dir(d) {
                if let Some(path) = reader
                    .map(|entry| entry.unwrap().path())
                    .find(|path| path.to_str().unwrap().starts_with(start_name)) {
                        d = path.clone();
                    } else {
                        continue;
                    }
            } else {
                continue;
            }
        } else {
            d.push(kratename.to_owned() + "-" + version);
        }

        d.push("src");
        debug!("crate path {:?}",d);

        // First, check for package name at root (src/kratename/lib.rs)
        d.push(kratename);
        d.push("lib.rs");
        if let Err(_) = File::open(&d) {
            // It doesn't exist, so assume src/lib.rs
            d.pop();
            d.pop();
            d.push("lib.rs");
        }
        debug!("crate path with lib.rs {:?}",d);

        if let Err(_) = File::open(&d) {
            trace!("failed to open crate path {:?}", d);
            // It doesn't exist, so try /lib.rs
            d.pop();
            d.pop();
            d.push("lib.rs");
        }

        if let Err(_) = File::open(&d) {
            trace!("failed to open crate path {:?}", d);
            continue;
        }

        return Some(d)
    }
    None
 }

/// Return path to library source if the Cargo.toml name matches crate_name
fn path_if_desired_lib(crate_name: &str, path: &Path, cargo_toml: &toml::Table) -> Option<PathBuf> {
    let parent = otry!(path.parent());

    // is it this lib?  (e.g. you're searching from tests to find the main library crate)
    let package_name = otry!(cargo_toml.get("package")
        .and_then(|package| package.as_table())
        .and_then(|package| package.get("name"))
        .and_then(|name| name.as_str()));

    let mut lib_name = package_name.replace("-", "_");
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

    if lib_name == crate_name {
        debug!("found {} as lib entry in Cargo.toml", crate_name);

        if ::std::fs::metadata(&lib_path).ok().map_or(false, |m| m.is_file()) {
            return Some(lib_path);
        }
    }

    None
}

fn find_src_via_tomlfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    trace!("find_src_via_tomlfile looking for {}", kratename);
    // only look for 'path' references here.
    // We find the git and crates.io stuff via the lockfile
    let table = otry!(parse_toml_file(cargofile));

    // Check if current cargo file is for requested library
    if let Some(lib_path) = path_if_desired_lib(kratename, cargofile, &table) {
        return Some(lib_path);
    }

    // otherwise search the dependencies
    let local_packages = get_local_packages(&table, cargofile, "dependencies").unwrap_or_default();
    let local_packages_dev = get_local_packages(&table, cargofile, "dev-dependencies").unwrap_or_default();

    // if no dependencies are found
    if local_packages.is_empty() && local_packages_dev.is_empty() {
        trace!("find_src_via_tomlfile didn't find local packages");
        return None;
    }

    debug!("find_src_via_tomlfile found local packages: {:?}", local_packages);
    debug!("find_src_via_tomlfile found local packages dev: {:?}", local_packages_dev);

    for package in local_packages.into_iter().chain(local_packages_dev) {
        if let Some(package_source) = package.source {
            if let Some(tomlfile) = find_cargo_tomlfile(package_source.clone()) {
                let package_name = get_package_name(tomlfile.as_path());

                debug!("find_src_via_tomlfile package_name: {}", package_name);

                if package_name == kratename {
                    return find_src_via_tomlfile(kratename, &tomlfile)
                }
            }
        }
    }
    None
}

fn get_local_packages(table: &BTreeMap<String, toml::Value>, cargofile: &Path, section_name: &str) -> Option<Vec<PackageInfo>> {
    debug!("get_local_packages found table {:?};\
           getting packages for section '{}'", table, section_name);

    let t = match table.get(section_name) {
        Some(&toml::Value::Table(ref t)) => t,
        _ => {
            trace!("get_local_packages didn't find section {}", section_name);
            return None
        }
    };

    let mut result = Vec::new();

    let parent = otry!(cargofile.parent());

    for (package_name, value) in t.iter() {
        let mut package_version = None;

        let package_source = match *value {
            toml::Value::Table(ref t) => {
                if let Some(relative_path) = getstr(t, "path") {
                    Some(parent.join(relative_path).join("src").join("lib.rs"))
                } else {
                    continue
                }
            },
            toml::Value::String(ref version) => {
                // versioned crate
                package_version = Some(version.to_owned());
                get_versioned_cratefile(package_name, version, cargofile)
            },
            _ => {
                trace!("get_local_packages couldn't find package_source for {}", package_name);
                continue
            },
        };

        result.push(PackageInfo {
            name: package_name.to_owned(),
            version: package_version,
            source: package_source
        });
    }

    Some(result)
}

fn find_cratesio_src_dirs(d: PathBuf) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for entry in vectry!(read_dir(d)) {
        let path = vectry!(entry).path();
        if path.is_dir() {
            if let Some(fname) = path.file_name().and_then(|s| s.to_str()) {
                if fname.starts_with("github.com-") {
                    out.push(path.clone());
                }
            }
        }
    }
    out
}

fn find_git_src_dir(d: PathBuf, name: &str, sha1: &str, branch: Option<&str>) -> Option<PathBuf> {
    for entry in otry2!(read_dir(d)) {
        let path = otry2!(entry).path();
        if path.is_dir() {
            if let Some(fname) = path.file_name().and_then(|s| s.to_str()) {
                if fname.starts_with(name) {
                    let mut d = path.clone();

                    // dirname can be the sha1 or master.
                    d.push(sha1);

                    if !d.is_dir() {
                        d.pop();
                        d.push(&sha1[0..7]);
                    }

                    if !d.is_dir() && branch.is_some() {
                        d.pop();
                        d.push(branch.unwrap());
                    }

                    if !d.is_dir() {
                        d.pop();
                        d.push("master");
                    }

                    let retval = d.clone();

                    // check that the checkout matches the commit sha1
                    d.push(".git");
                    d.push("refs");
                    d.push("heads");
                    d.push("master");

                    let mut headref = String::new();
                    otry2!(otry2!(File::open(d)).read_to_string(&mut headref));

                    debug!("git headref is {:?}", headref);

                    if headref.ends_with('\n') {
                        headref.pop();
                    }

                    if sha1 == headref {
                        return Some(retval);
                    }
                }
            }
        }
    }
    None
}

fn getstr(t: &toml::Table, k: &str) -> Option<String> {
    match t.get(k) {
        Some(&toml::Value::String(ref s)) => Some(s.clone()),
        _ => None
    }
}

#[inline]
fn find_cargo_tomlfile<P>(file: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    find_next_crate_root(file)
        .map(|mut f| { f.push("Cargo.toml"); f })
}

/// Find crate root by traversing up the directory tree searching
///
/// Any directory that contains a Cargo.toml is considered to be a crate root.
/// If the provided `path` contains a Cargo.toml, that path is returned.
fn find_next_crate_root<P>(path: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    let mut path = path.into();
    path.push("Cargo.toml");
    if path.exists() {
        path.pop();
        Some(path)
    } else if path.pop() && path.pop() {
        find_next_crate_root(path)
    } else {
        None
    }
}

fn get_override_paths(path: &Path) -> Vec<String> {
    macro_rules! vtry {
        ($opt:expr) => {
            match $opt {
                Some(thing) => thing,
                None => return Vec::new(),
            }
        }
    }

    let config = vtry!(parse_toml_file(path));
    let paths = vtry!(config.get("paths"));
    let paths = vtry!(paths.as_slice());

    paths
        .iter()
        .map(|v| v.as_str())
        .filter(|s| s.is_some())
        .map(|s| s.unwrap())
        .map(String::from)
        .collect()
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
fn get_crate_file_from_overrides<P>(crate_name: &str, path: P) -> Option<PathBuf>
    where P: Into<PathBuf>
{
    let path = path.into();
    trace!("get_crate_file_from_overrides; crate_name={:?}, path={:?}", crate_name, path);

    // For each .cargo/config file
    for cargo_config in CargoOverrides(path) {
        // For each path in .cargo/config `paths`
        for override_path in get_override_paths(cargo_config.as_path()).into_iter() {
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
                if let Some(lib_path) = path_if_desired_lib(crate_name, path.as_path(), &toml) {
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

/// Find the library root for kratename
///
/// The library is searched for by checking
///
/// 1. overrides
/// 2. lock file
/// 3. toml file
pub fn get_crate_file(kratename: &str, from_path: &Path) -> Option<PathBuf> {
    debug!("get_crate_file: from_path={:?}", from_path);
    if let Some(src) = get_crate_file_from_overrides(kratename, from_path) {
        return Some(src);
    }

    if let Some(tomlfile) = find_cargo_tomlfile(from_path.to_path_buf()) {
        // look in the lockfile first, if there is one
        trace!("get_crate_file tomlfile is {:?}", tomlfile);
        let mut lockfile = tomlfile.clone();
        lockfile.pop();
        lockfile.push("Cargo.lock");
        if lockfile.exists() {
            if let Some(f) = find_src_via_lockfile(kratename, &lockfile) {
                return Some(f);
            }
        } else {
            trace!("did not find lock file at {:?}", lockfile);
        }

        // oh, no luck with the lockfile. Try the tomlfile
        return find_src_via_tomlfile(kratename, &tomlfile)
    }
    None
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[test]
    fn get_crate_file_from_overrides() {
        let _ = ::env_logger::init();

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
}
