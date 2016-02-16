use std::fs::File;
use std::io::Read;
use std::env;
use std::path::{Path,PathBuf};
use std::collections::BTreeMap;
use util::{path_exists, is_dir};
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
            match branch.find("#") {
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

#[test]
fn gets_branch_from_git_source_with_hash() {
    let source = "git+https://github.com/phildawes/racer.git?branch=dev#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1".to_owned();
    let branch = get_branch_from_source(&source);
    assert_eq!(branch, Some("dev"));
}

#[test]
fn gets_branch_from_git_source_without_hash() {
    let source = "git+https://github.com/phildawes/racer.git?branch=dev".to_owned();
    let branch = get_branch_from_source(&source);
    assert_eq!(branch, Some("dev"));
}

#[test]
fn empty_if_no_branch() {
    let source = "git+https://github.com/phildawes/racer.git#9e04f91f0426c1cf8ec5e5023f74d7261f5a9dd1".to_owned();
    let branch = get_branch_from_source(&source);
    assert_eq!(branch, None);
}

fn find_src_via_lockfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    if let Some(packages) = get_cargo_packages(cargofile) {
        for package in packages {
            if let Some(package_source) = package.source.clone() {
                if let Some(tomlfile) = find_cargo_tomlfile(package_source.as_path()) {
                    let package_name = get_package_name(tomlfile.as_path());

                    debug!("find_src_via_lockfile package_name: {}", package_name);

                    if package_name == kratename {
                        return package.source;
                    }
                }
            }
        }
    }
    None
}

fn parse_toml_file(toml_file: &Path) -> Option<BTreeMap<String, toml::Value>> {
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
        if let &toml::Value::Table(ref package_table) = package_element {
            if let Some(&toml::Value::String(ref package_name)) = package_table.get("name") {
                let package_version = otry!(getstr(package_table, "version"));
                let package_source = otry!(getstr(package_table, "source"));

                let package_source = match package_source.split("+").nth(0) {
                    Some("registry") => {
                        get_versioned_cratefile(package_name, &package_version, cargofile)
                    },
                    Some("git") => {
                        let sha1 = otry!(package_source.split("#").last());
                        let mut d = otry!(get_cargo_rootdir(cargofile));
                        let branch = get_branch_from_source(&package_source);
                        d.push("git");
                        d.push("checkouts");
                        d = otry!(find_git_src_dir(d, package_name, &sha1, branch));
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
    let lock_table = parse_toml_file(cargofile).unwrap();

    debug!("get_package_name found lock_table {:?}", lock_table);

    let mut name = String::new();

    if let Some(&toml::Value::Table(ref lib_table)) = lock_table.get("lib") {
        if let Some(&toml::Value::String(ref package_name)) = lib_table.get("name") {
            name.push_str(package_name);
            return name;
        }
    }

    if let Some(&toml::Value::Table(ref package_table)) = lock_table.get("package") {
        if let Some(&toml::Value::String(ref package_name)) = package_table.get("name") {
            name.push_str(package_name);
        }
    }

    name
}

fn get_cargo_rootdir(cargofile: &Path) -> Option<PathBuf> {
    debug!("get_cargo_rootdir. {:?}",cargofile);
    match env::var_os("CARGO_HOME") {
        Some(cargohome) =>
        {
            debug!("get_cargo_rootdir. CARGO_HOME is set: {:?}",cargohome);
            let d = PathBuf::from(cargohome);
            if path_exists(&d) {
                return Some(d)
            } else {
                return None
            };
        },
        None => ()
    };

    let mut d = otry!(env::home_dir());

    // try multirust first, since people with multirust installed will often still 
    // have an old .cargo directory lying around
    d.push(".multirust");

    d.push("overrides");
    debug!("get_cargo_rootdir: looking for overrides file. {:?}", d);
    if let Ok(mut multirust_overides) = File::open(&d) {
        debug!("get_cargo_rootdir: found overrides file! {:?}", d);
        let mut s = String::new();
        otry2!(multirust_overides.read_to_string(&mut s));
        for line in s.lines() {
            let overridepath = line.split(";").nth(0).unwrap();
            if cargofile.starts_with(overridepath) {
                let overridepath = line.split(";").nth(1).unwrap();
                d.pop();
                d.push("toolchains");
                d.push(overridepath.trim());
                d.push("cargo");
                debug!("get_cargo_rootdir override root is {:?}",d);
                return Some(d)
            }
        }
    }

    d.pop();
    d.push("default");
    if let Ok(mut multirustdefault) = File::open(&d) {
        let mut s = String::new();
        otry2!(multirustdefault.read_to_string(&mut s));
        d.pop();
        d.push("toolchains");
        d.push(s.trim());
        d.push("cargo");
        debug!("get_cargo_rootdir root is {:?}",d);
        return Some(d)
    }

    d.pop();
    d.pop();
    d.push(".cargo");
    if path_exists(&d) {
        Some(d)
    } else {
        None
    }
}

fn get_versioned_cratefile(kratename: &str, version: &str, cargofile: &Path) -> Option<PathBuf> {
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
            d.push(kratename.to_owned() + "-" + &version);
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
            // It doesn't exist, so try /lib.rs
            d.pop();
            d.pop();
            d.push("lib.rs");
        }

        if let Err(_) = File::open(&d) {
            continue;
        }

        return Some(d)
    }
    None
 }

fn find_src_via_tomlfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    // only look for 'path' references here.
    // We find the git and crates.io stuff via the lockfile
    let table = parse_toml_file(cargofile).unwrap();

    // is it this lib?  (e.g. you're searching from tests to find the main library crate)
    {
        let package_name = if let Some(&toml::Value::Table(ref t)) = table.get("package") {
            if let Some(&toml::Value::String(ref name)) = t.get("name") {
                name
            } else {
                // it's invalid for a package to be nameless anyway
                return None;
            }
        } else {
            return None;
        };

        let mut lib_name = package_name;
        let mut lib_path = otry!(cargofile.parent()).join("src").join("lib.rs");
        if let Some(&toml::Value::Table(ref t)) = table.get("lib") {
            if let Some(&toml::Value::String(ref name)) = t.get("name") {
                lib_name = name;
            }
            if let Some(&toml::Value::String(ref pathstr)) = t.get("path") {
                let p = Path::new(pathstr);
                lib_path = otry!(cargofile.parent()).join(p);
            }
        }

        if lib_name == kratename {
            debug!("found {} as lib entry in Cargo.toml", kratename);
            if ::std::fs::metadata(&lib_path).ok().map(|m| m.is_file()).unwrap_or(false) {
                return Some(lib_path);
            }
        }
    }

    // otherwise search the dependencies
    let local_packages = get_local_packages(&table, cargofile, "dependencies").unwrap_or_default();
    let local_packages_dev = get_local_packages(&table, cargofile, "dev-dependencies").unwrap_or_default();

    // if no dependencies are found
    if local_packages.is_empty() && local_packages_dev.is_empty() {
        return None;
    }

    debug!("find_src_via_tomlfile found local packages: {:?}", local_packages);
    debug!("find_src_via_tomlfile found local packages dev: {:?}", local_packages_dev);

    for package in local_packages.iter().chain(local_packages_dev.iter()) {
        if let Some(package_source) = package.source.clone() {
            if let Some(tomlfile) = find_cargo_tomlfile(package_source.as_path()) {
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
    debug!("get_local_packages found table {:?}", table);

    let t = match table.get(section_name) {
        Some(&toml::Value::Table(ref t)) => t,
        _ => return None
    };

    let mut result = Vec::new();

    for (package_name, value) in t.iter() {
        let mut package_version = None;

        let package_source = match *value {
            toml::Value::Table(ref t) => {
                // local directory
                let relative_path = otry!(getstr(t, "path"));

                Some(otry!(cargofile.parent())
                    .join(relative_path)
                    .join("src")
                    .join("lib.rs"))
                },
            toml::Value::String(ref version) => {
                // versioned crate
                package_version = Some(version.to_owned());
                get_versioned_cratefile(package_name, version, cargofile)
            }
            _ => continue
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
        if is_dir(path.as_path()) {
            if let Some(ref fname) = path.file_name().and_then(|s| s.to_str()) {
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
        if is_dir(path.as_path()) {
            if let Some(ref fname) = path.file_name().and_then(|s| s.to_str()) {
                if fname.starts_with(name) {
                    let mut d = path.clone();

                    // dirname can be the sha1 or master.
                    d.push(sha1);

                    if !is_dir(d.as_path()) && branch.is_some() {
                        d.pop();
                        d.push(branch.unwrap());
                    }

                    if !is_dir(d.as_path()) {
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

                    if headref.ends_with("\n") {
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

fn find_cargo_tomlfile(currentfile: &Path) -> Option<PathBuf> {
    let mut f = currentfile.to_path_buf();
    f.push("Cargo.toml");
    if path_exists(f.as_path()) {
        Some(f)
    } else {
        if f.pop() && f.pop() {
            find_cargo_tomlfile(&f)
        } else {
            None
        }
    }
}

pub fn get_crate_file(kratename: &str, from_path: &Path) -> Option<PathBuf> {
    if let Some(tomlfile) = find_cargo_tomlfile(from_path) {
        // look in the lockfile first, if there is one
        debug!("get_crate_file tomlfile is {:?}", tomlfile);
        let mut lockfile = tomlfile.clone();
        lockfile.pop();
        lockfile.push("Cargo.lock");
        if path_exists(lockfile.as_path()) {
            if let Some(f) = find_src_via_lockfile(kratename, &lockfile) {
                return Some(f);
            }
        }

        // oh, no luck with the lockfile. Try the tomlfile
        return find_src_via_tomlfile(kratename, &tomlfile)
    }
    None
}
