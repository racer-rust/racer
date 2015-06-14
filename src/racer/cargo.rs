use std::fs::File;
use std::io::Read;
use std::env;
use std::path::{Path,PathBuf};
use racer::util::{path_exists, is_dir};
use std::fs::{read_dir};
use toml;

// otry is 'option try'
macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None })
}

// converts errors into None
macro_rules! otry2 {
    ($e:expr) => (match $e { Ok(e) => e, Err(e) => { error!("ERROR!: {:?} {} {}", e, file!(), line!()); return None } })
}

fn find_src_via_lockfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    let mut file = otry2!(File::open(cargofile));
    let mut string = String::new();
    otry2!(file.read_to_string(&mut string));
    let mut parser = toml::Parser::new(&string);
    let lock_table = parser.parse().unwrap();

    debug!("find_src_via_lockfile found lock table {:?}", lock_table);

    let t = match lock_table.get("package") {
        Some(&toml::Value::Array(ref t1)) => t1,
        _ => return None
    };

    for item in t {
        if let &toml::Value::Table(ref t) = item {
            if let Some(&toml::Value::String(ref name)) = t.get("name") {
                if name.replace("-", "_") == kratename {
                    debug!("found matching crate {:?}", t);
                    let version = otry!(getstr(t, "version"));
                    let source = otry!(getstr(t, "source"));

                    if Some("registry") == source.split("+").nth(0) {
                        return get_versioned_cratefile(name, &version);
                    } else if Some("git") == source.split("+").nth(0) {
                        let sha1 = otry!(source.split("#").last());
                        let mut d = otry!(get_cargo_rootdir());
                        d.push("git");
                        d.push("checkouts");
                        d = otry!(find_git_src_dir(d, name, &sha1));
                        d.push("src");
                        d.push("lib.rs");
                        return Some(d);
                    }
                }
            }
        }
    }
    None
}

fn get_cargo_rootdir() -> Option<PathBuf> {
    let mut d = otry!(env::home_dir());
    d.push(".cargo");
    if path_exists(&d) {
        return Some(d);
    }

    // try multirust
    d.pop();
    d.push(".multirust");
    d.push("default");
    if let Ok(mut multirustdefault) = File::open(&d) {
        let mut s = String::new();
        otry2!(multirustdefault.read_to_string(&mut s));
        d.pop();
        d.push("toolchains");
        d.push(s.trim());
        d.push("cargo");
        debug!("get_cargo_rootdir root is {:?}",d);
        Some(d)
    } else {
        None
    }
}

fn get_versioned_cratefile(kratename: &str, version: &str) -> Option<PathBuf> {
    let mut d = otry!(get_cargo_rootdir());
    d.push("registry");
    d.push("src");
    d = otry!(find_cratesio_src_dir(d));

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
                return None;
            }
        } else { return None; }
    } else {
        d.push(kratename.to_string() + "-" + &version);
    }
    
    d.push("src");
    debug!("crate path {:?}",d);

    // First, check for package name at root (src/kratename/lib.rs)
    d.push(kratename.to_string());
    if let Err(_) = File::open(&d) {
        // It doesn't exist, so assume src/lib.rs
        d.pop();
    }
    d.push("lib.rs");
    if let Err(_) = File::open(&d) {
        return None;
    }

    Some(d)
 }

fn find_src_via_tomlfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    // only look for 'path' references here.
    // We find the git and crates.io stuff via the lockfile

    let mut file = otry2!(File::open(cargofile));
    let mut string = String::new();
    otry2!(file.read_to_string(&mut string));
    let mut parser = toml::Parser::new(&string);
    let table = otry!(parser.parse());
    let t = match table.get("dependencies") {
        Some(&toml::Value::Table(ref t)) => t,
        _ => return None
    };

    let mut name = kratename;
    let value = if kratename.contains('_') {
        t.iter().find(|&(k, _)| k.replace("-", "_") == name).map(|(k,v)| {
            name = k;
            v
        })
    } else {
        t.get(kratename)
    };

    match value {
        Some(&toml::Value::Table(ref t)) => {
            // local directory
            let relative_path = otry!(getstr(t, "path"));
            return Some(otry!(cargofile.parent())
                        .join(relative_path)
                        .join("src")
                        .join("lib.rs"));
        },
        Some(&toml::Value::String(ref version)) => {
            // versioned crate
            return get_versioned_cratefile(name, version);
        }
        _ => return None
    }
}

fn find_cratesio_src_dir(d: PathBuf) -> Option<PathBuf> {
    for entry in otry2!(read_dir(d)) {
        let path = otry2!(entry).path();
        if is_dir(path.as_path()) {
            if let Some(ref fname) = path.file_name().and_then(|s| s.to_str()) {
                if fname.starts_with("github.com-") {
                    return Some(path.clone());
                }
            }
        }
    }
    None
}

fn find_git_src_dir(d: PathBuf, name: &str, sha1: &str) -> Option<PathBuf> {
    for entry in otry2!(read_dir(d)) {
        let path = otry2!(entry).path();
        if is_dir(path.as_path()) {
            if let Some(ref fname) = path.file_name().and_then(|s| s.to_str()) {
                if fname.starts_with(name) {
                    let mut d = path.clone();

                    // dirname can be the sha1 or master.
                    d.push(sha1);

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
        return Some(f);
    } else {
        if f.pop() && f.pop() {
            return find_cargo_tomlfile(&f);
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
