use std::fs::File;
use std::io::Read;
use std::env;
use std::path::{Path,PathBuf};
use std::fs::{PathExt,read_dir};
use toml;

macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None})
}

macro_rules! otry2 {
    ($e:expr) => (match $e { Ok(e) => e, Err(_) => return None})
}


fn find_src_via_lockfile(kratename: &str, cargofile: &Path) -> Option<PathBuf> {
    let mut file = otry2!(File::open(cargofile));
    let mut string = String::new();
    otry2!(file.read_to_string(&mut string));
    let mut parser = toml::Parser::new(&string);
    let lock_table = parser.parse().unwrap();

    debug!("PHIL found lock table {:?}",lock_table);

    let t = match lock_table.get("package") {
        Some(&toml::Value::Array(ref t1)) => t1,
        _ => return None
    };

    for item in t {
        if let &toml::Value::Table(ref t) = item {
            if Some(&toml::Value::String(kratename.to_string())) == t.get("name") {

                let version = otry!(getstr(t, "version"));
                let source = otry!(getstr(t, "source"));

                if Some("registry") == source.split("+").nth(0) {

                    let mut d = otry!(env::home_dir());
                    d.push(".cargo"); 
                    d.push("registry");
                    d.push("src");
                    d = otry!(find_git_dir(d));
                    d.push(kratename.to_string() + "-" + &version);
                    d.push("src");
                    d.push("lib.rs");
                    return Some(d)
                }
            }
        }
    }
    return None;
}

fn find_git_dir(d: PathBuf) -> Option<PathBuf> {
    for entry in otry2!(read_dir(d)) {
        let path = otry2!(entry).path();
        
        // Assume there is only one directory and it's the git one
        if path.is_dir() {
            if let Some(ref fname) = path.file_name().and_then(|s| s.to_str()) {
                println!("PHIL fname is {}",fname);
                if fname.starts_with("git-") { 
                    return Some(path.clone());
                }
            }
        }
    }
    return None;
}

fn getstr(t: &toml::Table, k: &str) -> Option<String> {
    match t.get(k) {
        Some(&toml::Value::String(ref s)) => Some(s.clone()),
        _ => None
    }
}

fn find_cargo_lockfile(currentfile: &Path) -> Option<PathBuf> {
    let mut f = currentfile.to_path_buf();
    f.push("Cargo.lock");
    if f.exists() {
        return Some(f);
    } else {
        if f.pop() && f.pop() {
            return find_cargo_lockfile(&f);
        } else {
            None
        }
    }
}


pub fn get_crate_file(name: &str, from_path: &Path) -> Option<PathBuf> {
    if let Some(lockfile) = find_cargo_lockfile(from_path) {
        return find_src_via_lockfile(name, &lockfile);
    }
    None
}
