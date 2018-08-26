extern crate racer_cargo_metadata;

use racer_cargo_metadata::find_manifest;
use std::path::PathBuf;

fn manifest_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

#[test]
fn exact() {
    let path = manifest_dir().join("Cargo.toml");
    assert_eq!(find_manifest(&path).unwrap(), path);
}

#[test]
fn from_src() {
    let path = manifest_dir().join("src").join("lib.rs");
    assert_eq!(
        find_manifest(&path).unwrap(),
        manifest_dir().join("Cargo.toml")
    );
}

#[test]
fn from_dir() {
    let path = manifest_dir().join("src");
    assert_eq!(
        find_manifest(&path).unwrap(),
        manifest_dir().join("Cargo.toml")
    );
}

#[test]
fn same_dir() {
    let path = manifest_dir().join("foo.txt");
    assert_eq!(
        find_manifest(&path).unwrap(),
        manifest_dir().join("Cargo.toml")
    );
}
