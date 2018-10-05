extern crate racer_cargo_metadata;
extern crate serde_json;
use racer_cargo_metadata::{mapping::PackageMap, metadata::Metadata};
use std::collections::BTreeSet;
use std::fs::File;
use std::io::prelude::*;

#[test]
fn full() {
    let mut file = File::open("test-data-full.json").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let meta: Metadata = serde_json::from_str(&buf).unwrap();
    assert!(meta.resolve.is_some());
    let pkg_map = PackageMap::from_metadata(meta);
    let regex = pkg_map.ids().find(|id| id.name() == "regex").unwrap();
    assert!(
        pkg_map
            .get_src_path_from_libname(pkg_map.id_to_idx(regex).unwrap(), "memchr")
            .is_some()
    );
}

#[test]
fn no_deps() {
    let mut file = File::open("test-data-no-deps.json").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let meta: Metadata = serde_json::from_str(&buf).unwrap();
    let packages: BTreeSet<_> = meta
        .packages
        .iter()
        .map(|p| p.id.name().to_owned())
        .collect();
    assert_eq!(
        packages,
        meta.workspace_members
            .iter()
            .map(|p| p.name().to_string())
            .collect()
    );
    assert!(meta.resolve.is_none());
}
