extern crate racer_cargo_metadata;
use racer_cargo_metadata::mapping::PackageMap;

use std::path::Path;

#[test]
fn get_self_metadata() {
    let manifest =
        racer_cargo_metadata::find_manifest(Path::new(env!("CARGO_MANIFEST_DIR"))).unwrap();
    let meta = racer_cargo_metadata::run(&manifest, false).unwrap();
    let pkg_map = PackageMap::from_metadata(meta);
    let racer_manifest = manifest
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("Cargo.toml");
    let racer = pkg_map.get_id(&racer_manifest).unwrap();
    assert!(racer.name() == "racer");
    assert!(
        pkg_map
            .get_src_path_from_libname(racer, "lazy_static")
            .is_some()
    );
    assert!(
        pkg_map
            .get_src_path_from_libname(racer, "im-not-a-crate")
            .is_none()
    );
}
