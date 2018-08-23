extern crate racer_cargo_metadata;

use std::path::Path;

#[test]
fn get_self_metadata() {
    let path = Path::new("Cargo.toml");
    let meta = racer_cargo_metadata::run(&path, false).unwrap();
    println!("{:?}", meta);
}
