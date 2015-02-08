#![feature(collections, core, io, env, path, rustc_private, std_misc)]

#[macro_use] extern crate log;

extern crate syntax;
extern crate collections;
extern crate core;

pub use racer::scopes::{coords_to_point, point_to_coords};
pub use racer::{find_definition, complete_from_file};
pub mod racer;

