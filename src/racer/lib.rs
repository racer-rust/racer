#![cfg_attr(feature = "nightly", feature(test))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate bitflags;

extern crate syntax;
#[macro_use]
extern crate derive_more;
extern crate rls_span;

#[macro_use]
mod testutils;

mod ast;
mod codecleaner;
mod codeiter;
mod core;
mod fileres;
mod matchers;
mod nameres;
mod project_model;
mod scopes;
mod snippets;
mod typeinf;
mod util;

pub use core::{
    complete_from_file, complete_fully_qualified_name, find_definition, to_coords, to_point,
};
pub use core::{BytePos, ByteRange, Coordinate, FileCache, FileLoader, Location, Session};
pub use core::{Match, MatchType, PathSearch};
pub use project_model::ProjectModelProvider;
pub use snippets::snippet_for_match;
pub use util::expand_ident;

pub use util::{get_rust_src_path, RustSrcPathError};

#[cfg(all(feature = "nightly", test))]
mod benches;
