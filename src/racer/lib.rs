#![feature(iterator_flatten)]
#![feature(slice_get_slice)]
#![cfg_attr(all(feature = "clippy", not(test)), deny(print_stdout))]
#![cfg_attr(feature = "nightly", feature(test))]

#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

extern crate cargo;
extern crate syntax;
#[macro_use]
extern crate derive_more;
extern crate rls_span;

#[macro_use]
mod testutils;

mod core;
mod scopes;
mod ast;
mod typeinf;
mod nameres;
mod util;
mod codeiter;
mod codecleaner;
mod matchers;
mod snippets;
mod fileres;

pub use core::{find_definition, complete_from_file, complete_fully_qualified_name, to_point, to_coords};
pub use snippets::snippet_for_match;
pub use core::{Match, MatchType, PathSearch};
pub use core::{FileCache, Session, Coordinate, Location, FileLoader, Point, SourceByteRange};
pub use util::expand_ident;

pub use util::{RustSrcPathError, get_rust_src_path};

#[cfg(all(feature = "nightly", test))]
mod benches;
