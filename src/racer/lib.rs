#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![cfg_attr(feature = "clippy", allow(clippy))]
#![cfg_attr(all(feature = "clippy", not(test)), deny(print_stdout))]

#[macro_use]
extern crate log;

extern crate env_logger;
extern crate syntex_errors;
extern crate syntex_syntax;
extern crate toml;
extern crate typed_arena;

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
mod cargo;

pub use core::{complete_from_file, complete_fully_qualified_name, find_definition, to_coords,
               to_point};
pub use snippets::snippet_for_match;
pub use core::{Match, MatchType, PathSearch};
pub use core::{Coordinate, FileCache, FileLoader, Location, Point, Session, SourceByteRange};
pub use util::expand_ident;

pub use util::{check_rust_src_env_var, RustSrcPathError};
