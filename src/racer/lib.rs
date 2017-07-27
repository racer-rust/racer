#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

#![cfg_attr(feature = "clippy", allow(clippy))]
#![cfg_attr(all(feature = "clippy", not(test)), deny(print_stdout))]

#[macro_use] extern crate log;

extern crate syntex_syntax;
extern crate syntex_errors;
extern crate toml;
extern crate env_logger;
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

pub use core::{find_definition, complete_from_file, complete_fully_qualified_name, to_point, to_coords};
pub use snippets::snippet_for_match;
pub use core::{Match, MatchType, PathSearch};
pub use core::{FileCache, Session, Coordinate, Location, FileLoader, Point, SourceByteRange};
pub use util::expand_ident;

pub use util::{RustSrcPathError, check_rust_src_env_var};
