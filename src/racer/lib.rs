#![cfg_attr(feature = "nightly", feature(test))]
#![feature(nll, try_trait)]

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
#[macro_use]
mod util;
mod ast;
mod ast_types;
mod codecleaner;
mod codeiter;
mod core;
mod fileres;
mod matchers;
#[cfg(feature = "metadata")]
mod metadata;
mod nameres;
mod primitive;
mod project_model;
mod scopes;
mod snippets;
mod typeinf;

pub use ast_types::PathSearch;
pub use core::{
    complete_from_file, complete_fully_qualified_name, find_definition, is_use_stmt, to_coords,
    to_point,
};
pub use core::{
    BytePos, ByteRange, Coordinate, FileCache, FileLoader, Location, Match, MatchType, Session,
};
pub use primitive::PrimKind;
pub use project_model::{Edition, ProjectModelProvider};
pub use snippets::snippet_for_match;
pub use util::expand_ident;

pub use util::{get_rust_src_path, RustSrcPathError};

#[cfg(all(feature = "nightly", test))]
mod benches;
