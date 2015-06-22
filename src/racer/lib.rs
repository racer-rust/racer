#![cfg_attr(all(test, feature = "nightly"), feature(test))] // we only need test feature when testing

#[macro_use] extern crate log;

extern crate syntex_syntax;
extern crate toml;
extern crate env_logger;

pub mod core;
pub mod scopes;
pub mod ast;
pub mod typeinf;
pub mod nameres;
pub mod codeiter;
pub mod codecleaner;
pub mod testutils;
pub mod util;
pub mod matchers;
pub mod snippets;
pub mod cargo;

