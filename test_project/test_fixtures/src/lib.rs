//! This is a test project for racer.
//!
//! # Example:
//! Basic Usage.
//!
//! ```
//! extern crate fixtures;
//! use fixtures::foo;
//! fn main {
//!     println!("Racer")
//! }
//! ```
//!
//! ## Notes:
//! - We should check racer can parse rust doc style comments
//! - and some comments...

extern crate test_crate2;

#[path = "submod/bar.rs"]
pub mod bar;

pub mod foo;

pub use test_crate2::useless_func;
