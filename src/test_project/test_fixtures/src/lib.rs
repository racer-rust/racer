//! This is a test project for racer.
//!
//! # Example:
//! Basic Usage.
//!
//! ```
//! extern crate test_fixtures;
//! use test_fixtures::foo;
//! fn main {
//!     println!("Racer")
//! }
//! ```
//!
//! ## Notes:
//! - We should check racer can parse rust doc style comments
//! - and some comments...
pub mod foo;
#[path = "submod/bar.rs"] pub mod bar;
