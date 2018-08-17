//! for #844
#![feature(test)]
extern crate racer_testutils;
extern crate test;
use test::Bencher;

use racer_testutils::*;

#[bench]
fn glob_imports5(b: &mut Bencher) {
    let src = r"
use a::*;
use b::*;
use c::*;
use d::*;
use e::*;
pub fn foo() -> () {
    Whatever::~
}
";
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn glob_imports6(b: &mut Bencher) {
    let src = r"
use a::*;
use b::*;
use c::*;
use d::*;
use e::*;
use f::*;
pub fn foo() -> () {
    Whatever::~
}
";
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn glob_imports7(b: &mut Bencher) {
    let src = r"
use a::*;
use b::*;
use c::*;
use d::*;
use e::*;
use f::*;
use g::*;
pub fn foo() -> () {
    Whatever::~
}
";
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}
