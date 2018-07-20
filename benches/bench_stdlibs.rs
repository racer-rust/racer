#![feature(test)]
extern crate racer_testutils;
extern crate test;
use test::Bencher;

use racer_testutils::*;

#[bench]
fn completes_hashmap(b: &mut Bencher) {
    let src = r"
    use std::collections::HashM~
";
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn completes_methods_for_vec(b: &mut Bencher) {
    let src = r"
    let vec = Vec::new();
    let a = vec.~
";
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn completes_methods_for_file_open(b: &mut Bencher) {
    let src = r#"
    use std::io;
    use std::io::prelude::*;
    use std::fs::File;
    let mut f = File::open("no-file-here"):
    f.~
"#;
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn follows_std(b: &mut Bencher) {
    let src = r#"
    use std::~
"#;
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}

#[bench]
fn follows_collections(b: &mut Bencher) {
    let src = r#"
    use std::collections::~
"#;
    let mut var = vec![];
    b.iter(|| {
        var = get_all_completions(src, None);
    })
}
