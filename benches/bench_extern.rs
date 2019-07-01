#![feature(test)]

extern crate test;
use test::Bencher;

use racer_testutils::*;

#[bench]
fn completes_rand(b: &mut Bencher) {
    let src = "
    extern crate rand;
    use rand::{Rng, thread_rng};
    fn main() {
        let mut rng: Box<Rng> = Box::new(thread_rng());
        rng.gen_rang~
    }
    ";
    let mut match_ = None;
    b.iter(|| {
        with_test_project(|dir| {
            let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
            match_ = Some(get_only_completion(src, Some(src_dir)));
        });
    })
}

#[bench]
fn completes_rayon(b: &mut Bencher) {
    let src = "
    extern crate rayon;
    extern crate rand;
    use rand::{Rng, thread_rng};
    fn main() {
        rayon::colle~
    }
    ";
    let mut match_ = None;
    b.iter(|| {
        with_test_project(|dir| {
            let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
            match_ = Some(get_only_completion(src, Some(src_dir)));
        });
    })
}
