extern crate test;

use std::env::var_string;
use std::old_io::File;
use self::test::Bencher;
use racer::codecleaner::code_chunks;
use racer::codeiter::iter_stmts;
use racer::scopes::{mask_comments, mask_sub_scopes};

fn get_rust_file_str(path: &[&str]) -> String {

    let mut src_path = match var_string("RUST_SRC_PATH") {
        Ok(env) => { Path::new(&env[]) },
        _ => panic!("Cannot find $RUST_SRC_PATH")
    };
    for &s in path.iter() { src_path.push(s); }

    File::open(&src_path).read_to_string().unwrap()
}

#[bench]
fn bench_code_chunks(b: &mut Bencher) {
    let src = &get_rust_file_str(&["libcollections", "bit.rs"])[];
    b.iter(|| {
        test::black_box(code_chunks(src).collect::<Vec<_>>());
    });
}

#[bench]
fn bench_iter_stmts(b: &mut Bencher) {
    let src = &get_rust_file_str(&["libcollections", "bit.rs"])[];
    b.iter(|| {
        test::black_box(iter_stmts(src).collect::<Vec<_>>());
    });
}

#[bench]
fn bench_mask_comments(b: &mut Bencher) {
    let src = &get_rust_file_str(&["libcollections", "bit.rs"])[];
    b.iter(|| {
        test::black_box(mask_comments(src));
    });
}

#[bench]
fn bench_mask_sub_scopes(b: &mut Bencher) {
    let src = &get_rust_file_str(&["libcollections", "bit.rs"])[];
    b.iter(|| {
        test::black_box(mask_sub_scopes(src));
    });
}

