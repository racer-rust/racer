#[cfg(feature = "nightly")]
mod racer_bench {
    extern crate test;

    use racer::codecleaner::code_chunks;
    use racer::codeiter::iter_stmts;
    use racer::scopes::{mask_comments, mask_sub_scopes};

    use self::test::Bencher;
    use std::env::var;
    use std::io::Read;
    use std::fs::File;
    use std::path::PathBuf;

    fn get_rust_file_str(path: &[&str]) -> String {
        let mut src_path = match var("RUST_SRC_PATH") {
            Ok(env) => { PathBuf::from(&env) },
            _ => panic!("Cannot find $RUST_SRC_PATH")
        };
        for &s in path.iter() { src_path.push(s); }

        let mut s = String::new();
        File::open(&src_path).unwrap().read_to_string(&mut s).unwrap();
        s
    }

    #[bench]
    fn bench_code_chunks(b: &mut Bencher) {
        let src = &get_rust_file_str(&["libcollections", "bit.rs"]);
        b.iter(|| {
            test::black_box(code_chunks(src).collect::<Vec<_>>());
        });
    }

    #[bench]
    fn bench_iter_stmts(b: &mut Bencher) {
        let src = &get_rust_file_str(&["libcollections", "bit.rs"]);
        b.iter(|| {
            test::black_box(iter_stmts(src).collect::<Vec<_>>());
        });
    }

    #[bench]
    fn bench_mask_comments(b: &mut Bencher) {
        let src = &get_rust_file_str(&["libcollections", "bit.rs"]);
        b.iter(|| {
            test::black_box(mask_comments(src));
        });
    }

    #[bench]
    fn bench_mask_sub_scopes(b: &mut Bencher) {
        let src = &get_rust_file_str(&["libcollections", "bit.rs"]);
        b.iter(|| {
            test::black_box(mask_sub_scopes(src));
        });
    }
}
