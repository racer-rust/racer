extern crate test;

use std::io::fs::PathExtensions;
use std::os::getenv;
use racer::codecleaner::code_chunks;
use std::io::File;
use self::test::Bencher;

#[bench]
fn bench_code_chunks(b: &mut Bencher) {
    
    let mut src_path = Path::new(&getenv("RUST_SRC_PATH").unwrap()[]);
    src_path.push("libcollections");
    src_path.push("bit.rs");
    
    let src = &File::open(&src_path).read_to_string().unwrap()[];
    
    b.iter(|| {
        let chunks = code_chunks(src).collect::<Vec<_>>();
    });
}


