#[allow(dead_code)];
#[allow(unused_imports)];

use racer::complete_from_file;
use std::io::File;

mod racer;

fn write_file(fname : &str, s : &str) {
    let tmppath = Path::new(fname);
    let mut f = File::create(&tmppath);
    f.write(s.as_bytes()).unwrap();
    f.flush().unwrap();
}

fn remove_file(fname : &str) {
    let tmppath = Path::new(fname);
    std::io::fs::unlink(&tmppath).unwrap();
}

#[test]
fn matches_mod() {
    let src="
mod apple
fn main() {
    let b = ap
}";
    let fname = "test.file2";
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 4, 14, &|s,_,_,_| got=s.to_owned());
    remove_file(fname);
    assert_eq!(got,~"apple");    
}

#[test]
fn matches_local_scope_let(){
    let src="
fn main() {
    let apple = 35;
    let b = ap
}";
    let fname = "test.file1";
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 4, 14, &|s,_,_,_| got=s.to_owned());
    remove_file(fname);
    assert_eq!(got,~"apple");
}

#[test]
fn matches_parent_scope_let(){
    let src="
fn main() {
    let apple = 35;
    if foo {
        let b = ap
    }
}";
    let fname = "test.file3";
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 5, 18, &|s,_,_,_| {
         got=s.to_owned()
    });
    remove_file(fname);
    assert_eq!(got,~"apple");
}
