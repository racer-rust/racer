#[allow(dead_code)];
#[allow(unused_imports)];

use racer::complete_from_file;
use std::io::File;
use std::task;

mod racer;

fn tmpname() -> ~str {
    let mut s = ~"";
    task::with_task_name(|name| s = name.unwrap().to_owned());
    return "tmpfile."+s;
}

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
    let fname = tmpname();
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 4, 14, &|m| got=m.matchstr.to_owned());
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
    let fname = tmpname();
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 4, 14, &|m| got=m.matchstr.to_owned());
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
    let fname = tmpname();
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 5, 18, &|m| got=m.matchstr.to_owned());
    remove_file(fname);
    assert_eq!(got,~"apple");
}

#[test]
fn matches_fields() {
    let src="
    struct Point {
        first: f64,
        second: f64
    } 

    let var = Point {35, 22};
    var.f
";
    let fname = tmpname();
    write_file(fname, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(fname, 8, 9, &|m| got=m.matchstr.to_owned());
    remove_file(fname);
    assert_eq!(got,~"first");
}
