#[feature(managed_boxes)];   // need this to use libsyntax
#[feature(phase)];
#[allow(dead_code)];
#[allow(unused_imports)];
#[phase(syntax, link)] extern crate log;
extern crate syntax;

use racer::complete_from_file;
use std::io::File;
use std::task;

mod racer;

fn tmpname() -> Path {
    let mut s = ~"";
    task::with_task_name(|name| s = name.unwrap().to_owned());
    return Path::new("tmpfile."+s);
}

fn write_file(tmppath:&Path, s : &str) {
    let mut f = File::create(tmppath);
    f.write(s.as_bytes()).unwrap();
    f.flush().unwrap();
}

fn remove_file(tmppath:&Path) {
    std::io::fs::unlink(tmppath).unwrap();
}

#[test]
fn matches_mod() {
    let src="
mod apple
fn main() {
    let b = ap
}";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 4, 14, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"apple");    
}

#[test]
fn matches_local_scope_let(){
    let src="
fn main() {
    let apple = 35;
    let b = ap
}";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 4, 14, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
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
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 5, 18, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
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
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 8, 9, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"first");
}
