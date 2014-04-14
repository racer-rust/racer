#![feature(managed_boxes,phase)]   // need this to use libsyntax
#![allow(dead_code,unused_imports,dead_code,attribute_usage,unused_variable)]
#[phase(syntax, link)] extern crate log;
extern crate syntax;

use racer::complete_from_file;
use racer::find_definition;
use std::io::File;
use std::task;

mod racer;
mod scopes;
mod resolve; 

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
fn completes_mod() {
    let src="
mod apple
fn main() {
    let b = ap
}";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    let pos = scopes::coords_to_point(src, 4, 14);
    complete_from_file(src, &path, pos, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"apple");    
}

#[test]
fn completes_local_scope_let(){
    let src="
fn main() {
    let apple = 35;
    let b = ap
}";
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 4, 14);
    let got = racer::first_match(|m| complete_from_file(src, &path, pos, m)).unwrap();
    remove_file(&path);
    assert_eq!(got.matchstr,~"apple");
    assert_eq!(got.point, 21);
}

#[test]
fn completes_via_parent_scope_let(){
    let src="
fn main() {
    let mut apple = 35;
    if foo {
        let b = ap
    }
}";
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 5, 18);
    let got = racer::first_match(|m| complete_from_file(src, &path, pos,m)).unwrap();
    remove_file(&path);
    assert_eq!(got.matchstr,~"apple");
    assert_eq!(got.point,25);
}

#[test]
fn follows_use() {
    let src2="
fn myfn() {
}
fn foo() {}
";
    let src="
use src2::{foo,myfn};

fn main() {
    myfn();
}
";
    write_file(&Path::new("src2.rs"), src2);
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 5, 6);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,~"myfn");
}

#[test]
fn completes_struct_field_via_assignment() {
    let src="
    struct Point {
        first: f64,
        second: f64
    } 

    let var = Point {first: 35, second: 22};
    var.f
";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = ~"NOTHING";
    let pos = scopes::coords_to_point(src, 8, 9);
    complete_from_file(src, &path, pos, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"first");
}
