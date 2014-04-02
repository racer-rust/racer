#[feature(managed_boxes)];   // need this to use libsyntax
#[feature(phase)];
#[allow(dead_code)];
#[allow(unused_imports)];
#[phase(syntax, link)] extern crate log;
extern crate syntax;

use racer::complete_from_file;
use racer::find_definition;
use std::io::File;
use std::task;

mod racer;
mod scopes;

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
    complete_from_file(&path, 4, 14, &|m| got=m.matchstr.to_owned());
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
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 4, 14, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"apple");
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
    let mut got : ~str = ~"NOTHING";
    complete_from_file(&path, 5, 18, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,~"apple");
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
mod src2;

fn main() {
    myfn();
}
";
    write_file(&Path::new("src2.rs"), src2);
    let path = tmpname();
    write_file(&path, src);

    let mut got = racer::Match{matchstr:~"NOTHING", filepath: path.clone(), point: 0, linetxt: ~""};
    find_definition(path, 6, 6, &|m| got=m);

    assert_eq!(got.matchstr,~"myfn");
}

#[test]
fn gets_type_of_variable_via_assignment() {
    let src="
    struct Point {a: uint};
    let var = Point {a: 35};
";
    let path = tmpname();
    write_file(&path, src);
    //let mut got : ~str = ~"NOTHING";
    // scopes::coords_to_point(src, );
    // getTypeOf(&path, 8, 9, &|m| got=m.matchstr.to_owned());
    remove_file(&path);
    // assert_eq!(got,~"first");
}

// #[test]
// fn completes_struct_field_via_assignment() {
//     let src="
//     struct Point {
//         first: f64,
//         second: f64
//     } 

//     let var = Point {35, 22};
//     var.f
// ";
//     let path = tmpname();
//     write_file(&path, src);
//     let mut got : ~str = ~"NOTHING";
//     complete_from_file(&path, 8, 9, &|m| got=m.matchstr.to_owned());
//     remove_file(&path);
//     assert_eq!(got,~"first");
// }
