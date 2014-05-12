#![feature(managed_boxes,phase)]   // need this to use libsyntax
#![allow(dead_code,unused_imports,dead_code,attribute_usage,unused_variable)]
#[phase(syntax, link)] extern crate log;

extern crate syntax;

use racer::complete_from_file;
use racer::find_definition;
use std::io::File;
use std::task;
use racer::scopes;
use testutils::rejustify;

mod racer;
mod testutils;
mod codecleaner;

fn tmpname() -> Path {
    let mut s = "".to_owned();
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
fn completes_fn() {
    let src="
    fn apple() {
    }

    fn main() {
        let b = ap
    }";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = "NOTHING".to_owned();
    let pos = scopes::coords_to_point(src, 6, 18);
    complete_from_file(src, &path, pos, &mut |m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!("apple".to_owned(), got);
}

#[test]
fn completes_pub_fn_locally() {
    let src="
    pub fn apple() {
    }

    fn main() {
        let b = ap
    }";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = "NOTHING".to_owned();
    let pos = scopes::coords_to_point(src, 6, 18);
    complete_from_file(src, &path, pos, &mut |m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!("apple".to_owned(), got);
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
    let pos = scopes::coords_to_point(src, 4, 18);
    let got = racer::first_match(|m| complete_from_file(src, &path, pos, m)).unwrap();
    remove_file(&path);
    assert_eq!("apple".to_owned(), got.matchstr);
    assert_eq!(29, got.point);
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
    assert_eq!(got.matchstr,"apple".to_owned());
    assert_eq!(got.point,25);
}

#[test]
fn follows_use() {
    let src2="
pub fn myfn() {}
pub fn foo() {}
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
    assert_eq!(got.matchstr,"myfn".to_owned());
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
    let mut got : ~str = "NOTHING".to_owned();
    let pos = scopes::coords_to_point(src, 8, 9);
    complete_from_file(src, &path, pos, &mut |m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,"first".to_owned());
}

#[test]
fn finds_defn_of_struct_field() {
    let src="
    struct Point {
        first: f64,
        second: f64
    } 

    let var = Point {first: 35, second: 22};
    var.first
";
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 8, 9);
    let got = find_definition(src, &path, pos).unwrap();
    remove_file(&path);
    assert_eq!(got.matchstr,"first".to_owned());
}

#[test]
fn finds_impl_fn() {
    let src="
    struct Foo;
    impl Foo {
        fn new() {}
    }

    Foo::new();
";
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 7, 10);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,"new".to_owned());
}

#[test]
fn follows_use_to_inline_mod() {
    let src="
    use foo::myfn;
    mod foo {
        pub fn myfn() {}
    }

    fn main() {
        myfn();
    }
    ";
    write_file(&Path::new("src.rs"), src);
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 8, 9);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,"myfn".to_owned());
}

#[test]
fn finds_enum() {
    let src="
    enum MyEnum {
        One, Two
    }
    
    fn myfn(e: MyEnum) {}
    ";
    write_file(&Path::new("src.rs"), src);
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 6, 16);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,"MyEnum".to_owned());    
}

#[test]
fn finds_enum_value() {
    let src="
    enum MyEnum {
        One, Two
    }

    Two;
    ";
    write_file(&Path::new("src.rs"), src);
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 6, 6);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,"Two".to_owned());    
}

#[test]
fn follows_self_use() {
    let modsrc = "
    pub use self::src2::{Foo,myfn};
    ";
    let src2 = "
    struct Foo;
    pub fn myfn() {}
    ";
    let src = "
    use mymod::{Foo,myfn};

    fn main() {
        myfn();
    }
    ";
    let basedir = tmpname();
    let moddir = basedir.join("mymod");
    std::io::fs::mkdir_recursive(&moddir, std::io::UserRWX).unwrap();

    write_file(&moddir.join("mod.rs"), modsrc);
    write_file(&moddir.join("src2.rs"), src2);
    let srcpath = basedir.join("src.rs");
    write_file(&srcpath, src);
    let pos = scopes::coords_to_point(src, 5, 10);
    let got = find_definition(src, &srcpath, pos).unwrap();
    assert_eq!(got.matchstr,"myfn".to_owned());
    assert_eq!(moddir.join("src2.rs").display().to_str(), 
               got.filepath.display().to_str());
    assert_eq!(28, got.point);
}


#[test]
fn follows_use_to_impl() {
    let modsrc = "
    pub struct Foo;
    impl Foo {       // impl doesn't need to be 'pub'
        pub fn new() -> Foo {
            Foo
        }
    }
    ";
    let src = "
    use mymod::{Foo};

    fn main() {
        Foo::new();
    }
    ";
    let basedir = tmpname();
    std::io::fs::mkdir_recursive(&basedir, std::io::UserRWX).unwrap();

    let modpath = basedir.join("mymod.rs");
    write_file(&modpath, modsrc);
    let srcpath = basedir.join("src.rs");
    write_file(&srcpath, src);
    let pos = scopes::coords_to_point(src, 5, 14);
    let got = find_definition(src, &srcpath, pos).unwrap();
    assert_eq!(got.matchstr,"new".to_owned());
    assert_eq!(90, got.point);
    assert_eq!(modpath.display().to_str(), 
               got.filepath.display().to_str());
}

#[test]
fn finds_templated_impl_fn() {
    let src="
    struct Foo<T>;
    impl<T> Foo<T> {
        fn new() {}
    }

    Foo::new();
";
    let path = tmpname();
    write_file(&path, src);
    let pos = scopes::coords_to_point(src, 7, 10);
    let got = find_definition(src, &path, pos).unwrap();
    assert_eq!(got.matchstr,"new".to_owned());
}

#[test]
fn follows_fn_to_method() {
    let src="
    struct Foo<T>;
    impl<T> Foo<T> {
        fn new() -> Foo<T> {}
        fn mymethod(&self) {}
    }

    fn main() {
        let v = Foo::new();
        v.my
    }
    ";
    let path = tmpname();
    write_file(&path, src);
    let mut got : ~str = "NOTHING".to_owned();
    let pos = scopes::coords_to_point(src, 10, 12);
    complete_from_file(src, &path, pos, &mut |m| got=m.matchstr.to_owned());
    remove_file(&path);
    assert_eq!(got,"mymethod".to_owned());
}
