#![deny(warnings)]
extern crate racer;
extern crate rand;

use racer::core::complete_from_file;
use racer::core::find_definition;
use racer::core;
use racer::scopes;
use racer::util;

use std::env;
use std::io::Write;
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::thread;

/// A temporary file that is removed on drop
///
/// With the new constructor, you provide contents and a file is created based on the name of the
/// current task. The with_name constructor allows you to choose a name. Neither forms are secure,
/// and both are subject to race conditions.
pub struct TmpFile {
    path_buf: PathBuf
}

impl TmpFile {
    /// Create a temp file with random name and `contents`.
    pub fn new(contents: &str) -> TmpFile {
        let tmp = TmpFile {
            path_buf: PathBuf::from(tmpname())
        };

        tmp.write_contents(contents);
        tmp
    }

    /// Create a file with `name` and `contents`.
    pub fn with_path<P: AsRef<Path>>(name: P, contents: &str) -> TmpFile {
        let tmp = TmpFile {
            path_buf: name.as_ref().to_path_buf()
        };

        tmp.write_contents(contents);
        tmp
    }

    /// Create a file with `name` and `contents`.
    pub fn with_name(name: &str, contents: &str) -> TmpFile {
        TmpFile::with_path(&Path::new(name), contents)
    }

    fn write_contents(&self, contents: &str) {
        let mut f = File::create(self.path()).unwrap();
        f.write_all(contents.as_bytes()).unwrap();
        f.flush().unwrap();
    }


    /// Get the Path of the TmpFile
    pub fn path<'a>(&'a self) -> &'a Path {
        self.path_buf.as_path()
    }
}

/// Make path for tmpfile
fn tmpname() -> String {
    use rand::Rng;

    let thread = thread::current();
    let taskname = thread.name().unwrap();
    let s = taskname.replace("::", "_");
    let mut p = "tmpfile.".to_string();
    p.push_str(&s[..]);
    // Add some random chars
    for c in ::rand::thread_rng().gen_ascii_chars().take(5) {
        p.push(c);
    }

    p
}

impl Drop for TmpFile {
    fn drop(&mut self) {
        fs::remove_file(self.path_buf.as_path()).unwrap();
    }
}

pub struct TmpDir {
    path_buf: PathBuf
}

impl TmpDir {
    pub fn new() -> TmpDir {
        TmpDir::with_name(&tmpname()[..])
    }

    pub fn with_name(name: &str) -> TmpDir {
        let pb = PathBuf::from(name);
        fs::create_dir_all(&pb).unwrap();

        TmpDir {
            path_buf: pb
        }
    }

    /// Create a new temp file in the directory.
    pub fn new_temp_file(&self, contents: &str) -> TmpFile {
        self.new_temp_file_with_name(&tmpname()[..], contents)
    }

    /// Create new temp file with name in the directory
    pub fn new_temp_file_with_name(&self, name: &str, contents: &str) -> TmpFile {
        let name = self.path_buf.join(name);
        TmpFile::with_path(name, contents)
    }

    pub fn pathbuf(&self) -> &PathBuf {
        &self.path_buf
    }
}

impl Drop for TmpDir {
    fn drop(&mut self) {
        fs::remove_dir_all(&self.path_buf).unwrap();
    }
}

#[test]
fn completes_fn() {
    let src="
    fn  apple() {
    }

    fn main() {
        let b = ap
    }";

    let f = TmpFile::new(src);
    let path = f.path();

    let pos = scopes::coords_to_point(src, 6, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, path, pos, &core::Session::from_path(&cache, path, path)).nth(0).unwrap();

    assert_eq!("apple".to_string(), got.matchstr.to_string());
}

#[test]
fn completes_fn_with_substitute_file() {
    let src="
    fn  apple() {
    }

    fn main() {
        let b = ap
    }";

    let substitute_file = TmpFile::new(src);
    let pos = scopes::coords_to_point(src, 6, 18);
    let cache = core::FileCache::new();
    let real_file = &Path::new("not_real.rs");
    let session = core::Session::from_path(&cache, &real_file, substitute_file.path());
    let got = complete_from_file(src, &real_file, pos, &session).nth(0).unwrap();

    assert_eq!("apple".to_string(), got.matchstr.to_string());
}

#[test]
fn completes_pub_fn_locally() {
    let src="
    pub fn apple() {
    }

    fn main() {
        let b = ap
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("apple".to_string(), got.matchstr.to_string());
}

#[test]
fn completes_pub_fn_locally_precached() {
    let src="
    pub fn apple() {
    }

    fn main() {
        let b = ap
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 18);
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    session.cache_file_contents(&path, src);
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple".to_string(), got.matchstr.to_string());
}

#[test]
fn completes_pub_fn_from_local_package() {
    let src="
    extern crate fixtures;

    use fixtures::foo;

    fn main() {
        let x = foo::
    }
    ";

    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 7, 21);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0);
    assert_eq!(got.unwrap().matchstr, "test".to_owned());
}

#[test]
fn overwriting_cached_files() {
    let src1 = "src1";
    let src2 = "src2";
    let src3 = "src3";
    let src4 = "src4";

    // Need session and path to cache files
    let path = &Path::new("not_on_disk");
    let cache = core::FileCache::new();

    // Cache contents for a file and assert that load_file and load_file_and_mask_comments return
    // the newly cached contents.
    macro_rules! cache_and_assert {
        ($src:ident) => {{
            let session = core::Session::from_path(&cache, &path, &path);
            session.cache_file_contents(&path, $src);
            assert_eq!($src, &session.load_file(&path).src.code[..]);
            assert_eq!($src, &session.load_file_and_mask_comments(&path).src.code[..]);
        }}
    }

    // Check for all srcN
    cache_and_assert!(src1);
    cache_and_assert!(src2);
    cache_and_assert!(src3);
    cache_and_assert!(src4);
}

#[test]
fn completes_pub_const_fn_locally() {
    let src="
    pub const fn apple() {
    }

    fn main() {
        let b = ap
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("apple".to_string(), got.matchstr.to_string());
}

#[test]
fn completes_local_scope_let() {
    let src="
    fn main() {
        let apple = 35;
        let b = ap
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("apple".to_string(), got.matchstr);
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
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("apple".to_string(), got.matchstr);
    assert_eq!(25, got.point);
}

#[test]
fn completes_trait_methods() {
    let src = "
mod sub {
    pub trait Trait {
        fn traitf() -> bool;
        fn traitm(&self) -> bool;
    }

    pub struct Foo(bool);

    impl Trait for Foo {
        fn traitf() -> bool { false }
        fn traitm(&self) -> bool { true }
    }
}

fn main() { // l16
    let t = sub::Foo(true);
    sub::Foo::
    t.t
}
";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos1 = scopes::coords_to_point(src, 18, 14);  // sub::Foo::
    let cache1 = core::FileCache::new();
    let got1 = complete_from_file(src, &path, pos1, &core::Session::from_path(&cache1, &path, &path)).nth(0);
    let pos2 = scopes::coords_to_point(src, 19, 7);   // t.t
    let cache2 = core::FileCache::new();
    let got2 = complete_from_file(src, &path, pos2, &core::Session::from_path(&cache2, &path, &path)).nth(0);
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.unwrap().matchstr, "traitf".to_string());
    assert_eq!(got2.unwrap().matchstr, "traitm".to_string());
}

#[test]
fn follows_use() {
    let src1="
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src="
    use src1::{foo,myfn};
    mod src1;
    fn main() {
        myfn();
    }
    ";
    let _f = TmpFile::with_name("src1.rs", src1);
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 5, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr,"myfn".to_string());
}

#[test]
fn follows_use_as() {
    let src2="
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src="
    use src2::myfn as myfoofn;
    mod src2;
    fn main() {
        myfoofn();
    }
    ";
    let _f = TmpFile::with_name("src2.rs", src2);
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 5, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "myfn".to_string());
}

#[test]
fn follows_use_glob() {
    let src3="
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src="
    use src3::*;
    mod src3;
    fn main() {
        myfn();
    }
    ";
    let _f = TmpFile::with_name("src3.rs", src3);
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 5, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "myfn".to_string());
}

#[test]
fn follows_use_local_package() {
    let src="
    extern crate fixtures;

    use fixtures::
    ";

    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 18);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0);
    assert_eq!(got.unwrap().matchstr, "foo".to_owned());
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
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 8, 9);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("first".to_string(), got.matchstr);
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
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 8, 9);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "first".to_string());
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
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 7, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "new".to_string());
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
    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 8, 9);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "myfn".to_string());
}

#[test]
fn struct_field_scalar_primitive_types() {
    let src = "
    struct Foo<'a> {
        reference: &'a u8,
        array: [u8; 5],
        slice: &'a [u8],
    }

    fn foo(x: Foo) {
        x.
    }
    ";

    let tmp = TmpFile::new(src);
    let path = tmp.path();
    let pos = scopes::coords_to_point(src, 9, 10);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path));

    let completions = got.collect::<Vec<_>>();
    assert_eq!(completions.len(), 3);

    for completion in completions.into_iter() {
        println!("match: {:?}", completion);
        let expected = match completion.matchstr.as_ref() {
            "reference" => "&u8",
            "array" => "[u8; 5]",
            "slice" => "&[u8]",
            _ => panic!("unexpected match from Foo struct ({})", completion.matchstr)
        };

        assert_eq!(completion.contextstr, expected);
    }
}

#[test]
fn finds_enum() {
    let src="
    enum MyEnum {
        One, Two
    }

    fn myfn(e: MyEnum) {}
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 16);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "MyEnum".to_string());
}

#[test]
fn finds_type() {
    let src="
    type SpannedIdent = Spanned<Ident>
    SpannedIdent;
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 5);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "SpannedIdent".to_string());
}

#[test]
fn finds_trait() {
    let src="
    pub trait MyTrait<E: Clone> {}
    MyTrait
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 5);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "MyTrait".to_string());
}

#[test]
fn finds_macro() {
    let src = "
    macro_rules! my_macro {
    	() => {}
    }
    my_macro!();
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 5);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "my_macro!".to_string());
}

#[test]
fn finds_fn_arg() {
    let src="
    fn myfn(myarg: &str) {
         myarg
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "myarg".to_string());
}

#[test]
fn finds_fn_arg_in_incomplete_fn() {
    let src="
    fn myfn(myarg: &str) {
         myarg
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "myarg".to_string());
}

#[test]
fn finds_inline_fn() {
    let src="
    #[inline]
    fn contains<'a>(&needle: &'a str) -> bool {
    }

    contains();
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 9);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "contains".to_string());
}

#[test]
fn follows_self_use() {
    let modsrc = "
    pub use self::src4::{Foo,myfn};
    pub mod src4;
    ";
    let src4 = "
    struct Foo;
    pub fn myfn() {}
    ";
    let src = "
    use mymod::{Foo,myfn};
    pub mod mymod;

    fn main() {
        myfn();
    }
    ";

    let mymod = TmpDir::with_name("mymod");
    let _modrs = mymod.new_temp_file_with_name("mod.rs", modsrc);
    let _src4 = mymod.new_temp_file_with_name("src4.rs", src4);

    let src_file = TmpFile::with_name("src.rs", src);
    let srcpath = src_file.path();

    let pos = scopes::coords_to_point(src, 6, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &srcpath, pos, &core::Session::from_path(&cache, &srcpath, &srcpath)).unwrap();
    assert_eq!(got.matchstr, "myfn".to_string());
    assert_eq!(mymod.pathbuf().join("src4.rs").display().to_string(),
               got.filepath.display().to_string());
    assert_eq!(28, got.point);
}

#[test]
fn finds_nested_submodule_file() {
    let rootsrc = "
    pub mod sub1 {
        pub mod sub2 {
            pub mod sub3;
        }
    }
    sub1::sub2::sub3::myfn();
    ";

    let sub3src = "
    pub fn myfn() {}
    ";

    let basedir = PathBuf::from(tmpname());
    let srcpath = basedir.join("root.rs");
    let sub2dir = basedir.join("sub1").join("sub2");
    let _dir = TmpDir::with_name(sub2dir.as_path().to_str().unwrap());

    let _src = TmpFile::with_path(&srcpath, rootsrc);
    let _src3 = TmpFile::with_path(&sub2dir.join("sub3.rs"), sub3src);

    let pos = scopes::coords_to_point(rootsrc, 7, 23);
    let cache = core::FileCache::new();
    let got = find_definition(rootsrc, &srcpath, pos, &core::Session::from_path(&cache, &srcpath, &srcpath)).unwrap();
    assert_eq!(got.matchstr, "myfn".to_string());
    assert_eq!(sub2dir.join("sub3.rs").display().to_string(),
               got.filepath.display().to_string());
}

#[test]
fn follows_super_in_sub_module() {
    let src="
    pub fn iamhere() { }
    mod inner { pub use super::iamhere; }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 33);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("iamhere", got.matchstr);
}

#[test]
fn follows_super_in_local_sub_module() {
    let src="
    mod inner {
      pub fn iamhere() { }
      mod inner2 { pub use super::iamhere; }
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 38);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("iamhere", got.matchstr);
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
    mod mymod;
    fn main() {
        Foo::new();
    }
    ";
    let dir = TmpDir::with_name("tmp");
    let mod_file = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let src_file = dir.new_temp_file_with_name("src.rs", src);
    let srcpath = src_file.path();

    let pos = scopes::coords_to_point(src, 5, 14);
    let cache = core::FileCache::new();
    let got = find_definition(src, &srcpath, pos, &core::Session::from_path(&cache, &srcpath, &srcpath)).unwrap();

    assert_eq!(got.matchstr, "new".to_string());
    assert_eq!(90, got.point);
    assert_eq!(mod_file.path().display().to_string(),
               got.filepath.display().to_string());
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
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 7, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "new".to_string());
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
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 10, 12);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("mymethod".to_string(), got.matchstr);
}

#[test]
fn follows_arg_to_method() {
    let src="
    struct Foo<T>;
    impl<T> Foo<T> {
        fn mymethod(&self) {}
    }

    fn myfn(v: &Foo) {
        v.my
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 8, 12);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("mymethod".to_string(), got.matchstr);
}

#[test]
fn follows_arg_to_enum_method() {
    let src="
    enum Foo<T> {
       EnumVal
    }
    impl<T> Foo<T> {
        fn mymethod(&self) {}
    }

    fn myfn(v: &Foo) {
        v.my
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 10, 12);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("mymethod".to_string(), got.matchstr);
}

#[test]
fn follows_let_method_call() {
    let src="
    struct Foo;
    struct Bar;
    impl Foo {
        fn mymethod(&self) -> Bar {}
    }
    impl Bar {
        fn mybarmethod(&self) -> Bar {}
    }

    fn myfn(v: &Foo) {
        let f = v.mymethod();
        f.my
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 13, 12);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("mybarmethod".to_string(), got.matchstr);
}

#[test]
fn follows_chained_method_call() {
    let src="
    struct Foo;
    struct Bar;
    impl<T> Foo<T> {
        fn mymethod(&self) -> Bar {}
    }
    impl<T> Bar<T> {
        fn mybarmethod(&self) -> Bar {}
    }

    fn myfn(v: &Foo) {
        v.mymethod().my
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 12, 23);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0).unwrap();
    assert_eq!("mybarmethod".to_string(), got.matchstr);
}

#[test]
fn discards_inner_fns() {
    let src="
    struct Foo;
    impl<T> Foo<T> {
        fn mymethod(&self) -> Bar {
            fn inner() {
            }
        }
    }

    fn myfn(v: &Foo) {
        v.i
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 11, 11);
    let cache = core::FileCache::new();
    let got = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).nth(0);
    assert!(got.is_none(), "should not match inner function");
}

#[test]
fn differentiates_type_and_value_namespaces() {
    let src = "
    enum MyEnum{ Foo }
    struct Foo;
    impl Foo { pub fn new() -> Foo {} }
    let l = Foo::new();
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 18);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    println!("{}", got.matchstr);
    println!("{:?}", got.mtype);
    assert_eq!("new", got.matchstr);
}

#[test]
fn follows_self_to_method() {
    let src= "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) {
        }

        pub fn another_method(self, feio: uint) {
            self.method()
        }
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 8, 20);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("method", got.matchstr);
}

#[test]
#[ignore]
fn follows_self_to_method_when_call_on_new_line() {
    let src= "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) -> Foo {
        }

        pub fn another_method(self, feio: uint) {
            self.method()
                .method()
        }
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 9, 20);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("method", got.matchstr);
}

#[test]
fn follows_self_to_trait_method() {
    let src= "
    trait Bar {
        pub fn method(self) {
        }
        pub fn another_method(self) {
            self.method()
        }
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 20);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("method", got.matchstr);
}

#[test]
fn finds_trait_method() {
    let src = "
    pub trait MyTrait {
        fn op(self);
        fn trait_method(self){}
    }

    struct Foo;
    impl MyTrait for Foo {
        fn op(self) {
            self.trait_method();
        }
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 10, 22);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("trait_method", got.matchstr);
}

#[test]
fn finds_field_type() {
    let src = "
    pub struct Blah { subfield: uint }

    pub struct Foo {
        myfield : Blah
    }

    let f = Foo{ myfield: Blah { subfield: 3}};
    f.myfield.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 9, 16);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_a_generic_retval_from_a_function() {
    let src="
    pub struct Blah { subfield: uint }
    pub struct Foo<T> {
        myfield: T
    }
    fn myfn() -> Foo<Blah> {}
    myfn().myfield.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 7, 24);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_an_enum_option_style_return_type() {
    let src="
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    impl MyOption<T> {
         pub fn unwrap(&self) -> T {}
    }
    fn myfn() -> MyOption<Blah> {}
    let s = myfn();
    s.unwrap().subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 12, 18);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_definition_of_const() {
    let src="
    pub const MYCONST:uint = 3;
    MYCONST
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 7);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("MYCONST", got.matchstr);
}

#[test]
fn finds_definition_of_static() {
    let src="
    pub static MYSTATIC:uint = 3;
    MYSTATIC
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 7);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("MYSTATIC", got.matchstr);
}

#[test]
fn handles_dotdot_before_searchstr() {
    let src="
    static MYLEN:uint = 30;
    let f = [0i32, ..MYLEN];
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 22);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("MYLEN", got.matchstr);
}

#[test]
#[ignore]
fn finds_definition_of_lambda_argument() {
    let src="
    fn myfn(&|int|) {}
    myfn(|a|a+3);
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 12);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_definition_of_let_tuple() {
    let src="
    let (a, b) = (2,3);
    a
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 4);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_let_type() {
    let src="
    pub struct Blah { subfield: uint }
    let (a, b): (uint, Blah);
    b.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 11);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_let_expr() {
    let src="
    pub struct Blah { subfield: uint }
    let (a, b) = (3, Blah{subfield:3});
    b.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 11);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_fn_retval() {
    let src="
    pub struct Blah { subfield: uint }
    fn myfn() -> (uint, Blah) {}
    let (a, b) = myfn();
    b.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 11);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_in_fn_arg() {
    let src="
    pub struct Blah { subfield: uint }
    fn myfn(a: uint, (b, c): (uint, Blah)) {
        c.subfield
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 11);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_namespaced_enum_variant() {
    let src="
    pub enum Blah { MyVariant }
    Blah::MyVariant
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 14);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("MyVariant", got.matchstr);
}

#[test]
fn finds_glob_imported_enum_variant() {
    let src="
    use self::Blah::*;
    pub enum Blah { MyVariant, MyVariant2 }
    MyVariant
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 8);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("MyVariant", got.matchstr);
}

#[test]
#[ignore]
fn uses_generic_arg_to_resolve_trait_method() {
    let src="
    pub trait MyTrait {
        fn trait_method(self){}
    }
    pub fn doit<T:MyTrait>(stream: &mut T) {
        T.trait_method
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 19);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("trait_method", got.matchstr);
}

#[test]
fn destructures_a_tuplestruct() {
    let src="
    pub struct Blah { subfield: uint }
    pub struct TupleStruct(Blah);
    let TupleStruct(var) = TupleStruct(Blah{subfield:35});
    var.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn destructures_a_tuplestruct_with_generic_arg() {
    let src="
    pub struct Blah { subfield: uint }
    pub struct TupleStruct<T>(T);
    let a : TupleStruct<Blah> = TupleStruct(Blah{subfield:35});
    let TupleStruct(var) = a;
    var.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 10);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_if_let_ident_defn() {
    let src="
    if let MyOption(myvar) = myvar {
        myvar
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 13);
    let cache = core::FileCache::new();
    let mut it = complete_from_file(src, &path, pos, &core::Session::from_path(&cache, &path, &path));
    let got = it.next().unwrap();
    assert_eq!("myvar", &*got.matchstr);
    assert!(it.next().is_none(), "should only match the first one");
}

#[test]
fn doesnt_find_if_let_if_not_in_the_subscope() {
    let src="
    let myvar = 3u32;
    if let MyOption(myvar) = myvar {
        myvar
    }
    myvar
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 6, 6);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("myvar", &*got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn finds_rebound_var_in_iflet() {
    let src="
    let o: MyOption<Blah>;
    if let MyOption::MySome(o) = o {
        o
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 4, 8);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(56, got.point);
}

#[test]
fn handles_if_let() {
    let src="
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    if let MyOption::MySome(a) = o {
        a.subfield
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 9, 13);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_if_let_as_expression() {
    let src="
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    let foo = if let MyOption::MySome(a) = o { // iflet is an expression
        a.subfield
    };
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 9, 13);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_match_arm_var() {
    let src="
    match foo {
       Some(a) => a
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 18);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_match_arm_var_in_scope() {
    let src="
    match foo {
       Some(a) => { a }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 20);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_match_arm_enum() {
    let src="
    enum MyEnum {
        Foo,
        Bar
    }
    match foo {
       MyEnum::Foo => 1,
       MyEnum::Bar => 2
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 7, 18);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("Foo", got.matchstr);
}

#[test]
fn finds_match_arm_var_with_nested_match() {
    let src="
    match foo {
       bar => {something}
       Some(a) => {
               let b = match blah {
                           None => ()
               }
               a
       }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 8, 15);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
}

#[test]
fn gets_type_via_match_arm() {
    let src="
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    match o {
        MyOption::MySome(a) => a.subfield
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 9, 38);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_default_arm() {
    let src="
    let o: MyOption<Blah>;
    match o {
        Foo => { }
        _ => o
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 13);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("o", got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn doesnt_match_rhs_of_let_in_same_stmt() {
    let src="
    let a = 3;      // <--- should match this 'a'
    let a = a + 2;  // not this one
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 3, 12);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!("a", got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn issue_223() {
    assert_eq!(true, util::path_exists(env::temp_dir()));
}


#[test]
fn finds_unsafe_fn() {
    let src="
    unsafe fn foo() {}

    fn bar() {
        foo()
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let pos = scopes::coords_to_point(src, 5, 9);
    let cache = core::FileCache::new();
    let got = find_definition(src, &path, pos, &core::Session::from_path(&cache, &path, &path)).unwrap();
    assert_eq!(got.matchstr, "foo".to_string());
    assert_eq!(got.point, 15);
}

#[test]
fn completes_methods_on_deref_type() {
    let modsrc = "
    pub trait Deref {
        type Target: ?Sized;

        fn deref(&self) -> &Self::Target;
    }

    pub struct B {
        c: C,
    }

    pub struct C;

    pub trait GetOne {
        fn one(&self) -> u32 { 1u32 }
    }

    impl GetOne for C {}

    impl Deref for B {
        type Target = C;
        fn deref(&self) -> &C {
            &self.c
        }
    }
    ";
    let src = "
    mod mymod;
    use mymod::{B, C, GetOne};

    fn main() {
        let b: B = B{ c: C};
        b.o
    }
    ";

    let dir = TmpDir::new();

    let _modfile = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let srcfile = dir.new_temp_file_with_name("src.rs", src);
    let srcpath = srcfile.path();

    let pos = scopes::coords_to_point(src, 7, 11);
    let cache = core::FileCache::new();

    let got_str = complete_from_file(src, &srcpath, pos, &core::Session::from_path(&cache, &srcpath, &srcpath))
                                    .nth(0).expect("No match found").matchstr;

    assert_eq!(got_str, "one".to_string());
}

#[test]
fn completes_methods_on_deref_generic_type() {
    let modsrc = "
    pub trait Deref {
        type Target: ?Sized;

        fn deref(&self) -> &Self::Target;
    }

    pub struct B<T> {
        c: T,
    }

    pub struct C;

    pub trait GetOne {
        fn one(&self) -> u32 { 1u32 }
    }

    impl GetOne for C {}

    impl<T> Deref for B<T> {
        type Target = T;
        fn deref(&self) -> &T {
            &self.c
        }
    }
    ";
    let src = "
    mod mymod;
    use mymod::{B, C, GetOne};

    fn main() {
        let b: B<C> = B{ c: C};
        b.o
    }
    ";
    let dir = TmpDir::new();
    let _modfile = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let srcfile = dir.new_temp_file_with_name("src.rs", src);
    let srcpath = srcfile.path();

    let pos = scopes::coords_to_point(src, 7, 11);
    let cache = core::FileCache::new();

    let got_str = complete_from_file(src, &srcpath, pos, &core::Session::from_path(&cache, &srcpath, &srcpath))
                                    .nth(0).expect("No match found").matchstr;
    assert_eq!(got_str, "one".to_string());
}
