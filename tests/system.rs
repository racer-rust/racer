#![deny(warnings)]
extern crate racer;
extern crate rand;

use racer::core::complete_from_file;
use racer::core::find_definition;
use racer::core;

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
    let mut p = String::from("tmpfile.") + &taskname.replace("::", "_");
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

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, path, path);
    let pos = session.load_file(path).coords_to_point(6, 18).unwrap();

    let got = complete_from_file(src, path, pos, &session).nth(0).unwrap();

    assert_eq!("apple", got.matchstr);
}


#[test]
fn finds_fn_docs() {
    let src="
    /// Orange
    /// juice
    fn apple() {
    }

    fn main() {
        apple
    }";

    let f = TmpFile::new(src);
    let path = f.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, path, path);
    let pos = session.load_file(path).coords_to_point(8, 13).unwrap();
    let got = complete_from_file(src, path, pos, &session).nth(0).unwrap();

    assert_eq!("apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn finds_struct_docs() {
    let src="
    /// Orange
    /// juice
    struct Apple {
    }

    fn main() {
        Apple
    }";

    let f = TmpFile::new(src);
    let path = f.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, path, path);
    let pos = session.load_file(path).coords_to_point(8, 13).unwrap();
    let got = complete_from_file(src, path, pos, &session).nth(0).unwrap();

    assert_eq!("Apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
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
    let cache = core::FileCache::new();
    let real_file = &Path::new("not_real.rs");
    let session = core::Session::from_path(&cache, &real_file, substitute_file.path());
    let pos = session.load_file(substitute_file.path()).coords_to_point(6, 18).unwrap();
    let got = complete_from_file(src, &real_file, pos, &session).nth(0).unwrap();

    assert_eq!("apple", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
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
    let cache = core::FileCache::new();
    cache.cache_file_contents(&path, src);
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 21).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0);
    assert_eq!(got.unwrap().matchstr, "test");
}

#[test]
fn completes_pub_fn_from_local_submodule_package() {
    let src="
    extern crate fixtures;

    use fixtures::bar;

    fn main() {
        let x = bar::
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 21).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0);
    assert_eq!(got.unwrap().matchstr, "bartest");
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
            cache.cache_file_contents(&path, $src);
            let session = core::Session::from_path(&cache, &path, &path);
            assert_eq!($src, &session.load_file(&path).code[..]);
            assert_eq!($src, &session.load_file_and_mask_comments(&path).code[..]);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
    assert_eq!(25, got.point);
}

#[test]
fn completes_for_vec_field_and_method() {
    let modsrc = "
    pub trait IntoIterator {
        type Item;

        type IntoIter: Iterator<Item=Self::Item>;

        fn into_iter(self) -> Self::IntoIter;
    }

    impl<T> IntoIterator for Vec<T> {
        type Item = T;
        type IntoIter = IntoIter<T>;

        fn into_iter(mut self) -> IntoIter<T> {}
    }

    pub struct IntoIter<T> {}

    impl<T> Iterator for IntoIter<T> {
        type Item = T;

        fn next(&mut self) -> Option<T> {}
    }

    pub struct Vec<T> {}

    pub enum Option<T> {
        None,
        Some(T)
    }
    ";
    let src="
    pub mod mymod;
    use mymod::{Vec, IntoIter, IntoIterator, Option};
    use Option::{Some, None};

    struct St
    {
        stfield: i32,
    }

    impl St {
        pub fn stmethod(&self) -> u32 {2}
    }

    fn main()
    {
        let mut arr: Vec<St> = Vec::new();
        arr.push( St{stfield: 4} );

        for it in arr
        {
            it.stf
            it.stm
        }
    }
    ";

    let dir = TmpDir::new();
    let _modfile = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let srcfile = dir.new_temp_file_with_name("src.rs", src);

    let path = srcfile.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos1 = session.load_file(path).coords_to_point(22, 18).unwrap();
    let got1 = complete_from_file(src, &path, pos1, &session).nth(0).unwrap();
    println!("{:?}", got1);
    assert_eq!("stfield", got1.matchstr);
    let pos2 = session.load_file(path).coords_to_point(23, 18).unwrap();
    let got2 = complete_from_file(src, &path, pos2, &session).nth(0).unwrap();
    println!("{:?}", got2);
    assert_eq!("stmethod", got2.matchstr);
}

#[test]
fn completes_trait_methods() {
    let src = "
mod sub {
    pub trait Trait {
        fn traitf() -> bool;
        fn traitm(&self) -> bool;
    }

    pub struct Foo(pub bool);

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
    let cache1 = core::FileCache::new();
    let session1 = core::Session::from_path(&cache1, &path, &path);
    let pos1 = session1.load_file(path).coords_to_point(18, 14).unwrap(); // sub::Foo::
    let got1 = complete_from_file(src, &path, pos1, &session1).nth(0).unwrap();
    let cache2 = core::FileCache::new();
    let session2 = core::Session::from_path(&cache2, &path, &path);
    let pos2 = session2.load_file(path).coords_to_point(19, 7).unwrap(); // t.t
    let got2 = complete_from_file(src, &path, pos2, &session2).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
}

#[test]
fn completes_trait_bounded_methods() {
    let src = "
    pub trait Trait1 {}
    
    impl Trait1 for Foo {}
    
    pub trait Trait2 {
        fn traitf() -> bool;
        fn traitm(&self) -> bool;
    }
    
    impl<T: Trait1> Trait2 for T {
        fn traitf() -> bool { true }
        fn traitm(&self) -> bool { false }
    }
    
    pub struct Foo(pub bool);
    
    fn main() {
        let t = Foo(true);
        Foo::tra
        t.tr
    }
";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache1 = core::FileCache::new();
    let session1 = core::Session::from_path(&cache1, &path, &path);
    let pos1 = session1.load_file(path).coords_to_point(20, 16).unwrap(); // sub::Foo::
    let got1 = complete_from_file(src, &path, pos1, &session1).nth(0).unwrap();
    let cache2 = core::FileCache::new();
    let session2 = core::Session::from_path(&cache2, &path, &path);
    let pos2 = session2.load_file(path).coords_to_point(21, 12).unwrap(); // t.t
    let got2 = complete_from_file(src, &path, pos2, &session2).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
}

#[test]
fn completes_trait_bounded_methods_generic_return() {
    let src = "
    pub trait Trait1 {
        fn traitfn(&self) -> u32 { 2 }
    }
    
    impl Trait1 for Foo {}
    
    pub trait Trait2 {
        fn traitm(self) -> Self;
    }
    
    impl<T: Trait1> Trait2 for T {
        fn traitm(self) -> T { self }
    }
    
    pub struct Foo(pub bool);
    
    impl Foo {
        pub fn structfn(&self) -> bool {self.0}
    }
    
    fn main() {
        let t = Foo(true);
        t.traitm().struc
        t.traitm().traitf
    }
";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos1 = session.load_file(path).coords_to_point(24, 24).unwrap();  // struc
    let pos2 = session.load_file(path).coords_to_point(25, 25).unwrap();  // traitf
    let got1 = complete_from_file(src, &path, pos1, &session).nth(0).unwrap();
    let got2 = complete_from_file(src, &path, pos2, &session).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "structfn");
    assert_eq!(got2.matchstr, "traitfn");
}

#[test]
fn completes_iter_variable_methods() {
    let modsrc = "
    pub trait Iterator {
        type Item;
        
        fn next(&mut self) -> Option<Self::Item>;
    }

    pub trait IntoIterator {
        type Item;
        
        type IntoIter: Iterator<Item=Self::Item>;
        
        fn into_iter(self) -> Self::IntoIter;
    }

    impl<I: Iterator> IntoIterator for I {
        type Item = I::Item;
        type IntoIter = I;
    
        fn into_iter(self) -> I {
            self
        }
    }

    impl<T> Iterator for IntoIter<T> {
        type Item = T;
        
        fn next(&mut self) -> Option<T> {}
    }

    pub enum Option<T> {
        None,
        Some(T)
    }
    ";
    
    let src = "
    pub mod mymod;
    use mymod::{Iterator, Option};
    use Option::{Some, None};

    struct St {
        pub item: StItem,
        pub used: bool
    }

    struct StItem {
        pub field: u32
    }
    
    impl Iterator for St {
        type Item: StItem;
    
        fn next(&mut self) -> Option<StItem> {
            if self.used {
                self.used = false;
                return Some(self.item);
            }
            None
        }
    }
    
    fn main()
    {
        let it = St {
            text: StItem { field: 22 },
            used: false
        }
        
        for item in it {
            item.fie
        }
    }
";
    let dir = TmpDir::new();
    let _modfile = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let srcfile = dir.new_temp_file_with_name("src.rs", src);

    let path = srcfile.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(35, 20).unwrap();  // item.fie
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    println!("{:?}", got);
    assert_eq!(got.matchstr, "field");
}

#[test]
fn completes_for_vec_iter_field_and_method() {
    let modsrc = "
    pub trait Iterator {
        type Item;
        
        fn next(&mut self) -> Option<Self::Item>;
    }

    pub trait IntoIterator {
        type Item;
        
        type IntoIter: Iterator<Item=Self::Item>;
        
        fn into_iter(self) -> Self::IntoIter;
    }
    
    impl<T> IntoIterator for Vec<T> {
        type Item = T;
        type IntoIter = IntoIter<T>;
        
        fn into_iter(mut self) -> IntoIter<T> {}
    }
    
    pub struct IntoIter<T> {}
    
    impl<T> Iterator for IntoIter<T> {
        type Item = T;
        
        fn next(&mut self) -> Option<T> {}
    }

    impl<I: Iterator> IntoIterator for I {
        type Item = I::Item;
        type IntoIter = I;
    
        fn into_iter(self) -> I {
            self
        }
    }
    
    pub struct Vec<T> {}

    pub enum Option<T> {
        None,
        Some(T)
    }
    ";
    let src="
    pub mod mymod;
    use mymod::{Vec, IntoIter, IntoIterator, Option};
    use Option::{Some, None};

    struct St
    {
        stfield: i32,
    }
    
    impl St {
        pub fn stmethod(&self) -> u32 {2}
    }
    
    fn main()
    {
        let mut arr: Vec<St> = Vec::new();
        arr.push( St{stfield: 4} );
    
        for it in arr.iter()
        {
            it.stf
            it.stm
        }
    }
    ";

    let dir = TmpDir::new();
    let _modfile = dir.new_temp_file_with_name("mymod.rs", modsrc);
    let srcfile = dir.new_temp_file_with_name("src.rs", src);

    let path = srcfile.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos1 = session.load_file(path).coords_to_point(22, 18).unwrap();
    let got1 = complete_from_file(src, &path, pos1, &session).nth(0).unwrap();
    println!("{:?}", got1);
    assert_eq!("stfield", got1.matchstr);
    let pos2 = session.load_file(path).coords_to_point(23, 18).unwrap();
    let got2 = complete_from_file(src, &path, pos2, &session).nth(0).unwrap();
    println!("{:?}", got2);
    assert_eq!("stmethod", got2.matchstr);
}

#[test]
fn completes_trait_methods_when_at_scope_end() {
    let src = "
mod sub {
    pub trait Trait {
        fn traitf() -> bool;
        fn traitm(&self) -> bool;
    }

    impl Trait for Foo {
        fn traitf() -> bool { false }
        fn traitm(&self) -> bool { true }
    }

    pub struct Foo(pub bool);
}

fn main() { // l16
    let t = sub::Foo(true);
    sub::Foo::
    t.t
}
";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos1 = session.load_file(path).coords_to_point(18, 14).unwrap();  // sub::Foo::
    let got1 = complete_from_file(src, &path, pos1, &session).nth(0).unwrap();
    let pos2 = session.load_file(path).coords_to_point(19, 7).unwrap();   // t.t
    let got2 = complete_from_file(src, &path, pos2, &session).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
    assert_eq!(got.contextstr, "pub fn myfn()");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
}

#[test]
fn follows_multiple_use_globs() {
    let src1 = "
    pub fn src1fn() {}
    ";
    let src2 = "
    pub fn src2fn() {}
    ";
    let src ="
    use multiple_glob_test1::*;
    use multiple_glob_test2::*;
    mod multiple_glob_test1;
    mod multiple_glob_test2;

    src
    ";

    let _tmpsrc1 = TmpFile::with_name("multiple_glob_test1.rs", src1);
    let _tmpsrc2 = TmpFile::with_name("multiple_glob_test2.rs", src2);
    let tmpsrc = TmpFile::new(src);
    let path = tmpsrc.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 7).unwrap();
    let got = complete_from_file(src, &path, pos, &session);
    let completion_strings = got.into_iter().map(|raw_match| raw_match.matchstr).collect::<Vec<_>>();

    assert!(completion_strings.contains(&"src1fn".to_string()) &&
            completion_strings.contains(&"src2fn".to_string()),
            format!("Results should contain BOTH \"src1fn\" and \"src2fn\". Actual returned results: {:?} ",
                    completion_strings));
}

#[test]
fn finds_external_mod_docs() {
    let src1="// Copyright notice

//! The mods multiline
//! documentation
    ";
    let src2="
    mod external_mod;
    use external_mod;

    fn main() {
        external_mod
    }";
    let _tmpsrc = TmpFile::with_name("external_mod.rs", src1);
    let f = TmpFile::new(src2);
    let path = f.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(2, 20).unwrap();
    let got = complete_from_file(src2, path, pos, &session).nth(0).unwrap();

    assert_eq!("external_mod", got.matchstr);
    assert_eq!("The mods multiline\ndocumentation", got.docs);
}

#[test]
fn finds_external_struct_docs() {
    let src1="
    /// Orange
    /// juice
    pub struct Apple {
        pub a: u8,
    }";
    let src2="
    use external_struct::Apple;
    mod external_struct;

    fn main() {
        Apple
    }";
    let _tmpsrc = TmpFile::with_name("external_struct.rs", src1);
    let f = TmpFile::new(src2);
    let path = f.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 13).unwrap();
    let got = complete_from_file(src2, path, pos, &session).nth(0).unwrap();

    assert_eq!("Apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn finds_external_fn_docs() {
    let src1="
    /// Orange
    /// juice
    pub fn apple() {
        let x = 1;
    }";
    let src2="
    use external_fn::apple;
    mod external_fn;

    fn main() {
        apple
    }";
    let _tmpsrc = TmpFile::with_name("external_fn.rs", src1);
    let f = TmpFile::new(src2);
    let path = f.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 13).unwrap();
    let got = complete_from_file(src2, path, pos, &session).nth(0).unwrap();

    assert_eq!("apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn follows_use_local_package() {
    let src="
    extern crate fixtures;

    use fixtures::
    ";

    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0);
    assert_eq!(got.unwrap().matchstr, "foo");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 9).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("first", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 9).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "first");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "new");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 9).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 10).unwrap();
    let got = complete_from_file(src, &path, pos, &session);

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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 16).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "MyEnum");
}

#[test]
fn finds_type() {
    let src="
    type SpannedIdent = Spanned<Ident>
    SpannedIdent;
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 5).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "SpannedIdent");
}

#[test]
fn finds_trait() {
    let src="
    pub trait MyTrait<E: Clone> {}
    MyTrait
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 5).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "MyTrait");
    assert_eq!(got.contextstr, "pub trait MyTrait<E: Clone>");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 5).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "my_macro!");
}

#[test]
fn finds_extern_crate() {
    let src = "
    extern crate fixtures;
    fixtures
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 5).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "fixtures");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myarg");
}

#[test]
fn finds_fn_arg_in_incomplete_fn() {
    let src="
    fn myfn(myarg: &str) {
         myarg
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myarg");
}

#[test]
fn finds_inline_fn() {
    let src="
    #[inline]
    fn contains<'a>(&needle: &'a str)
        -> bool {
    }

    contains();
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 9).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "contains");
    assert_eq!(got.contextstr, "fn contains<'a>(&needle: &'a str) -> bool");
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
    let path = src_file.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
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
    let path = basedir.join("root.rs");
    let sub2dir = basedir.join("sub1").join("sub2");
    let _rootdir = TmpDir::with_name(basedir.as_path().to_str().unwrap());
    let _dir2 = TmpDir::with_name(sub2dir.as_path().to_str().unwrap());

    let _src = TmpFile::with_path(&path, rootsrc);
    let _src3 = TmpFile::with_path(&sub2dir.join("sub3.rs"), sub3src);

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(&path).coords_to_point(7, 23).unwrap();
    let got = find_definition(rootsrc, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "myfn");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 33).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 38).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let path = src_file.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 14).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();

    assert_eq!(got.matchstr, "new");
    assert_eq!(90, got.point);
    assert_eq!(mod_file.path().display().to_string(),
               got.filepath.display().to_string());
}

#[test]
fn finds_templated_impl_fn() {
    let src = "
    struct Foo<T>;
    impl<T> Foo<T> {
        fn new() {}
    }

    Foo::new();
";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "new");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(10, 12).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn simple_struct_contextstr() {
    let src="
    struct Foo<T>;

    fn myfn() {
        let x: Foo
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!(got.contextstr, "struct Foo<T>;");
}

#[test]
fn struct_contextstr() {
    let src="
    struct
        Foo<T> {
        pub fn foo1();
    }

    fn myfn() {
        let x: Foo
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 18).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!(got.contextstr, "struct Foo<T>");
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 12).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("mymethod", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(10, 12).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("mymethod", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(13, 12).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("mybarmethod", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(12, 23).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!("mybarmethod", got.matchstr);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(11, 11).unwrap();
    let got = complete_from_file(src, &path, pos, &session).nth(0);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 18).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 20).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 20).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 20).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(10, 22).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 16).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 24).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(12, 18).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 7).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 7).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 22).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 12).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 4).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 11).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 11).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 11).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 11).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 14).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 8).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 19).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 10).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 13).unwrap();
    let mut it = complete_from_file(src, &path, pos, &session);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(6, 6).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(4, 8).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 13).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 13).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 18).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 20).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 18).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(8, 15).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(9, 38).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 13).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(3, 12).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!("a", got.matchstr);
    assert_eq!(9, got.point);
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
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(5, 9).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!(got.matchstr, "foo");
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
    let path = srcfile.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 11).unwrap();

    let got_str = complete_from_file(src, &path, pos, &session)
                                    .nth(0).expect("No match found").matchstr;

    assert_eq!(got_str, "one");
}

#[test]
fn finds_self_param_when_fn_has_generic_closure_arg() {
    // issue #508
    let src = "
    struct MyOption;

    impl MyOption {
        // needs to find 'self' here to see it is a method
        pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U> {
        }
    }

    let a: MyOption;
    a.map
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(11, 6).unwrap();
    let got = find_definition(src, &path, pos, &session).unwrap();
    assert_eq!("map", got.matchstr);
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
    let path = srcfile.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(7, 11).unwrap();

    let got_str = complete_from_file(src, &path, pos, &session)
                                    .nth(0).expect("No match found").matchstr;
    assert_eq!(got_str, "one");
}

#[test]
fn completes_multiple_use_bracket() {
    // issue # 96
    // wo: without bracket, wi: with bracket
    let modfile="
    pub struct StarWars {
        pub Vadar: u8,
    };
    pub struct StarTrek {
        pub Spock: u8,
    };";
    let srcwo="
    mod modfile1;
    use modfile1::S
    ";
    let srcwi="
    mod modfile1;
    use modfile1::{S
    ";
    let _tmpsrc = TmpFile::with_name("modfile1.rs", modfile);
    let fwo = TmpFile::new(srcwo);
    let fwi = TmpFile::new(srcwi);
    let pathwo = fwo.path();
    let pathwi = fwi.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &pathwo, &pathwo);
    let poswo = session.load_file(pathwo).coords_to_point(3, 18).unwrap();
    let poswi = session.load_file(pathwi).coords_to_point(3, 19).unwrap();
    let gotwo = complete_from_file(srcwo, &pathwo, poswo, &session);
    let gotwi = complete_from_file(srcwi, &pathwi, poswi, &session);

    assert_eq!(gotwo.size_hint().0, gotwi.size_hint().0);
    for (wo, wi) in gotwo.zip(gotwi) {
        assert_eq!(wo.matchstr, wi.matchstr);
    }
}

#[test]
fn completes_multiple_use_comma() {
    // issue # 96
    // wo: without comma, wi: with comma
    let modfile="
    pub struct StarWars {
        pub Kenobi: u8,
    };
    pub struct StarTrek {
        pub Spock: u8,
    };";
    let srcwo="
    mod modfile2;
    use modfile2::S
    ";
    let srcwi="
    mod modfile2;
    use modfile2::{StarWars, S
    ";
    let _tmpsrc = TmpFile::with_name("modfile2.rs", modfile);
    let fwo = TmpFile::new(srcwo);
    let fwi = TmpFile::new(srcwi);
    let pathwo = fwo.path();
    let pathwi = fwi.path();

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &pathwo, &pathwo);
    let poswo = session.load_file(pathwo).coords_to_point(3, 18).unwrap();
    let poswi = session.load_file(pathwi).coords_to_point(3, 29).unwrap();
    let gotwo = complete_from_file(srcwo, &pathwo, poswo, &session);
    let gotwi = complete_from_file(srcwi, &pathwi, poswi, &session);

    assert_eq!(gotwo.size_hint().0, gotwi.size_hint().0);
    for (wo, wi) in gotwo.zip(gotwi) {
        assert_eq!(wo.matchstr, wi.matchstr);
    }
}


#[test]
fn completes_trait_methods_in_trait_impl() {
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
";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, &path, &path);
    let pos = session.load_file(path).coords_to_point(11, 17).unwrap();  // fn trait
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!(got.matchstr, "traitf");
    assert_eq!(got.contextstr, "fn traitf() -> bool");

    let pos = session.load_file(path).coords_to_point(12, 17).unwrap();  // fn trait
    let got = complete_from_file(src, &path, pos, &session).nth(0).unwrap();
    assert_eq!(got.matchstr, "traitm");
    assert_eq!(got.contextstr, "fn traitm(&self) -> bool");
}
