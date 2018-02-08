extern crate racer;
extern crate rand;

#[macro_use]
extern crate lazy_static;

use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::thread;

use racer::{complete_from_file, find_definition, Match, MatchType, Coordinate, Point};

lazy_static! {
    static ref SYNC: Mutex<u8> = { Mutex::new(0) };
}

macro_rules! sync {
    () => {
        SYNC.lock().unwrap_or_else(|e| e.into_inner())
    }
}

/// Runs a function with the current directory set to the test project.
fn within_test_project<F, T>(func: F) -> T
    where F: FnOnce() -> T
{
    use std::env;
    use std::panic::{self, AssertUnwindSafe};

    let start = env::current_dir().unwrap();
    let mut test_project_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src");
    test_project_path.push("test_project");

    env::set_current_dir(&test_project_path).unwrap();
    let res = panic::catch_unwind(AssertUnwindSafe(|| func()));
    env::set_current_dir(&start).unwrap();

    match res {
        Err(err) => panic::resume_unwind(err),
        Ok(val) => val
    }
}

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

    fn write_contents(&self, contents: &str) {
        File::create(self.path()).unwrap().write_all(contents.as_bytes()).unwrap();
    }

    /// Get the Path of the TmpFile
    pub fn path(&self) -> &Path {
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
        if self.path().exists() {
            if let Err(e) = fs::remove_file(self.path()) {
                println!("could not remove tmpfile {}: {:?}", self.path().display(), e);
            }
        }
    }
}

pub struct TmpDir {
    path_buf: PathBuf
}

impl TmpDir {
    pub fn new() -> TmpDir {
        TmpDir::with_path(tmpname())
    }

    pub fn with_path<P: AsRef<Path>>(name: P) -> TmpDir {
        let pb = PathBuf::from(name.as_ref());
        fs::create_dir_all(&pb).unwrap();

        TmpDir {
            path_buf: pb
        }
    }

    /// Create new file with name in the directory
    pub fn write_file<P: AsRef<Path>>(&self, name: P, contents: &str) -> PathBuf {
        let name = self.path_buf.join(name);
        File::create(&name).unwrap().write_all(contents.as_bytes()).unwrap();
        name
    }

    pub fn path(&self) -> &Path {
        self.path_buf.as_path()
    }
}

impl Drop for TmpDir {
    fn drop(&mut self) {
        if self.path().exists() {
            if let Err(e) = fs::remove_dir_all(self.path()) {
                println!("could not remove tmpdir {}: {:?}", self.path().display(), e);
            }
        }
    }
}

fn get_pos_and_source(src: &str) -> (Point, String) {
    let point = src.find('~').unwrap();
    (point, src.replace('~', ""))
}

/// Return the completions for the given source.
///
/// The point to find completions at must be marked with '~'.
fn get_all_completions(src: &str, dir: Option<TmpDir>) -> Vec<Match> {
    let dir = dir.unwrap_or_else(|| TmpDir::new());
    let (completion_point, clean_src) = get_pos_and_source(src);
    let path = dir.write_file("src.rs", &clean_src);
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);

    complete_from_file(&path, completion_point, &session).collect()
}

/// Return the first completion for the given source.
fn get_one_completion(src: &str, dir: Option<TmpDir>) -> Match {
    get_all_completions(src, dir).swap_remove(0)
}

/// Return the first completion for the given source, which must be
/// the only one.
///
/// # Panics
/// Panics if there is not exactly one completion.
fn get_only_completion(src: &str, dir: Option<TmpDir>) -> Match {
    let mut all = get_all_completions(src, dir);
    assert_eq!(all.len(), 1);
    all.pop().unwrap()
}

/// Return the definition for the given source.
///
/// The point to find the definition at must be marked with '~'.
fn get_definition(src: &str, dir: Option<TmpDir>) -> Match {
    let dir = dir.unwrap_or_else(|| TmpDir::new());
    let (completion_point, clean_src) = get_pos_and_source(src);
    let path = dir.write_file("src.rs", &clean_src);
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);

    find_definition(&path, completion_point, &session).unwrap()
}


#[test]
fn completes_fn() {
    let _lock = sync!();

    let src = "
    fn  apple() {
    }

    fn main() {
        let b = ap~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
}


#[test]
fn finds_fn_docs() {
    let _lock = sync!();

    let src = "
    /// Orange
    /// juice
    fn apple() {
    }

    fn main() {
        apple~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn finds_struct_docs() {
    let _lock = sync!();

    let src = "
    /// Orange
    /// juice
    struct Apple {
    }

    fn main() {
        Apple~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("Apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn finds_struct_field_docs() {
    let _lock = sync!();

    let src = "
    struct Foo {
        /// Hello docs
        ///
        /// How are you?
        #[allow(dead_code)]
        hello: String,
    }

    fn do_things(f: Foo) -> String {
        f.h~ello.clone()
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("hello", got.matchstr);
    assert_eq!("Hello docs\n\nHow are you?", got.docs);
}

#[test]
fn finds_tuple_struct_field_docs() {
    let _lock = sync!();

    let src = "
    struct Bar(
        /// Hello docs
        String
    );

    fn do_things(b: Bar) -> String {
        b.~0.clone()
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("0", got.matchstr);
    assert_eq!("Hello docs", got.docs);
}

#[test]
fn completes_fn_with_substitute_file() {
    let _lock = sync!();

    let src = "
    fn  apple() {
    }

    fn main() {
        let b = ap~
    }";

    let (_pos, src) = get_pos_and_source(src);
    let cache = racer::FileCache::default();
    let real_file = Path::new("not_real.rs");
    let session = racer::Session::new(&cache);
    session.cache_file_contents(&real_file, src);
    let cursor = Coordinate { line: 6, column: 18 };
    let got = complete_from_file(real_file, cursor, &session).nth(0).unwrap();

    assert_eq!(Some(Coordinate { line: 2, column: 8 }), got.coords);
    assert_eq!("apple", got.matchstr);
}

#[test]
fn completes_pub_fn_locally() {
    let _lock = sync!();

    let src = "
    pub fn apple() {
    }

    fn main() {
        let b = ap~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
}

#[test]
fn completes_pub_fn_locally_precached() {
    let _lock = sync!();

    let src = "
    pub fn apple() {
    }

    fn main() {
        let b = ap~
    }";

    let (pos, src) = get_pos_and_source(src);
    let f = TmpFile::new(&src);
    let path = f.path();
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);
    session.cache_file_contents(&path, src.clone());
    let got = complete_from_file(&path, pos, &session).nth(0).unwrap();
    assert_eq!("apple", got.matchstr);
}

#[test]
fn completes_pub_fn_from_local_package() {
    let _lock = sync!();

    let src = "
    extern crate fixtures;

    use fixtures::foo;

    fn main() {
        let x = foo::~
    }
    ";

    within_test_project(|| {
        let got = get_one_completion(src, None);
        assert_eq!("test", got.matchstr);
    })
}

#[test]
fn completes_pub_fn_from_local_submodule_package() {
    let _lock = sync!();

    let src = "
    extern crate fixtures;

    use fixtures::bar;

    fn main() {
        let x = bar::~
    }
    ";

    within_test_project(|| {
        let got = get_one_completion(src, None);
        assert_eq!("bartest", got.matchstr);
    })
}

#[test]
fn completes_pub_const_fn_locally() {
    let _lock = sync!();

    let src = "
    pub const fn apple() {
    }

    fn main() {
        let b = ap~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
}

#[test]
fn completes_local_scope_let() {
    let _lock = sync!();

    let src = "
    fn main() {
        let apple = 35;
        let b = ap~
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
    assert_eq!(29, got.point);
}

#[test]
fn completes_via_parent_scope_let() {
    let _lock = sync!();

    let src = "
    fn main() {
        let mut apple = 35;
        if foo {
            let b = ap~
        }
    }";

    let got = get_one_completion(src, None);
    assert_eq!("apple", got.matchstr);
    assert_eq!(33, got.point);
}

#[test]
fn completes_for_vec_field_and_method() {
    let _lock = sync!();

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
    let src = "
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
    dir.write_file("mymod.rs", modsrc);
    let path = dir.write_file("src.rs", src);
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);
    let cursor1 = Coordinate { line: 22, column: 18 };
    let got1 = complete_from_file(&path, cursor1, &session).nth(0).unwrap();
    println!("{:?}", got1);
    assert_eq!("stfield", got1.matchstr);
    let cursor2 = Coordinate { line: 23, column: 18 };
    let got2 = complete_from_file(&path, cursor2, &session).nth(0).unwrap();
    println!("{:?}", got2);
    assert_eq!("stmethod", got2.matchstr);
}

#[test]
fn completes_trait_methods() {
    let _lock = sync!();

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
        sub::Foo::traitf();
        t.traitm();
    }
    ";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache1 = racer::FileCache::default();
    let session1 = racer::Session::new(&cache1);
    let cursor1 = Coordinate { line: 18, column: 18};
    let got1 = complete_from_file(&path, cursor1, &session1).nth(0).unwrap();
    let cache2 = racer::FileCache::default();
    let session2 = racer::Session::new(&cache2);
    let cursor2 = Coordinate { line: 19, column: 11};
    let got2 = complete_from_file(&path, cursor2, &session2).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
}

#[test]
fn completes_trait_bounded_methods() {
    let _lock = sync!();

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
    }";
    let f = TmpFile::new(src);
    let path = f.path();
    let cache1 = racer::FileCache::default();
    let session1 = racer::Session::new(&cache1);
    let cursor1 = Coordinate { line: 20, column: 16 };
    let got1 = complete_from_file(&path, cursor1, &session1).nth(0).unwrap();
    let cache2 = racer::FileCache::default();
    let session2 = racer::Session::new(&cache2);
    let cursor2 = Coordinate { line: 21, column: 12 };
    let got2 = complete_from_file(&path, cursor2, &session2).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
}

#[test]
fn completes_trait_bounded_methods_generic_return() {
    let _lock = sync!();

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
    }";

    let f = TmpFile::new(src);
    let path = f.path();
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);
    let cursor1 = Coordinate { line: 24, column: 24 };
    let cursor2 = Coordinate { line: 25, column: 25 };
    let got1 = complete_from_file(&path, cursor1, &session).nth(0).unwrap();
    let got2 = complete_from_file(&path, cursor2, &session).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "structfn");
    assert_eq!(got2.matchstr, "traitfn");
}

#[test]
fn completes_iter_variable_methods() {
    let _lock = sync!();

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
        };

        for item in it {
            item.fie~
        }
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("mymod.rs", modsrc);
    let got = get_one_completion(src, Some(dir));
    assert_eq!(got.matchstr, "field");
}

#[test]
fn completes_for_vec_iter_field_and_method() {
    let _lock = sync!();

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
    let src = "
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
    dir.write_file("mymod.rs", modsrc);
    let path = dir.write_file("src.rs", src);
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);
    let cursor1 = Coordinate { line: 22, column: 18 };
    let got1 = complete_from_file(&path, cursor1, &session).nth(0).unwrap();
    println!("{:?}", got1);
    assert_eq!("stfield", got1.matchstr);
    let cursor2 = Coordinate { line: 23, column: 18 };
    let got2 = complete_from_file(&path, cursor2, &session).nth(0).unwrap();
    println!("{:?}", got2);
    assert_eq!("stmethod", got2.matchstr);
}

#[test]
fn completes_trait_methods_when_at_scope_end() {
    let _lock = sync!();

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
        sub::Foo::traitf();
        t.traitm();
    }
    ";

    let f = TmpFile::new(src);
    let path = f.path();
    let cache = racer::FileCache::default();
    let session = racer::Session::new(&cache);
    let cursor1 = Coordinate { line: 18, column: 18 };
    let got1 = complete_from_file(&path, cursor1, &session).nth(0).unwrap();
    let cursor2 = Coordinate { line: 19, column: 11 };
    let got2 = complete_from_file(&path, cursor2, &session).nth(0).unwrap();
    println!("{:?}", got1);
    println!("{:?}", got2);
    assert_eq!(got1.matchstr, "traitf");
    assert_eq!(got2.matchstr, "traitm");
    assert_eq!(got1.contextstr, "fn traitf() -> bool");
    assert_eq!(got2.contextstr, "fn traitm(&self) -> bool");
}

#[test]
fn completes_for_type_alias() {
    let _lock = sync!();


    let src = "
    mod inner {
        pub type Alias = MyType;
        pub struct MyType;
        impl MyType {
            pub fn method(&self) {}
        }
    }

    fn foo() -> inner::Alias {
        inner::MyType
    }

    fn main() {
        foo().~
    }
    ";


    assert_eq!(get_all_completions(src, None)[0].matchstr, "method");
}

#[test]
fn follows_use() {
    let _lock = sync!();

    let src1 = "
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src = "
    use src1::{foo,myfn};
    mod src1;
    fn main() {
        myfn~();
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("src1.rs", src1);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfn");
    assert_eq!(got.contextstr, "pub fn myfn()");
}

#[test]
fn follows_use_in_braces() {
    let _lock = sync!();
    let src = "
    mod foo {
        pub fn myfn() {}
        pub fn second() {}
    }

    fn main() {
        use foo::{
            myfn, 
            second
        };
        
        my~fn();
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "myfn");
}

#[test]
fn follows_use_as() {
    let _lock = sync!();

    let src2 = "
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src = "
    use src2::myfn as myfoofn;
    mod src2;
    fn main() {
        my~foofn();
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("src2.rs", src2);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfoofn");
    assert_eq!(got.contextstr, "pub fn myfn()");
}

/// Verifies fix for https://github.com/racer-rust/racer/issues/753
#[test]
fn follows_use_as_in_braces() {
    let _lock = sync!();

    let src = "
        mod m {
        pub struct Wrapper {
            pub x: i32,
        }

        pub struct Second {
            pub y: i32,
        }
    }

    fn main() {
        use m::{Wrapper as Wpr, Second};
        let _ = W~pr { x: 1 };
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "Wpr");
    assert_eq!(got.contextstr, "pub struct Wrapper");
}

#[test]
fn follows_use_glob() {
    let _lock = sync!();

    let src3 = "
    pub fn myfn() {}
    pub fn foo() {}
    ";
    let src = "
    use src3::*;
    mod src3;
    fn main() {
        my~fn();
    }
    ";
    let dir = TmpDir::new();
    dir.write_file("src3.rs", src3);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfn");
}

#[test]
fn follows_multiple_use_globs() {
    let _lock = sync!();

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

    src~
    ";

    let dir = TmpDir::new();
    dir.write_file("multiple_glob_test1.rs", src1);
    dir.write_file("multiple_glob_test2.rs", src2);

    let mut has_1 = false;
    let mut has_2 = false;
    let completions = get_all_completions(src, Some(dir));
    for m in completions {
        if m.matchstr == "src1fn" { has_1 = true; }
        if m.matchstr == "src2fn" { has_2 = true; }
    }
    assert!(has_1 && has_2);
}

#[test]
fn single_import_shadows_glob_import() {
    let _lock = sync!();

    let src = "
    use shadowed::*;
    use shadower::Foo;

    mod shadowed {
        pub struct Foo;
    }

    mod shadower {
        pub struct Foo;
    }

    fn main() {
        Foo~;
    }
    ";
    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "Foo");
    println!("{}", got.filepath.display());
    println!("{}", got.point);
    assert_eq!(got.coords, Some(Coordinate { line: 10, column: 19 }));
}

#[test]
fn follows_use_self() {
    let _lock = sync!();

    let src ="
    use foo::use_self_test::{self, bar};

    mod foo {
        pub mod use_self_test {
            pub fn bar() {}
        }
    }

    use_s~
    ";

    let completions = get_all_completions(src, None);
    assert!(completions.into_iter().any(|m| m.matchstr == "use_self_test"));

    let src ="
    use use_self_test::self;

    mod use_self_test {
    }

    use_s~
    ";

    let completions = get_all_completions(src, None);
    assert!(completions.into_iter().any(|m| m.matchstr == "use_self_test"));
}

/// This test addresses https://github.com/racer-rust/racer/issues/645 by
/// confirming that racer will not return duplicate results for a module.
#[test]
fn completes_mod_exactly_once() {
    let _lock = sync!();
    let src = "
    mod sample {
        pub struct Bar;
    }

    mod happy {
        use sample;

        fn do_things(bar: sampl~e::Bar) {
            
        }
    }
    ";

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "sample");
    assert_eq!(got.mtype, MatchType::Module);
}

/// This test verifies that any result deduplication techniques
/// are robust enough to avoid deduplication of multiple results
/// which happen to share a match string.
#[test]
fn completes_mod_and_local_with_same_name() {
    let _lock = sync!();
    let src = "
    mod sample {
        pub struct Bar;
    }

    mod happy {
        use sample;

        fn do_things(bar: sample::Bar) {
            let sample = bar;
            let other = sampl~e::Bar;
        }
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(got.len(), 2);
    assert_eq!(got[0].matchstr, "sample");
    assert_eq!(got[1].matchstr, "sample");
}

#[test]
fn completes_out_of_order_mod_use_with_same_fn_name_as_mod() {
    let _lock = sync!();

    let src = "
    use foo::foo;

    mod foo {
        pub fn foo() {}
    }

    fn main() {
        f~
    }";

    let mut has_module = false;
    let mut has_function = false;
    let completions = get_all_completions(src, None);
    for m in completions {
        match (&*m.matchstr, m.mtype) {
            ("foo", MatchType::Module) => has_module = true,
            ("foo", MatchType::Function) => has_function = true,
            _ => (),
        }
    }
    assert!(has_module && has_function);
}

#[test]
fn ignores_self_referential_unresolved_import() {
    let _lock = sync!();

    let src = "use foo::foo;f~";

    let completions = get_all_completions(src, None);
    assert!(!completions.iter().any(|m| m.matchstr == "foo"));
}

#[test]
fn ignores_self_referential_unresolved_import_long() {
    let _lock = sync!();

    let src = "use foo::bar::foo;f~";

    let completions = get_all_completions(src, None);
    assert!(!completions.iter().any(|m| m.matchstr == "foo"));
}

#[test]
fn ignores_self_referential_unresolved_imports() {
    let _lock = sync!();

    let src = "
    use foo::bar;
    use bar::baz;
    use baz::foo;
    f~";

    let completions = get_all_completions(src, None);
    assert!(!completions.iter().any(|m| m.matchstr == "foo"));
}

#[test]
fn ignores_self_referential_unresolved_imports_across_modules() {
    let _lock = sync!();

    let src = "
    use foo::bar;

    mod foo {
        pub use super::bar;
    }
    b~";

    let completions = get_all_completions(src, None);
    assert!(!completions.iter().any(|m| m.matchstr == "bar"));
}

#[test]
fn finds_external_mod_docs() {
    let _lock = sync!();

    let src1 = "// Copyright notice

//! The mods multiline
//! documentation
    ";
    let src = "
    mod external_mod;
    use external_mod;

    fn main() {
        external_mod~
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("external_mod.rs", src1);
    let got = get_one_completion(src, Some(dir));
    assert_eq!("external_mod", got.matchstr);
    assert_eq!("The mods multiline\ndocumentation", got.docs);
}

#[test]
fn finds_external_struct_docs() {
    let _lock = sync!();

    let src1 = "
    /// Orange
    /// juice
    pub struct Apple {
        pub a: u8,
    }";
    let src = "
    use external_struct::Apple;
    mod external_struct;

    fn main() {
        Apple~
    }";

    let dir = TmpDir::new();
    dir.write_file("external_struct.rs", src1);
    let got = get_one_completion(src, Some(dir));
    assert_eq!("Apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn finds_external_fn_docs() {
    let _lock = sync!();

    let src1 = "
    /// Orange
    /// juice

    pub fn apple() {
        let x = 1;
    }";
    let src = "
    use external_fn::apple;
    mod external_fn;

    fn main() {
        apple~
    }";

    let dir = TmpDir::new();
    dir.write_file("external_fn.rs", src1);
    let got = get_one_completion(src, Some(dir));
    assert_eq!("apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

#[test]
fn keeps_newlines_in_external_mod_doc() {
    let _lock = sync!();

    // issue 683: do not remove newlines inside of mod-doc
    let src1 = "// Copyright notice

//! The mods multiline documentation
//!
//! with an empty line
    ";
    let src = "
    mod external_mod;
    use external_mod;

    fn main() {
        external_mod~
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("external_mod.rs", src1);
    let got = get_one_completion(src, Some(dir));
    assert_eq!("external_mod", got.matchstr);
    assert_eq!("The mods multiline documentation\n\nwith an empty line", got.docs);
}

/// Addresses https://github.com/racer-rust/racer/issues/618
#[test]
fn always_get_all_doc_lines() {
    let _lock = sync!();

    let src = "
/// Orange
/// juice
pub fn apple() {
    app~le()
}";

    let got = get_only_completion(src, None);
    assert_eq!("apple", got.matchstr);
    assert_eq!("Orange\njuice", got.docs);
}

/// Addresses https://github.com/racer-rust/racer/issues/594
#[test]
fn find_complete_docs_with_parentheses_on_last_line() {
    let _lock = sync!();

    let src = "
/// Hello world
/// (quux)
pub fn foo() {}

pub fn bar() {
    fo~o()
}
";

    let got = get_only_completion(src, None);
    assert_eq!("foo", got.matchstr);
    assert_eq!("Hello world\n(quux)", got.docs);
}

#[test]
fn follows_use_local_package() {
    let _lock = sync!();

    let src = "
    extern crate fixtures;

    use fixtures::~
    ";

    within_test_project(|| {
        let got = get_one_completion(src, None);
        assert_eq!(got.matchstr, "foo");
    })
}

#[test]
fn completes_struct_field_via_assignment() {
    let _lock = sync!();

    let src = "
    struct Point {
        /// The first item.
        first: f64,
        second: f64
    }

    let var = Point {first: 35, second: 22};
    var.f~
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "first");
    assert_eq!("The first item.", got.docs);
}

#[test]
fn finds_defn_of_struct_field() {
    let _lock = sync!();

    let src = "
    struct Point {
        /// The first item.
        first: f64,
        second: f64
    }

    let var = Point {first: 35, second: 22};
    var.f~irst
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "first");
    assert_eq!("The first item.", got.docs);
}

#[test]
fn finds_impl_fn() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn new() {}
    }

    Foo::n~ew();
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "new");
}

#[test]
fn follows_use_to_inline_mod() {
    let _lock = sync!();

    let src = "
    use foo::myfn;
    mod foo {
        pub fn myfn() {}
    }

    fn main() {
        m~yfn();
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "myfn");
}

#[test]
fn struct_field_scalar_primitive_types() {
    let _lock = sync!();

    let src = "
    struct Foo<'a> {
        reference: &'a u8,
        array: [u8; 5],
        slice: &'a [u8],
    }

    fn foo(x: Foo) {
        x.~
    }
    ";

    let completions = get_all_completions(src, None);
    assert_eq!(completions.len(), 3);

    for completion in completions {
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
    let _lock = sync!();

    let src = "
    enum MyEnum {
        One, Two
    }

    fn myfn(e: M~yEnum) {}
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "MyEnum");
}

#[test]
fn finds_type() {
    let _lock = sync!();

    let src = "
    type SpannedIdent = Spanned<Ident>
    S~pannedIdent;
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "SpannedIdent");
}

#[test]
fn finds_trait() {
    let _lock = sync!();

    let src = "
    pub trait MyTrait<E: Clone> {}
    M~yTrait
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "MyTrait");
    assert_eq!(got.contextstr, "pub trait MyTrait<E: Clone>");
}

#[test]
fn finds_macro() {
    let _lock = sync!();

    let src = "
    macro_rules! my_macro {
        () => {}
    }
    m~y_macro!();
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "my_macro!");
}

#[test]
fn finds_extern_crate() {
    let _lock = sync!();

    let src = "
    extern crate fixtures;
    f~ixtures
    ";

    within_test_project(|| {
        let got = get_definition(src, None);
        assert_eq!(got.matchstr, "fixtures");
    })
}

#[test]
fn finds_fn_arg() {
    let _lock = sync!();

    let src = "
    fn myfn(myarg: &str) {
         my~arg
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "myarg");
}

#[test]
fn finds_fn_arg_in_incomplete_fn() {
    let _lock = sync!();

    let src = "
    fn myfn(myarg: &str) {
         my~arg
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "myarg");
}

#[test]
fn finds_inline_fn() {
    let _lock = sync!();

    let src = "
    #[inline]
    fn contains<'a>(&needle: &'a str)
        -> bool {
    }

    conta~ins();
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "contains");
    assert_eq!(got.contextstr, "fn contains<'a>(&needle: &'a str) -> bool");
}

#[test]
fn follows_self_use() {
    let _lock = sync!();

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
        my~fn();
    }
    ";

    let dir = TmpDir::new();
    let mymod = TmpDir::with_path(dir.path().join("mymod"));
    mymod.write_file("mod.rs", modsrc);
    let src4path = mymod.write_file("src4.rs", src4);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfn");
    assert_eq!(src4path, got.filepath);
    assert_eq!(28, got.point);
}

#[test]
fn finds_nested_submodule_file() {
    let _lock = sync!();

    let sub3src = "
    pub fn myfn() {}
    ";
    let src = "
    pub mod sub1 {
        pub mod sub2 {
            pub mod sub3;
        }
    }
    sub1::sub2::sub3::m~yfn();
    ";

    let dir = TmpDir::new();
    let sub2name = dir.path().join("sub1").join("sub2");
    let _sub2dir = TmpDir::with_path(&sub2name);
    let src3 = TmpFile::with_path(&sub2name.join("sub3.rs"), sub3src);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfn");
    assert_eq!(src3.path(), got.filepath);
}

#[test]
fn follows_super_in_sub_module() {
    let _lock = sync!();

    let src = "
    pub fn iamhere() { }
    mod inner { pub use super::ia~mhere; }
    ";

    let got = get_definition(src, None);
    assert_eq!("iamhere", got.matchstr);
}

#[test]
fn follows_super_in_local_sub_module() {
    let _lock = sync!();

    let src = "
    mod inner {
      pub fn iamhere() { }
      mod inner2 { pub use super::iamh~ere; }
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("iamhere", got.matchstr);
}

#[test]
fn follows_use_to_impl() {
    let _lock = sync!();

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
        Foo::n~ew();
    }
    ";

    let dir = TmpDir::new();
    let mod_path = dir.write_file("mymod.rs", modsrc);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "new");
    assert_eq!(90, got.point);
    assert_eq!(mod_path, got.filepath);
}

#[test]
fn finds_templated_impl_fn() {
    let _lock = sync!();

    let src = "
    struct Foo<T>;
    impl<T> Foo<T> {
        fn new() {}
    }

    Foo::n~ew();
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "new");
}

#[test]
fn follows_fn_to_method() {
    let _lock = sync!();

    let src = "
    struct Foo<T>;
    impl<T> Foo<T> {
        fn new() -> Foo<T> {}
        fn mymethod(&self) {}
    }

    fn main() {
        let v = Foo::new();
        v.my~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn simple_struct_contextstr() {
    let _lock = sync!();

    let src = "
    struct Foo<T>;

    fn myfn() {
        let x: Foo~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.contextstr, "struct Foo<T>;");
}

#[test]
fn struct_contextstr() {
    let _lock = sync!();

    let src = "
    struct
        Foo<T> {
        pub fn foo1();
    }

    fn myfn() {
        let x: Foo~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.contextstr, "struct Foo<T>");
}

#[test]
fn follows_arg_to_method() {
    let _lock = sync!();

    let src = "
    struct Foo<T>;
    impl<T> Foo<T> {
        fn mymethod(&self) {}
    }

    fn myfn(v: &Foo) {
        v.my~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn follows_arg_to_enum_method() {
    let _lock = sync!();

    let src = "
    enum Foo<T> {
       EnumVal
    }
    impl<T> Foo<T> {
        fn mymethod(&self) {}
    }

    fn myfn(v: &Foo) {
        v.my~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn finds_enum_static_method() {
    let _lock = sync!();
    let src = "
    enum Foo {
        Bar,
        Baz
    }

    impl Foo {
        pub fn make_baz() -> Self {
            Foo::Baz
        }
    }

    fn myfn() -> Foo {
        Foo::ma~ke_baz()
    }
    ";

    let got = get_only_completion(src, None);
    assert_eq!("make_baz", got.matchstr);
    assert_eq!(MatchType::Function, got.mtype);
}

#[test]
fn finds_enum_variants_first() {
    let _lock = sync!();
    let src = "
    enum Foo {
        Bar,
        Baz
    }

    impl Foo {
        pub fn amazing() -> Self {
            Foo::Baz
        }
    }

    fn myfn() -> Foo {
        Foo::~Bar
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(3, got.len());
    assert_eq!("Bar", got[0].matchstr);
    assert_eq!("Baz", got[1].matchstr);
    assert_eq!("amazing", got[2].matchstr);
}

#[test]
fn follows_let_method_call() {
    let _lock = sync!();

    let src = "
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
        f.my~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!("mybarmethod", got.matchstr);
}

#[test]
fn follows_chained_method_call() {
    let _lock = sync!();

    let src = "
    struct Foo;
    struct Bar;
    impl<T> Foo<T> {
        fn mymethod(&self) -> Bar {}
    }
    impl<T> Bar<T> {
        fn mybarmethod(&self) -> Bar {}
    }

    fn myfn(v: &Foo) {
        v.mymethod().my~
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!("mybarmethod", got.matchstr);
}

#[test]
fn follows_chained_method_call_returning_self() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn mymethod(&self) {}
        fn new() -> Self {}
    }

    Foo::new().~
    ";

    let got = get_only_completion(src, None);
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn follows_chained_method_call_on_new_line() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn mymethod(&self) {}
        fn new() -> Self {}
    }

    Foo::
    // comment
    new()
    .~
    ";

    let got = get_only_completion(src, None);
    assert_eq!("mymethod", got.matchstr);
}

#[test]
fn discards_inner_fns() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl<T> Foo<T> {
        fn mymethod(&self) -> Bar {
            fn inner() {
            }
        }
    }

    fn myfn(v: &Foo) {
        v.i~
    }
    ";

    let got = get_all_completions(src, None);
    assert!(got.is_empty(), "should not match inner function");
}

#[test]
fn differentiates_type_and_value_namespaces() {
    let _lock = sync!();

    let src = "
    enum MyEnum{ Foo }
    struct Foo;
    impl Foo { pub fn new() -> Foo {} }
    let l = Foo::n~ew();
    ";

    let got = get_definition(src, None);
    println!("{}", got.matchstr);
    println!("{:?}", got.mtype);
    assert_eq!("new", got.matchstr);
}

#[test]
fn follows_self_to_method() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) {
        }

        pub fn another_method(self, feio: uint) {
            self.met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
#[ignore]
fn follows_self_to_method_when_call_on_new_line() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) -> Foo {
        }

        pub fn another_method(self, feio: uint) {
            self.method()
                .met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
fn follows_self_to_trait_method() {
    let _lock = sync!();

    let src = "
    trait Bar {
        pub fn method(self) {
        }
        pub fn another_method(self) {
            self.met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
fn finds_trait_method() {
    let _lock = sync!();

    let src = "
    pub trait MyTrait {
        fn op(self);
        fn trait_method(self){}
    }

    struct Foo;
    impl MyTrait for Foo {
        fn op(self) {
            self.trait~_method();
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("trait_method", got.matchstr);
}

#[test]
fn finds_field_type() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }

    pub struct Foo {
        myfield : Blah
    }

    let f = Foo{ myfield: Blah { subfield: 3}};
    f.myfield.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_tuple_struct_field_type() {
    let _lock = sync!();

    let src = "
    pub struct Blah(Foo);

    pub struct Foo {
        bar: usize,
    }

    let f = Blah(Foo { bar: 3 });
    f.0.b~ar
    ";

    let got = get_definition(src, None);
    assert_eq!("bar", got.matchstr);
}

#[test]
fn finds_a_generic_retval_from_a_function() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub struct Foo<T> {
        myfield: T
    }
    fn myfn() -> Foo<Blah> {}
    myfn().myfield.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_an_enum_option_style_return_type() {
    let _lock = sync!();

    let src = "
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
    s.unwrap().sub~field
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_definition_of_const() {
    let _lock = sync!();

    let src = "
    pub const MYCONST:uint = 3;
    MYC~ONST
    ";

    let got = get_definition(src, None);
    assert_eq!("MYCONST", got.matchstr);
}

#[test]
fn finds_definition_of_static() {
    let _lock = sync!();

    let src = "
    pub static MYSTATIC:uint = 3;
    MYS~TATIC
    ";

    let got = get_definition(src, None);
    assert_eq!("MYSTATIC", got.matchstr);
}

#[test]
fn handles_dotdot_before_searchstr() {
    let _lock = sync!();

    let src = "
    static MYLEN:uint = 30;
    let f = [0i32, ..M~YLEN];
    ";

    let got = get_definition(src, None);
    assert_eq!("MYLEN", got.matchstr);
}

#[test]
#[ignore]
fn finds_definition_of_lambda_argument() {
    let _lock = sync!();

    let src = "
    fn myfn(&|int|) {}
    myfn(|a|~a+3);
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_definition_of_let_tuple() {
    let _lock = sync!();

    let src = "
    let (a, b) = (2,3);
    ~a
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_let_type() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    let (a, b): (uint, Blah);
    b.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_let_expr() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    let (a, b) = (3, Blah{subfield:3});
    b.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_struct_member_via_let_expr() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub struct Foo { field: Blah }

    let Foo { ref field } = Foo { field: Blah { subfield: 1 }};
    field.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_via_fn_retval() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    fn myfn() -> (uint, Blah) {}
    let (a, b) = myfn();
    b.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_type_of_tuple_member_in_fn_arg() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    fn myfn(a: uint, (b, c): (uint, Blah)) {
        c.s~ubfield
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_namespaced_enum_variant() {
    let _lock = sync!();

    let src = "
    pub enum Blah { MyVariant }
    Blah::MyVa~riant
    ";

    let got = get_definition(src, None);
    assert_eq!("MyVariant", got.matchstr);
}

#[test]
fn finds_glob_imported_enum_variant() {
    let _lock = sync!();

    let src = "
    use self::Blah::*;
    pub enum Blah { MyVariant, MyVariant2 }
    MyVa~riant
    ";

    let got = get_definition(src, None);
    assert_eq!("MyVariant", got.matchstr);
}

#[test]
fn finds_enum_variant_through_recursive_glob_imports() {
    let _lock = sync!();

    let src = "
    use foo::*;
    use Bar::*;

    mod foo {
        pub enum Bar { MyVariant, MyVariant2 }
    }
    MyVa~riant
    ";

    let got = get_definition(src, None);
    assert_eq!("MyVariant", got.matchstr);
}

#[test]
#[ignore]
fn uses_generic_arg_to_resolve_trait_method() {
    let _lock = sync!();

    let src = "
    pub trait MyTrait {
        fn trait_method(self){}
    }
    pub fn doit<T:MyTrait>(stream: &mut T) {
        T.trait_met~hod
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("trait_method", got.matchstr);
}

#[test]
fn destructures_a_tuplestruct() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub struct TupleStruct(Blah);
    let TupleStruct(var) = TupleStruct(Blah{subfield:35});
    var.su~bfield
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn destructures_a_tuplestruct_with_generic_arg() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub struct TupleStruct<T>(T);
    let a : TupleStruct<Blah> = TupleStruct(Blah{subfield:35});
    let TupleStruct(var) = a;
    var.su~bfield
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_if_let_ident_defn() {
    let _lock = sync!();

    let src = "
    if let MyOption(myvar) = myvar {
        myvar~
    }
    ";

    let got = get_only_completion(src, None);
    assert_eq!("myvar", got.matchstr);
}

#[test]
fn doesnt_find_if_let_if_not_in_the_subscope() {
    let _lock = sync!();

    let src = "
    let myvar = 3u32;
    if let MyOption(myvar) = myvar {
        myvar
    }
    my~var
    ";

    let got = get_definition(src, None);
    assert_eq!("myvar", got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn finds_rebound_var_in_iflet() {
    let _lock = sync!();

    let src = "
    let o: MyOption<Blah>;
    if let MyOption::MySome(o) = o {
        ~o
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(56, got.point);
}

#[test]
fn handles_if_let() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    if let MyOption::MySome(a) = o {
        a.sub~field
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_if_let_as_expression() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    let foo = if let MyOption::MySome(a) = o { // iflet is an expression
        a.sub~field
    };
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn finds_match_arm_var() {
    let _lock = sync!();

    let src = "
    match foo {
       Some(a) => ~a
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_match_arm_var_in_scope() {
    let _lock = sync!();

    let src = "
    match foo {
       Some(a) => { ~a }
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
}

#[test]
fn finds_match_arm_enum() {
    let _lock = sync!();

    let src = "
    enum MyEnum {
        Foo,
        Bar
    }
    match foo {
       MyEnum::Foo~ => 1,
       MyEnum::Bar => 2
    ";

    let got = get_definition(src, None);
    assert_eq!("Foo", got.matchstr);
}

#[test]
fn finds_match_arm_var_with_nested_match() {
    let _lock = sync!();

    let src = "
    match foo {
       bar => {something}
       Some(a) => {
               let b = match blah {
                           None => ()
               }
               ~a
       }
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
}

#[test]
fn gets_type_via_match_arm() {
    let _lock = sync!();

    let src = "
    pub struct Blah { subfield: uint }
    pub enum MyOption<T> {
        MySome(T),
        MyNone
    }
    let o: MyOption<Blah>;
    match o {
        MyOption::MySome(a) => a.subfi~eld
    ";

    let got = get_definition(src, None);
    assert_eq!("subfield", got.matchstr);
}

#[test]
fn handles_default_arm() {
    let _lock = sync!();

    let src = "
    let o: MyOption<Blah>;
    match o {
        Foo => { }
        _ => ~o
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("o", got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn doesnt_match_rhs_of_let_in_same_stmt() {
    let _lock = sync!();

    let src = "
    let a = 3;      // <--- should match this 'a'
    let a = ~a + 2;  // not this one
    ";

    let got = get_definition(src, None);
    assert_eq!("a", got.matchstr);
    assert_eq!(9, got.point);
}

#[test]
fn finds_unsafe_fn() {
    let _lock = sync!();

    let src = "
    unsafe fn foo() {}

    fn bar() {
        f~oo()
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "foo");
    assert_eq!(got.point, 15);
}

#[test]
fn completes_methods_on_deref_type() {
    let _lock = sync!();

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
        b.o~
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("mymod.rs", modsrc);
    let got = get_one_completion(src, Some(dir));
    assert_eq!(got.matchstr, "one");
}

#[test]
fn finds_type_of_struct_field_reference() {
    let _lock = sync!();

    let src = "
    struct Dolor { sit: u8 }

    struct Lorem<'a> { ipsum: &'a Dolor }

    impl<'a> Lorem<'a> {
        fn sit(&self) {
            let _ = self.ipsum.s~it;
        }
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("sit", got.matchstr);
}

#[test]
fn finds_self_param_when_fn_has_generic_closure_arg() {
    let _lock = sync!();

    // issue #508
    let src = "
    struct MyOption;

    impl MyOption {
        // needs to find 'self' here to see it is a method
        pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Option<U> {
        }
    }

    let a: MyOption;
    a.~map()
    ";

    let got = get_definition(src, None);
    assert_eq!("map", got.matchstr);
}

#[test]
fn completes_methods_on_deref_generic_type() {
    let _lock = sync!();

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
        b.o~
    }
    ";

    let dir = TmpDir::new();
    dir.write_file("mymod.rs", modsrc);
    let got = get_one_completion(src, Some(dir));
    assert_eq!(got.matchstr, "one");
}

#[test]
fn completes_multiple_use_bracket() {
    let _lock = sync!();

    // issue # 96
    // wo: without bracket, wi: with bracket
    let modfile = "
    pub struct StarWars {
        pub Vadar: u8,
    };
    pub struct StarTrek {
        pub Spock: u8,
    };";
    let srcwo = "
    mod modfile1;
    use modfile1::~S
    ";
    let srcwi = "
    mod modfile1;
    use modfile1::{~S
    ";

    let dir = TmpDir::new();
    dir.write_file("modfile1.rs", modfile);
    let gotwo = get_all_completions(srcwo, Some(dir));
    let dir = TmpDir::new();
    dir.write_file("modfile1.rs", modfile);
    let gotwi = get_all_completions(srcwi, Some(dir));

    assert_eq!(gotwo.len(), gotwi.len());
    for (wo, wi) in gotwo.into_iter().zip(gotwi) {
        assert_eq!(wo.matchstr, wi.matchstr);
    }
}

#[test]
fn completes_multiple_use_comma() {
    let _lock = sync!();

    // issue # 96
    // wo: without comma, wi: with comma
    let modfile = "
    pub struct StarWars {
        pub Kenobi: u8,
    };
    pub struct StarTrek {
        pub Spock: u8,
    };";
    let srcwo = "
    mod modfile2;
    use modfile2::~S
    ";
    let srcwi = "
    mod modfile2;
    use modfile2::{StarWars, ~S
    ";

    let dir = TmpDir::new();
    dir.write_file("modfile2.rs", modfile);
    let gotwo = get_all_completions(srcwo, Some(dir));
    let dir = TmpDir::new();
    dir.write_file("modfile2.rs", modfile);
    let gotwi = get_all_completions(srcwi, Some(dir));

    assert_eq!(gotwo.len(), gotwi.len());
    for (wo, wi) in gotwo.into_iter().zip(gotwi) {
        assert_eq!(wo.matchstr, wi.matchstr);
    }
}

#[test]
fn completes_multiple_use_newline() {
    let _lock = sync!();

    let src = "
    mod foo {
        pub struct Bar;

        pub fn myfn() {}
    }

    fn main() {
        use foo::{
            Bar,
            my~fn
        };

        myfn();
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(got.len(), 1);
    assert_eq!(got[0].matchstr, "myfn");
}


#[test]
fn completes_trait_methods_in_trait_impl() {
    let _lock = sync!();

    let src = "
    mod sub {
        pub trait Trait {
            fn traitf() -> bool;
            fn traitm(&self) -> bool;
        }

        pub struct Foo(bool);

        impl Trait for Foo {
            fn traitf() -> bool { false }
            fn traitm~(&self) -> bool { true }
        }
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "traitm");
    assert_eq!(got.contextstr, "fn traitm(&self) -> bool");
}

/// Check if user is offered a completion for a static function defined by a trait.
#[test]
fn completes_trait_fn_in_trait_impl() {
    let _lock = sync!();

    let src = "
    mod sub {
        pub trait Trait {
            fn traitf() -> bool;
            fn traitm(&self) -> bool;
        }

        pub struct Foo(bool);

        impl Trait for Foo {
            fn traitf~() -> bool { false }
            fn traitm(&self) -> bool { true }
        }
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "traitf");
    assert_eq!(got.contextstr, "fn traitf() -> bool");
}

#[test]
fn completes_optional_trait_fn_in_trait_impl() {
    let _lock = sync!();

    let src = "
    mod sub {
        pub trait Trait {
            fn traitf() -> bool {
                true
            }
            
            fn traitm(&self) -> bool;
        }

        pub struct Foo(bool);

        impl Trait for Foo {
            fn traitf~() -> bool { false }
            fn traitm(&self) -> bool { true }
        }
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "traitf");
    assert_eq!(got.contextstr, "fn traitf() -> bool");
}

/// Addresses https://github.com/racer-rust/racer/issues/680. In this case,
/// `sub` should not be interpreted as a method name; it didn't appear after
/// `fn` and therefore would need `Self::`, `self.` or another qualified name
/// to be syntactically valid.
#[test]
fn finds_mod_with_same_name_as_trait_method_in_sig() {
    let _lock = sync!();

    let src = "
    mod sub {
        pub struct Formatter;

        pub trait Fmt {
            fn sub(&self, f: &Formatter);
        }
    }

    struct Sample;

    impl sub::Fmt for Sample {
        fn sub(&self, f: &sub::Fo~rmatter) {

        }
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "Formatter");
}

/// Also addresses issue #680.
#[test]
fn finds_mod_with_same_name_as_trait_method_in_body() {
    let _lock = sync!();

    let src = "
    mod sub {
        pub struct Formatter;

        pub trait Fmt {
            fn sub(&self) -> sub::Formatter;
        }
    }

    struct Sample;

    impl sub::Fmt for Sample {
        fn sub(&self) -> sub::Formatter {
            sub::Fo~rmatter
        }
    }
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "Formatter");
}

/// Also addresses #680
#[test]
fn finds_fmt_formatter() {
    let _lock = sync!();
    let src = r#"
    use std::fmt;

    struct Foo;

    impl fmt::Display for Foo {
        fn fmt(&self, f: &mut fmt::Formatt~er) -> fmt::Result {
            write!(f, "Hello")
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert!(!got.is_empty());
    assert_eq!(got[0].matchstr, "Formatter");
}

/// Also addresses #680
#[test]
fn finds_fmt_method() {
    let _lock = sync!();
    let src = r#"
    use std::fmt;

    struct Foo;

    impl fmt::Display for Foo {
        fn fm~t(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "Hello")
        }
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "fmt");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn finds_field_with_same_name_as_method() {
    let _lock = sync!();

    let src = "
    struct Foo { same_name: uint }
    impl Foo { fn same_name(&self){} }
    let a: Foo;
    a.same_na~me;
    ";

    let got = get_definition(src, None);
    assert_eq!("same_name", got.matchstr);
    assert_eq!(MatchType::StructField, got.mtype);
}

#[test]
fn finds_method_with_same_name_as_field() {
    let _lock = sync!();

    let src = "
    struct Foo { same_name: uint }
    impl Foo { fn same_name(&self){}}
    let a: Foo;
    a.same_na~me();
    ";

    let got = get_definition(src, None);
    assert_eq!("same_name", got.matchstr);
    assert_eq!(MatchType::Function, got.mtype);
}

#[test]
fn finds_self() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn foo() {
            Se~lf
        }
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("Foo", got.matchstr);
}

#[test]
fn finds_self_referenced_functions() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn foo() {
            Self::myfun~ction
        }
        fn myfunction() {}
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("myfunction", got.matchstr);
}

#[test]
fn closure_bracket_scope() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x | { x~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x |", got.contextstr);
}

#[test]
fn closure_bracket_scope_multiple_args() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x,y,z,u | { x~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x,y,z,u |", got.contextstr);
}

#[test]
fn closure_bracket_scope_multiple_args_different_definition() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x,y,z,u | { z~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("z", got.matchstr);
    assert_eq!("| x,y,z,u |", got.contextstr);
}

#[test]
fn closure_bracket_scope_overwrite() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x, y | { y~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("| x, y |", got.contextstr);
}

#[test]
fn closure_bracket_scope_with_types() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32, y: String | { y~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("| x: i32, y: String |", got.contextstr);
}

#[test]
fn closure_bracket_scope_find_outside() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | { y~ } );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("let y = Some(5);", got.contextstr);
}

#[test]
fn closure_scope() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x | x~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x |", got.contextstr);
}

#[test]
fn closure_scope_multiple_args() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x,y,z,u | x~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x,y,z,u |", got.contextstr);
}

#[test]
fn closure_scope_multiple_args_different_definition() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x,y,z,u | z~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("z", got.matchstr);
    assert_eq!("| x,y,z,u |", got.contextstr);
}

#[test]
fn closure_scope_overwrite() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x, y | y~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("| x, y |", got.contextstr);
}

#[test]
fn closure_scope_with_types() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32, y: String | y~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("| x: i32, y: String |", got.contextstr);
}

#[test]
fn finds_impl_with_bang() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl Foo {
        fn invert(&self, b: bool) -> bool { !b }

        fn tst(&self) -> bool {
            self.inv~ert(false)
        }
    ";

    let got = get_definition(src, None);
    assert_eq!("invert", got.matchstr);
}

#[test]
fn ignores_impl_macro() {
    let _lock = sync!();

    let src = "
    struct Foo;
    impl!(Foo);

    impl Foo {
        fn tst(&self) -> bool {
            self.ts~t()
        }
    ";

    let got = get_definition(src, None);
    assert_eq!("tst", got.matchstr);
}

#[test]
fn closure_scope_dont_match_type_annotations() {
    let _lock = sync!();
    let src = "
    struct Foo;
    fn main() {
        let y = Some(Foo);
        y.map(|x: Foo| Fo~o);
    }
    ";

    let got = get_definition(src, None);
    println!("{:?}", got);
    assert_eq!(MatchType::Struct, got.mtype);
    assert_eq!(2, got.coords.unwrap().line);
}

/// The variable `i` doesn't exist in `foo`, so trying to get the definition should
/// fail.
#[test]
#[should_panic]
fn closure_scope_dont_match_bitwise_or() {
    let _lock = sync!();
    let src = "
    fn foo() {
        i~
    }
    fn bar() {
        let i = 0;
        let x = 0 | i;
    }
    fn baz() {
        // 1 || 2;
    }
    ";

    let got = get_definition(src, None);
    println!("Unexpectedly found definition: {:?}", got);
}

#[test]
fn try_operator() {
    let _lock = sync!();

    let src = "
        pub struct Foo(u16);

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct OddError;

        fn be_even(val: Foo) -> Result<Foo, OddError> {
            if val.0 % 2 == 1 {
                Err(OddError)
            } else {
                Ok(val)
            }
        }

        pub fn half(val: Foo) -> Result<Foo, OddError> {
            Ok(Foo(be_even(val)?.~0 / 2))
        }
    ";

    let got = get_definition(src, None);
    assert_eq!("0", got.matchstr);
}

#[test]
fn try_operator_struct() {
    let _lock = sync!();
    let src = "
    struct Foo {
        pub bar: String,
        pub baz: bool,
    }

    struct LongError;

    fn validate(s: String) -> Result<Foo, LongError> {
        if s.chars().count() < 10 {
            Ok(Foo { bar: s, baz: true })
        } else {
            Err(())
        }
    }

    fn process(s: String) -> Result<bool, LongError> {
        Ok(validate(s)?.b~az)
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(2, got.len());
    assert_eq!("bar", got[0].matchstr);
    assert_eq!("baz", got[1].matchstr);
}

#[test]
fn let_then_try_with_struct() {
    let _lock = sync!();
    let src = "
    struct Foo {
        pub bar: String,
        pub baz: bool,
    }

    struct LongError;

    fn validate(s: String) -> Result<Foo, LongError> {
        if s.chars().count() < 10 {
            Ok(Foo { bar: s, baz: true })
        } else {
            Err(())
        }
    }

    fn process(s: String) -> Result<bool, LongError> {
        let foo = validate(s);
        Ok(foo?.b~az)
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(2, got.len());
    assert_eq!("bar", got[0].matchstr);
    assert_eq!("baz", got[1].matchstr);
}

#[test]
fn let_try() {
    let _lock = sync!();

    let src = "
    pub struct Foo(u16);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct OddError;

    fn be_even(val: Foo) -> Result<Foo, OddError> {
        if val.0 % 2 == 1 {
            Err(OddError)
        } else {
            Ok(val)
        }
    }

    pub fn half(val: Foo) -> Result<Foo, OddError> {
        let foo = be_even(val)?;
        Ok(Foo(foo.~0 / 2))
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("0", got.matchstr);
}

#[test]
fn closure_scope_find_outside() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | y~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("y", got.matchstr);
    assert_eq!("let y = Some(5);", got.contextstr);
}

#[test]
fn closure_scope_with_newlines() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(|


x: i32



| x~ );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("|


x: i32



|", got.contextstr);
}

#[test]
fn closure_bracket_scope_with_newlines() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(|


x: i32



| {x~} );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("|


x: i32



|", got.contextstr);
}

#[test]
fn closure_scope_nested() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | y.map(|z| z~) );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("z", got.matchstr);
    assert_eq!("|z|", got.contextstr);
}

#[test]
fn closure_bracket_scope_nested() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | { y.map(|z| { z~ }) });
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("z", got.matchstr);
    assert_eq!("|z|", got.contextstr);
}

#[test]
fn closure_scope_nested_math_outside() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | y.map(|z| x~) );
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x: i32 |", got.contextstr);
}

#[test]
fn closure_bracket_scope_nested_match_outside() {
    let _lock = sync!();

    let src = "
    fn main() {
        let y = Some(5);
        y.map(| x: i32 | { y.map(|z| { x~ }) });
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("x", got.matchstr);
    assert_eq!("| x: i32 |", got.contextstr);
}

// Issue: https://github.com/racer-rust/racer/issues/754
#[test]
fn closure_dont_detect_normal_pipes() {
    let _lock = sync!();

    let src = "
    enum Fruit {
        Apple = 1,
    }

    fn foo(ty: Fruit) -> bool {
        (1 as u8 | Fruit~::Apple as u8) == Fruit::Apple as u8
    }

    fn bar(ty: Fruit) -> bool {
        match ty {
            Fruit::Apple |
            Fruit::Apple => {
                false
            }
        }
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("Fruit", got.matchstr);
    assert_eq!(got.mtype, MatchType::Enum);
}

#[test]
fn closure_test_curly_brackets_in_args() {
    let _lock = sync!();
    
    let src ="
    struct Foo {
        bar: u16
    }

    fn example() -> Result<Foo, ()> {
        Ok(Foo { bar: 10 })
    }

    fn main() {
        example().and_then(|Foo { bar }| { println!(\"{}\", bar~); Ok(()) });
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("bar", got.matchstr);
    assert_eq!("|Foo { bar }|", got.contextstr);
}

#[test]
fn closure_test_multiple_curly_brackets_in_args() {
    let _lock = sync!();
    
    let src ="
    struct Foo {
        bar: u16
    }

    fn example() -> Result<Foo, ()> {
        Ok(Foo { bar: 10 })
    }

    fn main() {
        example().and_then(|Foo { bar }, Foo { ex }, Foo { b }| { println!(\"{}\", bar~); Ok(()) });
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("bar", got.matchstr);
    assert_eq!("|Foo { bar }, Foo { ex }, Foo { b }|", got.contextstr);
}


#[test]
fn literal_string_method() {
    let _lock = sync!();
    let src = r#"
        fn check() {
            "hello".st~arts_with("he");
        }
    "#;

    let got = get_definition(src, None);
    assert_eq!("starts_with", got.matchstr);
}

#[test]
fn literal_string_completes() {
    let _lock = sync!();
    let src = r#"
    fn in_let() {
        let foo = "hello";
        foo.end~s_with("lo");
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("ends_with", got[0].matchstr);
}

#[test]
fn crate_restricted_fn_completes() {
    let _lock = sync!();
    let src = r#"
    pub(crate) fn do_stuff() {
        println!("Hello");
    }

    fn more_stuff() {
        do_~stuff();
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("do_stuff", got[0].matchstr);
}

#[test]
fn mod_restricted_fn_completes() {
    let _lock = sync!();
    let src = r#"
    pub(in some::place_where) fn do_stuff() {
        println!("Hello");
    }

    fn more_stuff() {
        do_~stuff();
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("do_stuff", got[0].matchstr);
}

#[test]
fn finds_definition_of_fn_arg() {
    let _lock = sync!();
    let src = r#"
    pub fn say_hello(name: String) {
        println!("{}", nam~e);
    }
    "#;

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "name");
}

#[test]
fn finds_definition_of_crate_restricted_fn_arg() {
    let _lock = sync!();
    let src = r#"
    pub(crate) fn say_hello(name: String) {
        println!("{}", nam~e);
    }
    "#;

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "name");
}

/// This test should work, but may be failing because there is no `mod foo`
/// in the generated code we parse to get the signature.
#[test]
#[ignore]
fn finds_definition_of_mod_restricted_fn_arg() {
    let _lock = sync!();
    let src = r#"
    pub(in foo) fn say_hello(name: String) {
        println!("{}", nam~e);
    }
    "#;

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "name");
}

#[test]
fn finds_definition_of_super_restricted_fn() {
    let _lock = sync!();
    let src = r#"
    pub(super) fn do_stuff() {
        println!("Hello");
    }

    fn more_stuff() {
        do_~stuff();
    }
    "#;

    let got = get_definition(src, None);
    assert_eq!("do_stuff", got.matchstr);
}

#[test]
fn crate_restricted_struct_completes() {
    let _lock = sync!();
    let src = r#"
    mod codegen { 
        pub(crate) struct Foo {
            pub bar: String,
        }

        fn stuff(f: Foo) -> String {
            f.b~ar
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("bar", got[0].matchstr);
}

#[test]
fn crate_restricted_named_struct_field_completes() {
    let _lock = sync!();
    let src = r#"
    mod codegen { 
        pub struct Foo {
            pub(crate) bar: String,
        }

        fn stuff(f: Foo) -> String {
            f.b~ar
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("bar", got[0].matchstr);
}

#[test]
fn crate_restricted_static_method_completes() {
    let _lock = sync!();
    let src = r#"
    mod codegen { 
        pub struct Foo {
            pub bar: String,
        }

        impl Foo {
            pub(crate) fn with_bar(b: String) -> Self {
                Foo { bar: b }
            }
        }

        fn stuff() -> String {
            Foo::wi~th_bar("Hello".to_string()).bar
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("with_bar", got[0].matchstr);
}

#[test]
fn crate_restricted_impl_method_completes() {
    let _lock = sync!();
    let src = r#"
    mod codegen { 
        pub struct Foo {
            bar: String,
        }

        impl Foo {
            pub(crate) fn get_bar(&self) -> &str {
                &self.bar
            }
        }

        fn stuff(f: Foo) -> String {
            f.ge~t_bar().clone()
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("get_bar", got[0].matchstr);
}

/// This test _should_ pass, but the bogofile produces errors:
///
/// ```ignore
/// error: expected identifier, found keyword `in`
///  --> bogofile:1:5
///   |
/// 1 | pub(in codegen) struct Foo {
///   |     ^^
/// 
/// error: expected one of `)` or `::`, found `codegen`
///  --> bogofile:1:8
///   |
/// 1 | pub(in codegen) struct Foo {
///   |        ^^^^^^^
/// ```
#[test]
#[ignore]
fn mod_restricted_struct_completes() {
    let _lock = sync!();
    let src = r#"
    mod codegen { 
        pub(in codegen) struct Foo {
            pub bar: String,
        }

        fn stuff(f: Foo) -> String {
            f.b~ar
        }
    }
    "#;

    let got = get_all_completions(src, None);
    assert_eq!(1, got.len());
    assert_eq!("bar", got[0].matchstr);
}

#[test]
fn completes_for_global_path_in_fn_return() {
    let _lock = sync!();

    let src = "
    mod bar {
        pub struct Foo;
    }

    mod baz {
        fn foo() -> ::bar::F~oo {
            Foo
        }
    }

    fn main() {}
    ";

    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "Foo");
}

#[test]
fn completes_for_global_path_in_trait_impl_decl() {
    let _lock = sync!();

    let src = "
    mod foo {
        pub trait Bar {}
    }

    mod baz {
        pub struct Test;

        impl ::foo::~Bar for Test {}
    }

    fn main() {}
    ";

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "Bar");
    assert_eq!(got.mtype, MatchType::Trait);
}

// Issue: https://github.com/racer-rust/racer/issues/755
#[test]
fn completes_for_match_type_inference_let_expr() {
    let _lock = sync!();

    let src = r#"
    use std::fs::File;

    fn main() {
        let f = File::open("hey");

        let f = match f {
            Ok(file) => file,
            Err(error) =>  {
                panic!("Error opening file: {:?}", error)
            }
        };
        f.set_p~ermissions(/* args */);
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "set_permissions");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn completes_for_match_type_inference_let_expr_with_block() {
    let _lock = sync!();

    let src = r#"
    use std::fs::File;

    fn main() {
        let f = File::open("hey");

        let f = match f {
            Ok(file) => { file },
            Err(error) =>  {
                panic!("Error opening file: {:?}", error)
            }
        };
        f.set_p~ermissions(/* args */);
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "set_permissions");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn completes_for_match_type_inference_let_expr_with_return() {
    let _lock = sync!();

    let src = r#"
    use std::fs::File;

    fn test() -> String {
        let f = File::open("hey");

        let f = match f {
            Err(error) =>  {
                return "result".to_string();
            },
            Ok(file) => { file }
        };

        f.set_p~ermissions(/* args */);
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "set_permissions");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn completes_for_let_if_let() {
    let _lock = sync!();

    let src = r#"
    use std::fs::File;

    fn test() -> String {
        let f = File::open("hey");

        let f = if let Ok(f) = f { f } else { return "result".to_string(); };

        f.set_p~ermissions(/* args */);
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "set_permissions");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn completes_for_match_type_inference_with_if() {
    let _lock = sync!();

    let src = r#"
    use std::fs::File;

    fn test() -> String {
        let f = File::open("hey");

        let f = match f {
            Err(error) =>  {
                return "result".to_string();
            },
            Ok(file) => { if file.sync_data().is_ok() { return "nice".to_string(); } else { file } }
        };

        f.set_p~ermissions(/* args */);
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "set_permissions");
    assert_eq!(got.mtype, MatchType::Function);
}

#[test]
fn completes_before_first_statement() {
    let _lock = sync!();

    let src = r#"
    fn test() {
        ~
        let x = 8;
    }
    "#;

    let completions = get_all_completions(src, None);
    assert!(completions.into_iter().any(|m| m.matchstr == "std"));
}

#[test]
fn completes_between_statements() {
    let _lock = sync!();

    let src = r#"
    fn test() {
        let x = 8;
        ~
        let y = 55;
    }
    "#;

    let completions = get_all_completions(src, None);
    assert!(completions.into_iter().any(|m| m.matchstr == "std"));
}

// For issue 816
#[test]
fn completes_for_let_after_comments_with_multibyte_char() {
    let _lock = sync!();
    let src = "
    fn main() {
        let option = Some(5);
        let _ = match option {
            // multibyte comment ☆
            Some(variable) => {
                let b = vari~;
                3
            }
            None => 4,
        };
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "variable");
}

// For issue 818
#[test]
fn completes_for_let_destracted_var_over_comment() {
    let _lock = sync!();
    let src = "
    fn main() {
        let option = Some(5);
        let _ = match option {
            Some(variable) /* C-style-comment*/
            // one -liner comment
            /*  nested and /* multiline
                                comment*/  */
            => {
                let b = vari~;
                3
            }
            None => 4,
        };
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "variable");
}

// For Issue #815
#[test]
fn completes_methods_after_raw_string() {
    let _lock = sync!();
    let src = r##"
    fn main() {
        let s = r#"""#;
        let v = Vec::<u32>::new();
        v.l~
    }
    "##;
    assert!(get_all_completions(src, None).iter().any(|ma| ma.matchstr == "len"));
}
