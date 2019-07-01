use racer::Coordinate;
use racer_testutils::*;

#[test]
fn complets_static_methods_for_aliased_struct() {
    let src = r#"
        mod mymod {
            pub struct St {
                mem1: String,
                mem2: usize,
            }
            impl St {
                pub fn new() -> Self {
                    St { mem1: "a".to_owned(), mem2: 5 }
                }
            }
        }
        fn main() {
            use mymod::St as S;
            let s = S::ne~
        }
    "#;
    assert_eq!(get_only_completion(src, None).matchstr, "new");
}

#[test]
fn completes_names_for_aliased_module() {
    let src = r#"
        mod mymod {
            pub struct St {
                mem1: String,
                mem2: usize,
            }
            impl St {
                pub fn new() -> Self {
                    St { mem1: "a".to_owned(), mem2: 5 }
                }
            }
        }
        fn main() {
            use mymod as m;
            let s = m::St::ne~
        }
    "#;
    assert_eq!(get_only_completion(src, None).matchstr, "new");
}

#[test]
fn finds_definition_of_use_as() {
    let src = r#"
        mod mymod {
            pub struct St {
                mem1: String,
                mem2: usize,
            }
        }
        fn main() {
            use mymod::St as S;
            let s = S~::new();
        }
    "#;
    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "S");
    assert_eq!(got.coords.unwrap(), Coordinate::new(9, 29));
}

#[test]
fn finds_definition_of_use_self_as() {
    let src = r#"
        mod mymod {
            pub struct St {
                mem1: String,
                mem2: usize,
            }
        }
        fn main() {
            use mymod::{self as m, St};
            let s = m~
        }
    "#;
    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "m");
    assert_eq!(got.coords.unwrap(), Coordinate::new(9, 32));
}

// moved from system.rs
#[test]
fn follows_use_as() {
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
    let _src2 = dir.write_file("src2.rs", src2);
    let got = get_definition(src, Some(dir));
    assert_eq!(got.matchstr, "myfoofn");
    assert_eq!(got.contextstr, "src2::myfn as myfoofn");
}

// moved from system.rs
/// Verifies fix for https://github.com/racer-rust/racer/issues/753
#[test]
fn follows_use_as_in_braces() {
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
    assert_eq!(got.contextstr, "Wrapper as Wpr");
}

// moved from system.rs
#[test]
fn completes_for_type_alias() {
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

    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

// for use_nested_groups
// moved from system.rs
#[test]
fn follows_use_aliased_self() {
    let src = r"
    use std::collections::{self as col, hash_map::*, HashMap};
    fn main() {
        let heap = col::BinaryHeap::ne~w();
    }
    ";

    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "new");
}

#[test]
fn completes_for_alias_with_resolved_generics() {
    let src = "
    type Svec = Vec<String>;
    fn main() {
        let v = Svec::new();
        v[0].capa~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "capacity");
}

#[test]
fn completes_for_alias_with_type_param() {
    let src = "
    struct MyError;
    impl MyError {
        fn method(&self) {}
    }
    type Result<V> = Result<V, MyError>;
    fn fail() -> Result<V> {
       Err(MyError)
    }
    fn main() {
        if let Err(e) = fail() {
            e.meth~
        }
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}
