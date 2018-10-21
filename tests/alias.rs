extern crate racer;
extern crate racer_testutils;
use racer::Coordinate;
use racer_testutils::*;

#[test]
fn complets_static_methods_for_alias() {
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
