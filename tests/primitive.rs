extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn finds_def_of_i64() {
    let src = "
    fn main() {
        let a: i64~ = 0;
    }
    ";
    let got = get_definition(src, None);
    assert_eq!(got.matchstr, "i64", "{:?}", got);
}

#[test]
fn get_def_of_str_method() {
    let src = r#"
        fn check() {
            "hello".to_lowerca~se();
        }
    "#;

    let got = get_definition(src, None);
    assert_eq!("to_lowercase", got.matchstr);
}

#[test]
fn completes_liballoc_method_for_str() {
    let src = r#"
    fn in_let() {
        let foo = "hello";
        foo.to_lowerc~
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "to_lowercase");
}

#[test]
fn completes_libcore_method_for_str() {
    let src = r#"
    fn in_let() {
        let foo = "hello";
        foo.le~
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "len");
}

// Experimental featrue
#[test]
fn completes_tuple_field() {
    let src = "
    fn foo() -> (usize, usize) { (1, 2) }
    fn main() {
        foo().~
    }
    ";
    let got = get_all_completions(src, None);
    assert!(
        got.into_iter()
            .all(|ma| ma.matchstr == "0" || ma.matchstr == "1")
    )
}

#[test]
fn completes_methods_for_char() {
    let src = r#"
    fn main() {
        'c'.is_upperc~
    }
    "#;

    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "is_uppercase");
}
