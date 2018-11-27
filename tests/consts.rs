extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn completes_methods_for_consts() {
    let src = r#"
    const FILE: &'static str = "main.rs";
    fn main() {
        FILE.chars~
    }
    "#;
    assert_eq!(get_only_completion(src, None).matchstr, "chars");
}

#[test]
fn completes_methods_for_static() {
    let src = r#"
    static FILE: &'static str = "main.rs";
    fn main() {
        FILE.chars~
    }
    "#;
    assert_eq!(get_only_completion(src, None).matchstr, "chars");
}
