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
