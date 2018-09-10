extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn enum_with_type_annotation() {
    let src = "
    fn main() {
        let s: Option<String> = None;
        if let Some(s) = s{
           s.capa~
        }
    }
";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn enum_variant_with_path() {
    let src = "
    fn main() {
        let s = Some(String::new());
        if let Some(s) = s{
           s.capa~
        }
    }
";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn enum_variant_with_tuple() {
    let src = "
    fn main() {
        let s = Some((String::new(), 0u32));
        if let Some((s, _)) = s{
           s.capa~
        }
    }
";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}
