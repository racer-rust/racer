use racer_testutils::*;

#[test]
fn completes_methods_for_vec_index() {
    let src = "
    fn main() {
        let v: Vec<String> = vec![];
        v[0].capa~
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn completes_methods_for_vecdeque_index() {
    let src = "
    use std::collections::VecDeque;
    fn main() {
        let v: VecDeque<String> = f();
        v[0].capa~
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}
