extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn completes_methods_for_for_arg() {
    let src = "
    fn main() {
        let v: Vec<String> = vec![];
        for s in v {
            s.ca~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn completes_methods_for_ref_for_arg() {
    let src = "
    #[derive(Copy, Clone)]
    struct Usize(usize);
    impl Usize { fn method(&self) {} }
    fn main() {
        let v: Vec<&Usize> = vec![];
        for &u in v {
            u.me~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "method");
}

#[test]
fn completes_methods_for_tupled_for_arg() {
    let src = "
    fn main() {
        let v: Vec<(String, usize)> = vec![];
        for (s, i) in v {
            s.ca~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn completes_methods_for_tupls_for_arg() {
    let src = "
    fn main() {
        struct St(String);
        let v: Vec<St> = vec![];
        for St(s) in v {
            s.cap~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}

#[test]
fn completes_methods_for_complex_tupled_for_arg() {
    let src = "
    fn main() {
        struct St(String);
        let v: Vec<(String, St, usize)> = vec![];
        for (s1, St(s2), i) in v {
            s2.ca~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
    let src = "
    #[derive(Copy, Clone)]
    struct Usize(usize);
    impl Usize { fn method(&self) {} }
    fn main() {
        let v: Vec<(String, &Usize)> = vec![];
        for (s, &u) in v {
            u.me~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "method");
}

// blocked by #946
#[test]
#[ignore]
fn completes_methods_for_struct_for_arg() {
    let src = "
    fn main() {
        struct St {
            name: String,
            num: usize,
            __guard: (),
        }
        let v: Vec<St> = vec![];
        for St { name, num: n, .. } in v {
            name.cap~
        }
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "capacity");
}
