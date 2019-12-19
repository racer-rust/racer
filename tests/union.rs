use racer_testutils::*;

#[test]
fn completes_union() {
    let src = r#"
    #[repr(C)]
    union MyUnion {
        f1: u32,
        f2: f32,
    }
    let u: MyU~
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "MyUnion");
}

#[test]
fn completes_maybe_uninit() {
    let src = r#"
    let u: std::mem::Mayb~
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "MaybeUninit");
}
