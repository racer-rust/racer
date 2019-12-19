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

#[test]
fn completes_union_member() {
    let src = r#"
    #[repr(C)]
    union MyUnion {
        uint_member: u32,
        float_member: f32,
    }
    impl MyUnion {
        fn new() -> Self { Self { uint_member: 10 } }
    }
    let uni = unsafe { MyUnion::new().uint~ };
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "uint_member");
}

#[test]
fn completes_union_method() {
    let src = r#"
    #[repr(C)]
    union MyUnion {
        uint_member: u32,
        float_member: f32,
    }
    impl MyUnion {
        fn new() -> Self { Self { uint_member: 10 } }
        fn double(self) -> Self {
            Self {
                uint_member: unsafe { self.uint_member * 2 }
            }
        }
    }
    let uni = unsafe { MyUnion::new().dou~ };
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "double");
}
