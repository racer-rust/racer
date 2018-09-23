extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn follows_self_to_method() {
    let src = "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) {
        }

        pub fn another_method(self, feio: uint) {
            self.met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
fn follows_self_to_method_in_vis_crate() {
    let src = "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) {
        }

        crate fn another_method(self, feio: uint) {
            self.met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
fn follows_self_to_method_when_call_on_new_line() {
    let src = "
    struct Foo;
    impl Bar for Foo {
        pub fn method(self) -> Foo {
        }

        pub fn another_method(self, feio: uint) {
            self.method()
                .met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}

#[test]
fn follows_self_to_trait_method() {
    let src = "
    trait Bar {
        fn method(self) {
        }
        fn another_method(self) {
            self.met~hod()
        }
    }";

    let got = get_definition(src, None);
    assert_eq!("method", got.matchstr);
}
