extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn try_operator() {
    let src = "
        pub struct Foo(u16);

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct OddError;

        fn be_even(val: Foo) -> Result<Foo, OddError> {
            if val.0 % 2 == 1 {
                Err(OddError)
            } else {
                Ok(val)
            }
        }

        pub fn half(val: Foo) -> Result<Foo, OddError> {
            Ok(Foo(be_even(val)?.~0 / 2))
        }
    ";

    let got = get_definition(src, None);
    assert_eq!("0", got.matchstr);
}

#[test]
fn try_operator_struct() {
    let src = "
    struct Foo {
        pub bar: String,
        pub baz: bool,
    }

    struct LongError;

    fn validate(s: String) -> Result<Foo, LongError> {
        if s.chars().count() < 10 {
            Ok(Foo { bar: s, baz: true })
        } else {
            Err(())
        }
    }

    fn process(s: String) -> Result<bool, LongError> {
        Ok(validate(s)?.b~az)
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(2, got.len());
    assert_eq!("bar", got[0].matchstr);
    assert_eq!("baz", got[1].matchstr);
}

#[test]
fn let_then_try_with_struct() {
    let src = "
    struct Foo {
        pub bar: String,
        pub baz: bool,
    }

    struct LongError;

    fn validate(s: String) -> Result<Foo, LongError> {
        if s.chars().count() < 10 {
            Ok(Foo { bar: s, baz: true })
        } else {
            Err(())
        }
    }

    fn process(s: String) -> Result<bool, LongError> {
        let foo = validate(s);
        Ok(foo?.b~az)
    }
    ";

    let got = get_all_completions(src, None);
    assert_eq!(2, got.len());
    assert_eq!("bar", got[0].matchstr);
    assert_eq!("baz", got[1].matchstr);
}

#[test]
fn let_try() {
    let src = "
    pub struct Foo(u16);

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct OddError;

    fn be_even(val: Foo) -> Result<Foo, OddError> {
        if val.0 % 2 == 1 {
            Err(OddError)
        } else {
            Ok(val)
        }
    }

    pub fn half(val: Foo) -> Result<Foo, OddError> {
        let foo = be_even(val)?;
        Ok(Foo(foo.~0 / 2))
    }
    ";

    let got = get_definition(src, None);
    assert_eq!("0", got.matchstr);
}

#[test]
fn let_try_socket() {
    let src = r#"
    use std::net::UdpSocket;
    use std::io;
    fn main() -> io::Result<()> {
        let sock = UdpSocket::bind("127.0.0.1:1234")?;
        sock.multicast_~
    }
"#;
    let got = get_all_completions(src, None);
    assert!(got.into_iter().any(|ma| ma.matchstr == "multicast_loop_v6"));
}

#[test]
fn let_try_option() {
    let src = r#"
    fn f() -> Option<String> {
        Some(String::new())
    }
    fn f2() -> Option<()> {
        let s = f()?;
        s.as_mut_v~
    }
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "as_mut_vec");
}

#[test]
fn try_option() {
    let src = r#"
    fn f() -> Option<String> {
        Some(String::new())
    }
    fn f2() -> Option<()> {
        f()?.as_mut_v~
    }
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "as_mut_vec");
}
