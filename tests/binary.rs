use racer_testutils::*;

#[test]
fn completes_fields_for_binary_operators() {
    let src = r"
    use std::ops::Add;
    struct Left {
        x: i32,
        y: i32,
    }
    impl Add for Left {
        type Output = Left;
        fn add(self, other: Left) -> Self::Output {
            Left {
                x: self.x + other.x,
                y: self.y + other.y,
            }
        }
    }
    fn main() {
        let a = Left { x: 1, y: 0 } + Left { x: 2, y: 3 };
        a.~
    }
";
    let got = get_all_completions(src, None);
    assert!(got.iter().any(|ma| ma.matchstr == "x"));
    assert!(got.iter().any(|ma| ma.matchstr == "y"));
    assert!(got.iter().any(|ma| ma.matchstr == "add"));
}

#[test]
fn completes_binary_operators_for_different_types() {
    let src = r"
    use std::ops::Sub;
    struct Left {
        x: i32,
        y: i32,
    }
    struct Right {
        x: i32,
        y: i32
    }
    impl Sub<Right> for Left {
        type Output = Left;
        fn sub(self, other: Right) -> Self::Output {
            Left {
                x: self.x - other.x,
                y: self.y - other.y,
            }
        }
    }
    fn main() {
        let a = Left { x: 1, y: 0 } - Right { x: 2, y: 3 };
        a.~
    }
";
    let got = get_all_completions(src, None);
    assert!(got.iter().any(|ma| ma.matchstr == "x"));
    assert!(got.iter().any(|ma| ma.matchstr == "y"));
    assert!(got.iter().any(|ma| ma.matchstr == "sub"));
}

#[test]
fn test_operator_precedence_given_to_type_on_left() {
    let src = r"
    use std::ops::Sub;
    struct Left {
        x: i32,
        y: i32,
    }
    struct Right {
        x: i32,
        y: i32
    }
    struct Foo;
    impl Foo {
        fn foo(&self) -> Option<i32>  {
            Some(33)
        }
    }
    struct Bar;
    impl Bar {
        fn bar(&self) -> Option<i32>  {
            Some(33)
        }
    }
    impl Sub<Right> for Left {
        type Output = Foo;
        fn sub(self, other: Right) -> Self::Output {
             Foo
        }
    }
    impl Sub<Left> for Right {
        type Output = Bar;
        fn sub(self, other: Right) -> Self::Output {
             Bar
        }
    }
    fn main() {
        let a = Left { x: 1, y: 0 } - Right { x: 2, y: 3 };
        a.~
    }
";
    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "foo");
}

#[test]
fn completes_if_operator_trait_is_not_explicit() {
    let src = r"
    use std::ops::Sub;
    struct Left {
        x: i32,
        y: i32,
    }
    struct Bar;
    impl Bar {
        fn bar(&self) -> Option<i32>  {
            Some(33)
        }
    }
    impl Sub for Left {
        type Output = Bar;
        fn sub(self, other: Left) -> Self::Output {
             Bar
        }
    }
    fn main() {
        let a = Left { x: 1, y: 0 } - Left { x: 2, y: 3 };
        a.~
    }
";
    let got = get_one_completion(src, None);
    assert_eq!(got.matchstr, "bar");
}
