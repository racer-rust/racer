extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn finds_type_parameter_for_fnarg() {
    let src = "
        fn main() {
            trait Trait {
                fn method(&self);
            }
            fn func<T: Trait>(arg: &T) {
                arg.meth~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_methods_for_fnarg_by_trait_bounds() {
    let src = "
        fn main() {
            trait Trait {
                fn method(&self);
            }
            fn func<T: Trait>(arg: &T) {
                arg.meth~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_methods_for_fnarg_by_where_clause() {
    let src = "
        fn main() {
            trait Trait {
                fn method(&self);
            }
            fn func<T>(arg: &T)
            where
                T: Trait,
            {
                arg.meth~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_external_methods_for_fnarg_by_trait_bounds() {
    let src = "
        fn main() {
            fn func<T: Debug + Clone>(arg: &T) {
                arg.clo~
            }
        }
        ";
    assert!(
        get_all_completions(src, None)
            .into_iter()
            .any(|ma| ma.matchstr == "clone")
    );
}

#[test]
fn completes_inherited_methods_for_fnarg_by_trait_bounds() {
    let src = "
        fn main() {
            trait Inherited {
                fn inherited(&self);
            }
            trait Trait: Inherited {
                fn method(&self);
            }
            fn func<T: Trait>(arg: &T) {
                arg.inheri~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "inherited");
}

// test for checking racer don't cause INF loop
#[test]
fn completes_inherited_methods_with_cycle() {
    let src = "
        fn main() {
            trait Inherited2: Inherited1 {}
            trait Inherited1: Inherited2 {
                fn inherited(&self);
            }
            trait Trait: Inherited1 {
                fn method(&self);
            }
            fn func<T: Trait>(arg: &T) {
                arg.inheri~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "inherited");
}

#[test]
fn completes_impled_bounds_for_fnarg() {
    let src = "
        fn main() {
            struct St<T>(T);
            trait Trait: Sized {
                fn method(&self);
            }
            impl<T: Trait> St<T> {
                fn new(t: T) -> Self {
                    t.met~
                }
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_impled_bounds_for_self() {
    let src = "
        fn main() {
            trait Trait: Sized {
                fn method(&self);
            }
            impl<T: Trait> Clone for T {
                fn f(self) -> Self {
                    self.me~
                }
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_impled_bounds_for_self_field() {
    let src = "
        fn main() {
            struct St<T> {
                mem1: String,
                mem2: T,
            }
            trait Trait: Sized {
                fn method(&self);
            }
            impl<T: Trait> St<T> {
                fn f(self) -> Self {
                    self.mem2.m~
                }
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_impled_bounds_for_ref_self_field() {
    let src = "
        fn main() {
            struct St<'a, T> {
                mem1: String,
                mem2: &'a T,
            }
            trait Trait: Sized {
                fn method(&self);
            }
            impl<T: Trait> St<T> {
                fn f(self) -> Self {
                    self.mem2.m~
                }
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}
