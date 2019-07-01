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
    assert!(get_all_completions(src, None)
        .into_iter()
        .any(|ma| ma.matchstr == "clone"));
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

#[test]
fn completes_fn_bounds_for_struct_member() {
    let src = "
        fn main() {
            struct St<T> {
                mem1: String,
                mem2: T,
            }
            fn f<T: Clone>(st: St<T>) {
                st.mem2.clone_fro~
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "clone_from");
}

#[test]
fn completes_impl_trait_in_arg_position() {
    let src = "
    struct MyType;
    trait Foo {
        fn foo(&self);
    }
    impl Foo for MyType {
        fn foo(&self) {
            unimplemented!();
        }
    }

    fn bar(b: impl Foo) {
        b.~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "foo");
}

#[test]
fn completes_impl_trait_in_fn_return_position() {
    let src = "
    struct MyType;
    trait Foo {
        fn foo(&self);
    }
    impl Foo for MyType {
        fn foo(&self) {
            unimplemented!();
        }
    }
    fn bar() -> impl Foo {
        MyType
    }
    fn main() {
        let mytype = bar();
        mytype.~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "foo");
}

#[test]
fn completes_returned_impl_trait_in_othermod() {
    let src = "
     fn main() {
         m::it().meth~
     }
mod empty {                                     }
mod m {
     pub trait Trait {
         fn method(&self) {}
     }
     pub fn it() -> impl Trait {
     }
}
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_arg_impl_trait_in_othermod() {
    let src = "
     fn f(t: impl m::Trait) {
          t.me~
     }
mod empty {                                     }
mod m {
     pub trait Trait {
         fn method(&self) {}
     }
}
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_unsafe_trait_methods_for_fnarg() {
    let src = "
        fn main() {
            unsafe trait Trait {
                fn method(&self);
            }
            fn func<T: Trait>(arg: &T) {
                unsafe { arg.meth~ }
            }
        }
        ";
    assert_eq!(get_only_completion(src, None).matchstr, "method");
}

#[test]
fn completes_assoc_type_for_type_param_in_fn() {
    let src = "
    trait Object {
        type BaseObj: Object;
    }
    fn f<O: Object>(o: O) {
        O::Base~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "BaseObj");
}

#[test]
fn completes_assoc_fn_for_type_param_in_fn() {
    let src = "
    trait Object {
        fn init_typeobj() {}
    }
    fn f<O: Object>(o: O) {
        O::init_~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "init_typeobj");
}

#[test]
fn completes_assoc_type_for_type_param_in_impl() {
    let src = "
    trait Object {
        type BaseObj: Object;
    }
    struct ObjectWrapper<O> {
        inner: O,
    }
    impl<O: Object> ObjectWrapper<O> {
        const A: O::BaseO~
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "BaseObj");
}

#[test]
fn completes_assoc_fn_for_type_param_in_impl() {
    let src = "
    trait Object {
        fn init_typeobj() {}
    }
    struct ObjectWrapper<O> {
        inner: O,
    }
    impl<O: Object> ObjectWrapper<O> {
        fn method(&self) {
            O::init~
        }
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "init_typeobj");
}

#[test]
fn completes_assoc_constant_for_type_param_impl_bound() {
    let src = "
    trait Object {
        const OFFSET: usize;
    }
    struct ObjectWrapper<O> {
        inner: O,
    }
    impl<O: Object> ObjectWrapper<O> {
        fn method(&self) {
            O::OFF~
        }
    }
    ";
    assert_eq!(get_only_completion(src, None).matchstr, "OFFSET");
}

#[test]
fn print_trait_object() {
    let src = "
    trait Object {}
    struct Obj(Box<dyn Object>);
    fn main() {
        let obj: Obj = function();
        obj.~
    }
    ";
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "0");
    assert_eq!(got.contextstr, "Box<dyn Object>");
}
