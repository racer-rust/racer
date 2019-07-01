use racer_testutils::*;

#[test]
fn completes_pub_fn_from_local_package() {
    let src = "
    extern crate fixtures;

    use fixtures::foo;

    fn main() {
        let x = foo::~
    }
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_one_completion(src, Some(srcdir));
        assert_eq!("test", got.matchstr);
    })
}

#[test]
fn completes_pub_fn_from_local_submodule_package() {
    let src = "
    extern crate fixtures;

    use fixtures::bar;

    fn main() {
        let x = bar::~
    }
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_one_completion(src, Some(srcdir));
        assert_eq!("bartest", got.matchstr);
    })
}

#[test]
fn follows_use_local_package() {
    let src = "
    extern crate fixtures;

    use fixtures::~
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_all_completions(src, Some(srcdir));
        assert!(got.into_iter().any(|ma| ma.matchstr == "foo"));
    })
}

#[test]
fn follows_use_local_package_2018() {
    let src = "
    use test_crat~
    ";

    with_test_project(|dir| {
        let cratedir = dir.nested_dir("test-crate4");
        let testdir = cratedir.nested_dir("tests");
        let got = get_all_completions(src, Some(testdir));
        assert!(got.into_iter().any(|ma| ma.matchstr == "test_crate4"));
    })
}

#[test]
fn finds_extern_crate() {
    let src = "
    extern crate fixtures;
    f~ixtures
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_definition(src, Some(srcdir));
        assert_eq!(got.matchstr, "fixtures");
    })
}

// regression test for #800
#[test]
fn completes_typedef_in_external_crate() {
    let src = "
    extern crate fixtures;

    use fixtures::Usize~
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_only_completion(src, Some(srcdir));
        assert_eq!(got.matchstr, "UsizeVec");
        assert_eq!(got.contextstr, "pub type UsizeVec = Vec<usize>;");
    })
}

#[test]
fn follows_external_re_export() {
    let src = "
    extern crate rayon;
    fn main() {
        rayon::sco~
    }
    ";
    with_test_project(|dir| {
        let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
        let got = get_only_completion(src, Some(src_dir));
        assert_eq!(got.matchstr, "scope");
    });
}

#[test]
fn follows_rand_crate() {
    let src = "
    extern crate rand;
    use rand::{Rng, thread_rng};
    fn main() {
        let mut rng: Box<Rng> = Box::new(thread_rng());
        rng.gen_rang~
    }
    ";
    with_test_project(|dir| {
        let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
        let got = get_only_completion(src, Some(src_dir));
        assert_eq!(got.matchstr, "gen_range");
    });
}

// For issue 826
#[test]
fn find_crate_doc() {
    let src = "
    extern crate fixtures;
    use fixtur~
    ";
    let doc_str = r#"This is a test project for racer.

# Example:
Basic Usage.

```
extern crate fixtures;
use fixtures::foo;
fn main {
    println!("Racer")
}
```

## Notes:
- We should check racer can parse rust doc style comments
- and some comments..."#;
    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_one_completion(src, Some(srcdir));
        assert_eq!(doc_str, got.docs);
    })
}

// test for re-export
#[test]
fn follows_use_for_reexport() {
    let src = "
    extern crate fixtures;

    use fixtures::use~;
    ";
    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_only_completion(src, Some(srcdir));
        assert_eq!(got.matchstr, "useless_func");
    })
}

#[test]
fn completes_cratename_without_extern() {
    let src = "
    fn main() {
        rayon::sco~
    }
    ";
    with_test_project(|dir| {
        let src_dir = dir.nested_dir("test-crate4").nested_dir("src");
        let got = get_only_completion(src, Some(src_dir));
        assert_eq!(got.matchstr, "scope");
    });
}

#[test]
fn doesnt_complete_cratename_without_extern_in_2015() {
    let src = "
    fn main() {
        rayon::sco~
    }
    ";
    with_test_project(|dir| {
        let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
        let got = get_all_completions(src, Some(src_dir));
        assert!(got.is_empty());
    });
}

#[test]
fn complete_extern_crate() {
    let src = "
    extern crate ray~
    ";
    with_test_project(|dir| {
        let src_dir = dir.nested_dir("test-crate3").nested_dir("src");
        let got = get_only_completion(src, Some(src_dir));
        assert_eq!(got.matchstr, "rayon");
    });
}
