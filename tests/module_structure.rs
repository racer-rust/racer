use racer_testutils::*;

#[test]
fn finds_old_type_of_submodule() {
    let src = "
    use submod::f~oo;
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_definition(src, Some(srcdir));
        assert_eq!(got.matchstr, "foo");
        assert!(got.filepath.ends_with("src/submod/foo.rs"));
    })
}

#[test]
fn finds_old_type_of_module() {
    let src = "
    use s~ubmod::foo;
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_definition(src, Some(srcdir));
        assert_eq!(got.matchstr, "submod");
        assert!(got.filepath.ends_with("src/submod/mod.rs"));
    })
}

#[test]
fn finds_new_type_of_module() {
    let src = "
    use s~ubmod2018::foo2018;
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_definition(src, Some(srcdir));
        assert_eq!(got.matchstr, "submod2018");
        assert!(got.filepath.ends_with("src/submod2018.rs"));
    })
}

#[test]
fn finds_new_type_of_submodule() {
    let src = "
    use submod2018::f~oo2018;
    ";

    with_test_project(|dir| {
        let srcdir = dir.nested_dir("src");
        let got = get_definition(src, Some(srcdir));
        assert_eq!(got.matchstr, "foo2018");
        assert!(got.filepath.ends_with("src/submod2018/foo2018.rs"));
    })
}
