extern crate racer;
extern crate racer_testutils;
use racer_testutils::*;

#[test]
fn follows_struct_field_in_constructor() {
    let src = "
    pub struct UserData {
        name: String,
        id: usize,
    }
    fn main() {
        UserData {
            na~  
        }
    }";
    assert_eq!(get_only_completion(src, None).matchstr, "name");
}

#[test]
fn follows_enum_variant_field_in_constructor() {
    let src = "
    enum UserData {
        Type1 { name: String, id: usize },
        Type2,
    }
    fn main() {
        UserData::Type1 { nam~ }
    }";
    assert_eq!(get_only_completion(src, None).matchstr, "name");
}

#[test]
fn follows_struct_field_in_let() {
    let src = "
    struct UserData {
        name: String,
        id: usize,
    }
    fn main() {
        let UserData { id, nam~  } = f();
    }";
    assert_eq!(get_only_completion(src, None).matchstr, "name");
}

#[test]
fn follows_struct_field_in_if_let() {
    let src = "
    struct UserData {
        name: String,
        id: usize,
    }
    fn main() {
        if let UserData { id, nam~  } = f() {

        }
    }";
    assert_eq!(get_only_completion(src, None).matchstr, "name");
}

#[test]
fn finds_struct_field_in_constructor() {
    let src = r#"
    struct UserData {
        name: String,
        id: usize,
    }
    fn main() {
        UserData {
            name: "abc".to_owned(),
            i~d: 
        }
    }"#;
    assert_eq!(get_definition(src, None).matchstr, "id");
}
