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
fn follows_struct_field_for_typedef() {
    let src = "
    pub struct UserData {
        name: String,
        id: usize,
    }
    type U = UserData;
    fn main() {
        U {
            na~
        }
    }";
    assert_eq!(get_only_completion(src, None).matchstr, "name");
}

#[test]
fn follows_struct_field_for_use_as() {
    let src = "
    mod m {
        pub struct UserData {
            name: String,
            id: usize,
        }
    }
    fn main() {
        use m::UserData as U;
        U {
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

#[test]
fn dont_find_struct_field_in_unsafe() {
    let src = r#"
    fn main() {
        unsafe {
            println~
        }
    }"#;
    assert_eq!(get_definition(src, None).matchstr, "println!");
}

#[test]
fn struct_field_scalar_primitive_types() {
    let src = "
    struct Foo<'a> {
        reference: &'a u8,
        array: [u8; 5],
        slice: &'a [u8],
    }

    fn foo(x: Foo) {
        x.~
    }
    ";

    let completions = get_all_completions(src, None);
    assert_eq!(completions.len(), 3);

    for completion in completions {
        let expected = match completion.matchstr.as_ref() {
            "reference" => "reference: &'a u8",
            "array" => "array: [u8; 5]",
            "slice" => "slice: &'a [u8]",
            _ => panic!("unexpected match from Foo struct ({})", completion.matchstr),
        };

        assert_eq!(completion.contextstr, expected);
    }
}
