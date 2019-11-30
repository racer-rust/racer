use racer_testutils::*;

#[test]
fn completes_async_fn() {
    let src = r#"
    async fn say_hey() {
        println!("Hey!")
    }
    fn main() {
        say_h~
    }
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "say_hey");
}

#[test]
fn completes_poll() {
    let src = r#"
    async fn say_hey() {
        println!("Hey!")
    }
    async fn waiting_for() {
        let handle = say_hey();
        handle.po~
    }
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "poll");
}

#[test]
fn completes_await() {
    let src = r#"
    async fn say_hey() {
        println!("Hey!")
    }
    async fn waiting_for() {
        let handle = say_hey();
        handle.awa~
    }
"#;
    let got = get_only_completion(src, None);
    assert_eq!(got.matchstr, "await");
}

#[test]
fn completion_in_async_block() {
    let src = r#"
    fn main() {
        async {
            println~
        }
    }"#;
    assert_eq!(get_definition(src, None).matchstr, "println!");
}
