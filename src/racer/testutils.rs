#[cfg(test)]
pub fn rejustify(src: &str) -> String {
    let s = &src[1..]; // remove the newline
    let mut sb = String::new();
    for l in s.lines() {
        let tabless = &l[4..];
        sb.push_str(tabless);
        if tabless.len() != 0 {
            sb.push_str("\n");
        }
    }
    let newlen = sb.len()-1; // remove the trailing newline
    sb.truncate(newlen);
    sb
}

#[cfg(test)]
pub fn slice(src: &str, (begin, end): (usize, usize)) -> &str {
    &src[begin..end]
}

#[macro_export]
macro_rules! test_source {
    (rejustify $e:expr) => {
        $crate::core::new_source(rejustify($e))
    };
    ($e:expr) => {
        $crate::core::new_source(String::from($e))
    };
}
