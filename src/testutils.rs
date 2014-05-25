#[cfg(test)]
pub fn rejustify(src: &str) -> String {
    let s = src.slice_from(1); // remove the newline
    let mut sb = String::new();
    for l in s.lines() {
        let tabless = l.slice_from(4);
        sb.push_str(tabless); 
        if tabless.len() != 0 { 
            sb.push_str("\n");
        }
    }
    let newlen = sb.len()-1; // remove the trailing newline
    sb.truncate(newlen);
    return sb;
}

#[cfg(test)]
pub fn slice<'a>(src: &'a str, (begin, end): (uint, uint)) -> &'a str{
    return src.slice(begin, end);
}
