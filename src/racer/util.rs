// Small functions of utility

use racer::SearchType::{self, ExactMatch, StartsWith};

use std;
use std::cmp;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

extern crate regex;

pub fn getline(filepath: &Path, linenum: usize) -> String {
    let mut i = 0;
    let file = BufReader::new(File::open(filepath).unwrap());
    for line in file.lines() {
        //print!("{}", line);
        i += 1;
        if i == linenum {
            return line.unwrap().to_string();
        }
    }
    "not found".to_string()
}

pub fn is_pattern_char(c: char) -> bool {
    c.is_alphanumeric() || c.is_whitespace() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_search_expr_char(c: char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || (c == '_')
}

pub fn txt_matches(stype: SearchType, needle: &str, haystack: &str) -> bool {
    if needle.is_empty() {
        return true;
    }
    let available_chars_in_ident = "r[:alnum:]|_";
    let chars_not_in_ident = String::new() + r"[^" + &available_chars_in_ident + r"]";
    let starts_with_re = String::new() + r"(" + &chars_not_in_ident + r"|^)" + &needle;
    match stype {
        ExactMatch => {
            let match_re = String::new() + &starts_with_re + r"(" + &chars_not_in_ident + r"|$)";
            regex::Regex::new(&match_re).unwrap().is_match(haystack)
        },
        StartsWith => {
            regex::Regex::new(&starts_with_re).unwrap().is_match(haystack)
        }
    }
}

pub fn symbol_matches(stype: SearchType, searchstr: &str, candidate: &str) -> bool {
   match stype {
        ExactMatch => searchstr == candidate,
        StartsWith => candidate.starts_with(searchstr)
    }
}

// pub fn get_backtrace() -> String {
//     let mut m = std::old_io::MemWriter::new();
//     let s = std::rt::backtrace::write(&mut m)
//         .ok().map_or("NO backtrace".to_string(),
//                      |_| String::from_utf8_lossy(m.get_ref()).to_string());
//     return s;
// }

pub fn is_double_dot(msrc: &str, i: usize) -> bool {
    (i > 1) && &msrc[i-1..i+1] == ".."
}

#[test]
fn txt_matches_matches_stuff() {
    assert_eq!(true, txt_matches(ExactMatch, "Vec","Vec"));
    assert_eq!(true, txt_matches(StartsWith, "Vec","Vector"));
    assert_eq!(false, txt_matches(ExactMatch, "Vec","use Vector"));
    assert_eq!(true, txt_matches(StartsWith, "Vec","use Vector"));
    assert_eq!(false, txt_matches(StartsWith, "Vec","use aVector"));
    assert_eq!(true, txt_matches(ExactMatch, "Vec","use Vec"));
}


pub fn expand_ident(s: &str, pos: usize) -> (usize, usize) {
    // TODO: Would this better be an assertion ? Why are out-of-bound values getting here ?
    // They are coming from the command-line, question is, if they should be handled beforehand
    // clamp pos into allowed range
    let pos = cmp::min(s.len(), pos);
    let sb = &s[..pos];
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
        if !is_ident_char(c) {
            break;
        }
        start = i;
    }
    (start, pos)
}

pub fn find_ident_end(s: &str, pos: usize) -> usize {
    // find end of word
    let sa = &s[pos..];
    for (i, c) in sa.char_indices() {
        if !is_ident_char(c) {
            return pos + i;
        }
    }
    s.len()
}

#[test]
fn find_ident_end_ascii() {
    assert_eq!(5, find_ident_end("ident", 0));
    assert_eq!(6, find_ident_end("(ident)", 1));
    assert_eq!(17, find_ident_end("let an_identifier = 100;", 4));
}

#[test]
fn find_ident_end_unicode() {
    assert_eq!(7, find_ident_end("num_µs", 0));
    assert_eq!(10, find_ident_end("ends_in_µ", 0));
}

pub fn to_refs<'a>(v: &'a Vec<String>) -> Vec<&'a str> {
    let mut out = Vec::new();
    for item in v.iter() {
        out.push(&item[..]);
    }
    out
}

pub fn find_last_str(needle: &str, mut haystack: &str) -> Option<usize> {
    let mut res = None;
    while let Some(n) = haystack.find(needle) {
        res = Some(n);
        haystack = &haystack[n+1..];
    }
    res
}

// PD: short term replacement for .char_at() function. Should be replaced once
// that stabilizes
pub fn char_at(src: &str, i: usize) -> char {
    src[i..].chars().next().unwrap()
}

// PD: short term replacement for path.exists() (PathExt trait). Replace once
// that stabilizes
pub fn path_exists<P: AsRef<Path>>(path: P) -> bool {
    is_dir(path.as_ref()) || File::open(path).is_ok()
}

// PD: short term replacement for path.is_dir() (PathExt trait). Replace once
// that stabilizes
pub fn is_dir<P: AsRef<Path>>(path: P) -> bool {
    match std::fs::metadata(path) {
        Ok(file_info) => file_info.is_dir(),
        _ => false
    }
}
