// Small functions of utility
use std::io::{File, BufferedReader};
use racer::{SearchType};
use racer::SearchType::{ExactMatch, StartsWith};
use std;

pub fn getline(filepath : &Path, linenum : usize) -> String {
    let mut i = 0;
    let mut file = BufferedReader::new(File::open(filepath));
    for line in file.lines() {
        //print!("{}", line);
        i += 1;
        if i == linenum {
            return line.unwrap().to_string();
        }
    }
    return "not found".to_string();
}

pub fn is_pattern_char(c : char) -> bool {
    c.is_alphanumeric() || c.is_whitespace() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_search_expr_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == ':') || (c == '.')
}


pub fn is_ident_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_')
}

pub fn txt_matches(stype: SearchType, needle: &str, haystack: &str) -> bool { 
    return match stype {
        ExactMatch => {
            let nlen = needle.len();
            let hlen = haystack.len();

            if nlen == 0 {
                return true;
            }

            for (n,_) in haystack.match_indices(needle) {
                if (n == 0  || !is_ident_char(haystack.char_at(n-1))) && 
                    (n+nlen == hlen || !is_ident_char(haystack.char_at(n+nlen))) {
                    return true;
                }
            }
            return false;
        },
        StartsWith => {
            if needle.is_empty() {
                return true;
            }

            for (n,_) in haystack.match_indices(needle) {
                if n == 0  || !is_ident_char(haystack.char_at(n-1)) {
                    return true;
                }
            }
            return false;
        }
    }
}

pub fn symbol_matches(stype: SearchType, searchstr: &str, candidate: &str) -> bool {
   return match stype {
        ExactMatch => {
            return searchstr == candidate;
        },
        StartsWith => {
            return candidate.starts_with(searchstr);
        }
    }
}

pub fn get_backtrace() -> String {
    let mut m = std::io::MemWriter::new();
    let s = std::rt::backtrace::write(&mut m)
        .ok().map_or("NO backtrace".to_string(), 
                     |_| String::from_utf8_lossy(m.get_ref()).to_string());
    return s;
}

pub fn is_double_dot(msrc: &str, i: usize) -> bool {
    (i > 1) && msrc.slice(i-1, i+1) == ".."
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


pub fn expand_ident(s : &str, pos : usize) -> (usize,usize) {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
        if !is_ident_char(c) {
            break;
        }
        start = i;
    }
    return (start, pos);
}

pub fn find_ident_end(s : &str, pos : usize) -> usize {
    // find end of word
    let sa = s.slice_from(pos);
    let mut end = pos;
    for (i, c) in sa.char_indices() {
        if !is_ident_char(c) {
            break;
        }
        end = pos + i + 1;
    }
    return end;
}

pub fn to_refs<'a>(v: &'a Vec<String>) -> Vec<&'a str> {
    let mut out = Vec::new();
    for item in v.iter() {
        out.push(item.as_slice()); 
    }
    return out;
}

pub fn find_last_str(needle: &str, mut haystack: &str) -> Option<usize> {
    let mut res = None;
    while let Some(n) = haystack.find_str(needle) {
        res = Some(n);
        haystack = haystack.slice_from(n+1);
    }
    return res;
}
