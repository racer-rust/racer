use {ast, typeinf, util};
use core::{Src, SessionRef, CompletionType};
#[cfg(test)] use core;

use std::iter::Iterator;
use std::path::Path;
use std::str::from_utf8;
use util::char_at;

fn find_close<'a, A>(iter: A, open: u8, close: u8, level_end: u32) -> Option<usize> where A: Iterator<Item=&'a u8> {
    let mut count = 0usize;
    let mut levels = 0u32;
    for &b in iter {
        if b == close {
            if levels == level_end { return Some(count); }
            levels -= 1;
        } else if b == open { levels += 1; }
        count += 1;
    }
    None
}

pub fn find_closing_paren(src: &str, pos: usize) -> usize {
    find_close(src.as_bytes()[pos..].iter(), b'(', b')', 0)
    .map_or(src.len(), |count| pos + count)
}

pub fn scope_start(src: Src, point: usize) -> usize {
    let masked_src = mask_comments(src.to(point));
    find_close(masked_src.as_bytes().iter().rev(), b'}', b'{', 0)
    .map_or(0, |count| point - count)
}

pub fn find_stmt_start(msrc: Src, point: usize) -> Option<usize> {
    // iterate the scope to find the start of the statement
    let scopestart = scope_start(msrc, point);
    msrc.from(scopestart).iter_stmts()
        .find(|&(_, end)| scopestart + end > point)
        .map(|(start, _)| scopestart + start)
}

pub fn get_local_module_path(msrc: Src, point: usize) -> Vec<String> {
    let mut v = Vec::new();
    get_local_module_path_(msrc, point, &mut v);
    v
}

fn get_local_module_path_(msrc: Src, point: usize, out: &mut Vec<String>) {
    for (start, end) in msrc.iter_stmts() {
        if start < point && end > point {
            let blob = msrc.from_to(start, end);
            if blob.starts_with("pub mod ") || blob.starts_with("mod ") {
                let p = typeinf::generate_skeleton_for_parsing(&blob);
                ast::parse_mod(p).name.map(|name| {
                    out.push(name);
                    let newstart = blob.find("{").unwrap() + 1;
                    get_local_module_path_(blob.from(newstart),
                                           point - start - newstart, out);
                });
            }
        }
    }
}

pub fn find_impl_start(msrc: Src, point: usize, scopestart: usize) -> Option<usize> {
    let len = point-scopestart;
    match msrc.from(scopestart).iter_stmts().find(|&(_, end)| end > len) {
        Some((start, _)) => {
            let blob = msrc.from(scopestart + start);
            // TODO:: the following is a bit weak at matching traits. make this better
            if blob.starts_with("impl") || blob.starts_with("trait") || blob.starts_with("pub trait") {
                Some(scopestart + start)
            } else {
                let newstart = blob.find("{").unwrap() + 1;
                find_impl_start(msrc, point, scopestart+start+newstart)
            }
        },
        None => None
    }
}
#[test]
fn finds_subnested_module() {
    use core;
    let src = "
    pub mod foo {
        pub mod bar {
            here
        }
    }";
    let point = coords_to_point(&src, 4, 12);
    let src = core::new_source(String::from(src));
    let v = get_local_module_path(src.as_ref(), point);
    assert_eq!("foo", &v[0][..]);
    assert_eq!("bar", &v[1][..]);

    let point = coords_to_point(&src, 3, 8);
    let v = get_local_module_path(src.as_ref(), point);
    assert_eq!("foo", &v[0][..]);
}


pub fn split_into_context_and_completion(s: &str) -> (&str, &str, CompletionType) {
    match s.char_indices().rev().find(|&(_, c)| !util::is_ident_char(c)) {
        Some((i,c)) => {
            //println!("PHIL s '{}' i {} c '{}'",s,i,c);
            match c {
                '.' => (&s[..i], &s[(i+1)..], CompletionType::CompleteField),
                ':' if s.len() > 1 => (&s[..(i-1)], &s[(i+1)..], CompletionType::CompletePath),
                _   => (&s[..(i+1)], &s[(i+1)..], CompletionType::CompletePath)
            }
        },
        None => ("", s, CompletionType::CompletePath)
    }
}

pub fn get_start_of_search_expr(src: &str, point: usize) -> usize {
    let mut i = point;
    let mut levels = 0u32;
    for &b in src.as_bytes()[..point].iter().rev() {
        i -= 1;
        match b {
            b'(' => {
                if levels == 0 { return i+1; }
                levels -= 1;
            },
            b')' => { levels += 1; },
            _ => {
                if levels == 0 &&
                    !util::is_search_expr_char(char_at(src, i)) ||
                    util::is_double_dot(src,i) {
                    return i+1;
                }
            }
        }
    }
    0
}

pub fn get_start_of_pattern(src: &str, point: usize) -> usize {
    let mut i = point-1;
    let mut levels = 0u32;
    for &b in src.as_bytes()[..point].iter().rev() {
        match b {
            b'(' => {
                if levels == 0 { return i+1; }
                levels -= 1;
            },
            b')' => { levels += 1; },
            _ => {
                if levels == 0 &&
                    !util::is_pattern_char(char_at(src, i)) {
                    return i+1;
                }
            }
        }
        i -= 1;
    }
    0
}

#[test]
fn get_start_of_pattern_handles_variant() {
    assert_eq!(4, get_start_of_pattern("foo, Some(a) =>",13));
}

#[test]
fn get_start_of_pattern_handles_variant2() {
    assert_eq!(4, get_start_of_pattern("bla, ast::PatTup(ref tuple_elements) => {",36));
}

pub fn expand_search_expr(msrc: &str, point: usize) -> (usize, usize) {
    let start = get_start_of_search_expr(msrc, point);
    (start, util::find_ident_end(msrc, point))
}

#[test]
fn expand_search_expr_finds_ident() {
    assert_eq!((0, 7), expand_search_expr("foo.bar", 5))
}

#[test]
fn expand_search_expr_handles_chained_calls() {
    assert_eq!((0, 20), expand_search_expr("yeah::blah.foo().bar", 18))
}

#[test]
fn expand_search_expr_handles_inline_closures() {
    assert_eq!((0, 24), expand_search_expr("yeah::blah.foo(||{}).bar", 22))
}
#[test]
fn expand_search_expr_handles_a_function_arg() {
    assert_eq!((5, 25), expand_search_expr("myfn(foo::new().baz().com)", 23))
}

#[test]
fn expand_search_expr_handles_pos_at_end_of_search_str() {
    assert_eq!((0, 7), expand_search_expr("foo.bar", 7))
}

pub fn mask_comments(src: Src) -> String {
    let mut result = String::with_capacity(src.len());
    let buf_byte = &[b' '; 128];
    let buffer = from_utf8(buf_byte).unwrap();
    let mut prev: usize = 0;
    for (start, end) in src.chunk_indices() {
        for _ in 0..((start-prev)/128) { result.push_str(buffer); }
        result.push_str(&buffer[..((start-prev)%128)]);
        result.push_str(&src[start..end]);
        prev = end;
    }
    result
}

pub fn mask_sub_scopes(src: &str) -> String {
    let mut result = String::with_capacity(src.len());
    let buf_byte = [b' '; 128];
    let buffer = from_utf8(&buf_byte).unwrap();
    let mut levels = 0i32;
    let mut start = 0usize;
    let mut pos = 0usize;

    for &b in src.as_bytes() {
        pos += 1;
        match b {
            b'{' => {
                if levels == 0 {
                    result.push_str(&src[start..(pos)]);
                    start = pos+1;
                }
                levels += 1;
            },
            b'}' => {
                if levels == 1 {
                    let num_spaces = pos-start;
                    for _ in 0..(num_spaces/128) { result.push_str(buffer); }
                    result.push_str(&buffer[..((num_spaces)%128)]);
                    result.push_str("}");
                    start = pos;
                }
                levels -= 1;
            },
            b'\n' if levels > 0 => {
                for _ in 0..((pos-start)/128) { result.push_str(buffer); }
                result.push_str(&buffer[..((pos-start)%128)]);
                result.push('\n');
                start = pos+1;
            },
            _ => {}
        }
    }
    if start > pos {
        start = pos;
    }
    if levels > 0 {
        for _ in 0..((pos - start)/128) { result.push_str(buffer); }
        result.push_str(&buffer[..((pos-start)%128)]);
    } else {
        result.push_str(&src[start..pos]);
    }
    result
}

pub fn end_of_next_scope(src: &str) -> &str {
    match find_close(src.as_bytes().iter(), b'{', b'}', 1) {
        Some(count) => &src[..count+1],
        None => ""
    }
}

pub fn coords_to_point(src: &str, mut linenum: usize, col: usize) -> usize {
    let mut point = 0;
    for line in src.lines() {
        linenum -= 1;
        if linenum == 0 { break }
        point += line.len() + 1;  // +1 for the \n
    }
    point + col
}

pub fn point_to_coords(src: &str, point: usize) -> (usize, usize) {
    let mut i = 0;
    let mut linestart = 0;
    let mut nlines = 1;  // lines start at 1
    for &b in src[..point].as_bytes() {
        i += 1;
        if b == b'\n' {
            nlines += 1;
            linestart = i;
        }
    }
    (nlines, point - linestart)
}

pub fn point_to_coords_from_file(path: &Path, point: usize, session: SessionRef) -> Option<(usize, usize)> {
    let mut lineno = 0;
    let mut p = 0;
    for line in session.load_file(path).lines() {
        lineno += 1;
        if point < (p + line.len()) {
            return Some((lineno, point - p));
        }
        p += line.len() + 1;  // +1 for the newline char
    }
    None
}


#[test]
fn coords_to_point_works() {
    let src = "
fn myfn() {
    let a = 3;
    print(a);
}";
    assert!(coords_to_point(src, 3, 5) == 18);
}

#[test]
fn test_scope_start() {
    let src = String::from("
fn myfn() {
    let a = 3;
    print(a);
}
");
    let src = core::new_source(src);
    let point = coords_to_point(&src, 4, 10);
    let start = scope_start(src.as_ref(), point);
    assert!(start == 12);
}

#[test]
fn test_scope_start_handles_sub_scopes() {
    let src = String::from("
fn myfn() {
    let a = 3;
    {
      let b = 4;
    }
    print(a);
}
");
    let src = core::new_source(src);
    let point = coords_to_point(&src, 7, 10);
    let start = scope_start(src.as_ref(), point);
    assert!(start == 12);
}

#[test]
fn masks_out_comments() {
    let src = String::from("
this is some code
this is a line // with a comment
some more
");
    let src = core::new_source(src);
    let r = mask_comments(src.as_ref());

    assert!(src.len() == r.len());
    // characters at the start are the same
    assert!(src.as_bytes()[5] == r.as_bytes()[5]);
    // characters in the comments are masked
    let commentoffset = coords_to_point(&src,3,23);
    assert!(char_at(&r, commentoffset) == ' ');
    assert!(src.as_bytes()[commentoffset] != r.as_bytes()[commentoffset]);
    // characters afterwards are the same
    assert!(src.as_bytes()[src.len()-3] == r.as_bytes()[src.len()-3]);
}

#[test]
fn test_point_to_coords() {
    let src = "
fn myfn(b:usize) {
   let a = 3;
   if b == 12 {
       let a = 24;
       do_something_with(a);
   }
   do_something_with(a);
}
";
    round_trip_point_and_coords(src, 4, 5);
}

pub fn round_trip_point_and_coords(src: &str, lineno: usize, charno: usize) {
    let (a,b) = point_to_coords(src, coords_to_point(src, lineno, charno));
     assert_eq!((a,b), (lineno,charno));
}

#[test]
fn finds_end_of_struct_scope() {
    let src="
struct foo {
   a: usize,
   blah: ~str
}
Some other junk";

    let expected="
struct foo {
   a: usize,
   blah: ~str
}";
    let s = end_of_next_scope(src);
    assert_eq!(expected, s);
}
