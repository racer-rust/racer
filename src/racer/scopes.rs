use {ast, typeinf, util};
use core::{Src, CompletionType, Point, SourceByteRange};
#[cfg(test)]
use core::{self, Coordinate};

use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use util::{closure_valid_arg_scope, char_at};

fn find_close<'a, A>(iter: A, open: u8, close: u8, level_end: u32) -> Option<Point> where A: Iterator<Item=&'a u8> {
    let mut levels = 0u32;
    for (count, &b) in iter.enumerate() {
        if b == close {
            if levels == level_end { return Some(count); }
            if levels == 0 { return None; }
            levels -= 1;
        } else if b == open { levels += 1; }
    }
    None
}

pub fn find_closing_paren(src: &str, pos: Point) -> Point {
    find_close(src.as_bytes()[pos..].iter(), b'(', b')', 0)
    .map_or(src.len(), |count| pos + count)
}

pub fn find_closure_scope_start(src: Src, point: Point, parentheses_open_pos: Point) -> Option<Point> {
    let masked_src = mask_comments(src.from(point));

    let closing_paren_pos = find_closing_paren(masked_src.as_str(), 0) + point;

    let src_between_parent = mask_comments(src.from_to(parentheses_open_pos, closing_paren_pos));

    closure_valid_arg_scope(&src_between_parent).map(|_ |parentheses_open_pos)}

pub fn scope_start(src: Src, point: Point) -> Point {
    let masked_src = mask_comments(src.to(point));

    let mut curly_parent_open_pos = find_close(masked_src.as_bytes().iter().rev(), b'}', b'{', 0)
        .map_or(0, |count| point - count);

    // We've found a multi-use statement, such as `use foo::{bar, baz};`, so we shouldn't consider
    // the brace to be the start of the scope.
    if curly_parent_open_pos > 0 && masked_src[..curly_parent_open_pos].ends_with("::{") {
        trace!("scope_start landed in a use statement for {}; broadening search", point);
        curly_parent_open_pos = find_close(
            mask_comments(src.to(curly_parent_open_pos - 1)).as_bytes().iter().rev(), 
            b'}', 
            b'{', 
            0).map_or(0, |count| point - count);
    }

    let parent_open_pos = find_close(masked_src.as_bytes().iter().rev(), b')', b'(', 0)
        .map_or(0, |count| point - count);

    if curly_parent_open_pos > parent_open_pos {
        curly_parent_open_pos
    } else if let Some(scope_pos) = find_closure_scope_start(src, point, parent_open_pos) {
        scope_pos
    } else {
        curly_parent_open_pos
    }
}

pub fn find_stmt_start(msrc: Src, point: Point) -> Option<Point> {
    // iterate the scope to find the start of the statement
    let scopestart = scope_start(msrc, point);
    msrc.from(scopestart).iter_stmts()
        .find(|&(_, end)| scopestart + end > point)
        .map(|(start, _)| scopestart + start)
}

/// Finds a statement start or panics.
pub fn expect_stmt_start(msrc: Src, point: Point) -> Point {
    find_stmt_start(msrc, point).expect("Statement has a beginning")
}

/// Finds the start of a `let` statement; includes handling of struct pattern matches in the
/// statement.
pub fn find_let_start(msrc: Src, point: Point) -> Option<Point> {
    let mut scopestart = scope_start(msrc, point);
    let mut let_start = None;

    // To avoid infinite loops, we cap the number of times we'll 
    // expand the search in an attempt to find statements.
    for step in 1..6 {
        let_start = msrc.from(scopestart).iter_stmts()
            .find(|&(_, end)| scopestart + end > point);

        if let Some((ref start, ref end)) = let_start {
            // Check if we've actually reached the start of the "let" stmt.
            let stmt = &msrc.src.code[(scopestart+start)..(scopestart+end)];
            if stmt.starts_with("let") {
                break;
            }
        }
        
        debug!("find_let_start failed to find start on attempt {}: Restarting search from {} ({:?})",
            step,
            scopestart - 1,
            msrc.src.point_to_coords(scopestart - 1));
        scopestart = scope_start(msrc, scopestart - 1);
    }

    let_start.map(|(start, _)| scopestart + start)
}

pub fn get_local_module_path(msrc: Src, point: Point) -> Vec<String> {
    let mut v = Vec::new();
    get_local_module_path_(msrc, point, &mut v);
    v
}

fn get_local_module_path_(msrc: Src, point: Point, out: &mut Vec<String>) {
    for (start, end) in msrc.iter_stmts() {
        if start < point && end > point {
            let blob = msrc.from_to(start, end);
            if blob.starts_with("pub mod ") || blob.starts_with("mod ") {
                let p = typeinf::generate_skeleton_for_parsing(&blob);
                ast::parse_mod(p).name.map(|name| {
                    out.push(name);
                    let newstart = blob.find('{').unwrap() + 1;
                    get_local_module_path_(blob.from(newstart),
                                           point - start - newstart, out);
                });
            }
        }
    }
}

pub fn get_module_file_from_path(msrc: Src, point: Point, parentdir: &Path) -> Option<PathBuf> {
    let mut iter = msrc.iter_stmts();
    while let Some((start, end)) = iter.next() {
        let blob = msrc.from_to(start, end);
        if blob.starts_with("#[path ")  {
            if let Some((_,modend)) = iter.next(){
                if start < point && modend > point {
                    let pathstart = blob.find('"').unwrap() + 1;
                    let pathend = blob[pathstart..].find('"').unwrap();
                    let path = &blob[pathstart..pathstart+pathend];

                    debug!("found a path attribute, path = |{}|", path);
                    let filepath = parentdir.join(path);
                    if filepath.exists() {
                        return Some(filepath);
                    }
                }
            }
        }
    }
    None
}

pub fn find_impl_start(msrc: Src, point: Point, scopestart: Point) -> Option<Point> {
    let len = point-scopestart;
    match msrc.from(scopestart).iter_stmts().find(|&(_, end)| end > len) {
        Some((start, _)) => {
            let blob = msrc.from(scopestart + start);
            // TODO:: the following is a bit weak at matching traits. make this better
            if blob.starts_with("impl") || blob.starts_with("trait") || blob.starts_with("pub trait") {
                Some(scopestart + start)
            } else {
                let newstart = blob.find('{').unwrap() + 1;
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
    let src = core::new_source(String::from(src));
    let point = src.coords_to_point(&Coordinate { line: 4, column: 12}).unwrap();
    let v = get_local_module_path(src.as_src(), point);
    assert_eq!("foo", &v[0][..]);
    assert_eq!("bar", &v[1][..]);

    let point = src.coords_to_point(&Coordinate { line: 3, column: 8}).unwrap();
    let v = get_local_module_path(src.as_src(), point);
    assert_eq!("foo", &v[0][..]);
}

pub fn split_into_context_and_completion(s: &str) -> (&str, &str, CompletionType) {
    match s.char_indices().rev().find(|&(_, c)| !util::is_ident_char(c)) {
        Some((i,c)) => {
            match c {
                '.' => (&s[..i], &s[(i+1)..], CompletionType::Field),
                ':' if s.len() > 1 => (&s[..(i-1)], &s[(i+1)..], CompletionType::Path),
                _   => (&s[..(i+1)], &s[(i+1)..], CompletionType::Path)
            }
        },
        None => ("", s, CompletionType::Path)
    }
}

pub fn get_line(src: &str, point: Point) -> Point {
    let mut i = point;
    for &b in src.as_bytes()[..point].iter().rev() {
        i-=1;
        if b == b'\n' {
            return i+1;
        }
    }
    0
}

/// search in reverse for the start of the current expression 
/// allow . and :: to be surrounded by white chars to enable multi line call chains 
pub fn get_start_of_search_expr(src: &str, point: Point) -> Point {

    enum State {
        /// In parentheses; the value inside identifies depth.
        Levels(usize),
        /// In a string
        StringLiteral,
        StartsWithDot,
        MustEndsWithDot(usize),
        StartsWithCol(usize),
        None,
        Result(Point),
    }
    let mut ws_ok = State::None;
    for (i, c) in src.as_bytes()[..point].iter().enumerate().rev() {
        ws_ok = match (*c,ws_ok) {
            (b'(', State::None) => State::Result(i+1),
            (b'(', State::Levels(1)) =>  State::None,
            (b'(', State::Levels(lev)) =>  State::Levels(lev-1),
            (b')', State::Levels(lev)) =>  State::Levels(lev+1),
            (b')', State::None) |
            (b')', State::StartsWithDot) =>  State::Levels(1),
            (b'.', State::None) =>  State::StartsWithDot,
            (b'.', State::StartsWithDot) => State::Result(i+2) ,
            (b'.', State::MustEndsWithDot(_)) =>  State::None,
            (b':', State::MustEndsWithDot(index)) =>  State::StartsWithCol(index),
            (b':', State::StartsWithCol(_)) =>  State::None,
            (b'"', State::None) |
            (b'"', State::StartsWithDot) => State::StringLiteral,
            (b'"', State::StringLiteral) => State::None,
            (b'?', State::StartsWithDot) => State::None,
            (_ , State::StringLiteral) => State::StringLiteral,
            ( _ , State::StartsWithCol(index)) => State::Result(index) ,
            ( _ , State::None) if char_at(src, i).is_whitespace() =>  State::MustEndsWithDot(i+1),
            ( _ , State::MustEndsWithDot(index)) if char_at(src, i).is_whitespace() => State::MustEndsWithDot(index),
            ( _ , State::StartsWithDot ) if char_at(src, i).is_whitespace() => State::StartsWithDot,
            ( _ , State::MustEndsWithDot(index)) => State::Result(index) ,
            ( _ , State::None) if !util::is_search_expr_char(char_at(src, i)) => State::Result(i+1) ,
            ( _ , State::None) => State::None,
            ( _ , s@State::Levels(_)) => s,
            ( _ , State::StartsWithDot) if util::is_search_expr_char(char_at(src, i)) =>  State::None,
            ( _ , State::StartsWithDot) => State::Result(i+1) ,
            ( _ , State::Result(_)) => unreachable!() ,
        };
        if let State::Result(index) = ws_ok {
            return index;
        }
    }
    0
}

pub fn get_start_of_pattern(src: &str, point: Point) -> Point {
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

pub fn expand_search_expr(msrc: &str, point: Point) -> SourceByteRange {
    let start = get_start_of_search_expr(msrc, point);
    (start, util::find_ident_end(msrc, point))
}

#[test]
fn expand_search_expr_finds_ident() {
    assert_eq!((0, 7), expand_search_expr("foo.bar", 5))
}

#[test]
fn expand_search_expr_ignores_bang_at_start() {
    assert_eq!((1, 4), expand_search_expr("!foo", 1))
}

#[test]
fn expand_search_expr_handles_chained_calls() {
    assert_eq!((0, 20), expand_search_expr("yeah::blah.foo().bar", 18))
}

#[test]
fn expand_search_expr_handles_inline_closures() {
    assert_eq!((0, 29), expand_search_expr("yeah::blah.foo(|x:foo|{}).bar", 27))
}
#[test]
fn expand_search_expr_handles_a_function_arg() {
    assert_eq!((5, 25), expand_search_expr("myfn(foo::new().baz().com)", 23))
}

#[test]
fn expand_search_expr_handles_macros() {
    assert_eq!((0, 9), expand_search_expr("my_macro!()", 8))
}

#[test]
fn expand_search_expr_handles_pos_at_end_of_search_str() {
    assert_eq!((0, 7), expand_search_expr("foo.bar", 7))
}

#[test]
fn expand_search_expr_handles_type_definition() {
    assert_eq!((4, 7), expand_search_expr("x : foo", 7))
}

#[test]
fn expand_search_expr_handles_ws_before_dot() {
    assert_eq!((0, 8), expand_search_expr("foo .bar", 7))
}

#[test]
fn expand_search_expr_handles_ws_after_dot() {
    assert_eq!((0, 8), expand_search_expr("foo. bar", 7))
}

#[test]
fn expand_search_expr_handles_ws_dot() {
    assert_eq!((0, 13), expand_search_expr("foo. bar .foo", 12))
}

#[test]
fn expand_search_expr_handles_let() {
    assert_eq!((8, 11), expand_search_expr("let b = foo", 10))
}

#[test]
fn expand_search_expr_handles_double_dot() {
    assert_eq!((2, 5), expand_search_expr("..foo", 4))
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


#[test]
fn test_scope_start() {
    let src = String::from("
fn myfn() {
    let a = 3;
    print(a);
}
");
    let src = core::new_source(src);
    let point = src.coords_to_point(&Coordinate { line: 4, column: 10}).unwrap();
    let start = scope_start(src.as_src(), point);
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
    let point = src.coords_to_point(&Coordinate { line: 7, column: 10}).unwrap();
    let start = scope_start(src.as_src(), point);
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
    let r = mask_comments(src.as_src());

    assert!(src.len() == r.len());
    // characters at the start are the same
    assert!(src.as_bytes()[5] == r.as_bytes()[5]);
    // characters in the comments are masked
    let commentoffset = src.coords_to_point(&Coordinate { line: 3, column: 23}).unwrap();
    assert!(char_at(&r, commentoffset) == ' ');
    assert!(src.as_bytes()[commentoffset] != r.as_bytes()[commentoffset]);
    // characters afterwards are the same
    assert!(src.as_bytes()[src.len()-3] == r.as_bytes()[src.len()-3]);
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
