use {ast, typeinf, util};
use core::{self, Src, CompletionType, BytePos, ByteRange};
#[cfg(test)]
use core::Coordinate;

use std::iter::Iterator;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use util::*;
use codecleaner::comment_skip_iter_rev;

fn find_close<'a, A>(iter: A, open: u8, close: u8, level_end: u32) -> Option<BytePos>
where
    A: Iterator<Item = &'a u8>
{
    let mut levels = 0u32;
    for (count, &b) in iter.enumerate() {
        if b == close {
            if levels == level_end { return Some(count.into()); }
            if levels == 0 { return None; }
            levels -= 1;
        } else if b == open { levels += 1; }
    }
    None
}

pub fn find_closing_paren(src: &str, pos: BytePos) -> BytePos {
    find_close(src.as_bytes()[pos.0..].iter(), b'(', b')', 0)
        .map_or(src.len().into(), |count| pos + count)
}

pub fn find_closure_scope_start(
    src: Src,
    point: BytePos,
    parentheses_open_pos: BytePos
) -> Option<BytePos> {
    let masked_src = mask_comments(src.shift_start(point));
    let closing_paren_pos = find_closing_paren(masked_src.as_str(), BytePos::ZERO) + point;
    let src_between_parent = mask_comments(src.shift_range(ByteRange::new(parentheses_open_pos, closing_paren_pos)));
    closure_valid_arg_scope(&src_between_parent).map(|_ |parentheses_open_pos)
}

pub fn scope_start(src: Src, point: BytePos) -> BytePos {
    let masked_src = mask_comments(src.change_length(point));
    let bytes = masked_src.as_bytes();
    let mut curly_parent_open_pos = find_close(bytes.iter().rev(), b'}', b'{', 0)
        .map_or(BytePos::ZERO, |count| point - count);

    // We've found a multi-use statement, such as `use foo::{bar, baz};`, so we shouldn't consider
    // the brace to be the start of the scope.
    if curly_parent_open_pos > BytePos::ZERO
        && masked_src[..curly_parent_open_pos.0].ends_with("::{")
    {
        if let Some(pos) = masked_src[..curly_parent_open_pos.0]
            .rfind(|c: char|  c == '\n' || c == ';')
        {
            curly_parent_open_pos = find_close(
                mask_comments(src.change_length(pos.into())).as_bytes().iter().rev(),
                b'}',
                b'{',
                0,
            ).map_or(BytePos::ZERO, |count| point - count);
        } else {
            warn!("[scope_start] \n or ; are not found for use statement");
        }
    }

    let parent_open_pos = find_close(masked_src.as_bytes().iter().rev(), b')', b'(', 0)
        .map_or(BytePos::ZERO, |count| point - count);

    if curly_parent_open_pos > parent_open_pos {
        curly_parent_open_pos
    } else if let Some(scope_pos) = find_closure_scope_start(src, point, parent_open_pos) {
        scope_pos
    } else {
        curly_parent_open_pos
    }
}

pub fn find_stmt_start(msrc: Src, point: BytePos) -> Option<BytePos> {
    let scopestart = scope_start(msrc, point);
    // Iterate the scope to find the start of the statement that surrounds the point.
    debug!(
        "[find_stmt_start] now we are in scope {:?} ~ {:?}, {}",
        scopestart,
        point,
        &msrc[scopestart.0..point.increment().0]
    );
    msrc.shift_start(scopestart).iter_stmts()
        .find(|&range| scopestart + range.start < point && point < scopestart + range.end)
        .map(|range| scopestart + range.start)
}

/// Finds a statement start or panics.
pub fn expect_stmt_start(msrc: Src, point: BytePos) -> BytePos {
    find_stmt_start(msrc, point).expect("Statement does not have a beginning")
}

/// Finds the start of a `let` statement; includes handling of struct pattern matches in the
/// statement.
pub fn find_let_start(msrc: Src, point: BytePos) -> Option<BytePos> {
    let mut scopestart = scope_start(msrc, point);
    let mut let_start = None;

    // To avoid infinite loops, we cap the number of times we'll 
    // expand the search in an attempt to find statements.
    // TODO: it's too hacky, and, 1..6 is appropriate?
    for step in 1..6 {
        let_start = msrc.shift_start(scopestart).iter_stmts()
            .find(|&range| scopestart + range.end > point);

        if let Some(ref range) = let_start {
            // Check if we've actually reached the start of the "let" stmt.
            let stmt = &msrc.src.code[range.shift(scopestart).to_range()];
            if stmt.starts_with("let") {
                break;
            }
        }
        
        debug!(
            "find_let_start failed to find start on attempt {}: Restarting search from {:?} ({:?})",
            step,
            scopestart.decrement(),
            msrc.src.point_to_coords(scopestart.decrement()),
        );
        scopestart = scope_start(msrc, scopestart.decrement());
    }

    let_start.map(|range| scopestart + range.start)
}

pub fn get_local_module_path(msrc: Src, point: BytePos) -> Vec<String> {
    let mut v = Vec::new();
    get_local_module_path_(msrc, point, &mut v);
    v
}

fn get_local_module_path_(msrc: Src, point: BytePos, out: &mut Vec<String>) {
    for range in msrc.iter_stmts() {
        if range.contains_exclusive(point) {
            let blob = msrc.shift_range(range);
            if blob.starts_with("pub mod ") || blob.starts_with("mod ") {
                let p = typeinf::generate_skeleton_for_parsing(&blob);
                if let Some(name) = ast::parse_mod(p).name {
                    out.push(name);
                    let newstart = blob.find('{').unwrap() + 1;
                    get_local_module_path_(
                        blob.shift_start(newstart.into()),
                        point - range.start - newstart.into(),
                        out,
                    );
                }
            }
        }
    }
}

pub fn get_module_file_from_path(msrc: Src, point: BytePos, parentdir: &Path) -> Option<PathBuf> {    
    let mut iter = msrc.iter_stmts();
    while let Some(range) = iter.next() {
        let blob = msrc.shift_range(range);
        let start = range.start;
        if blob.starts_with("#[path ")  {
            if let Some(ByteRange { start: _, end: modend }) = iter.next() {
                if start < point && modend > point {
                    let pathstart = blob.find('"')? + 1;
                    let pathend = blob[pathstart..].find('"').unwrap();
                    let path = &blob[pathstart..pathstart + pathend];
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

pub fn find_impl_start(msrc: Src, point: BytePos, scopestart: BytePos) -> Option<BytePos> {
    let len = point - scopestart;
    msrc
        .shift_start(scopestart)
        .iter_stmts()
        .find(|range| range.end > len)
        .and_then(|range| {
            let blob = msrc.shift_start(scopestart + range.start);
            // TODO:: the following is a bit weak at matching traits. make this better
            if blob.starts_with("impl") || blob.starts_with("trait") || blob.starts_with("pub trait") {
                Some(scopestart + range.start)
            } else {
                let newstart = blob.find('{').expect("[find_impl_start] { was not found.") + 1;
                find_impl_start(msrc, point, scopestart + range.start + newstart.into())
            }
        })
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
    let point = src.coords_to_point(&Coordinate::new(4, 12)).unwrap();
    let v = get_local_module_path(src.as_src(), point);
    assert_eq!("foo", &v[0][..]);
    assert_eq!("bar", &v[1][..]);

    let point = src.coords_to_point(&Coordinate::new(3, 8)).unwrap();
    let v = get_local_module_path(src.as_src(), point);
    assert_eq!("foo", &v[0][..]);
}

// TODO: This function can't handle use_nested_groups
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

pub fn get_line(src: &str, point: BytePos) -> BytePos {
    src.as_bytes()[..point.0]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, byte)| **byte == b'\n')
        .map(|t| t.0.into())
        .unwrap_or(BytePos::ZERO)
}

/// search in reverse for the start of the current expression 
/// allow . and :: to be surrounded by white chars to enable multi line call chains 
pub fn get_start_of_search_expr(src: &str, point: BytePos) -> BytePos {
    enum State {
        /// In parentheses; the value inside identifies depth.
        Levels(usize),
        /// In a string
        StringLiteral,
        StartsWithDot,
        MustEndsWithDot(usize),
        StartsWithCol(usize),
        None,
        Result(usize),
    }
    let mut ws_ok = State::None;
    for (i, c) in src.as_bytes()[..point.0].iter().enumerate().rev() {
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
            return index.into();
        }
    }
    BytePos::ZERO
}

pub fn get_start_of_pattern(src: &str, point: BytePos) -> BytePos {
    let mut levels = 0u32;
    for (c, i) in comment_skip_iter_rev(src, point) {
        match c {
            '(' => {
                if levels == 0 {
                    return i.increment();
                }
                levels -= 1;
            },
            ')' => { levels += 1; },
            _ => {
                if levels == 0 && !util::is_pattern_char(c) {
                    return i.increment();
                }
            }
        }
    }
    BytePos::ZERO
}

#[cfg(test)]
mod test_get_start_of_pattern {
    use super::{get_start_of_pattern, BytePos};
    fn get_start_of_pattern_(s: &str, u: usize) -> usize {
        get_start_of_pattern(s, BytePos(u)).0
    }
    #[test]
    fn handles_variant() {
        assert_eq!(4, get_start_of_pattern_("foo, Some(a) =>", 13));
    }

    #[test]
    fn handles_variant2() {
        assert_eq!(4, get_start_of_pattern_("bla, ast::PatTup(ref tuple_elements) => {", 36));
    }

    #[test]
    fn with_block_comments() {
        assert_eq!(6, get_start_of_pattern_("break, Some(b) /* if expr is Some */ => {", 36));
    }
}


pub fn expand_search_expr(msrc: &str, point: BytePos) -> ByteRange {
    let start = get_start_of_search_expr(msrc, point);
    ByteRange::new(start, util::find_ident_end(msrc, point))
}

#[cfg(test)]
mod test_expand_seacrh_expr {
    use super::{BytePos, expand_search_expr};
    fn expand_search_expr_(s: &str, u: usize) -> (usize, usize) {
        let res = expand_search_expr(s, BytePos(u));
        (res.start.0, res.end.0)
    }
    #[test]
    fn finds_ident() {
        assert_eq!((0, 7), expand_search_expr_("foo.bar", 5))
    }

    #[test]
    fn ignores_bang_at_start() {
        assert_eq!((1, 4), expand_search_expr_("!foo", 1))
    }

    #[test]
    fn handles_chained_calls() {
        assert_eq!((0, 20), expand_search_expr_("yeah::blah.foo().bar", 18))
    }

    #[test]
    fn handles_inline_closures() {
        assert_eq!((0, 29), expand_search_expr_("yeah::blah.foo(|x:foo|{}).bar", 27))
    }
    #[test]
    fn handles_a_function_arg() {
        assert_eq!((5, 25), expand_search_expr_("myfn(foo::new().baz().com)", 23))
    }

    #[test]
    fn handles_macros() {
        assert_eq!((0, 9), expand_search_expr_("my_macro!()", 8))
    }

    #[test]
    fn handles_pos_at_end_of_search_str() {
        assert_eq!((0, 7), expand_search_expr_("foo.bar", 7))
    }

    #[test]
    fn handles_type_definition() {
        assert_eq!((4, 7), expand_search_expr_("x : foo", 7))
    }

    #[test]
    fn handles_ws_before_dot() {
        assert_eq!((0, 8), expand_search_expr_("foo .bar", 7))
    }

    #[test]
    fn handles_ws_after_dot() {
        assert_eq!((0, 8), expand_search_expr_("foo. bar", 7))
    }

    #[test]
    fn handles_ws_dot() {
        assert_eq!((0, 13), expand_search_expr_("foo. bar .foo", 12))
    }

    #[test]
    fn handles_let() {
        assert_eq!((8, 11), expand_search_expr_("let b = foo", 10))
    }

    #[test]
    fn handles_double_dot() {
        assert_eq!((2, 5), expand_search_expr_("..foo", 4))
    }
}



fn fill_gaps(buffer: &str, result: &mut String, start: usize, prev: usize) {
    for _ in 0..((start-prev)/buffer.len()) { result.push_str(buffer); }
    result.push_str(&buffer[..((start-prev)%buffer.len())]);
}

pub fn mask_comments(src: Src) -> String {
    let mut result = String::with_capacity(src.len());
    let buf_byte = &[b' '; 128];
    let buffer = from_utf8(buf_byte).unwrap();
    let mut prev = BytePos::ZERO;
    for range in src.chunk_indices() {
        fill_gaps(buffer, &mut result, range.start.0, prev.0);
        result.push_str(&src[range.to_range()]);
        prev = range.end;
    }

    // Fill up if the comment was at the end
    if src.len() > prev.0 {
        fill_gaps(buffer, &mut result, src.len(), prev.0);
    }
    assert_eq!(src.len(), result.len());
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
                    fill_gaps(buffer, &mut result, pos, start);
                    result.push_str("}");
                    start = pos;
                }
                levels -= 1;
            },
            b'\n' if levels > 0 => {
                fill_gaps(buffer, &mut result, pos, start);
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
        fill_gaps(buffer, &mut result, pos, start);
    } else {
        result.push_str(&src[start..pos]);
    }
    result
}

pub fn end_of_next_scope(src: &str) -> &str {
    match find_close(src.as_bytes().iter(), b'{', b'}', 1) {
        Some(count) => &src[..count.0 + 1],
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
    let point = src.coords_to_point(&Coordinate::new(4, 10)).unwrap();
    let start = scope_start(src.as_src(), point);
    assert_eq!(start, BytePos(12));
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
    let point = src.coords_to_point(&Coordinate::new(7, 10)).unwrap();
    let start = scope_start(src.as_src(), point);
    assert_eq!(start, BytePos(12));
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
    let commentoffset = src.coords_to_point(&Coordinate::new(3, 23)).unwrap();
    assert!(char_at(&r, commentoffset.0) == ' ');
    assert!(src.as_bytes()[commentoffset.0] != r.as_bytes()[commentoffset.0]);
    // characters afterwards are the same
    assert!(src.as_bytes()[src.len() - 3] == r.as_bytes()[src.len() - 3]);
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

/// get start of path from use statements
/// e.g. get Some(16) from "pub(crate)  use a"
pub(crate) fn use_stmt_start(line_str: &str) -> Option<BytePos> {
    let use_start = strip_visivilities(line_str).unwrap_or(BytePos::ZERO);
    strip_word(&line_str[use_start.0..], "use").map(|b| b + use_start)
}

#[test]
fn test_use_stmt_start() {
    assert_eq!(use_stmt_start("pub(crate)   use   some::").unwrap().0, 19);
}

/// get path from use statement, supposing completion point is end of expr
/// e.g. "use std::collections::{hash_map,  Hash" -> P["std", "collections", "Hash"]
pub(crate) fn construct_path_from_use_tree(expr: &str) -> core::Path {
    let mut segments = Vec::new();
    let bytes = expr.as_bytes();
    let mut i = bytes.len();
    let mut ident_end = Some(i - 1);
    while i > 0 {
        i -= 1;
        if is_ident_char(bytes[i] as char) {
            if ident_end.is_none() {
                ident_end = Some(i)
            }
        } else {
            if let Some(end) = ident_end {
                segments.push(&expr[i + 1..=end]);
                ident_end = None;
            }
            if let Some(point) = expr[..=i].rfind("::") {
                if point > 0 {
                    i = point;
                    continue
                }
            }
            break;
        }
    }
    if let Some(end) = ident_end {
        segments.push(&expr[0..=end]);
    }
    if let Some(&"crate") = segments.last() {
        segments.pop();
    }
    segments.reverse();
    let is_global = expr.starts_with("::");
    core::Path::from_vec(is_global, segments)
}

#[test]
fn test_construct_path_from_use_tree() {
    let get_path_idents = |s| {
        let s = construct_path_from_use_tree(s);
        s.segments.into_iter().map(|seg| seg.name).collect::<Vec<_>>()
    };
    assert_eq!(
        get_path_idents("std::collections::HashMa"),
        vec!["std", "collections", "HashMa"],
    );
    assert_eq!(
        get_path_idents("std::{collections::{HashMap, hash_ma"),
        vec!["std", "collections", "hash_ma"],
    );
    assert_eq!(
        get_path_idents("std::{collections::{HashMap, "),
        vec!["std", "collections", ""],
    );
    assert_eq!(
        get_path_idents("std::collections::{"),
        vec!["std", "collections", ""],
    );
}


