use std::io::File;
use std::io::BufferedReader;

use racer;
use racer::util;
use racer::codecleaner;
use racer::codeiter;
use racer::typeinf;
use racer::ast;

pub fn find_closing_paren(src:&str, mut pos:usize) -> usize {
    let openparen: u8 = "(".as_bytes()[0] as u8;
    let closeparen: u8 = ")".as_bytes()[0] as u8;
    let src_bytes = src.as_bytes();

    let mut levels = 0i32;
    loop {
        if src_bytes[pos] == closeparen {
            if levels == 0 {
                break;
            } else {
                levels -= 1;
            }
        }
        if src_bytes[pos] == openparen {
            levels += 1;
        }
        pos += 1;
    }
    return pos;
}


pub fn scope_start(src:&str, point:usize) -> usize {
    let masked_src = mask_comments(src);
    let s = masked_src.as_slice().slice(0,point);
    let mut pt = point;
    let mut levels = 0i32;
    for c in s.chars().rev() {
        if c == '{' {
            if levels == 0 {
                break;
            } else {
                levels -= 1;
            }
        }
        if c == '}' {
            levels += 1;
        }
        pt -= 1;
    }
    return pt;
}

pub fn find_stmt_start(msrc: &str, point: usize) -> Option<usize> {
    // iterate the scope to find the start of the statement
    let scopestart = scope_start(msrc, point);
    for (start, end) in codeiter::iter_stmts(msrc.slice_from(scopestart)) {
        if (scopestart + end) > point {
            return Some(scopestart+start);
        }
    }
    return None;
}

pub fn get_local_module_path(msrc: &str, point: usize) -> Vec<String> {
    let mut v = Vec::new();
    get_local_module_path_(msrc, point, &mut v);
    return v;
}

fn get_local_module_path_(msrc: &str, point: usize, out: &mut Vec<String>) {
    for (start, end) in codeiter::iter_stmts(msrc) {
        if start < point && end > point {
            let blob = msrc.slice(start, end);
            if blob.starts_with("pub mod "){
                let p = typeinf::generate_skeleton_for_parsing(blob);
                ast::parse_mod(p).name.map(|name|{

                    let newstart = blob.find_str("{").unwrap() + 1;
                    out.push(name);
                    get_local_module_path_(blob.slice_from(newstart), 
                                       point - start - newstart, out);
                });
            }
        }
    }
}

pub fn find_impl_start(msrc: &str, point: usize, scopestart: usize) -> Option<usize> {

    for (start, end) in codeiter::iter_stmts(msrc.slice_from(scopestart)) {
        if (scopestart + end) > point {
            let blob = msrc.slice_from(scopestart + start);
            // TODO:: the following is a bit weak at matching traits. make this better
            if blob.starts_with("impl") || 
                blob.starts_with("trait") || blob.starts_with("pub trait") {
                return Some(scopestart + start);
            } else {
                let newstart = blob.find_str("{").unwrap() + 1;
                return find_impl_start(msrc, point, scopestart+start+newstart);
            }
        }
    }
    return None;    
}


#[test]
fn finds_subnested_module() {
    let src = "
    pub mod foo {
        pub mod bar {
            here
        }
    }";
    let point = coords_to_point(src, 4, 12);
    let v = get_local_module_path(src, point);
    assert_eq!("foo", v[0].as_slice());
    assert_eq!("bar", v[1].as_slice());

    let point = coords_to_point(src, 2, 8);
    let v = get_local_module_path(src, point);
    assert_eq!("foo", v[0].as_slice());
}


pub fn split_into_context_and_completion<'a>(s: &'a str) -> (&'a str, &'a str, racer::CompletionType) {

    let mut start = 0;
    let s_bytes = s.as_bytes();
    let colon: u8 = ":".as_bytes()[0];
    let dot: u8 = ".".as_bytes()[0];

    for (i, c) in s.char_indices().rev() {
        if ! util::is_ident_char(c) {
            start = i+1;
            break;
        }
    }

    if start != 0 && s_bytes[start-1] == dot {    // field completion
        return (s.slice_to(start-1), s.slice_from(start), racer::CompletionType::CompleteField);
    }

    if start > 0 && s_bytes[start-1] == colon {  // path completion
        return (s.slice_to(start-2), s.slice_from(start), racer::CompletionType::CompletePath);
    }

    return (s.slice_to(start), s.slice_from(start), racer::CompletionType::CompletePath);
}

pub fn get_start_of_search_expr(msrc: &str, point: usize) -> usize {
    let openparen: u8 = "(".as_bytes()[0];
    let closeparen: u8 = ")".as_bytes()[0];
    let msrc_bytes = msrc.as_bytes();
    let mut levels = 0i32;
    let mut i = point-1;
    loop {
        if i == -1 {
            i = 0;
            break;
        }

        if msrc_bytes[i] == closeparen {
            levels += 1;
        }
        if levels == 0 && (!util::is_search_expr_char(msrc.char_at(i)) || 
                           util::is_double_dot(msrc,i)) {
            i += 1;
            break;
        }
        if msrc_bytes[i] == openparen && levels > 0 {
            levels -= 1;
        }

        i -= 1;
    }
    return i;
}

pub fn get_start_of_pattern(msrc: &str, point: usize) -> usize {
    let openparen: u8 = "(".as_bytes()[0];
    let closeparen: u8 = ")".as_bytes()[0];
    let msrc_bytes = msrc.as_bytes();
    let mut levels = 0i32;
    let mut i = point-1;
    loop {
        if i == -1 {
            i = 0;
            break;
        }

        if msrc_bytes[i] == closeparen {
            levels += 1;
        }
        if levels == 0 && !util::is_pattern_char(msrc.char_at(i)) {
            i += 1;
            break;
        }
        if msrc_bytes[i] == openparen && levels > 0 {
            levels -= 1;
        }

        i -= 1;
    }
    return i;
}

#[test]
fn get_start_of_pattern_handles_variant() {
    assert_eq!(4, get_start_of_pattern("foo, Some(a) =>",13));
}

#[test]
fn get_start_of_pattern_handles_variant2() {
    assert_eq!(4, get_start_of_pattern("bla, ast::PatTup(ref tuple_elements) => {",36));
}

pub fn expand_search_expr(msrc: &str, point: usize) -> (usize,usize) {
    let start = get_start_of_search_expr(msrc, point);
    return (start, util::find_ident_end(msrc, point));
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

pub fn mask_comments(src: &str) -> String {
    let mut result = String::new();
    let space = " ";

    let mut prev: usize = 0;
    for (start, end) in codecleaner::code_chunks(src) {
        for _ in ::std::iter::range(prev, start) {
            result.push_str(space);
        }
        result.push_str(src.slice(start,end));
        prev = end;
    }
    return result;
}

pub fn mask_sub_scopes(src:&str) -> String {
    let mut result = String::new();
    let mut levels = 0i32;
    
    for c in src.chars() {
        if c == '}' {
            levels -= 1;
        }
 
        if c == '\n' {
            result.push(c);
        } else if levels > 0 {
            result.push(' ');
        } else {
            result.push(c);
        }

        if c == '{' {
            levels += 1;
        } 
    }    
    return result;
}
 
pub fn end_of_next_scope<'a>(src: &'a str) -> &'a str {
    let mut level = 0i32;
    let mut end = 0;
    for (i,c) in src.char_indices() {
        if c == '}' {
            level -= 1;
            if level == 0 {
                end = i + 1;
                break;
            }
        } else if c == '{' {
            level += 1;
        }
    }
    return src.slice_to(end);
}

pub fn coords_to_point(src: &str, mut linenum: usize, col: usize) -> usize {
    let mut point=0;
    for line in src.lines() {
        linenum -= 1;
        if linenum == 0 { break }
        point+=line.len() + 1;  // +1 for the \n
    }
    return point + col;
}

pub fn point_to_coords(src:&str, point:usize) -> (usize, usize) {
    let mut i = 0;
    let mut linestart = 0;
    let mut nlines = 1;  // lines start at 1
    while i != point {
        if src.char_at(i) == '\n' {
            nlines += 1;
            linestart = i+1;
        }
        i+=1;
    }
    return (nlines, point - linestart);
}

pub fn point_to_coords2(path: &Path, point:usize) -> Option<(usize, usize)> {
    let mut lineno = 0;
    let mut file = BufferedReader::new(File::open(path));
    let mut p = 0;
    for line_r in file.lines() {
        let line = line_r.unwrap();
        lineno += 1;
        if point < (p + line.len()) {
            return Some((lineno, point - p));
        }
        p += line.len();
    }
    return None;
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
    let src = "
fn myfn() {
    let a = 3;
    print(a);
}
";
    let point = coords_to_point(src, 4, 10);
    let start = scope_start(src,point);
    assert!(start == 12);
}

#[test]
fn test_scope_start_handles_sub_scopes() {
    let src = "
fn myfn() {
    let a = 3;
    {
      let b = 4;
    }
    print(a);
}
";
    let point = coords_to_point(src, 7, 10);
    let start = scope_start(src,point);
    assert!(start == 12);
}



#[test]
fn masks_out_comments() {
    let src = "
this is some code
this is a line // with a comment
some more
";
    let r = mask_comments(src);

    assert!(src.len() == r.len());
    // characters at the start are the same
    assert!(src.as_bytes()[5] == r.as_bytes()[5]);
    // characters in the comments are masked
    let commentoffset = coords_to_point(src,3,23);
    assert!(r.as_slice().char_at(commentoffset) == ' ');
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

pub fn round_trip_point_and_coords(src:&str, lineno:usize, charno:usize) {
    let (a,b) = point_to_coords(src, coords_to_point(src, lineno, charno));
     assert_eq!((a,b),(lineno,charno));
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

