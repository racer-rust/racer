//#[cfg(test)] use racer::testutils::{rejustify, slice};

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

pub fn slice(src: &str, (begin, end): (usize, usize)) -> &str{
    &src[begin..end]
}

enum State {
    StateCode,
    StateComment,
    StateCommentBlock,
    StateString,
    StateFinished
}

pub struct CodeIndicesIter<'a> {
    src: &'a str,
    start: usize,
    pos: usize,
    nesting_level: usize,
    state: State
}

impl<'a> Iterator for CodeIndicesIter<'a> {
    type Item = (usize, usize);

    #[inline]
    fn next(&mut self) -> Option<(usize, usize)> {
        match self.state {
            State::StateCode => code(self),
            State::StateComment => comment(self),
            State::StateCommentBlock  => comment_block(self),
            State::StateString => string(self),
            State::StateFinished => None
        }
    }
}

fn code(self_: &mut CodeIndicesIter) -> Option<(usize,usize)> {
    
    let src_bytes = self_.src.as_bytes();
    let mut is_prev_slash = self_.pos > 0 && src_bytes[self_.pos-1] == b'/';

    for (i, &b) in src_bytes[self_.pos..].iter().enumerate() {
        match b {
            b'/' => { 
                if is_prev_slash {
                    return code_return(self_, i, State::StateComment, i-1); 
                }
                is_prev_slash = true;
            },
            b'*' if is_prev_slash => {
                self_.nesting_level = 0;
                return code_return(self_, i, State::StateCommentBlock, i-1);
            },
            b'"' => { 
                // include the dblquote in the code
                return code_return(self_, i, State::StateString, i+1);
            },
            _ => { is_prev_slash = false; }
        }
    }

    self_.state = State::StateFinished;
    Some((self_.start, src_bytes.len()))
}

fn code_return(self_: &mut CodeIndicesIter, i: usize, state: State, inner_end: usize) 
        -> Option<(usize,usize)>{
    let res = Some((self_.start, self_.pos+inner_end));
    self_.pos += i+1;
    self_.start = self_.pos;
    self_.state = state;
    return res;
}

fn comment(self_: &mut CodeIndicesIter) -> Option<(usize,usize)> {
    let src_bytes = self_.src.as_bytes();
    for (i, &b) in src_bytes[self_.pos..].iter().enumerate() {
        if b == b'\n' {
            self_.pos += i+1;
            self_.start = self_.pos;
            self_.state = State::StateCode;
            return code(self_);
        }
    }
    self_.state = State::StateFinished;
    Some((self_.start, src_bytes.len()))
}

fn comment_block(self_: &mut CodeIndicesIter) -> Option<(usize,usize)> {
    
    let src_bytes = self_.src.as_bytes();
    // previous character
    let mut prev = if self_.pos > 0 { src_bytes[self_.pos-1] } else { b' ' };

    for (i, &b) in src_bytes[self_.pos..].iter().enumerate() {
        match b {
            b'/' if prev == b'*' => { 
                if self_.nesting_level == 0 {
                    self_.pos += i+1;
                    self_.start = self_.pos;
                    self_.state = State::StateCode;
                    return code(self_);
                } else {
                    self_.nesting_level -= 1;
                }
            },
            b'*' if prev == b'/' => {
                self_.nesting_level += 1;
            },
            _ => { prev = b; }
        }
    }
    self_.state = State::StateFinished;
    return Some((self_.start, src_bytes.len()));
}

fn string(self_: &mut CodeIndicesIter) -> Option<(usize,usize)> {
    let src_bytes = self_.src.as_bytes();
    let mut is_escaped = false;
    for (i, &b) in src_bytes[self_.pos..].iter().enumerate() {
        match b {
            b'"' if !is_escaped  => { 
                self_.start = self_.pos+i;  // include the dblquote as code
                self_.pos = self_.start+1;
                self_.state = State::StateCode;
                return code(self_);
            },
            b'\\' => { is_escaped = !is_escaped; },
            _ => { if is_escaped {is_escaped = false;} }
        }
    }
    self_.state = State::StateFinished;
    return Some((self_.start, src_bytes.len()));
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks<'a>(src: &'a str) -> CodeIndicesIter<'a> {
    CodeIndicesIter { src: src, start: 0, pos: 0, state: State::StateCode, nesting_level: 0 }
}

#[test]
fn removes_a_comment() {
    let src = &rejustify("
    this is some code // this is a comment
    some more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_contents() {
    let src = &rejustify("
    this is some code \"this is a string\" more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_contents_with_a_comment_in_it() {
    let src = &rejustify("
    this is some code \"string with a // fake comment \" more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_a_comment_with_a_dbl_quote_in_it() {
    let src = &rejustify("
    this is some code // comment with \" double quote
    some more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_multiline_comment() {
    let src = &rejustify("
    this is some code /* this is a
    \"multiline\" comment */some more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn handles_nesting_of_block_comments() {
    let src = &rejustify("
    this is some code /* nested /* block */ comment */ some more code
    ")[];
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!(" some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_dblquote_in_it() {
    let src = &rejustify("
    this is some code \"string with a \\\" escaped dblquote fake comment \" more code
    ")[];

    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_slash_before_dblquote_in_it() {
    let src = &rejustify("
    this is some code \"string with an escaped slash, so dbl quote does end the string after all \\\\\" more code
    ")[];

    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn handles_tricky_bit_from_str_rs() {
    let src = &rejustify("
        before(\"\\\\\'\\\\\\\"\\\\\\\\\");
        more_code(\" skip me \")
    ")[];

    for (start,end) in code_chunks(src) {
        println!("BLOB |{}|",&src[start..end]);
        if (&src[start..end]).contains("skip me") {
            panic!("{}", &src[start..end]);
        }
    }
}

// fn main() {
//     use std::io::BufferedReader;
//     use std::io::File;
//     use std::str;

//     //let filetxt = BufferedReader::new(File::open(&Path::new("/usr/local/src/rust/src/libstd/prelude.rs"))).read_to_end().unwrap();
//     //let filetxt = BufferedReader::new(File::open(&Path::new("/usr/local/src/rust/src/libstd/prelude.rs"))).read_to_end().unwrap();
//     let filetxt = BufferedReader::new(File::open(&Path::new("/usr/local/src/rust/src/libcollections/str.rs"))).read_to_end().unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();

//     for (start,end) in code_chunks(src) {
//         println!("BLOB |{}|",src.slice(start,end));
//     }

// }
