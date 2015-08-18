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

pub fn slice(src: &str, (begin, end): (usize, usize)) -> &str {
    &src[begin..end]
}

#[derive(Clone,Copy)]
enum State {
    StateCode,
    StateComment,
    StateCommentBlock,
    StateString,
    StateChar,
    StateFinished
}

#[derive(Clone,Copy)]
pub struct CodeIndicesIter<'a> {
    src: &'a str,
    pos: usize,
    state: State
}

impl<'a> Iterator for CodeIndicesIter<'a> {
    type Item = (usize, usize);

    #[inline]
    fn next(&mut self) -> Option<(usize, usize)> {
        let res = match self.state {
            State::StateCode => Some(code(self)),
            State::StateComment => Some(comment(self)),
            State::StateCommentBlock  => Some(comment_block(self)),
            State::StateString => Some(string(self)),
            State::StateChar => Some(char(self)),
            State::StateFinished => None
        };
        res
    }
}

fn code(self_: &mut CodeIndicesIter) -> (usize, usize) {
    let start = match self_.state {
        State::StateString |
        State::StateChar => { self_.pos-1 }, // include quote
        _ => { self_.pos }
    };
    let src_bytes = self_.src.as_bytes();
    for &b in src_bytes[self_.pos..].iter() {
        self_.pos += 1;
        match b {
            b'/' => match src_bytes[self_.pos] {
                b'/' => {
                    self_.state = State::StateComment;
                    self_.pos += 1;
                    return (start, self_.pos-2);
                },
                b'*' => {
                    self_.state = State::StateCommentBlock;
                    self_.pos += 1;
                    return (start, self_.pos-2);
                },
                _ => {}
            },
            b'"' => {    // "
                self_.state = State::StateString;
                return (start, self_.pos); // include dblquotes
            },
            b'\'' => {
                // single quotes are also used for lifetimes, so we need to
                // be confident that this is not a lifetime.
                // Look for closing quote:
                if src_bytes.len() > self_.pos + 2 &&
                    (src_bytes[self_.pos+1] == b'\'' ||
                     src_bytes[self_.pos+2] == b'\'') {
                    self_.state = State::StateChar;
                    return (start, self_.pos); // include single quote
                }
            },
            _ => {}
        }
    }

    self_.state = State::StateFinished;
    (start, self_.src.len())
}

fn comment(self_: &mut CodeIndicesIter) -> (usize, usize) {
    for &b in self_.src.as_bytes()[self_.pos..].iter() {
        self_.pos += 1;
        if b == b'\n' { break; }
    }
    code(self_)
}

fn comment_block(self_: &mut CodeIndicesIter) -> (usize, usize) {
    let mut nesting_level = 0u16; // should be enough
    let mut prev = b' ';
    for &b in self_.src.as_bytes()[self_.pos..].iter() {
        self_.pos += 1;
        match b {
            b'/' if prev == b'*' => {
                if nesting_level == 0 {
                    break;
                } else {
                    nesting_level -= 1;
                }
            },
            b'*' if prev == b'/' => {
                nesting_level += 1;
            },
            _ => { prev = b; }
        }
    }
    code(self_)
}

fn string(self_: &mut CodeIndicesIter) -> (usize, usize) {
    let src_bytes = self_.src.as_bytes();
    if self_.pos > 1 && src_bytes[self_.pos-2] == b'r' {
        // raw string (eg br"\"): no escape
        match src_bytes[self_.pos..].iter().position(|&b| b == b'"') {
            Some(p) => self_.pos += p+1,
            None    => self_.pos = src_bytes.len()
        }
    } else {
        let mut is_not_escaped = true;
        for &b in src_bytes[self_.pos..].iter() {
            self_.pos += 1;
            match b {
                b'"' if is_not_escaped  => { break; }, // "
                b'\\' => { is_not_escaped = !is_not_escaped; },
                _ => { is_not_escaped = true; }
            }
        }
    }
    code(self_)
}

fn char(self_: &mut CodeIndicesIter) -> (usize, usize) {
    let mut is_not_escaped = true;
    for &b in self_.src.as_bytes()[self_.pos..].iter() {
        self_.pos += 1;
        match b {
            b'\'' if is_not_escaped  => { break; },
            b'\\' => { is_not_escaped = !is_not_escaped; },
            _ => { is_not_escaped = true; }
        }
    }
    code(self_)
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks<'a>(src: &'a str) -> CodeIndicesIter<'a> {
    CodeIndicesIter { src: src, state: State::StateCode, pos: 0 }
}

#[test]
fn removes_a_comment() {
    let src = &rejustify("
    this is some code // this is a comment
    some more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_contents() {
    let src = &rejustify("
    this is some code \"this is a string\" more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_char_contents() {
    let src = &rejustify("
    this is some code \'\"\' more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code \'", slice(src, it.next().unwrap()));
    assert_eq!("\' more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_contents_with_a_comment_in_it() {
    let src = &rejustify("
    this is some code \"string with a // fake comment \" more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_a_comment_with_a_dbl_quote_in_it() {
    let src = &rejustify("
    this is some code // comment with \" double quote
    some more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_multiline_comment() {
    let src = &rejustify("
    this is some code /* this is a
    \"multiline\" comment */some more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!("some more code", slice(src, it.next().unwrap()));
}

#[test]
fn handles_nesting_of_block_comments() {
    let src = &rejustify("
    this is some code /* nested /* block */ comment */ some more code
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code ", slice(src, it.next().unwrap()));
    assert_eq!(" some more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_dblquote_in_it() {
    let src = &rejustify("
    this is some code \"string with a \\\" escaped dblquote fake comment \" more code
    ");

    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_raw_string_with_dangling_escape_in_it() {
    let src = &rejustify("
    this is some code br\" escaped dblquote raw string \\\" more code
    ");

    let mut it = code_chunks(src);
    assert_eq!("this is some code br\"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_slash_before_dblquote_in_it() {
    let src = &rejustify("
    this is some code \"string with an escaped slash, so dbl quote does end the string after all \\\\\" more code
    ");

    let mut it = code_chunks(src);
    assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
    assert_eq!("\" more code", slice(src, it.next().unwrap()));
}

#[test]
fn handles_tricky_bit_from_str_rs() {
    let src = &rejustify("
        before(\"\\\\\'\\\\\\\"\\\\\\\\\");
        more_code(\" skip me \")
    ");

    for (start, end) in code_chunks(src) {
        println!("BLOB |{}|", &src[start..end]);
        if (&src[start..end]).contains("skip me") {
            panic!("{}", &src[start..end]);
        }
    }
}

// fn main() {
//     use std::old_io::BufferedReader;
//     use std::old_io::File;
//     use std::str;

//     let filetxt = BufferedReader::new(File::open(&Path::new("/tmp/testcode.rs"))).read_to_end().unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();

//     for (start,end) in code_chunks(src) {
//         println!("BLOB |{}|",src.slice(start,end));
//     }

// }