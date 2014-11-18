//#[cfg(test)] use racer::testutils::{rejustify, slice};

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

pub fn slice<'a>(src: &'a str, (begin, end): (uint, uint)) -> &'a str{
    return src.slice(begin, end);
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
    start: uint,
    pos: uint,
    nesting_level: uint,
    state: State
}

impl<'a> Iterator<(uint, uint)> for CodeIndicesIter<'a> {
    #[inline]
    fn next(&mut self) -> Option<(uint, uint)> {
        return match self.state {
            State::StateCode => code(self),
            State::StateComment => comment(self),
            State::StateCommentBlock  => comment_block(self),
            State::StateString => string(self),
            State::StateFinished => None
        }
    }
}

fn code(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let slash: u8 = "/".as_bytes()[0] as u8;
    let star: u8 = "*".as_bytes()[0] as u8;
    let dblquote: u8 = "\"".as_bytes()[0] as u8;

    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let src_bytes = src.as_bytes();
    let start = self_.start;
    while pos < end {
        if pos > 0 && src_bytes[pos] == slash && src_bytes[pos-1] == slash {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = State::StateComment;
            return Some((start, pos-1));
        }

        if pos > 0 && src_bytes[pos] == star && src_bytes[pos-1] == slash {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = State::StateCommentBlock;
            self_.nesting_level = 0;
            return Some((start, pos-1));
        }

        if src_bytes[pos] == dblquote {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = State::StateString;
            return Some((start, pos+1)); // include the dblquote in the code
        }

        pos += 1;
    }
    self_.state = State::StateFinished;
    return Some((start, end));
}

fn comment(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let newline: u8 = "\n".as_bytes()[0] as u8;
    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let src_bytes = src.as_bytes();
    let start = pos;
    while pos < end {
        if src_bytes[pos] == newline {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = State::StateCode;
            return code(self_);
        }
        pos += 1;
    }
    self_.state = State::StateFinished;
    return Some((start, end));
}

fn comment_block(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let slash: u8 = "/".as_bytes()[0] as u8;
    let star: u8 = "*".as_bytes()[0] as u8;
    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let src_bytes = src.as_bytes();
    let start = pos;
    while pos < end {
        if pos > 0 && src_bytes[pos] == star && src_bytes[pos-1] == slash {
            self_.nesting_level += 1;
        }

        if pos > 0 && src_bytes[pos] == slash && src_bytes[pos-1] == star {
            if self_.nesting_level == 0 {
                self_.start = pos+1;
                self_.pos = pos+1;
                self_.state = State::StateCode;
                return code(self_);
            } else {
                self_.nesting_level -= 1;
            }
        }
        pos += 1;
    }
    self_.state = State::StateFinished;
    return Some((start, end));
}


// returns true if char at position is escaped
fn escaped(src_bytes: &[u8], mut pos: uint) -> bool {
    let mut num_backslashes = 0u;
    let backslash: u8 = "\\".as_bytes()[0] as u8;
    while pos > 0 && src_bytes[pos-1] == backslash {
        num_backslashes += 1;
        pos -= 1;
    }
    return num_backslashes % 2 == 1;
}

fn string(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let dblquote: u8 = "\"".as_bytes()[0] as u8;

    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let src_bytes = src.as_bytes();
    let start = self_.start;
    while pos < end {
        if src_bytes[pos] == dblquote && !escaped(src_bytes, pos) {
            self_.start = pos;   // include the dblquote as code
            self_.pos = pos+1;
            self_.state = State::StateCode;
            return code(self_);
        }
        pos += 1;
    }
    self_.state = State::StateFinished;
    return Some((start, end));
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks<'a>(src: &'a str) -> CodeIndicesIter<'a> {
    CodeIndicesIter { src: src, start: 0, pos: 0, state: State::StateCode, nesting_level: 0 }
}

#[test]
fn removes_a_comment() {
    let src = rejustify("
    this is some code // this is a comment
    some more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code ", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("some more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_string_contents() {
    let src = rejustify("
    this is some code \"this is a string\" more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code \"", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("\" more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_string_contents_with_a_comment_in_it() {
    let src = rejustify("
    this is some code \"string with a // fake comment \" more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code \"", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("\" more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_a_comment_with_a_dbl_quote_in_it() {
    let src = rejustify("
    this is some code // comment with \" double quote
    some more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code ", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("some more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_multiline_comment() {
    let src = rejustify("
    this is some code /* this is a
    \"multiline\" comment */some more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code ", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("some more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn handles_nesting_of_block_comments() {
    let src = rejustify("
    this is some code /* nested /* block */ comment */ some more code
    ");
    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code ", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!(" some more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_dblquote_in_it() {
    let src = rejustify("
    this is some code \"string with a \\\" escaped dblquote fake comment \" more code
    ");

    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code \"", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("\" more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn removes_string_with_escaped_slash_before_dblquote_in_it() {
    let src = rejustify("
    this is some code \"string with an escaped slash, so dbl quote does end the string after all \\\\\" more code
    ");

    let mut it = code_chunks(src.as_slice());
    assert_eq!("this is some code \"", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("\" more code", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn handles_tricky_bit_from_str_rs() {
    let src = rejustify("
        before(\"\\\\\'\\\\\\\"\\\\\\\\\");
        more_code(\" skip me \")
    ");

    let src = src.as_slice();

    for (start,end) in code_chunks(src) {
        println!("BLOB |{}|",src.slice(start,end));
        if src.slice(start,end).contains("skip me") {
            panic!("{}", src.slice(start,end));
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
