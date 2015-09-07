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
        match self.state {
            State::StateCode => Some(self.code()),
            State::StateComment => Some(self.comment()),
            State::StateCommentBlock  => Some(self.comment_block()),
            State::StateString => Some(self.string()),
            State::StateChar => Some(self.char()),
            State::StateFinished => None
        }
    }
}

impl<'a> CodeIndicesIter<'a> {
    fn code(&mut self) -> (usize, usize) {
        let mut pos = self.pos;
        let start = match self.state {
            State::StateString |
            State::StateChar => { pos-1 }, // include quote
            _ => { pos }
        };
        let src_bytes = self.src.as_bytes();
        for &b in &src_bytes[pos..] {
            pos += 1;
            match b {
                b'/' if src_bytes.len() > pos => match src_bytes[pos] {
                    b'/' => {
                        self.state = State::StateComment;
                        self.pos = pos + 1;
                        return (start, pos-1);
                    },
                    b'*' => {
                        self.state = State::StateCommentBlock;
                        self.pos = pos + 1;
                        return (start, pos-1);
                    },
                    _ => {}
                },
                b'"' => {    // "
                    self.state = State::StateString;
                    self.pos = pos;
                    return (start, pos); // include dblquotes
                },
                b'\'' => {
                    // single quotes are also used for lifetimes, so we need to
                    // be confident that this is not a lifetime.
                    // Look for backslash starting the escape, or a closing quote:
                    if src_bytes.len() > pos + 1 &&
                        (src_bytes[pos] == b'\\' ||
                         src_bytes[pos+1] == b'\'') {
                        self.state = State::StateChar;
                        self.pos = pos;
                        return (start, pos); // include single quote
                    }
                },
                _ => {}
            }
        }

        self.state = State::StateFinished;
        (start, self.src.len())
    }

    fn comment(&mut self) -> (usize, usize) {
        let mut pos = self.pos;
        let src_bytes = self.src.as_bytes();
        for &b in &src_bytes[pos..] {
            pos += 1;
            if b == b'\n' {
                if pos + 2 <= src_bytes.len() && &src_bytes[pos..pos+2] == &[b'/', b'/'] {
                    continue;
                }
                break;
            }
        }
        self.pos = pos;
        self.code()
    }

    fn comment_block(&mut self) -> (usize, usize) {
        let mut nesting_level = 0usize;
        let mut prev = b' ';
        let mut pos = self.pos;
        for &b in &self.src.as_bytes()[pos..] {
            pos += 1;
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
        self.pos = pos;
        self.code()
    }

    fn string(&mut self) -> (usize, usize) {
        let src_bytes = self.src.as_bytes();
        let mut pos = self.pos;
        if pos > 1 && src_bytes[pos-2] == b'r' {
            // raw string (eg br"\"): no escape
            match src_bytes[pos..].iter().position(|&b| b == b'"') {
                Some(p) => pos += p+1,
                None    => pos = src_bytes.len()
            }
        } else {
            let mut is_not_escaped = true;
            for &b in &src_bytes[pos..] {
                pos += 1;
                match b {
                    b'"' if is_not_escaped  => { break; }, // "
                    b'\\' => { is_not_escaped = !is_not_escaped; },
                    _ => { is_not_escaped = true; }
                }
            }
        }
        self.pos = pos;
        self.code()
    }

    fn char(&mut self) -> (usize, usize) {
        let mut is_not_escaped = true;
        let mut pos = self.pos;
        for &b in &self.src.as_bytes()[pos..] {
            pos += 1;
            match b {
                b'\'' if is_not_escaped  => { break; },
                b'\\' => { is_not_escaped = !is_not_escaped; },
                _ => { is_not_escaped = true; }
            }
        }
        self.pos = pos;
        self.code()
    }
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks(src: &str) -> CodeIndicesIter {
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
fn removes_consecutive_comments() {
    let src = &rejustify("
    this is some code // this is a comment
    // this is more comment
    // another comment
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
    this is some code \'\"\' more code \'\\x00\' and \'\\\'\' that\'s it
    ");
    let mut it = code_chunks(src);
    assert_eq!("this is some code \'", slice(src, it.next().unwrap()));
    assert_eq!("\' more code \'", slice(src, it.next().unwrap()));
    assert_eq!("\' and \'", slice(src, it.next().unwrap()));
    assert_eq!("\' that\'s it", slice(src, it.next().unwrap()));
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
