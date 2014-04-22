use racer::testutils::{rejustify, slice};

enum State {
    Code,
    Comment,
    MultilineComment,
    String,
    Finished
}

pub struct CodeIndicesIter<'a> {
    src: &'a str,
    start: uint,
    pos: uint,
    state: State
}

impl<'a> Iterator<(uint, uint)> for CodeIndicesIter<'a> {
    #[inline]
    fn next(&mut self) -> Option<(uint, uint)> {
        return match self.state {
            Code => code(self),
            Comment => comment(self),
            MultilineComment => multiline_comment(self),
            String => string(self),
            Finished => None
        }
    }
}

fn code(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let slash: u8 = "/"[0] as u8;
    let star: u8 = "*"[0] as u8;
    let dblquote: u8 = "\""[0] as u8;

    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let start = self_.start;
    while pos < end {
        if pos > 0 && src[pos] == slash && src[pos-1] == slash {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = Comment;
            return Some((start, pos-1));
        } 

        if pos > 0 && src[pos] == star && src[pos-1] == slash {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = MultilineComment;
            return Some((start, pos-1));
        } 

        if src[pos] == dblquote {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = String;
            return Some((start, pos+1)); // include the dblquote in the code
        }

        pos += 1;
    }
    self_.state = Finished;
    return Some((start, end));
}

fn comment(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let newline: u8 = "\n"[0] as u8;
    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let start = pos;
    while pos < end {
        if src[pos] == newline {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = Code;
            return code(self_);
        } 
        pos += 1;
    }
    self_.state = Finished;
    return Some((start, end));    
}

fn multiline_comment(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let slash: u8 = "/"[0] as u8;
    let star: u8 = "*"[0] as u8;
    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let start = pos;
    while pos < end {
        if pos > 0 && src[pos] == slash && src[pos-1] == star {
            self_.start = pos+1;
            self_.pos = pos+1;
            self_.state = Code;
            return code(self_);
        } 
        pos += 1;
    }
    self_.state = Finished;
    return Some((start, end));    
}


fn string(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let dblquote: u8 = "\""[0] as u8;
    let backslash: u8 = "\\"[0] as u8;

    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let start = self_.start;
    while pos < end {
        if src[pos] == dblquote && src[pos-1] != backslash {
            self_.start = pos;   // include the dblquote as code
            self_.pos = pos+1;
            self_.state = Code;
            return code(self_);
        } 
        pos += 1;
    }
    self_.state = Finished;
    return Some((start, end));
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks<'a>(src: &'a str) -> CodeIndicesIter<'a> {
    CodeIndicesIter { src: src, start: 0, pos: 0, state: Code }
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

