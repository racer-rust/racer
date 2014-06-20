#[cfg(test)] use racer::testutils::{rejustify, slice};

enum State {
    Code,
    Comment,
    CommentBlock,
    String,
    Finished
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
            Code => code(self),
            Comment => comment(self),
            CommentBlock  => comment_block(self),
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
            self_.state = CommentBlock;
            self_.nesting_level = 0;
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

fn comment_block(self_: &mut CodeIndicesIter) -> Option<(uint,uint)> {
    let slash: u8 = "/"[0] as u8;
    let star: u8 = "*"[0] as u8;
    let (mut pos, src, end) = (self_.pos, self_.src, self_.src.len());
    let start = pos;
    while pos < end {
        if pos > 0 && src[pos] == star && src[pos-1] == slash {
            self_.nesting_level += 1;
        }

        if pos > 0 && src[pos] == slash && src[pos-1] == star {
            if self_.nesting_level == 0 {
                self_.start = pos+1;
                self_.pos = pos+1;
                self_.state = Code;
                return code(self_);
            } else {
                self_.nesting_level -= 1;
            }
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
        // is the dblquote escaped? Is the escape char escaped?
        if (src[pos] == dblquote && src[pos-1] != backslash) || 
           (src[pos] == dblquote && src[pos-1] == backslash && src[pos-2] == backslash){
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
    CodeIndicesIter { src: src, start: 0, pos: 0, state: Code, nesting_level: 0 }
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
