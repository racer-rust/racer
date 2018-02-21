use core::{Point, SourceByteRange};

/// Type of the string
#[derive(Clone, Copy, Debug)]
enum StrStyle {
    /// normal string starts with "
    Cooked,
    /// Raw(n) => raw string started with n #s
    Raw(usize),
}

#[derive(Clone,Copy)]
enum State {
    Code,
    Comment,
    CommentBlock,
    String(StrStyle),
    Char,
    Finished
}

#[derive(Clone,Copy)]
pub struct CodeIndicesIter<'a> {
    src: &'a str,
    pos: Point,
    state: State
}

impl<'a> Iterator for CodeIndicesIter<'a> {
    type Item = SourceByteRange;

    #[inline]
    fn next(&mut self) -> Option<SourceByteRange> {
        match self.state {
            State::Code => Some(self.code()),
            State::Comment => Some(self.comment()),
            State::CommentBlock  => Some(self.comment_block()),
            State::String(level) => Some(self.string(level)),
            State::Char => Some(self.char()),
            State::Finished => None
        }
    }
}

impl<'a> CodeIndicesIter<'a> {
    fn code(&mut self) -> SourceByteRange {
        let mut pos = self.pos;
        let start = match self.state {
            State::String(_) |
            State::Char => { pos-1 }, // include quote
            _ => { pos }
        };
        let src_bytes = self.src.as_bytes();
        for &b in &src_bytes[pos..] {
            pos += 1;
            match b {
                b'/' if src_bytes.len() > pos => match src_bytes[pos] {
                    b'/' => {
                        self.state = State::Comment;
                        self.pos = pos + 1;
                        return (start, pos-1);
                    },
                    b'*' => {
                        self.state = State::CommentBlock;
                        self.pos = pos + 1;
                        return (start, pos-1);
                    },
                    _ => {}
                },
                b'"' => {    // "
                    let str_type = self.detect_str_type(pos);
                    self.state = State::String(str_type);
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
                        self.state = State::Char;
                        self.pos = pos;
                        return (start, pos); // include single quote
                    }
                },
                _ => {}
            }
        }

        self.state = State::Finished;
        (start, self.src.len())
    }

    fn comment(&mut self) -> SourceByteRange {
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

    fn comment_block(&mut self) -> SourceByteRange {
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

    fn string(&mut self, str_type: StrStyle) -> SourceByteRange {
        let src_bytes = self.src.as_bytes();
        let mut pos = self.pos;
        match str_type {
            StrStyle::Raw(level) => {
                // raw string (eg br#"\"#)
                // detect corresponding end(if start is r##", "##) greedily
                enum SharpState {
                    Sharp((usize, usize)), // (Num of preceding #s, Pos of end ")
                    None, // No preceding "##...
                }
                let mut cur_state = SharpState::None;
                let mut end_was_found = false;
                for (i, &b) in src_bytes[self.pos..].iter().enumerate() {
                    match cur_state {
                        SharpState::Sharp((n_sharp, pos_quote)) => {
                            cur_state = match b {
                                b'#' => SharpState::Sharp((n_sharp + 1, pos_quote)),
                                b'"' => SharpState::Sharp((0, i)),
                                _ => SharpState::None,
                            }
                        }
                        SharpState::None => {
                            if b == b'"' {
                                cur_state = SharpState::Sharp((0, i));
                            }
                        }
                    }
                    if let SharpState::Sharp((n_sharp, pos_quote)) = cur_state {
                        if n_sharp == level {
                            end_was_found = true;
                            pos += pos_quote + 1;
                            break;
                        }
                    }
                }
                if !end_was_found {
                    pos = src_bytes.len();
                }
            }
            StrStyle::Cooked => {
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
        };
        self.pos = pos;
        self.code()
    }

    fn char(&mut self) -> SourceByteRange {
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

    fn detect_str_type(&self, pos: usize) -> StrStyle {
        let src_bytes = self.src.as_bytes();
        let mut sharp = 0;
        if pos == 0 {
            return StrStyle::Cooked;
        }
        // now pos is at one byte after ", so we have to start at pos - 2
        for &b in src_bytes[..pos - 1].iter().rev() {
            match b {
                b'#' => sharp += 1,
                b'r' => return StrStyle::Raw(sharp),
                _ => return StrStyle::Cooked,
            }
        }
        StrStyle::Cooked
    }
}

/// Returns indices of chunks of code (minus comments and string contents)
pub fn code_chunks(src: &str) -> CodeIndicesIter {
    CodeIndicesIter { src: src, state: State::Code, pos: 0 }
}

/// Reverse Iterator for reading the source bytes skipping comments.
/// This is written for get_start_of_pattern and maybe not so robust.
pub struct CommentSkipIterRev<'a> {
    src: &'a str,
    pos: Point,
}

/// This produce CommentSkipIterRev for range [0, start)
pub fn comment_skip_iter_rev(s: &str, start: Point) -> CommentSkipIterRev {
    let start = if start > s.len() { 0 } else { start };
    CommentSkipIterRev { src: s, pos: start }
}

impl<'a> Iterator for CommentSkipIterRev<'a> {
    type Item = (char, Point);
    fn next(&mut self) -> Option<(char, Point)> {
        let cur_byte = self.cur_byte()?;
        match cur_byte {
            b'\n' => {
                let pos = self.pos;
                self.pos = self.skip_line_comment();
                Some((cur_byte as char, pos - 1))
            }
            b'/' => {
                if let Some(next_byte) = self.get_byte(self.pos - 1) {
                    if next_byte == b'*' {
                        self.pos = self.skip_block_comment();
                        Some((self.cur_byte()? as char, self.pos - 1))
                    } else {
                        self.code()
                    }
                } else {
                    self.code()
                }
            }
            _ => self.code(),
        }
    }
}

impl<'a> CommentSkipIterRev<'a> {

    fn cur_byte(&self) -> Option<u8> {
        self.get_byte(self.pos)
    }

    fn code(&mut self) -> Option<(char, Point)> {
        let cur_byte = self.cur_byte()?;
        self.pos -= 1;
        Some((cur_byte as char, self.pos))
    }

    fn get_byte(&self, p: Point) -> Option<u8> {
        if p == 0 {
            None
        } else {
            let b = self.src.as_bytes()[p - 1];
            Some(b)
        }
    }

    // return where 'pos' shuld be after skipping block comments
    fn skip_block_comment(&self) -> Point {
        let mut nest_level = 0;
        let mut prev = b' ';
        for i in (0..self.pos - 2).rev() {
            let b = self.src.as_bytes()[i];
            match b {
                b'/' if prev == b'*' => {
                    if nest_level == 0 {
                        return i;
                    } else {
                        nest_level -= 1;
                    }
                }
                b'*' if prev == b'/' => {
                    nest_level += 1;
                }
                _ => {
                    prev = b;
                }
            }
        }
        0
    }

    // return where 'pos' shuld be after skipping line comments
    fn skip_line_comment(&self) -> Point {
        let skip_cr = |p: Point| -> Point {
            if let Some(b) = self.get_byte(p) {
                if b == b'\r' {
                    return p - 1;
                }
            }
            p
        };
        let mut pos = self.pos;
        let mut skipped_whole_line = true;
        while skipped_whole_line && pos > 0 {
            // now pos >= 1 && self.src.as_bytes()[pos - 1] == '\n'
            skipped_whole_line = false;
            let comment_start = if let Some(next_newline) = self.src[..pos - 1].rfind('\n') {
                if let Some(start) = self.src[next_newline + 1..pos - 1].find("//") {
                    skipped_whole_line = start == 0;
                    start + next_newline + 1
                } else {
                    return skip_cr(pos - 1);
                }
            } else {
                if let Some(start) = self.src[..pos - 1].find("//") {
                    start
                } else {
                    return skip_cr(pos - 1);
                }
            };
            pos = comment_start;
        }
        pos
    }
}


#[cfg(test)]
mod code_indices_iter_test {
    use super::*;
    use ::testutils::{rejustify, slice};

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
            if src[start..end].contains("skip me") {
                panic!("{}", &src[start..end]);
            }
        }
    }

    #[test]
    fn removes_nested_rawstr() {
        let src = &rejustify(r####"
    this is some code br###""" r##""##"### more code
    "####);

        let mut it = code_chunks(src);
        assert_eq!("this is some code br###\"", slice(src, it.next().unwrap()));
        assert_eq!("\"### more code", slice(src, it.next().unwrap()));
    }

}

#[cfg(test)]
mod comment_skip_iter_rev_test {
    use super::*;
    use ::testutils::rejustify;
    #[test]
    fn removes_consecutive_comments_with_comment_skip_iter_rev() {
         let src = &rejustify("
    this is some code // this is a comment
    // this is more comment
    // another comment
    some more code
    ");
        let result: String = comment_skip_iter_rev(&src, src.len()).map(|c| c.0).collect();
        assert_eq!(&result, "edoc erom emos\n edoc emos si siht");
    }
    #[test]
    fn removes_nested_block_comments_with_comment_skip_iter_rev() {
         let src = &rejustify("
    this is some code // this is a comment
    /* /* nested comment */ */
    some more code
    ");
        let result: String = comment_skip_iter_rev(&src, src.len()).map(|c| c.0).collect();
        assert_eq!(&result, "edoc erom emos\n\n\n edoc emos si siht");
    }
    #[test]
    fn removes_multiline_comment_with_comment_skip_iter_rev() {
        let src = &rejustify("
    this is some code /* this is a
    \"multiline\" comment */some more code
    ");
        let result: String = comment_skip_iter_rev(&src, src.len()).map(|c| c.0).collect();
        assert_eq!(&result, "edoc erom emos  edoc emos si siht");
    }
}
