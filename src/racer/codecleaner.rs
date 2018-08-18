use core::{BytePos, ByteRange};

/// Type of the string
#[derive(Clone, Copy, Debug)]
enum StrStyle {
    /// normal string starts with "
    Cooked,
    /// Raw(n) => raw string started with n #s
    Raw(usize),
}

#[derive(Clone, Copy)]
enum State {
    Code,
    Comment,
    CommentBlock,
    String(StrStyle),
    Char,
    Finished,
}

#[derive(Clone, Copy)]
pub struct CodeIndicesIter<'a> {
    src: &'a str,
    pos: BytePos,
    state: State,
}

impl<'a> Iterator for CodeIndicesIter<'a> {
    type Item = ByteRange;

    fn next(&mut self) -> Option<ByteRange> {
        match self.state {
            State::Code => Some(self.code()),
            State::Comment => Some(self.comment()),
            State::CommentBlock => Some(self.comment_block()),
            State::String(style) => Some(self.string(style)),
            State::Char => Some(self.char()),
            State::Finished => None,
        }
    }
}

impl<'a> CodeIndicesIter<'a> {
    fn code(&mut self) -> ByteRange {
        let mut pos = self.pos;
        let start = match self.state {
            State::String(_) | State::Char => pos.decrement(), // include quote
            _ => pos,
        };
        let src_bytes = self.src.as_bytes();
        for &b in &src_bytes[pos.0..] {
            pos = pos.increment();
            match b {
                b'/' if src_bytes.len() > pos.0 => match src_bytes[pos.0] {
                    b'/' => {
                        self.state = State::Comment;
                        self.pos = pos.increment();
                        return ByteRange::new(start, pos.decrement());
                    }
                    b'*' => {
                        self.state = State::CommentBlock;
                        self.pos = pos.increment();
                        return ByteRange::new(start, pos.decrement());
                    }
                    _ => {}
                },
                b'"' => {
                    // "
                    let str_type = self.detect_str_type(pos);
                    self.state = State::String(str_type);
                    self.pos = pos;
                    return ByteRange::new(start, pos); // include dblquotes
                }
                b'\'' => {
                    // single quotes are also used for lifetimes, so we need to
                    // be confident that this is not a lifetime.
                    // Look for backslash starting the escape, or a closing quote:
                    if src_bytes.len() > pos.increment().0
                        && (src_bytes[pos.0] == b'\\' || src_bytes[pos.increment().0] == b'\'')
                    {
                        self.state = State::Char;
                        self.pos = pos;
                        return ByteRange::new(start, pos); // include single quote
                    }
                }
                _ => {}
            }
        }

        self.state = State::Finished;
        ByteRange::new(start, self.src.len().into())
    }

    fn comment(&mut self) -> ByteRange {
        let mut pos = self.pos;
        let src_bytes = self.src.as_bytes();
        for &b in &src_bytes[pos.0..] {
            pos = pos.increment();
            if b == b'\n' {
                if pos.0 + 2 <= src_bytes.len() && src_bytes[pos.0..pos.0 + 2] == [b'/', b'/'] {
                    continue;
                }
                break;
            }
        }
        self.pos = pos;
        self.code()
    }

    fn comment_block(&mut self) -> ByteRange {
        let mut nesting_level = 0usize;
        let mut prev = b' ';
        let mut pos = self.pos;
        for &b in &self.src.as_bytes()[pos.0..] {
            pos = pos.increment();
            match b {
                b'/' if prev == b'*' => {
                    if nesting_level == 0 {
                        break;
                    } else {
                        nesting_level -= 1;
                    }
                }
                b'*' if prev == b'/' => {
                    nesting_level += 1;
                }
                _ => {
                    prev = b;
                }
            }
        }
        self.pos = pos;
        self.code()
    }

    fn string(&mut self, str_type: StrStyle) -> ByteRange {
        let src_bytes = self.src.as_bytes();
        let mut pos = self.pos;
        match str_type {
            StrStyle::Raw(level) => {
                // raw string (e.g. br#"\"#)
                #[derive(Debug)]
                enum SharpState {
                    Sharp {
                        // number of preceding #s
                        num_sharps: usize,
                        // Position of last "
                        quote_pos: BytePos,
                    },
                    None, // No preceding "##...
                }
                let mut cur_state = SharpState::None;
                let mut end_was_found = false;
                // detect corresponding end(if start is r##", "##) greedily
                for (i, &b) in src_bytes[self.pos.0..].iter().enumerate() {
                    match cur_state {
                        SharpState::Sharp {
                            num_sharps,
                            quote_pos,
                        } => {
                            cur_state = match b {
                                b'#' => SharpState::Sharp {
                                    num_sharps: num_sharps + 1,
                                    quote_pos,
                                },
                                b'"' => SharpState::Sharp {
                                    num_sharps: 0,
                                    quote_pos: BytePos(i),
                                },
                                _ => SharpState::None,
                            }
                        }
                        SharpState::None => {
                            if b == b'"' {
                                cur_state = SharpState::Sharp {
                                    num_sharps: 0,
                                    quote_pos: BytePos(i),
                                };
                            }
                        }
                    }
                    if let SharpState::Sharp {
                        num_sharps,
                        quote_pos,
                    } = cur_state
                    {
                        if num_sharps == level {
                            end_was_found = true;
                            pos += quote_pos.increment();
                            break;
                        }
                    }
                }
                if !end_was_found {
                    pos = src_bytes.len().into();
                }
            }
            StrStyle::Cooked => {
                let mut is_not_escaped = true;
                for &b in &src_bytes[pos.0..] {
                    pos = pos.increment();
                    match b {
                        b'"' if is_not_escaped => {
                            break;
                        } // "
                        b'\\' => {
                            is_not_escaped = !is_not_escaped;
                        }
                        _ => {
                            is_not_escaped = true;
                        }
                    }
                }
            }
        };
        self.pos = pos;
        self.code()
    }

    fn char(&mut self) -> ByteRange {
        let mut is_not_escaped = true;
        let mut pos = self.pos;
        for &b in &self.src.as_bytes()[pos.0..] {
            pos = pos.increment();
            match b {
                b'\'' if is_not_escaped => {
                    break;
                }
                b'\\' => {
                    is_not_escaped = !is_not_escaped;
                }
                _ => {
                    is_not_escaped = true;
                }
            }
        }
        self.pos = pos;
        self.code()
    }

    fn detect_str_type(&self, pos: BytePos) -> StrStyle {
        let src_bytes = self.src.as_bytes();
        let mut sharp = 0;
        if pos == BytePos::ZERO {
            return StrStyle::Cooked;
        }
        // now pos is at one byte after ", so we have to start at pos - 2
        for &b in src_bytes[..pos.decrement().0].iter().rev() {
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
    CodeIndicesIter {
        src,
        state: State::Code,
        pos: BytePos::ZERO,
    }
}

/// Reverse Iterator for reading the source bytes skipping comments.
/// This is written for get_start_of_pattern and maybe not so robust.
pub struct CommentSkipIterRev<'a> {
    src: &'a [u8],
    pos: BytePos,
}

/// This produce CommentSkipIterRev for range [0, start)
pub fn comment_skip_iter_rev(s: &str, start: BytePos) -> CommentSkipIterRev {
    let start = if start.0 > s.len() {
        BytePos::ZERO
    } else {
        start
    };
    CommentSkipIterRev {
        src: s.as_bytes(),
        pos: start,
    }
}

impl<'a> Iterator for CommentSkipIterRev<'a> {
    type Item = (u8, BytePos);
    fn next(&mut self) -> Option<(u8, BytePos)> {
        let pos = self.pos.0;
        if pos == 0 {
            return None;
        }
        let cur_byte = self.src[pos - 1];
        match cur_byte {
            b'\n' => {
                let pos = self.pos;
                self.pos = self.skip_line_comment();
                Some((cur_byte, pos.decrement()))
            }
            b'/' => {
                if pos > 1 && self.src[pos - 2] == b'*' {
                    self.pos = self.skip_block_comment();
                    Some((self.cur_byte()?, self.pos.decrement()))
                } else {
                    Some(self.code())
                }
            }
            _ => Some(self.code()),
        }
    }
}

impl<'a> CommentSkipIterRev<'a> {
    fn cur_byte(&self) -> Option<u8> {
        let pos = self.pos.0;
        if pos == 0 {
            None
        } else {
            Some(self.src[pos - 1])
        }
    }

    fn code(&mut self) -> (u8, BytePos) {
        self.pos = self.pos.decrement();
        let b = self.src[self.pos.0];
        (b, self.pos)
    }

    fn get_byte(&self, p: BytePos) -> Option<u8> {
        if p == BytePos::ZERO {
            None
        } else {
            Some(self.src[p.0 - 1])
        }
    }

    // return where 'pos' shuld be after skipping block comments
    fn skip_block_comment(&self) -> BytePos {
        let mut nest_level = 0;
        let mut prev = b' ';
        for i in (0..self.pos.0 - 2).rev() {
            let b = self.src[i];
            match b {
                b'/' if prev == b'*' => {
                    if nest_level == 0 {
                        return i.into();
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
        BytePos::ZERO
    }

    // return where 'pos' shuld be after skipping line comments
    fn skip_line_comment(&self) -> BytePos {
        let skip_cr = |p: BytePos| -> BytePos {
            if let Some(b) = self.get_byte(p) {
                if b == b'\r' {
                    return p.decrement();
                }
            }
            p
        };
        let mut pos = self.pos;
        let mut skipped_whole_line = true;
        while skipped_whole_line && pos > BytePos::ZERO {
            // now pos >= 1 && self.src[pos - 1] == '\n'
            let mut before = b'\n';
            let mut last_comment = None;
            let mut i = pos.0 - 1;
            while i > 0 {
                i -= 1;
                let cur = self.src[i];
                match cur {
                    b'/' if before == b'/' => last_comment = Some(i),
                    b'\n' => break,
                    _ => {}
                }
                before = cur;
            }
            if let Some(start) = last_comment {
                skipped_whole_line = i > 0 && start == i + 1;
                pos = BytePos(start);
            } else {
                return skip_cr(pos.decrement());
            }
        }
        pos
    }
}

#[cfg(test)]
mod code_indices_iter_test {
    use super::*;
    use testutils::{rejustify, slice};

    #[test]
    fn removes_a_comment() {
        let src = &rejustify(
            "
    this is some code // this is a comment
    some more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code ", slice(src, it.next().unwrap()));
        assert_eq!("some more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_consecutive_comments() {
        let src = &rejustify(
            "
    this is some code // this is a comment
    // this is more comment
    // another comment
    some more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code ", slice(src, it.next().unwrap()));
        assert_eq!("some more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_string_contents() {
        let src = &rejustify(
            "
    this is some code \"this is a string\" more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
        assert_eq!("\" more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_char_contents() {
        let src = &rejustify(
            "
    this is some code \'\"\' more code \'\\x00\' and \'\\\'\' that\'s it
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code \'", slice(src, it.next().unwrap()));
        assert_eq!("\' more code \'", slice(src, it.next().unwrap()));
        assert_eq!("\' and \'", slice(src, it.next().unwrap()));
        assert_eq!("\' that\'s it", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_string_contents_with_a_comment_in_it() {
        let src = &rejustify(
            "
    this is some code \"string with a // fake comment \" more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
        assert_eq!("\" more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_a_comment_with_a_dbl_quote_in_it() {
        let src = &rejustify(
            "
    this is some code // comment with \" double quote
    some more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code ", slice(src, it.next().unwrap()));
        assert_eq!("some more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_multiline_comment() {
        let src = &rejustify(
            "
    this is some code /* this is a
    \"multiline\" comment */some more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code ", slice(src, it.next().unwrap()));
        assert_eq!("some more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn handles_nesting_of_block_comments() {
        let src = &rejustify(
            "
    this is some code /* nested /* block */ comment */ some more code
    ",
        );
        let mut it = code_chunks(src);
        assert_eq!("this is some code ", slice(src, it.next().unwrap()));
        assert_eq!(" some more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_string_with_escaped_dblquote_in_it() {
        let src = &rejustify(
            "
    this is some code \"string with a \\\" escaped dblquote fake comment \" more code
    ",
        );

        let mut it = code_chunks(src);
        assert_eq!("this is some code \"", slice(src, it.next().unwrap()));
        assert_eq!("\" more code", slice(src, it.next().unwrap()));
    }

    #[test]
    fn removes_raw_string_with_dangling_escape_in_it() {
        let src = &rejustify(
            "
    this is some code br\" escaped dblquote raw string \\\" more code
    ",
        );

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
        let src = &rejustify(
            "
        before(\"\\\\\'\\\\\\\"\\\\\\\\\");
        more_code(\" skip me \")
    ",
        );

        for range in code_chunks(src) {
            let range = || range.to_range();
            println!("BLOB |{}|", &src[range()]);
            if src[range()].contains("skip me") {
                panic!("{}", &src[range()]);
            }
        }
    }

    #[test]
    fn removes_nested_rawstr() {
        let src = &rejustify(
            r####"
    this is some code br###""" r##""##"### more code
    "####,
        );

        let mut it = code_chunks(src);
        assert_eq!("this is some code br###\"", slice(src, it.next().unwrap()));
        assert_eq!("\"### more code", slice(src, it.next().unwrap()));
    }

}

#[cfg(test)]
mod comment_skip_iter_rev_test {
    use super::*;
    use testutils::rejustify;
    #[test]
    fn removes_consecutive_comments_with_comment_skip_iter_rev() {
        let src = &rejustify(
            "
    this is some code // this is a comment
    // this is more comment
    // another comment
    some more code
    ",
        );
        let result: String = comment_skip_iter_rev(&src, BytePos(src.len()))
            .map(|c| c.0 as char)
            .collect();
        assert_eq!(&result, "edoc erom emos\n edoc emos si siht");
    }
    #[test]
    fn removes_nested_block_comments_with_comment_skip_iter_rev() {
        let src = &rejustify(
            "
    this is some code // this is a comment
    /* /* nested comment */ */
    some more code
    ",
        );
        let result: String = comment_skip_iter_rev(&src, BytePos(src.len()))
            .map(|c| c.0 as char)
            .collect();
        assert_eq!(&result, "edoc erom emos\n\n\n edoc emos si siht");
    }
    #[test]
    fn removes_multiline_comment_with_comment_skip_iter_rev() {
        let src = &rejustify(
            "
    this is some code /* this is a
    \"multiline\" comment */some more code
    ",
        );
        let result: String = comment_skip_iter_rev(&src, BytePos(src.len()))
            .map(|c| c.0 as char)
            .collect();
        assert_eq!(&result, "edoc erom emos  edoc emos si siht");
    }
}
