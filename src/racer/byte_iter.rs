use BytePos;

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
