use std::iter::{Fuse, Iterator};

use core::{BytePos, ByteRange};

pub struct StmtIndicesIter<'a,I>
    where I: Iterator<Item = ByteRange>
{
    src: &'a str,
    it: I,
    pos: BytePos,
    end: BytePos
}

impl<'a,I> Iterator for StmtIndicesIter<'a,I>
    where I: Iterator<Item = ByteRange>
{
    type Item = ByteRange;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let src_bytes = self.src.as_bytes();
        let mut enddelim = b';';
        let mut bracelevel = 0isize;
        let mut parenlevel = 0isize;
        let mut bracketlevel = 0isize;
        let mut start = self.pos;
        let mut pos = self.pos;

        // loop on all code_chunks until we find a relevant open/close pattern
        loop {
            // do we need the next chunk?
            if self.end == pos {
                // get the next chunk of code
                match self.it.next() {
                    Some(ch_range) => {
                        self.end = ch_range.end;
                        if start == pos {
                            start = ch_range.start;
                        }
                        pos = ch_range.start;
                    }
                    None => {
                        // no more chunks. finished
                        self.pos = pos;
                        if start < self.end {
                            return Some(ByteRange::new(start, self.end));
                        } else {
                            return None;
                        }
                    }
                }
            }

            if start == pos {
                // if this is a new stmt block, skip the whitespace
                for &b in &src_bytes[pos.0..self.end.0] {
                    match b {
                        b' ' | b'\r' | b'\n' | b'\t' => {
                            pos += BytePos(1);
                        },
                        _ => { break; }
                    }
                }
                start = pos;
                // test attribute   #[foo = bar]
                if pos < self.end && src_bytes[pos.0] == b'#' {
                    enddelim = b']'
                };
            }

            // iterate through the chunk, looking for stmt end
            for &b in &src_bytes[pos.0..self.end.0] {
                pos += BytePos(1);
                match b {
                    b'(' => { parenlevel += 1; },
                    b')' => { parenlevel -= 1; },
                    b'[' => { bracketlevel += 1; },
                    b']' => { bracketlevel -= 1; },
                    b'{' => {
                        // if we are top level and stmt is not a 'use' or 'let' then
                        // closebrace finishes the stmt
                        if bracelevel == 0 && parenlevel == 0
                            && !(is_a_use_stmt(src_bytes, start, pos) || is_a_let_stmt(src_bytes, start, pos)) {
                            enddelim = b'}';
                        }
                        bracelevel += 1;
                    },
                    b'}' => {
                        // have we reached the end of the scope?
                        if bracelevel == 0 {
                            self.pos = pos;
                            return None;
                        }
                        bracelevel -= 1;
                    },
                    b'!' => {
                        // macro if followed by at least one space or (
                        // FIXME: test with boolean 'not' expression
                        if parenlevel == 0 && bracelevel == 0
                            && pos < self.end && (pos - start).0 > 1 {
                            match src_bytes[pos.0] {
                                b' ' | b'\r' | b'\n' | b'\t' | b'('  => {
                                    enddelim = b')';
                                },
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
                if parenlevel < 0 || bracelevel < 0 || bracketlevel < 0
                    || (enddelim == b && bracelevel == 0 && parenlevel == 0 && bracketlevel == 0)
                {
                    self.pos = pos;
                    return Some(ByteRange::new(start, pos));
                }
            }
        }
    }
}

fn is_a_use_stmt(src_bytes: &[u8], start: BytePos, pos: BytePos) -> bool {
    let whitespace = b" {\t\r\n";
    (pos.0 > 3 && &src_bytes[start.0..start.0 + 3] == b"use" &&
     whitespace.contains(&src_bytes[start.0 + 3])) ||
    (pos.0 > 7 && &src_bytes[start.0..(start.0 + 7)] == b"pub use" &&
     whitespace.contains(&src_bytes[start.0 + 7]))
}

fn is_a_let_stmt(src_bytes: &[u8], start: BytePos, pos: BytePos) -> bool {
    let whitespace = b" {\t\r\n";
    pos.0 > 3 && &src_bytes[start.0..start.0 + 3] == b"let"
        && whitespace.contains(&src_bytes[start.0 + 3])
}

impl<'a, I> StmtIndicesIter<'a,I>
    where I: Iterator<Item = ByteRange>
{
    pub fn from_parts(src: &str, it: I) -> Fuse<StmtIndicesIter<I>> {
        StmtIndicesIter{ src, it, pos: BytePos::ZERO, end: BytePos::ZERO }.fuse()
    }
}


#[cfg(test)]
mod test {
    use std::iter::Fuse;

    use codecleaner;
    use testutils::{rejustify, slice};

    use super::*;

    fn iter_stmts(src: &str) -> Fuse<StmtIndicesIter<codecleaner::CodeIndicesIter>> {
        let it = codecleaner::code_chunks(src);
        StmtIndicesIter{ src: src, it: it, pos: BytePos::ZERO, end: BytePos::ZERO }.fuse()
    }


    #[test]
    fn iterates_single_use_stmts() {
        let src = rejustify("
            use std::Foo; // a comment
            use std::Bar;
        ");

        let mut it = iter_stmts(src.as_ref());
        assert_eq!("use std::Foo;", slice(&src, it.next().unwrap()));
        assert_eq!("use std::Bar;", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_array_stmts() {
        let src = rejustify("
            let a: [i32; 2] = [1, 2];
            let b = [[0], [1], [2]];
            let c = ([1, 2, 3])[1];
        ");

        let mut it = iter_stmts(src.as_ref());
        assert_eq!("let a: [i32; 2] = [1, 2];", slice(&src, it.next().unwrap()));
        assert_eq!("let b = [[0], [1], [2]];", slice(&src, it.next().unwrap()));
        assert_eq!("let c = ([1, 2, 3])[1];", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_use_stmt_over_two_lines() {
        let src = rejustify("
        use std::{Foo,
                  Bar}; // a comment
        ");
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("use std::{Foo,
              Bar};", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_use_stmt_without_the_prefix() {
        let src = rejustify("
        pub use {Foo,
                 Bar}; // this is also legit apparently
        ");
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("pub use {Foo,
             Bar};", slice(&src, it.next().unwrap())
        );
    }

    #[test]
    fn iterates_while_stmt() {
        let src = rejustify("
            while self.pos < 3 { }
        ");
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("while self.pos < 3 { }", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_lambda_arg() {
        let src = rejustify("
            myfn(|n|{});
        ");
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("myfn(|n|{});", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_macro() {
        let src = "
        mod foo;
        macro_rules! otry(
            ($e:expr) => (match $e { Some(e) => e, None => return })
        )
        mod bar;
        ";
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("mod foo;", slice(&src, it.next().unwrap()));
        assert_eq!("macro_rules! otry(
            ($e:expr) => (match $e { Some(e) => e, None => return })
        )", slice(&src, it.next().unwrap()));
        assert_eq!("mod bar;", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_macro_invocation() {
        let src = "
            mod foo;
            local_data_key!(local_stdout: Box<Writer + Send>)  // no ';'
            mod bar;
        ";
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("mod foo;", slice(&src, it.next().unwrap()));
        assert_eq!("local_data_key!(local_stdout: Box<Writer + Send>)", slice(&src, it.next().unwrap()));
        assert_eq!("mod bar;", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_if_else_stmt() {
        let src = "
            if self.pos < 3 { } else { }
        ";
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("if self.pos < 3 { }", slice(&src, it.next().unwrap()));
        assert_eq!("else { }", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_inner_scope() {
        let src = &"
        while(self.pos < 3 {
            let a = 35;
            return a + 35;  // should iterate this
        }
        {
            b = foo;       // but not this
        }
        "[29..];

        let mut it = iter_stmts(src.as_ref());

        assert_eq!("let a = 35;", slice(&src, it.next().unwrap()));
        assert_eq!("return a + 35;", slice(&src, it.next().unwrap()));
        assert_eq!(None, it.next());
    }

    #[test]
    fn iterates_module_attribute() {
        let src = rejustify("
            #![license = \"BSD\"]
            #[test]
        ");
        let mut it = iter_stmts(src.as_ref());
        assert_eq!("#![license = \"BSD\"]", slice(&src, it.next().unwrap()));
        assert_eq!("#[test]", slice(&src, it.next().unwrap()));
    }

    #[test]
    fn iterates_half_open_subscope_if_is_the_last_thing() {
        let src = "
            let something = 35;
            while self.pos < 3 {
            let a = 35;
            return a + 35;  // should iterate this
        ";

        let mut it = iter_stmts(src.as_ref());
        assert_eq!("let something = 35;", slice(&src, it.next().unwrap()));
        assert_eq!("while self.pos < 3 {
            let a = 35;
            return a + 35;  // should iterate this
        ", slice(&src, it.next().unwrap()));
    }

}
