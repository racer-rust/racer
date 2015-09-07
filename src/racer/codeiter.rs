use codecleaner;
use std::iter::{Fuse, Iterator};

pub struct StmtIndicesIter<'a,I> 
    where I: Iterator<Item=(usize,usize)>
{
    src: &'a str,
    it: I,
    pos: usize,
    end: usize
}

impl<'a,I> Iterator for StmtIndicesIter<'a,I> 
    where I: Iterator<Item=(usize,usize)>
{
    type Item = (usize, usize);

    #[inline]
    fn next(&mut self) -> Option<(usize, usize)> {
        let src_bytes = self.src.as_bytes();
        let mut enddelim = b';';
        let mut bracelevel = 0isize;
        let mut parenlevel = 0isize;
        let mut start = self.pos;
        let mut pos = self.pos;

        // loop on all code_chunks until we find a relevant open/close pattern
        loop {
            // do we need the next chunk?
            if self.end == pos {
                // get the next chunk of code
                match self.it.next() {
                    Some((ch_start, ch_end)) => {
                        self.end = ch_end;
                        if start == pos { start = ch_start; }
                        pos = ch_start;
                    }
                    None => {
                        // no more chunks. finished
                        self.pos = pos;
                        return if start < self.end { Some((start, self.end)) }
                               else { None }
                    }
                }
            }

            if start == pos {
                // if this is a new stmt block, skip the whitespace
                for &b in &src_bytes[pos..self.end] {
                    match b {
                        b' ' | b'\r' | b'\n' | b'\t' => { pos += 1; },
                        _ => { break; }
                    }
                }
                start = pos;

                // test attribute   #[foo = bar]
                if pos < self.end && src_bytes[pos] == b'#' {
                    enddelim = b']'
                };
            }

            // iterate through the chunk, looking for stmt end
            for &b in &src_bytes[pos..self.end] {
                pos += 1;

                match b {
                    b'(' => { parenlevel += 1; },
                    b')' => { parenlevel -= 1; },
                    b'{' => {
                        // if we are top level and stmt is not a 'use' then
                        // closebrace finishes the stmt
                        if bracelevel == 0 && parenlevel == 0
                            && !is_a_use_stmt(src_bytes, start, pos) {
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
                            && pos < self.end && (pos-start) > 1 {
                            match src_bytes[pos] {
                                b' ' | b'\r' | b'\n' | b'\t' | b'('  => {
                                    enddelim = b')';
                                },
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }

                if enddelim == b && bracelevel == 0 && parenlevel == 0 {
                    self.pos = pos;
                    return Some((start, pos));
                }
            }
        }
    }
}

fn is_a_use_stmt(src_bytes: &[u8], start: usize, pos: usize) -> bool {
    let whitespace = b" {\t\r\n";
    (pos > 3 && &src_bytes[start..start+3] == b"use" &&
     whitespace.contains(&src_bytes[start+3])) ||
    (pos > 7 && &src_bytes[start..(start+7)] == b"pub use" &&
     whitespace.contains(&src_bytes[start+7]))
}

pub fn iter_stmts(src: &str) -> Fuse<StmtIndicesIter<codecleaner::CodeIndicesIter>> {
    let it = codecleaner::code_chunks(src);
    StmtIndicesIter{ src: src, it: it, pos: 0, end: 0 }.fuse()
}

impl<'a,I> StmtIndicesIter<'a,I> 
        where I: Iterator<Item=(usize,usize)> {

    pub fn from_parts(src: &str, it: I) -> Fuse<StmtIndicesIter<I>> {
        StmtIndicesIter{ src: src, it: it, pos: 0, end: 0 }.fuse()
    }
}


#[cfg(test)]
mod test {
    use testutils::{rejustify, slice};
    use super::*;

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
