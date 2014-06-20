// #![feature(phase)]
// #[phase(plugin, link)] extern crate log;

use racer::codecleaner::{code_chunks,CodeIndicesIter}; 
//use codecleaner::{code_chunks,CodeIndicesIter};
//mod codecleaner;

#[cfg(test)] use racer::testutils::{rejustify, slice};

pub struct StmtIndicesIter<'a> {
    src: &'a str,
    it: CodeIndicesIter<'a>,
    pos: uint,
    start: uint,
    end: uint,
    level: int,
    enddelim: u8
}

impl<'a> Iterator<(uint, uint)> for StmtIndicesIter<'a> {
    #[inline]
    fn next(&mut self) -> Option<(uint, uint)> {
        let semicolon: u8 = ";"[0];
        let hash: u8 = "#"[0];
        let colon: u8 = ":"[0];
        let openbrace: u8 = "{"[0];
        let closebrace: u8 = "}"[0];
        let closesqbrace: u8 = "]"[0];
        let whitespace = " \t\n".as_bytes();

        loop {

            if self.end <= self.pos {
                // get the next chunk of code
                match self.it.next() {
                    Some((start, end)) => {
                        self.end = end;
                        if self.start == self.pos {
                            self.start = start;
                        }
                        self.pos = start;
                    }
                    None => {
                        return None
                    }
                }
            }

            // if this is a new stmt block, skip the whitespace
            if self.pos == self.start {
                while self.pos < self.end {
                    if !whitespace.contains(&self.src[self.pos]) {
                        break;
                    } else {
                        self.pos += 1;
                    }
                }
                self.start = self.pos;
            }


            // iterate through the chunk, looking for stmt end
            while self.pos < self.end {

                if self.src[self.pos] == openbrace {
                    // if is not a ::{Foo,Bar}; then closebrace finishes the stmt
                    if self.level == 0 &&  
                       !(self.pos > 1 && 
                         self.src[self.pos-1] == colon && 
                         self.src[self.pos-2] == colon) {
                           self.enddelim = closebrace;
                    }
                    self.level += 1;
                } else if self.src[self.pos] == closebrace {
                    self.level -= 1;
                    // have we reached the end of the scope?
                    if self.level < 0 {
                        self.fuse();
                        return None;
                    }
                }

                // attribute   #[foo = bar]
                if self.level == 0 && self.start == self.pos && 
                    self.src[self.pos] == hash {
                    self.enddelim = closesqbrace;
                }

                if self.level == 0 && self.src[self.pos] == self.enddelim {
                    let start = self.start;
                    self.start = self.pos+1;
                    self.pos = self.pos+1;
                    self.enddelim = semicolon;
                    return Some((start, self.pos));
                }

                self.pos += 1;
            }
        }
    }
}

pub fn iter_stmts<'a>(src: &'a str) -> StmtIndicesIter<'a> {
    let semicolon: u8 = ";"[0];
    StmtIndicesIter{src: src, it: code_chunks(src), 
                    pos: 0, start: 0, end: 0, level: 0, enddelim: semicolon}
}


#[test]
fn iterates_single_use_stmts() {
    let src = rejustify("
    use std::Foo; // a comment
    use std::Bar;
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("use std::Foo;", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("use std::Bar;", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn iterates_use_stmt_over_two_lines() {
    let src = rejustify("
    use std::{Foo,
              Bar}; // a comment
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("use std::{Foo,
          Bar};", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn iterates_while_stmt() {
    let src = rejustify("
    while self.pos < 3 { }
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("while self.pos < 3 { }", slice(src.as_slice(), it.next().unwrap()));
}

// #[test]
// fn iterates_if_else_stmt() {
//     let src = rejustify("
//     if self.pos < 3 { } else { }
//     ");
//     let mut it = iter_stmts(src.as_slice());
//     assert_eq!("if self.pos < 3 { } else { }", slice(src.as_slice(), it.next().unwrap()));
// }

#[test]
fn iterates_inner_scope() {
    let src = rejustify("
    while self.pos < 3 { 
       let a = 35;
       return a + 35;  // should iterate this
    }
    {
       b = foo;       // but not this
    }
    ");

    let scope = src.as_slice().slice_from(25);
    debug!("blah{}",scope);
    let mut it = iter_stmts(scope);
    
    assert_eq!("let a = 35;", slice(scope, it.next().unwrap()));
    assert_eq!("return a + 35;", slice(scope, it.next().unwrap()));
    assert_eq!(None, it.next());
}


#[test]
fn iterates_module_attribute() {
    let src = rejustify("
    #![license = \"BSD\"]
    #[test]
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("#![license = \"BSD\"]", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("#[test]", slice(src.as_slice(), it.next().unwrap()));
}

// fn main() {
//     use std::io::BufferedReader;
//     use std::io::File;
//     use std::str;

//     let filetxt = BufferedReader::new(File::open(&Path::new("/usr/local/src/rust/src/libstd/prelude.rs"))).read_to_end().unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();

//     for (start,end) in iter_stmts(src) {
//         println!("BLOB |{}|",src.slice(start,end));
//     }
    
// }
