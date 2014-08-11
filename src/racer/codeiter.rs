// #![feature(phase)]
// #[phase(plugin, link)] extern crate log;

use racer::codecleaner::{code_chunks,CodeIndicesIter};
// use codecleaner::{code_chunks,CodeIndicesIter};
// mod codecleaner;

#[cfg(test)] use racer::testutils::{rejustify, slice};

pub struct StmtIndicesIter<'a> {
    src: &'a str,
    it: CodeIndicesIter<'a>,
    pos: uint,
    start: uint,
    end: uint,
    bracelevel: int,
    parenlevel: int,
    is_macro: bool,
    enddelim: u8
}

impl<'a> Iterator<(uint, uint)> for StmtIndicesIter<'a> {
    #[inline]
    fn next(&mut self) -> Option<(uint, uint)> {
        let semicolon: u8 = ";".as_bytes()[0];
        let hash: u8 = "#".as_bytes()[0];
        let openbrace: u8 = "{".as_bytes()[0];
        let closebrace: u8 = "}".as_bytes()[0];
        let openparen: u8 = "(".as_bytes()[0];
        let closeparen: u8 = ")".as_bytes()[0];
        let closesqbrace: u8 = "]".as_bytes()[0];
        let bang: u8 = "!".as_bytes()[0];
        let whitespace = " \t\n".as_bytes();

        let __u: u8 = "u".as_bytes()[0];
        let __s: u8 = "s".as_bytes()[0];
        let __e: u8 = "e".as_bytes()[0];


        let src_bytes = self.src.as_bytes();

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
                    if !whitespace.contains(&src_bytes[self.pos]) {
                        break;
                    } else {
                        self.pos += 1;
                    }
                }
                self.start = self.pos;
            }

            
            // if the statement starts with 'macro_rules!' then we're in a macro
            // We need to know this because a closeparen can terminate a macro
            if (self.end - self.pos) > 12 && 
               self.src.slice(self.pos, self.pos+12) == "macro_rules!" {
                self.is_macro = true;
            }

            // iterate through the chunk, looking for stmt end
            while self.pos < self.end {
                if src_bytes[self.pos] == openparen {

                    // macros can be terminated by closeparen if opened by one
                    if self.is_macro &&
                        self.bracelevel == 0 && 
                        self.parenlevel == 0 {
                        self.enddelim = closeparen;
                    }
                    
                    // also macro invocations can too
                    if self.pos > 0 && src_bytes[self.pos-1] == bang &&
                        self.bracelevel == 0 && 
                        self.parenlevel == 0 {
                        self.enddelim = closeparen;
                    }

                    self.parenlevel += 1;

                } else if src_bytes[self.pos] == closeparen {
                    self.parenlevel -= 1;

                } else if src_bytes[self.pos] == openbrace {
                    // if we are top level and stmt is not a 'use' then 
                    // closebrace finishes the stmt
                    if self.bracelevel == 0 && 
                        self.parenlevel == 0 &&
                       !(is_a_use_stmt(self.src, self.start, self.pos)) {
                           self.enddelim = closebrace;
                    }
                    self.bracelevel += 1;

                } else if src_bytes[self.pos] == closebrace {
                    self.bracelevel -= 1;
                    // have we reached the end of the scope?
                    if self.bracelevel < 0 {
                        return None;
                    }
                }

                // attribute   #[foo = bar]
                if self.bracelevel == 0 && self.start == self.pos && 
                    src_bytes[self.pos] == hash {
                    self.enddelim = closesqbrace;
                }

                if self.bracelevel == 0 && self.parenlevel == 0 && 
                   src_bytes[self.pos] == self.enddelim {
                    let start = self.start;
                    self.start = self.pos+1;
                    self.pos = self.pos+1;
                    self.enddelim = semicolon;
                    self.is_macro = false;
                    return Some((start, self.pos));
                }

                self.pos += 1;
            }
        }
    }
}

fn is_a_use_stmt(src: &str, start: uint, pos: uint) -> bool {
    let src_bytes = src.as_bytes();
    let whitespace = " {\t\n".as_bytes();
    (pos > 3 && src_bytes.slice(start, start+3) == "use".as_bytes() && 
     whitespace.contains(&src_bytes[start+3])) || 
        (pos > 7 && src_bytes.slice(start, start+7) == "pub use".as_bytes() &&
                      whitespace.contains(&src_bytes[start+7]))
}

pub fn iter_stmts<'a>(src: &'a str) -> StmtIndicesIter<'a> {
    let semicolon: u8 = ";".as_bytes()[0];
    StmtIndicesIter{src: src, it: code_chunks(src), 
                    pos: 0, start: 0, end: 0, bracelevel: 0, 
                    parenlevel: 0, enddelim: semicolon, is_macro: false}
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
fn iterates_use_stmt_without_the_prefix() {
    let src = rejustify("
    pub use {Foo,
              Bar}; // this is also legit apparently
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("pub use {Foo,
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

#[test]
fn iterates_lambda_arg() {
    let src = rejustify("
    myfn(|n|{});
    ");
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("myfn(|n|{});", slice(src.as_slice(), it.next().unwrap()));
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
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("mod foo;", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("macro_rules! otry(
        ($e:expr) => (match $e { Some(e) => e, None => return })
    )", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("mod bar;", slice(src.as_slice(), it.next().unwrap()));
}

#[test]
fn iterates_macro_invocation() {
    let src = "
    mod foo;
    local_data_key!(local_stdout: Box<Writer + Send>)  // no ';'
    mod bar;
    ";
    let mut it = iter_stmts(src.as_slice());
    assert_eq!("mod foo;", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("local_data_key!(local_stdout: Box<Writer + Send>)", slice(src.as_slice(), it.next().unwrap()));
    assert_eq!("mod bar;", slice(src.as_slice(), it.next().unwrap()));
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
    let src = "
    while self.pos < 3 {
       let a = 35;
       return a + 35;  // should iterate this
    }
    {
       b = foo;       // but not this
    }
    ";

    let scope = src.as_slice().slice_from(25);
    //println!("blah{}",scope);
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

//     let filetxt = BufferedReader::new(File::open(&Path::new("/usr/local/src/rust/src/libstd/io/stdio.rs"))).read_to_end().unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();

//     for (start,end) in iter_stmts(src) {
//         println!("BLOB |{}|",src.slice(start,end));
//     }

// }
