#[feature(managed_boxes)];   // need this to use libsyntax
extern crate syntax;
use syntax::ast;
use syntax::parse::{new_parse_sess};
use syntax::parse::{ParseSess};
use syntax::parse::{new_parser_from_source_str};
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::visit;
use syntax::codemap;
use std::task;

// This code ripped from libsyntax::parser_testing
pub fn string_to_parser<'a>(ps: &'a ParseSess, source_str: ~str) -> Parser<'a> {
    new_parser_from_source_str(ps, Vec::new(), ~"bogofile", source_str)
}

fn with_error_checking_parse<T>(s: ~str, f: |&mut Parser| -> T) -> T {
    let ps = new_parse_sess();
    let mut p = string_to_parser(&ps, s);
    let x = f(&mut p);
    p.abort_if_errors();
    x
}

// parse a string, return an expr
pub fn string_to_expr (source_str : ~str) -> @ast::Expr {
    with_error_checking_parse(source_str, |p| {
        p.parse_expr()
    })
}

// parse a string, return an item
pub fn string_to_item (source_str : ~str) -> Option<@ast::Item> {
    with_error_checking_parse(source_str, |p| {
        p.parse_item(Vec::new())
    })
}

// parse a string, return a stmt
pub fn string_to_stmt(source_str : ~str) -> @ast::Stmt {
    with_error_checking_parse(source_str, |p| {
        p.parse_stmt(Vec::new())
    })
}

// parse a string, return a crate.
pub fn string_to_crate (source_str : ~str) -> ast::Crate {
    with_error_checking_parse(source_str, |p| {
        p.parse_crate_mod()
    })
}


struct MyViewItemVisitor {
    results : Vec<Vec<~str>>
}

impl visit::Visitor<()> for MyViewItemVisitor {
    fn visit_view_item(&mut self, i: &ast::ViewItem, e: ()) { 
        println!("PHIL - visited view item! {:?}",i);
        match i.node {
            ast::ViewItemUse(ref paths) => {
                assert_eq!(paths.len(), 1);
                match paths.get(0).node {
                    ast::ViewPathSimple(_, ref path, _) => {
                        let mut v = Vec::new();
                        for seg in path.segments.iter() {
                            v.push(token::get_ident(seg.identifier).get().to_owned())
                        }
                        self.results.push(v);
                    },
                    ast::ViewPathList(ref pth, ref paths, _) => {
                        let mut v = Vec::new();

                        for seg in pth.segments.iter() {
                            v.push(token::get_ident(seg.identifier).get().to_owned())
                        }

                        for path in paths.iter() {
                            let mut vv = v.clone();
                            println!("view path list item {}",token::get_ident(path.node.name));
                            vv.push(token::get_ident(path.node.name).get().to_owned());
                            self.results.push(vv);
                        }
                    }
                    ast::ViewPathGlob(_, id) => {
                        println!("PHIL got glob {:?}",id);
                    }
                }
            },
            ast::ViewItemExternCrate(..) => {
            }
        }

        visit::walk_view_item(self, i, e) 
    }
}




pub fn parse_view_item(s: ~str) -> Vec<Vec<~str>> {
    // parser can fail!() so isolate it in another task
    let result = task::try(proc() { 
        return _parse_view_items(s);
    });
    match result {
        Ok(s) => {return s;},
        Err(_) => {
            return Vec::new();
        }
    }
}

fn _parse_view_items(s: ~str)-> Vec<Vec<~str>> {
    let cr = string_to_crate(s);
    let mut v = MyViewItemVisitor{results: Vec::new()};
    visit::walk_crate(&mut v, &cr, ());
    return v.results;
}

struct MyLetVisitor { 
    result: Option<LetResult>
}

pub struct LetResult { 
    name: ~str,
    point: uint
}

impl visit::Visitor<()> for MyLetVisitor {
    fn visit_decl(&mut self, decl: &ast::Decl, e: ()) { 

        
        match decl.node {
            ast::DeclLocal(local) => {
                match local.pat.node {
                    ast::PatWild => {},
                    ast::PatWildMulti => {},
                    ast::PatIdent(_ , ref path, _) => {
                        let mut v = Vec::new();
                        let codemap::BytePos(point) = path.span.lo;

                        for seg in path.segments.iter() {
                             v.push(token::get_ident(seg.identifier).get().to_owned())
                        }
                        self.result = Some(LetResult{name: v.get(0).to_owned(), 
                                                point: point.to_uint().unwrap()});
                    },
                    ast::PatEnum(_,_) => {}, 
                    ast::PatStruct(_,_,_) => {},
                    ast::PatTup(_) => {},
                    ast::PatUniq(_) => {},
                    ast::PatRegion(_) => {},
                    ast::PatLit(_) => {},
                    ast::PatRange(_,_) => {},
                    ast::PatVec(_,_,_ ) => {}
                    
                }
                
            }
            ast::DeclItem(_) => {}
        }

        visit::walk_decl(self, decl, e);
    }
}

pub fn parse_let(s: ~str) -> Option<LetResult> {
    // parser can fail!() so isolate it in another task
    let result = task::try(proc() { 
        return _parse_let(s);
    });
    match result {
        Ok(s) => {return s;},
        Err(_) => {
            return None;
        }
    }
}

fn _parse_let(s: ~str)-> Option<LetResult> {
    let cr = string_to_stmt(s);
    let mut v = MyLetVisitor{ result: None};
    visit::walk_stmt(&mut v, cr, ());
    return v.result;
}


#[test]
fn blah() {
    let cr = string_to_stmt(~"let mut myvar = 3;");
    let mut v = MyLetVisitor{ result: None};
    visit::walk_stmt(&mut v, cr, ());
    //let r = parse_view_item(s);
    println!("result = {:?}", v.result);
    //let cr = string_to_crate(~"use racer::{getline,search_crate,Match};");
    
    // let mut v = MyViewItemVisitor{results: Vec::new()};
    // visit::walk_crate(&mut v, &cr, ());
    // println!("PHIL {}",v.results);

    fail!("hello");
}
