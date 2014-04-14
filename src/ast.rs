#![feature(managed_boxes)]   // need this to use libsyntax
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

struct MyLetVisitor { 
    result: Option<LetResult>
}

pub struct LetResult { 
    pub name: ~str,
    pub point: uint,
    pub init: Vec<~str>
}


fn path_to_vec(pth: &ast::Path) -> Vec<~str> {
    let mut v = Vec::new();
    for seg in pth.segments.iter() {
        v.push(token::get_ident(seg.identifier).get().to_owned())
    }
    return v;
}

impl MyLetVisitor {
    fn visit_let_initializer(&mut self, name: ~str, point: uint, init: Option<@ast::Expr> ) {
        init.map(|init| {
            match init.node {
                ast::ExprStruct(ref path, _, _) => {
                    self.result = Some(LetResult{name: name.clone(),
                                                 point: point,
                                                 init: path_to_vec(path)
                                                 });
                }
                _ => {
                    self.result = Some(LetResult{name: name.clone(),
                                                 point: point,
                                                 init: vec!()
                                                 });                    
                }
            }
        });
    }
}

impl visit::Visitor<()> for MyLetVisitor {

    fn visit_decl(&mut self, decl: &ast::Decl, e: ()) { 
        match decl.node {
            ast::DeclLocal(local) => {
                match local.pat.node {
                    ast::PatWild => {},
                    ast::PatWildMulti => {},
                    ast::PatIdent(_ , ref path, _) => {
                        let codemap::BytePos(point) = path.span.lo;
                        let pathv = path_to_vec(path);
                        self.visit_let_initializer(pathv.get(0).to_owned(),
                                                   point.to_uint().unwrap(),
                                                   local.init);
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

struct StructVisitor {
    fields: Vec<~str>
}

impl visit::Visitor<()> for StructVisitor {
    fn visit_struct_def(&mut self, s: &ast::StructDef, i: ast::Ident, g: &ast::Generics, n: ast::NodeId, e: ()) {
        visit::walk_struct_def(self, s, i, g, n, e)
    }
    fn visit_struct_field(&mut self, field: &ast::StructField, e: ()) { 
        match field.node.kind {
            ast::NamedField(name, _) => {
                //visitor.visit_ident(struct_field.span, name, env.clone())
                let n = token::get_ident(name).get().to_owned();
                self.fields.push(n);
                //println!("PHIL field name {}",n);
                    
            }
            _ => {}
        }

        //visit::walk_struct_field(self, s, e)
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
    let stmt = string_to_stmt(s);
    let mut v = MyLetVisitor{ result: None};
    visit::walk_stmt(&mut v, stmt, ());
    return v.result;
}

pub fn parse_struct_fields(s: ~str) -> Vec<~str> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = StructVisitor{ fields: Vec::new() };
        visit::walk_stmt(&mut v, stmt, ());
        return v.fields;
    }).ok().unwrap_or(Vec::new());
}


#[test]
fn blah() {
    let src = ~"struct Point {
        first: f64,
        second: f64
    }";

    let cr = string_to_stmt(src);
    let mut v = StructVisitor { fields: Vec::new()};
    visit::walk_stmt(&mut v, cr, ());
    // //let r = parse_view_item(s);
    println!("result = {}", v.fields);
    // //let cr = string_to_crate(~"use racer::{getline,search_crate,Match};");
    
    // // let mut v = MyViewItemVisitor{results: Vec::new()};
    // // visit::walk_crate(&mut v, &cr, ());
    // // println!("PHIL {}",v.results);

    fail!("hello");
}
