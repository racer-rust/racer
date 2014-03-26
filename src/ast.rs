#[feature(managed_boxes)];   // need this to use libsyntax
extern crate syntax;
use syntax::ast;
use syntax::parse::{new_parse_sess};
use syntax::parse::{ParseSess};
use syntax::parse::{new_parser_from_source_str};
use syntax::parse::parser::Parser;
use syntax::parse::token;
use syntax::visit;
use syntax::codemap::Span;
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

struct MyVisitor;
impl visit::Visitor<()> for MyVisitor {

    fn visit_mod(&mut self, m: &ast::Mod, _s: Span, _n:ast::NodeId, e: ()) { 
        println!("PHIL - visited mod! {:?} {:?} {:?}",m,_s,_n);
        visit::walk_mod(self, m, e) 
    }

    fn visit_view_item(&mut self, i: &ast::ViewItem, e: ()) { 
        println!("PHIL - visited view item! {:?}",i);
        match i.node {
            ast::ViewItemUse(ref paths) => {
                println!("PHIL use paths {:?}",paths);
                // (from rustdoc: rustc no longer supports "use foo, bar;")
                assert_eq!(paths.len(), 1);
                match paths.get(0).node {
                    ast::ViewPathSimple(_, _, id) => {
                        println!("view path simple {:?}",id);
                    },
                    ast::ViewPathList(ref p, ref paths, ref b) => {
                        for path in paths.iter() {
                            println!("view path list item {}",token::get_ident(path.node.name));
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

    fn visit_expr(&mut self, a:&ast::Expr, e: ()) {
        println!("PHIL - visited expr! {:?}",a);
        visit::walk_expr(self, a, e)
    }

    fn visit_decl(&mut self, d: &ast::Decl, e: ()) { 
        //walk_decl(self, d, e) 
        println!("PHIL - visited decl! {:?}",d);
        visit::walk_decl(self, d, e)
    }

    fn visit_path(&mut self, path: &ast::Path, _id: ast::NodeId, e: ()) {
        println!("PHIL - visited path! {:?}", path);
        println!("PHIL - path segments! {:?}", path.segments);
        for seg in path.segments.iter() {
            println!("PHIL - path segment {}", token::get_ident(seg.identifier));
        }

        visit::walk_path(self, path, e)
    }

}

// fn main() {
//     // let expr = string_to_expr(~"a = 3");
//     // println!("PHIL 1: {:?}", expr);
//     // println!("PHIL 2: {:?}", string_to_expr(~"(a, b) = (3, 4)"));
//     // //string_to_item(~"fn a (b : int) { b; }")

//     // println!("PHIL 3: {:?}", item);

//     //let stmt = string_to_stmt(~"let foo = 3;aoueaoeu baoeb some other stuff");
//     let stmt = string_to_stmt(~"let foo = blah(); baoeib");
//     //println!("PHIL 4: {:?}", stmt);

//     let mut v = MyVisitor;
//     //visit::walk_expr(&mut v,expr,());
//     visit::walk_stmt(&mut v, stmt, ())
    
// }


struct MyViewItemVisitor {
    results : Vec<Vec<~str>>
}

impl visit::Visitor<()> for MyViewItemVisitor {
    fn visit_view_item(&mut self, i: &ast::ViewItem, e: ()) { 
        println!("PHIL - visited view item! {:?}",i);
        match i.node {
            ast::ViewItemUse(ref paths) => {
                println!("PHIL use paths {:?}",paths);
                // (from rustdoc: rustc no longer supports "use foo, bar;")
                assert_eq!(paths.len(), 1);
                match paths.get(0).node {
                    ast::ViewPathSimple(_, ref path, _) => {
                        //println!("view path simple {:?}",id);
                        let mut v = Vec::new();
                        for seg in path.segments.iter() {
                            v.push(token::get_ident(seg.identifier).get().to_owned())
                        }
                        self.results.push(v);
                    },
                    ast::ViewPathList(ref pth, ref paths, ref b) => {
                        let mut v = Vec::new();

                        for seg in pth.segments.iter() {
                            println!("PHIL - path segment {:?}", token::get_ident(seg.identifier).get());
                            v.push(token::get_ident(seg.identifier).get().to_owned())
                        }

                        

                        //println!("view path item {}",token::get_ident(pth.name));
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

#[test]
fn blah() {
    let s = ~"use racer::{getline,search_crate,Match};";
    let r = parse_view_item(s);
    println!("result = {}", r)
    //let cr = string_to_crate(~"use racer::{getline,search_crate,Match};");
    
    // let mut v = MyViewItemVisitor{results: Vec::new()};
    // visit::walk_crate(&mut v, &cr, ());
    // println!("PHIL {}",v.results);

    //fail!("hello");
}
