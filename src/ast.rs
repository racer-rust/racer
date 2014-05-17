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
use racer::Match;
use racer;

// This code ripped from libsyntax::util::parser_testing
pub fn string_to_parser<'a>(ps: &'a ParseSess, source_str: StrBuf) -> Parser<'a> {
    new_parser_from_source_str(ps,
                               Vec::new(),
                               "bogofile".to_strbuf(),
                               source_str)
}

// pub fn string_to_parser<'a>(ps: &'a ParseSess, source_str: ~str) -> Parser<'a> {
//     new_parser_from_source_str(ps, Vec::new(), "bogofile".to_owned(), source_str)
// }

fn with_error_checking_parse<T>(s: StrBuf, f: |&mut Parser| -> T) -> T {
    let ps = new_parse_sess();
    let mut p = string_to_parser(&ps, s);
    let x = f(&mut p);
    p.abort_if_errors();
    x
}

// parse a string, return an expr
pub fn string_to_expr (source_str : StrBuf) -> @ast::Expr {
    with_error_checking_parse(source_str, |p| {
        p.parse_expr()
    })
}

// parse a string, return an item
pub fn string_to_item (source_str : StrBuf) -> Option<@ast::Item> {
    with_error_checking_parse(source_str, |p| {
        p.parse_item(Vec::new())
    })
}

// parse a string, return a stmt
pub fn string_to_stmt(source_str : StrBuf) -> @ast::Stmt {
    with_error_checking_parse(source_str, |p| {
        p.parse_stmt(Vec::new())
    })
}

// parse a string, return a crate.
pub fn string_to_crate (source_str : StrBuf) -> ast::Crate {
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
            ast::ViewItemUse(ref path) => {
                match path.node {
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
                            //debug!("PHIL view path list item {}",token::get_ident(path.node.name));
                            vv.push(token::get_ident(path.node.name).get().to_owned());
                            self.results.push(vv);
                        }
                    }
                    ast::ViewPathGlob(_, id) => {
                        debug!("PHIL got glob {:?}",id);
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
        v.push(token::get_ident(seg.identifier).get().to_owned());
    }
    return v;
}

impl MyLetVisitor {
    fn visit_let_initializer(&mut self, name: ~str, point: uint, init: Option<@ast::Expr> ) {

        // chances are we can't parse the init yet, so the default is to leave blank
        self.result = Some(LetResult{name: name.clone(),
                                point: point,
                                init: vec!()});

        debug!("PHIL result before is {:?}",self.result);
        // attempt to parse the init
        init.map(|init| {
            debug!("PHIL init node is {:?}",init.node);
            match init.node {
                ast::ExprCall(callee_expression, _ /*ref arguments*/) => {
                    debug!("PHIL init is an exprCall {:?}",callee_expression);
                    // for argument in arguments.iter() {
                    //     visitor.visit_expr(*argument, env.clone())
                    // }
                    // visitor.visit_expr(callee_expression, env.clone())

                    match callee_expression.node {
                        ast::ExprPath(ref path) => {
                            debug!("PHIL init callee is a path {}",path_to_vec(path));
                            self.result = Some(LetResult{name: name.clone(),
                                                         point: point,
                                                         init: path_to_vec(path)
                            });
                        }
                        _ => {}
                    }
                }
                ast::ExprStruct(ref path, _, _) => {
                    self.result = Some(LetResult{name: name.clone(),
                                                 point: point,
                                                 init: path_to_vec(path)
                                                 });
                }
                _ => {
                    debug!("PHIL dont handle decl: {:?}",init.node);
                }
            }
        });

        debug!("PHIL result is {:?}",self.result);
    }
}

impl visit::Visitor<()> for MyLetVisitor {

    fn visit_decl(&mut self, decl: &ast::Decl, e: ()) { 
        debug!("PHIL decl {:?}",decl);
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
    pub fields: Vec<(StrBuf, uint)>
}

impl visit::Visitor<()> for StructVisitor {
    fn visit_struct_def(&mut self, s: &ast::StructDef, _: ast::Ident, _: &ast::Generics, _: ast::NodeId, e: ()) {
        visit::walk_struct_def(self, s, e)
    }
    fn visit_struct_field(&mut self, field: &ast::StructField, _: ()) { 
        let codemap::BytePos(point) = field.span.lo;
        match field.node.kind {
            ast::NamedField(name, _) => {
                //visitor.visit_ident(struct_field.span, name, env.clone())
                let n = StrBuf::from_str(token::get_ident(name).get());
                self.fields.push((n, point as uint));
                    
            }
            _ => {}
        }

        //visit::walk_struct_field(self, s, e)
    }
}


struct ImplVisitor {
    name_path: Vec<~str>
}

impl visit::Visitor<()> for ImplVisitor {
    fn visit_item(&mut self, item: &ast::Item, _: ()) { 
        match item.node {
            ast::ItemImpl(_, _,typ,_) => { 
                match typ.node {
                    ast::TyPath(ref path, _, _) => {
                        self.name_path = path_to_vec(path);
                    }
                    _ => {}
                }
            },
            _ => {}
        }
    }
}

pub struct FnVisitor {
    pub name: ~str,
    pub output: Vec<~str>,
    pub args: Vec<(StrBuf, uint, Vec<~str>)>,
    pub is_method: bool
}

impl visit::Visitor<()> for FnVisitor {
    fn visit_fn(&mut self, fk: &visit::FnKind, fd: &ast::FnDecl, _: &ast::Block, _: codemap::Span, _: ast::NodeId, _: ()) {

        self.name = token::get_ident(visit::name_of_fn(fk)).get().to_owned();

        for arg in fd.inputs.iter() {
            debug!("PHIL fn arg ast is {:?}",arg);
            let res  = 
                match arg.pat.node {
                    ast::PatIdent(_ , ref path, _) => {
                        let codemap::BytePos(point) = path.span.lo;
                        let pathv = path_to_vec(path);
                        assert!(pathv.len() == 1);
                        
                        // self.args.push(
                        //     (StrBuf::from_str(pathv.get(0).as_slice()), point as uint)
                        Some((StrBuf::from_str(pathv.get(0).as_slice()), point as uint))
                    }
                    _ => None
                };

            if res.is_none() {
                return;
            }

            let (name, pos) = res.unwrap();

            let typepath = match arg.ty.node {
                ast::TyRptr(_, ref ty) => {
                    match ty.ty.node {
                        ast::TyPath(ref path, _, _) => {
                            let type_ = path_to_vec(path);
                            debug!("PHIL arg type is {}", type_);
                            type_
                        }
                        _ => Vec::new()
                    }
                }
                ast::TyPath(ref path, _, _) => {
                    let type_ = path_to_vec(path);
                    debug!("PHIL arg type is {}", type_);
                    type_
                }
                _ => Vec::new()  
            };

            debug!("PHIL typepath {}", typepath);
            self.args.push((name, pos, typepath))
        }

        debug!("PHIL parsed args: {}", self.args);

        match fd.output.node {
            ast::TyPath(ref path, _, _) => {
                self.output = path_to_vec(path);
            }
            _ => {}
        }

        self.is_method = match *fk {
            visit::FkMethod(_, _, _) => true,
            _ => false
        }

        //visit::walk_fn(self, fk, fd, b, s, n , e)
    }

}

pub struct EnumVisitor {
    pub name: StrBuf,
    pub values: Vec<(StrBuf, uint)>,
}

impl visit::Visitor<()> for EnumVisitor {
    fn visit_item(&mut self, i: &ast::Item, _: ()) { 
        match i.node {
            ast::ItemEnum(ref enum_definition, _) => {
                self.name = StrBuf::from_str(token::get_ident(i.ident).get());
                //visitor.visit_generics(type_parameters, env.clone());
                //visit::walk_enum_def(self, enum_definition, type_parameters, e)

                let codemap::BytePos(point) = i.span.lo;
                let codemap::BytePos(point2) = i.span.hi;
                debug!("PHIL name point is {} {}",point,point2);

                for &variant in enum_definition.variants.iter() {
                    let codemap::BytePos(point) = variant.span.lo;
                    self.values.push((StrBuf::from_str(token::get_ident(variant.node.name).get()), point as uint));
                }
            },
            _ => {}
        }
    }
}


pub fn parse_view_item(s: StrBuf) -> Vec<Vec<~str>> {
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

fn _parse_view_items(s: StrBuf)-> Vec<Vec<~str>> {
    let cr = string_to_crate(s);
    let mut v = MyViewItemVisitor{results: Vec::new()};
    visit::walk_crate(&mut v, &cr, ());
    return v.results;
}

pub fn parse_let(s: StrBuf) -> Option<LetResult> {
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

fn _parse_let(s: StrBuf)-> Option<LetResult> {
    let stmt = string_to_stmt(s);
    let mut v = MyLetVisitor{ result: None};
    visit::walk_stmt(&mut v, stmt, ());
    return v.result;
}

pub fn parse_struct_fields(s: StrBuf) -> Vec<(StrBuf, uint)> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = StructVisitor{ fields: Vec::new() };
        visit::walk_stmt(&mut v, stmt, ());
        return v.fields;
    }).ok().unwrap_or(Vec::new());
}

pub fn parse_impl_name(s: StrBuf) -> Option<~str> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = ImplVisitor{ name_path: Vec::new() };
        visit::walk_stmt(&mut v, stmt, ());
        if v.name_path.len() == 1 {
            return Some(v.name_path.pop().unwrap());
        } else {
            return None;
        }
    }).ok().unwrap_or(None);
}

pub fn parse_fn_output(s: StrBuf) -> Vec<~str> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = FnVisitor { name: "".to_owned(), args: Vec::new(), output: Vec::new(), is_method: false };
        visit::walk_stmt(&mut v, stmt, ());
        return v.output;
    }).ok().unwrap();
}

pub fn parse_fn(s: StrBuf) -> FnVisitor {
    debug!("PHIL parse_fn |{}|",s);
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = FnVisitor { name: "".to_owned(), args: Vec::new(), output: Vec::new(), is_method: false };
        visit::walk_stmt(&mut v, stmt, ());
        return v;
    }).ok().unwrap();
}


pub fn parse_enum(s: StrBuf) -> EnumVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = EnumVisitor { name: StrBuf::new(), values: Vec::new()};
        visit::walk_stmt(&mut v, stmt, ());
        return v;
    }).ok().unwrap();
}


struct ExprTypeVisitor {
    scope: Match,
    result: Option<Match>
}

fn find_match(fqn: &Vec<~str>, fpath: &Path, pos: uint) -> Option<Match> {
    let myfqn = racer::to_refs(fqn);  
    return racer::first_match(|m| racer::do_local_search(
        myfqn.as_slice(),
        fpath,
        pos,
        racer::ExactMatch,
        m));
}

impl visit::Visitor<()> for ExprTypeVisitor {
    fn visit_expr(&mut self, expr: &ast::Expr, _: ()) { 
        //debug!("visit_expr {:?}",expr);
        //walk_expr(self, ex, e) 
        match expr.node {
            ast::ExprPath(ref path) => {
                debug!("PHIL expr is a path {}",path_to_vec(path));
                let pathvec = path_to_vec(path);
                self.result = find_match(&pathvec, 
                                         &self.scope.filepath, 
                                         self.scope.point);
            }
            ast::ExprCall(callee_expression, _/*ref arguments*/) => {
                self.visit_expr(callee_expression, ());
                let mut newres: Option<Match> = None;
                {
                    let res = &self.result;
                    match *res {
                        Some(ref m) => {
                            let fqn = racer::resolve::get_return_type_of_function(m);
                            newres = find_match(&fqn, &m.filepath, m.point);
                        },
                        None => {}
                    }
                }
                self.result = newres;
            }
            // ast::ExprMethodCall(ref spannedident, ref types, _/*ref arguments*/) => {
            //     // spannedident.node is an ident I think
            //     let name = token::get_ident(spannedident.node).get().to_owned();
            //     debug!("PHIL method call {}",name);
            // }
            _ => {}
        }
    }

}

pub fn get_type_of(s: StrBuf, fpath: &Path, pos: uint) -> Option<Match> {
    let myfpath = fpath.clone();

    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let startscope = Match{ 
            matchstr: "".to_owned(),
            filepath: myfpath,
            point: pos,
            linetxt: "".to_owned(),
            local: true,
            mtype: racer::Module
        };

        let mut v = ExprTypeVisitor{ scope: startscope,
                                     result: None};
        visit::walk_stmt(&mut v, stmt, ());
        return v.result;
    }).ok().unwrap();
}


#[test]
fn blah() {
    // let src = ~"fn myfn(a: uint) -> Foo {}";
    // let src = ~"impl blah{    fn visit_item(&mut self, item: &ast::Item, _: ()) {} }";

    //let src = "Foo::Bar().baz(32)";
    //let src = "std::vec::Vec::new().push_all()";
    let src = "impl visit::Visitor<()> for ExprTypeVisitor {}";

    let stmt = string_to_stmt(StrBuf::from_str(src));

    println!("PHIL stmt {:?}",stmt);


    let mut v = ImplVisitor{ name_path: Vec::new() };
    visit::walk_stmt(&mut v, stmt, ());

    println!("v {}",v.name_path);
// pub struct Match {
//     pub matchstr: ~str,
//     pub filepath: Path,
//     pub point: uint,
//     pub linetxt: ~str,
//     pub local: bool,
//     pub mtype: MatchType
// }

    // let startscope = Match{ 
    //     matchstr: "".to_owned(),
    //     filepath: Path::new("./ast.rs"),
    //     point: 0,
    //     linetxt: "".to_owned(),
    //     local: true,
    //     mtype: racer::Module
    // };

    // let mut v = ExprTypeVisitor{ scope: startscope,
    //                              result: None};
    // visit::walk_stmt(&mut v, stmt, ());

    // println!("PHIL result was {:?}",v.result);
    //return v.result;

    // let mut v = EnumVisitor { name: StrBuf::new(), values: Vec::new()};
    // visit::walk_stmt(&mut v, cr, ());
    // println!("PHIL {} {}", v.name, v.values);

    // let src = "let v = Foo::new();".to_owned();

    // let res = parse_let(src);
    // debug!("PHIL res {}",res.unwrap().init);

    fail!("hello");
}

