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
use racer::nameres::{resolve_path_with_str};
use racer::typeinf;
use syntax::ptr::P;
use syntax::visit::Visitor;
use racer::nameres;

#[deriving(Clone)]
struct Scope {
    pub filepath: Path,
    pub point: uint
}

// This code ripped from libsyntax::util::parser_testing
pub fn string_to_parser<'a>(ps: &'a ParseSess, source_str: String) -> Parser<'a> {
    new_parser_from_source_str(ps,
                               Vec::new(),
                               "bogofile".to_string(),
                               source_str)
}

fn with_error_checking_parse<T>(s: String, f: |&mut Parser| -> T) -> T {
    let ps = new_parse_sess();
    let mut p = string_to_parser(&ps, s);
    let x = f(&mut p);
    p.abort_if_errors();
    x
}

// parse a string, return an expr
pub fn string_to_expr (source_str : String) -> P<ast::Expr> {
    with_error_checking_parse(source_str, |p| {
        p.parse_expr()
    })
}

// parse a string, return an item
pub fn string_to_item (source_str : String) -> Option<P<ast::Item>> {
    with_error_checking_parse(source_str, |p| {
        p.parse_item(Vec::new())
    })
}

// parse a string, return a stmt
pub fn string_to_stmt(source_str : String) -> P<ast::Stmt> {
    with_error_checking_parse(source_str, |p| {
        p.parse_stmt(Vec::new())
    })
}

// parse a string, return a crate.
pub fn string_to_crate (source_str : String) -> ast::Crate {
    with_error_checking_parse(source_str, |p| {
        p.parse_crate_mod()
    })
}

pub struct ViewItemVisitor {
    pub ident : Option<String>,
    pub paths : Vec<racer::Path>
}

impl<'v> visit::Visitor<'v> for ViewItemVisitor {
    fn visit_view_item(&mut self, i: &ast::ViewItem) {
        match i.node {
            ast::ViewItemUse(ref path) => {
                match path.node {
                    ast::ViewPathSimple(ident, ref path, _) => {
                        self.paths.push(to_racer_path(path));
                        self.ident = Some(token::get_ident(ident).get().to_string());
                    },
                    ast::ViewPathList(ref pth, ref paths, _) => {
                        let basepath = to_racer_path(pth);
                        for path in paths.iter() {
                            match path.node {
                                ast::PathListIdent{name, ..} => {
                                    let name = token::get_ident(name).get().to_string();
                                    let seg = racer::PathSegment{ name: name, types: Vec::new() };
                                    let mut newpath = basepath.clone();

                                    newpath.segments.push(seg);
                                    self.paths.push(newpath);
                                }
                                ast::PathListMod{..} => (), // TODO
                            }
                        }
                    }
                    ast::ViewPathGlob(_, id) => {
                        debug!("PHIL got glob {:?}",id);
                    }
                }
            },
            ast::ViewItemExternCrate(ident, ref loc, _) => {
                self.ident = Some(token::get_ident(ident).get().to_string());

                let ll = loc.clone();
                ll.map(|(ref istr, _ /* str style */)| {
                    let name = istr.get().to_string();

                    let pathseg = racer::PathSegment{ name: name, 
                                                      types: Vec::new() };

                    let path = racer::Path{ global: true, 
                                 segments: vec!(pathseg) };
                    self.paths.push(path);
                });
            }
        }

        visit::walk_view_item(self, i)
    }
}


struct LetVisitor2 {
    ident_points: Vec<(uint,uint)>
}

impl<'v> visit::Visitor<'v> for LetVisitor2 {

    fn visit_local(&mut self, local: &'v ast::Local) {  
        // don't visit the RHS (init) side of the let stmt
        self.visit_pat(&*local.pat);
        //self.visit_ty(&*local.ty);
    }

    fn visit_pat(&mut self, p: &'v ast::Pat) {
        println!("YEAH {:?}",p);
        match p.node {
            ast::PatIdent(_ , ref spannedident, _) => {
                let codemap::BytePos(lo) = spannedident.span.lo;
                let codemap::BytePos(hi) = spannedident.span.hi;
                self.ident_points.push((lo as uint, hi as uint));
            }
            _ => {
                visit::walk_pat(self, p);
            }
        }
    }
}

struct LetVisitor3;
impl<'v> visit::Visitor<'v> for LetVisitor3 {
    fn visit_local(&mut self, local: &'v ast::Local) {
        // local.init

        // match local.pat.node {
        //     // ast::PatIdent(_ , ref spannedident, _) => {
        //     // }
        //     ast::PatTup(ref tuple_elements) => {
        //         // for tuple_element in tuple_elements.iter() {
        //         //     visitor.visit_pat(&**tuple_element)
        //         // }

                
        //     }
        // }
    }
}


struct LetVisitor {
    scope: Scope,
    need_type: bool,
    result: Option<LetResult>
}

pub struct LetResult { 
    pub name: String,
    pub point: uint,
    pub inittype: Option<Ty>
}

fn resolve_ast_path(path: &ast::Path, filepath: &Path, pos: uint) -> Option<Match> {
    debug!("PHIL resolve_ast_path {}",to_racer_path(path));
    return nameres::resolve_path_with_str(&to_racer_path(path), filepath, pos, racer::ExactMatch, racer::BothNamespaces).nth(0);
}

fn to_racer_path(pth: &ast::Path) -> racer::Path {

    let mut v = Vec::new();    
    for seg in pth.segments.iter() {
        let name = token::get_ident(seg.identifier).get().to_string();
        let mut types = Vec::new();    
        for ty in seg.types.iter() {
            types.push(match ty.node {
                ast::TyPath(ref path, _, _) => to_racer_path(path),
                _ => fail!(format!("Cannot handle type {:?}", ty.node))
            });
        }
        v.push(racer::PathSegment{ name: name, types: types}); 
    }
    return racer::Path{ global: pth.global, segments: v} ;
}

struct ExprTypeVisitor {
    scope: Scope,
    result: Option<Ty>
}

fn find_match(path: &racer::Path, fpath: &Path, pos: uint) -> Option<Match> {
    return resolve_path_with_str(path, fpath, pos, racer::ExactMatch,
        racer::BothNamespaces).nth(0);
}

fn find_type_match(path: &racer::Path, fpath: &Path, pos: uint) -> Option<Ty> {
    debug!("PHIL find_type_match {}",path);
    let res = resolve_path_with_str(path, fpath, pos, racer::ExactMatch, 
               racer::TypeNamespace).nth(0).and_then(|m| {
                   match m.mtype {
                       racer::Type => get_type_of_typedef(m),
                       _ => Some(m)
                   }
               });

    return res.and_then(|m|{
        // add generic types to match (if any)
        let types: Vec<racer::PathSearch> = path.generic_types()
            .map(|typepath| 
                 racer::PathSearch{ 
                     path: typepath.clone(),
                     filepath: fpath.clone(),
                     point: pos
                 }).collect();

        if types.is_empty() {
            return Some(TyMatch(m));
        } else {
            return Some(TyMatch(m.with_generic_types(types.clone())));
        }
    });
}

fn get_type_of_typedef(m: Match) -> Option<Match> {
    debug!("PHIL get_type_of_typedef match is {}",m);
    let msrc = racer::load_file_and_mask_comments(&m.filepath);
    let blobstart = m.point - 5;  // - 5 because 'type '
    let blob = msrc.as_slice().slice_from(blobstart);

    return racer::codeiter::iter_stmts(blob).nth(0).and_then(|(start, end)|{
        let blob = msrc.as_slice().slice(blobstart + start, blobstart+end).to_string();
        debug!("PHIL get_type_of_typedef blob string {}",blob);
        let res = parse_type(blob);
        debug!("PHIL get_type_of_typedef parsed type {}",res.type_);
        return res.type_;
    }).and_then(|type_|{
        nameres::resolve_path_with_str(&type_, &m.filepath, m.point, racer::ExactMatch, racer::TypeNamespace).nth(0)
    });
}

fn get_type_of_path(path: &racer::Path, fpath: &Path, pos: uint) -> Option<Ty> {
    let om = find_match(path, fpath, pos);
    if om.is_some() {
        let m = om.unwrap();
        let msrc = racer::load_file_and_mask_comments(&m.filepath);
        return typeinf::get_type_of_match(m, msrc.as_slice());
    } else {
        return None;
    }
}

impl<'v> visit::Visitor<'v> for ExprTypeVisitor {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        debug!("PHIL visit_expr {:?}",expr);
        //walk_expr(self, ex, e) 
        match expr.node {
            ast::ExprPath(ref path) => {
                debug!("PHIL expr is a path {}",to_racer_path(path));
                self.result = resolve_ast_path(path, 
                                 &self.scope.filepath, 
                                 self.scope.point).and_then(|m| {
                   let msrc = racer::load_file_and_mask_comments(&m.filepath);
                   typeinf::get_type_of_match(m, msrc.as_slice())
                                 });
            }
            ast::ExprCall(ref callee_expression, _/*ref arguments*/) => {
                self.visit_expr(&**callee_expression);
                let mut newres: Option<Ty> = None;

                newres = self.result.as_ref().and_then(|m|
                    match m {
                        &TyMatch(ref m) => 
                            racer::typeinf::get_return_type_of_function(m)
                            .and_then(|path| 
                                      find_type_match(&path, &m.filepath, m.point)),
                        _ => None
                    }
                );

            }
            ast::ExprStruct(ref path, _, _) => {
                let pathvec = to_racer_path(path);
                self.result = find_type_match(&pathvec,
                                              &self.scope.filepath,
                                              self.scope.point);
            }

            ast::ExprMethodCall(ref spannedident, ref types, ref arguments) => {
                // spannedident.node is an ident I think
                let methodname = token::get_ident(spannedident.node).get().to_string();
                debug!("PHIL method call ast name {}",methodname);
                debug!("PHIL method call ast types {:?} {}",types, types.len());
                
                let objexpr = &arguments[0];
                //println!("PHIL obj expr is {:?}",objexpr);
                self.visit_expr(&**objexpr);

                self.result = self.result.as_ref().and_then(|contextm|{
                    match contextm {
                        &TyMatch(ref contextm) => {

                    let omethod = nameres::search_for_impl_methods(
                        contextm.matchstr.as_slice(),
                        methodname.as_slice(), 
                        contextm.point, 
                        &contextm.filepath,
                        contextm.local,
                        racer::ExactMatch).nth(0);

                    omethod.and_then(|method| 
                         racer::typeinf::get_return_type_of_function(&method).and_then(|returntypepath| 
                                            find_type_match_including_generics(&returntypepath, 
                                                                               &method.filepath, 
                                                                               method.point, 
                                                                               contextm)))

                        }
                        _ => None
                    }
                });
            }

            ast::ExprField(ref subexpression, spannedident, _) => {
                let fieldname = token::get_ident(spannedident.node).get().to_string();
                debug!("PHIL exprfield {}",fieldname);
                self.visit_expr(&**subexpression);
                self.result = self.result.as_ref()
                      .and_then(|structm| 
                                match structm {
                                    &TyMatch(ref structm) => {
                                typeinf::get_struct_field_type(fieldname.as_slice(), structm)
                                .and_then(|fieldtypepath| 
                                          find_type_match_including_generics(&fieldtypepath,
                                                                             &structm.filepath,
                                                                             structm.point,
                                                                             structm))
                                    },
                                    _ => None
                                });

            }

            _ => {
                println!("PHIL - Could not match expr node type: {:?}",expr.node);
            }
        }
    }

}

fn find_type_match_including_generics(fieldtypepath: &racer::Path,
                                      filepath: &Path,
                                      pos: uint,
                                      structm: &racer::Match) -> Option<Ty>{
    if fieldtypepath.segments.len() == 1 {
        // could be a generic arg! - try and resolve it
        let ref typename = fieldtypepath.segments[0].name;
        let mut it = structm.generic_args.iter()
            .zip(structm.generic_types.iter());
        for (name, typesearch) in it {
            if name == typename {
                // yes! a generic type match!
                return find_type_match(&typesearch.path, 
                                       &typesearch.filepath, 
                                       typesearch.point);
            }
        }
    }
    
    return find_type_match(fieldtypepath, 
                           filepath, 
                           pos);
}

impl<'v> visit::Visitor<'v> for LetVisitor {

    fn visit_decl(&mut self, decl: &ast::Decl) {
        debug!("PHIL visit_decl {:?}",decl);
        match decl.node {
            ast::DeclLocal(ref local) => {
                debug!("PHIL visit_decl is a DeclLocal {:?}",local);
                match local.pat.node {
                    ast::PatIdent(_ , ref spannedident, _) => {
                        debug!("PHIL visit_decl is a patIdent {:?}",local);
                        let codemap::BytePos(point) = spannedident.span.lo;
                        let name = token::get_ident(spannedident.node).get().to_string();


                        self.result = Some(LetResult{name: name.to_string(),
                                                     point: point as uint,
                                                     inittype: None});
                        if !self.need_type {
                            // don't need the type. All done
                            debug!("PHIL visit_decl don't need the type. returning");
                            
                            return;
                        }

                        // Parse the type
                        debug!("PHIL visit_decl local.ty.node {:?}", local.ty.node);

                        let typepath = match local.ty.node {
                            ast::TyRptr(_, ref ty) => {
                                match ty.ty.node {
                                    ast::TyPath(ref path, _, _) => {
                                        let type_ = to_racer_path(path);
                                        debug!("PHIL struct field type is {}", type_);
                                        Some(type_)
                                    }
                                    _ => None
                                }
                            }
                            ast::TyPath(ref path, _, _) => {
                                let type_ = to_racer_path(path);
                                debug!("PHIL struct field type is {}", type_);
                                Some(type_)
                            }
                            _ => {
                                None
                            }
                        };
                        
                        if typepath.is_some() {
                            debug!("PHIL visit_decl typepath is {}", typepath);
                            let ty = get_type_of_path(&typepath.unwrap(),
                                                     &self.scope.filepath,
                                                     self.scope.point);
                            debug!("PHIL visit_decl init type {}", ty);
                            self.result = Some(LetResult{name: name.to_string(), 
                                                         point: point as uint,
                                                         inittype: ty});
                            return;
                        }

                        debug!("PHIL result before is {:?}",self.result);

                        // That didn't work. Attempt to parse the init
                        local.init.as_ref().map(|initexpr| {
                            debug!("PHIL init node is {:?}",initexpr.node);

                            let mut v = ExprTypeVisitor{ scope: self.scope.clone(),
                                                         result: None};
                            v.visit_expr(&**initexpr);

                            self.result = Some(LetResult{name: name.to_string(), 
                                                         point: point as uint,
                                                         inittype: v.result
                            });

                        });

                    },
                    _ => {}
                }

                
            }
            ast::DeclItem(_) => {
                println!("PHIL WARN visit_decl is a DeclItem. Not handled");
            }
        }

        visit::walk_decl(self, decl);
    }

}

struct StructVisitor {
    pub fields: Vec<(String, uint, Option<racer::Path>)>
}

impl<'v> visit::Visitor<'v> for StructVisitor {
    fn visit_struct_def(&mut self, struct_definition: &ast::StructDef, _: ast::Ident, _: &ast::Generics, _: ast::NodeId) {

        for field in struct_definition.fields.iter() {
            let codemap::BytePos(point) = field.span.lo;

            match field.node.kind {
                ast::NamedField(name, _) => {
                    //visitor.visit_ident(struct_field.span, name, env.clone())
                    let n = String::from_str(token::get_ident(name).get());
                    

                    // we have the name. Now get the type
                    let typepath = match field.node.ty.node {
                        ast::TyRptr(_, ref ty) => {
                            match ty.ty.node {
                                ast::TyPath(ref path, _, _) => {
                                    let type_ = to_racer_path(path);
                                    debug!("PHIL struct field type is {}", type_);
                                    Some(type_)
                                }
                                _ => None
                            }
                        }
                        ast::TyPath(ref path, _, _) => {
                            let type_ = to_racer_path(path);
                            debug!("PHIL struct field type is {}", type_);
                            Some(type_)
                        }
                        _ => None
                    };

                    
                    self.fields.push((n, point as uint, typepath));
                }
                _ => {}
            }



        }
    }
}

pub struct TypeVisitor {
    pub name: Option<String>,
    pub type_: Option<racer::Path>
}

impl<'v> visit::Visitor<'v> for TypeVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        match item.node {
            ast::ItemTy(ref ty, _) => {
                self.name = Some(token::get_ident(item.ident).get().to_string());

                let typepath = match ty.node {
                    ast::TyRptr(_, ref ty) => {
                        match ty.ty.node {
                            ast::TyPath(ref path, _, _) => {
                                let type_ = to_racer_path(path);
                                debug!("PHIL type type is {}", type_);
                                Some(type_)
                            }
                            _ => None
                        }
                    }
                    ast::TyPath(ref path, _, _) => {
                        let type_ = to_racer_path(path);
                        debug!("PHIL type type is {}", type_);
                        Some(type_)
                    }
                    _ => None
                };
                self.type_ = typepath;
                debug!("PHIL typevisitor type is {}",self.type_);
            }
            _ => ()
        }
    }
}

pub struct TraitVisitor {
    pub name: Option<String>
}

impl<'v> visit::Visitor<'v> for TraitVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        match item.node {
            ast::ItemTrait(_, _, _, _) => {
                self.name = Some(token::get_ident(item.ident).get().to_string());
            }
            _ => ()
        }
    }
}

pub struct ImplVisitor {
    pub name_path: Option<racer::Path>,
    pub trait_path: Option<racer::Path>,
}

impl<'v> visit::Visitor<'v> for ImplVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        match item.node {
            ast::ItemImpl(_,ref otrait, ref typ,_) => {
                match typ.node {
                    ast::TyPath(ref path, _, _) => {
                        self.name_path = Some(to_racer_path(path));
                    }
                    ast::TyRptr(_, ref ty) => {
                        // HACK for now, treat refs the same as unboxed types 
                        // so that we can match '&str' to 'str'
                        match ty.ty.node {
                            ast::TyPath(ref path, _, _) => {
                                self.name_path = Some(to_racer_path(path));
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
                otrait.as_ref().map(|ref t|{
                    self.trait_path = Some(to_racer_path(&t.path));
                });

            },
            _ => {}
        }
    }
}

// pub struct FnTypeVisitor {
//     m: Option<Match>,
//     ctx: Match
// }
//
// impl visit::Visitor<String> for FnTypeVisitor {
//     fn visit_fn(&mut self, _: &visit::FnKind, fd: &ast::FnDecl, _: &ast::Block, _: codemap::Span, _: ast::NodeId, name: String) {
//         if name.as_slice() == "return_value" {
//             match fd.output.node {
//                 ast::TyPath(ref path, _, _) => {
//                     //self.output = path_to_vec(path);
//                     self.m = resolve_ast_path(path,
//                                               &self.ctx.filepath,
//                                               self.ctx.point);
//                     debug!("PHIL visit_fn: return type is {}",self.m);
//                 }
//                 _ => {}
//             }
//         }
//     }
// }

pub struct FnVisitor {
    pub name: String,
    pub output: Option<racer::Path>,
    pub args: Vec<(String, uint, Option<racer::Path>)>,
    pub is_method: bool
}

impl<'v> visit::Visitor<'v> for FnVisitor {
    fn visit_fn(&mut self, fk: visit::FnKind, fd: &ast::FnDecl, _: &ast::Block, _: codemap::Span, _: ast::NodeId) {

        let name = match fk {
            visit::FkItemFn(name, _, _, _) | visit::FkMethod(name, _, _) => name,
            visit::FkFnBlock(..) => syntax::parse::token::special_idents::invalid
        };
        self.name = token::get_ident(name).get().to_string();

        for arg in fd.inputs.iter() {
            debug!("PHIL fn arg ast is {:?}",arg);
            let res  = 
                match arg.pat.node {
                    ast::PatIdent(_ , ref spannedident, _) => {
                        let codemap::BytePos(point) = spannedident.span.lo;
                        let argname = token::get_ident(spannedident.node).get().to_string();
                        Some((String::from_str(argname.as_slice()), point as uint))
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
                            let type_ = to_racer_path(path);
                            debug!("PHIL arg type is {}", type_);
                            Some(type_)
                        }
                        _ => None
                    }
                }
                ast::TyPath(ref path, _, _) => {
                    let type_ = to_racer_path(path);
                    debug!("PHIL arg type is {}", type_);
                    Some(type_)
                }
                _ => None
            };

            debug!("PHIL typepath {}", typepath);
            self.args.push((name, pos, typepath))
        }

        debug!("PHIL parsed args: {}", self.args);

        match fd.output.node {
            ast::TyPath(ref path, _, _) => {
                self.output = Some(to_racer_path(path));
            }
            _ => {}
        }

        self.is_method = match fk {
            visit::FkMethod(_, _, _) => true,
            _ => false
        }
    }

}

pub struct ModVisitor {
    pub name: Option<String>
}

impl<'v> visit::Visitor<'v> for ModVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        match item.node {
            ast::ItemMod(_) => {
                self.name = Some(String::from_str(token::get_ident(item.ident).get()));
            }
            _ => {}
        }
    }

}

pub struct GenericsVisitor {
    pub generic_args: Vec<String>
}

impl<'v> visit::Visitor<'v> for GenericsVisitor {
    fn visit_generics(&mut self, g: &ast::Generics) {
        for ty in g.ty_params.iter() {
            self.generic_args.push(String::from_str(token::get_ident(ty.ident).get()));
        }
    }
}

pub struct StructDefVisitor {
    pub name: Option<(String,uint)>,
    pub generic_args: Vec<String>
}

impl<'v> visit::Visitor<'v> for StructDefVisitor {
    fn visit_generics(&mut self, g: &ast::Generics) {
        for ty in g.ty_params.iter() {
            self.generic_args.push(String::from_str(token::get_ident(ty.ident).get()));
        }
    }

    fn visit_ident(&mut self, _sp: codemap::Span, _ident: ast::Ident) {
        /*! Visit the idents */
        let codemap::BytePos(point) = _sp.lo;
        let name = String::from_str(token::get_ident(_ident).get());
        self.name = Some((name,point as uint));
    }
}




pub struct EnumVisitor {
    pub name: String,
    pub values: Vec<(String, uint)>,
}

impl<'v> visit::Visitor<'v> for EnumVisitor {
    fn visit_item(&mut self, i: &ast::Item) {
        match i.node {
            ast::ItemEnum(ref enum_definition, _) => {
                self.name = String::from_str(token::get_ident(i.ident).get());
                //visitor.visit_generics(type_parameters, env.clone());
                //visit::walk_enum_def(self, enum_definition, type_parameters, e)

                let codemap::BytePos(point) = i.span.lo;
                let codemap::BytePos(point2) = i.span.hi;
                debug!("PHIL name point is {} {}",point,point2);

                for variant in enum_definition.variants.iter() {
                    let codemap::BytePos(point) = variant.span.lo;
                    self.values.push((String::from_str(token::get_ident(variant.node.name).get()), point as uint));
                }
            },
            _ => {}
        }
    }
}


pub fn parse_view_item(s: String) -> ViewItemVisitor {
    // parser can fail!() so isolate it in another task
    let result = task::try(proc() { 
        let cr = string_to_crate(s);
        let mut v = ViewItemVisitor{ident: None, paths: Vec::new()};
        visit::walk_crate(&mut v, &cr);
        return v;
    });
    match result {
        Ok(s) => {return s;},
        Err(_) => {
            return ViewItemVisitor{ident: None, paths: Vec::new()};
        }
    }
}

pub fn parse_let2(s: String) -> Vec<(uint,uint)> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = LetVisitor2 { ident_points: Vec::new() };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap().ident_points;
}


// pub fn prototype_get_type_of_letvar(s: String) -> Vec<(uint,uint)> {
//     return task::try(proc() {
//         let stmt = string_to_stmt(s);
//         let mut v = LetVisitor3;
//         visit::walk_stmt(&mut v, &*stmt);
//         return v;
//     }).ok().unwrap().ident_points;
// }


pub fn parse_let(s: String, fpath: Path, pos: uint, need_type: bool) -> Option<LetResult> {

    let result = task::try(proc() { 
        debug!("PHIL parse_let s=|{}| need_type={}",s, need_type);
        let stmt = string_to_stmt(s);
        debug!("PHIL parse_let stmt={:?}",stmt);
        let scope = Scope{filepath: fpath, point: pos};

        let mut v = LetVisitor{ scope: scope, result: None, need_type: need_type};
        visit::walk_stmt(&mut v, &*stmt);
        return v.result;
    });
    match result {
        Ok(s) => {return s;},
        Err(_) => {
            return None;
        }
    }
}

pub fn parse_struct_fields(s: String) -> Vec<(String, uint, Option<racer::Path>)> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = StructVisitor{ fields: Vec::new() };
        visit::walk_stmt(&mut v, &*stmt);
        return v.fields;
    }).ok().unwrap_or(Vec::new());
}

pub fn parse_impl(s: String) -> ImplVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = ImplVisitor { name_path: None, trait_path: None };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();    
}

pub fn parse_trait(s: String) -> TraitVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = TraitVisitor { name: None };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();    
}

pub fn parse_struct_def(s: String) -> StructDefVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = StructDefVisitor { name: None, generic_args: Vec::new() };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();
}

pub fn parse_generics(s: String) -> GenericsVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = GenericsVisitor { generic_args: Vec::new() };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();
}

pub fn parse_type(s: String) -> TypeVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = TypeVisitor { name: None, type_: None };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();    
}


pub fn parse_fn_output(s: String) -> Option<racer::Path> {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = FnVisitor { name: "".to_string(), args: Vec::new(), 
                                output: None, is_method: false };
        visit::walk_stmt(&mut v, &*stmt);
        return v.output;
    }).ok().unwrap();
}

pub fn parse_fn(s: String) -> FnVisitor {
    debug!("PHIL parse_fn |{}|",s);
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = FnVisitor { name: "".to_string(), args: Vec::new(), 
                                output: None, is_method: false };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();
}

pub fn parse_mod(s: String) -> ModVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = ModVisitor { name: None };
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();    
}

pub fn parse_enum(s: String) -> EnumVisitor {
    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let mut v = EnumVisitor { name: String::new(), values: Vec::new()};
        visit::walk_stmt(&mut v, &*stmt);
        return v;
    }).ok().unwrap();
}


pub fn get_type_of(s: String, fpath: &Path, pos: uint) -> Option<Ty> {
    let myfpath = fpath.clone();

    return task::try(proc() {
        let stmt = string_to_stmt(s);
        let startscope = Scope {
            filepath: myfpath,
            point: pos
        };

        let mut v = ExprTypeVisitor{ scope: startscope,
                                     result: None};
        visit::walk_stmt(&mut v, &*stmt);
        return v.result;
    }).ok().unwrap();
}

//------------------------------------- SPIKE ----------------

struct Expr {
    expr: String,
    fpath: Path,
    point: uint
}

#[deriving(Show)]
pub enum Ty {
    TyMatch(Match),
    TyPath(racer::Path)
}

//------------------------------------------------------------


// pub fn get_return_type_of_function(fnmatch: &Match) -> Option<Match> {
//     let filetxt = BufferedReader::new(File::open(&fnmatch.filepath)).read_to_end().unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();
//     let point = scopes::find_stmt_start(src, fnmatch.point).unwrap();
//     return src.slice_from(point).find_str("{").and_then(|n|{
//         // wrap in "impl blah { }" so that methods get parsed correctly too
//         let mut decl = String::new();
//         decl.push_str("impl blah {");
//         decl.push_str(src.slice(point, point+n+1));
//         decl.push_str("}}");
//         debug!("PHIL: passing in |{}|",decl);
//         //return ast::(decl);

//         let fnmatch = fnmatch.clone();  // copy for the closure
//         return task::try(proc() {
//             let stmt = string_to_stmt(decl);
//             let mut v = FnTypeVisitor { m: None, ctx: fnmatch };
//             visit::walk_stmt(&mut v, &*stmt, "return_value".to_string());
//             return v.m.unwrap(); // if None then will fail and parent will return None
//         }).ok();
//     });
// }


#[test]
fn ast_sandbox() {
    //let src = "let (a,b) = (2,3);";

    // let src = "let (a,b) : (uint,uint);";

    // //let src = "fn foo(a: int, b: |int|->int) {};";

    // let src = "let myvar = |blah| {};";

    // let src = "(myvar, foo) = (3,4);";

    // //let src = "fn myfn((a,b) : (uint, uint)) {}";

    // let stmt = string_to_stmt(String::from_str(src));
    // println!("PHIL stmt {} ", stmt);

    // let out = parse_let2(src.to_string());

    // for &(l,h) in out.iter() {
    //     println!("PHIL {}",src.slice(l,h));
    // }

    // let src = "let (a,b) : (uint,uint);";

    // let result = task::try(proc() { 
    //     return string_to_stmt(String::from_str(src));
    // });


    //println!("PHIL out {} ", result);
    //fail!();

    //parse_let("let l : Vec<Blah>;".to_string(), Path::new("./ast.rs"), 0, true);
    //parse_let("let l = Vec<Blah>::new();".to_string(), Path::new("./ast.rs"), 0, true);

    //get_type_of("let l : Vec<Blah>;".to_string(), &Path::new("./ast.rs"), 0);
    //fail!();


    // let src = "pub struct Foo<T>;";
    // let s = parse_generics(src.to_string());
    // println!("PHIL out {} ", s.generic_args);
    // let stmt = string_to_stmt(String::from_str(src));
    // println!("PHIL stmt is {:?}",stmt);
    // fail!();
    // // let mut v = LetVisitor{ scope: Scope {filepath: Path::new("./foo"), point: 0} , result: None, parseinit: true};
    // let mut v = StructDefVisitor {nam}
    // visit::walk_stmt(&mut v, &*stmt, ());

    // println!("PHIL {:?}", stmt);
    // fail!();
    // println!("PHIL {}", v.name);
    // fail!("");
    // let mut v = ExprTypeVisitor{ scope: startscope,
    //                              result: None};
    

    // let src = ~"fn myfn(a: uint) -> Foo {}";
    // let src = ~"impl blah{    fn visit_item(&mut self, item: &ast::Item, _: ()) {} }";

    // let src = "Foo::Bar().baz(32)";
    //let src = "std::vec::Vec::new().push_all()";
    // let src = "impl visit::Visitor<()> for ExprTypeVisitor {}";
    // let src = "impl<'a> StrSlice<'a> for &'a str {}";

    //let src = "a(|b|{});";
    // let src = "(a,b) = (3,4);";
    // let src = "fn foo() -> (a,b) {}";

    // let stmt = string_to_stmt(String::from_str(src));

    // let src = "extern crate core_collections = \"collections\";";
    // let src = "use foo = bar;";
    // let src = "use bar::baz;";

    // let src = "use bar::{baz,blah};";
    // let src = "extern crate collections;";
    // let cr = string_to_crate(String::from_str(src));
    // let mut v = ViewItemVisitor{ ident: None, paths: Vec::new() };
    // visit::walk_crate(&mut v, &cr, ());

    // println!("PHIL v {} {}",v.ident, v.paths);

    //visit::walk_stmt(&mut v, stmt, ());

    // println!("PHIL stmt {:?}",stmt);


    // let mut v = ImplVisitor{ name_path: Vec::new() };
    // visit::walk_stmt(&mut v, stmt, ());


    // println!("v {}",v.name_path);

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

    // let startscope = Scope {
    //     filepath: Path::new("./ast.rs"),
    //     point: 0
    // };

    // let mut v = ExprTypeVisitor{ scope: startscope,
    //                              result: None};
    // visit::walk_stmt(&mut v, stmt, ());

    // println!("PHIL result was {:?}",v.result);
    //return v.result;

    // let mut v = EnumVisitor { name: String::new(), values: Vec::new()};
    // visit::walk_stmt(&mut v, cr, ());
    // println!("PHIL {} {}", v.name, v.values);

    // let src = "let v = Foo::new();".to_owned();

    // let res = parse_let(src);
    // debug!("PHIL res {}",res.unwrap().init);
}

