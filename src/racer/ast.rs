use core::{self, Match, MatchType, Scope, Ty, SessionRef};
use typeinf;
use nameres::{self, resolve_path_with_str};
use core::Ty::{TyTuple, TyPathSearch, TyMatch, TyUnsupported};

use std::path::Path;

use syntex_syntax::ast;
use syntex_syntax::codemap;
use syntex_syntax::parse::parser::Parser;
use syntex_syntax::parse::{lexer, ParseSess};
use syntex_syntax::ptr::P;
use syntex_syntax::visit::{self, Visitor};
use syntex_syntax::diagnostic::{ColorConfig, Handler, SpanHandler};

// This code ripped from libsyntax::util::parser_testing
pub fn string_to_parser(ps: &ParseSess, source_str: String) -> Option<Parser> {
    let fm = ps.codemap().new_filemap("bogofile".into(), source_str);
    let srdr = lexer::StringReader::new(&ps.span_diagnostic, fm);
    let p = Parser::new(ps, Vec::new(), Box::new(srdr));
    Some(p)
}

pub fn with_error_checking_parse<F, T>(s: String, f: F) -> Option<T> where F: Fn(&mut Parser) -> Option<T> {
    let sh = SpanHandler::new(Handler::new(ColorConfig::Never, None, false),
                              codemap::CodeMap::new());
    let ps = ParseSess::with_span_handler(sh);

    let mut p = match string_to_parser(&ps, s) {
        Some(p) => p,
        None => return None
    };
    f(&mut p)
}

// parse a string, return a stmt
pub fn string_to_stmt(source_str: String) -> Option<P<ast::Stmt>> {
    with_error_checking_parse(source_str, |p| {
        use syntex_syntax::diagnostic::FatalError;
        match p.parse_stmt_nopanic() {
            Ok(p) => p,
            Err(FatalError) => None
        }
    })
}

// parse a string, return a crate.
pub fn string_to_crate(source_str: String) -> Option<ast::Crate> {
    with_error_checking_parse(source_str.clone(), |p| {
        use std::result::Result::{Ok, Err};
        use syntex_syntax::diagnostic::FatalError;
        match p.parse_crate_mod() {
            Ok(e) => Some(e),
            Err(FatalError) => {
                debug!("unable to parse crate. Returning None |{}|", source_str);
                None
            }
        }
    })
}


#[derive(Debug)]
pub struct UseVisitor {
    pub ident : Option<String>,
    pub paths : Vec<core::Path>,
    pub is_glob: bool
}

impl<'v> visit::Visitor<'v> for UseVisitor {
    fn visit_item(&mut self, i: &'v ast::Item) {
        if let ast::ItemUse(ref path) = i.node {
            match path.node {
                ast::ViewPathSimple(ident, ref path) => {
                    self.paths.push(to_racer_path(path));
                    self.ident = Some(ident.name.to_string());
                },
                ast::ViewPathList(ref pth, ref paths) => {
                    let basepath = to_racer_path(pth);
                    for path in paths {
                        match path.node {
                            ast::PathListIdent{name, ..} => {
                                let name = name.name.to_string();
                                let seg = core::PathSegment{ name: name, types: Vec::new() };
                                let mut newpath = basepath.clone();

                                newpath.segments.push(seg);
                                self.paths.push(newpath);
                            },
                            ast::PathListMod{..} => {
                                self.paths.push(basepath.clone());
                            }
                        }
                    }
                }
                ast::ViewPathGlob(ref pth) => {
                    let basepath = to_racer_path(pth);
                    self.paths.push(basepath);
                    self.is_glob = true;
                }
            }
        }
    }
}

pub struct PatBindVisitor {
    ident_points: Vec<(usize, usize)>
}

impl<'v> visit::Visitor<'v> for PatBindVisitor {
    fn visit_local(&mut self, local: &'v ast::Local) {
        // don't visit the RHS (init) side of the let stmt
        self.visit_pat(&*local.pat);
    }

    fn visit_expr(&mut self, ex: &'v ast::Expr) {
        // don't visit the RHS or block of an 'if let' or 'for' stmt
        if let ast::ExprIfLet(ref pattern, _,_,_) = ex.node {
            self.visit_pat(pattern);
        } else if let ast::ExprWhileLet(ref pattern, _,_,_) = ex.node {
            self.visit_pat(pattern);
        } else if let ast::ExprForLoop(ref pattern, _, _, _) = ex.node {
            self.visit_pat(pattern);
        } else {
            visit::walk_expr(self, ex)
        }
    }

    fn visit_pat(&mut self, p: &'v ast::Pat) {
        match p.node {
            ast::PatIdent(_ , ref spannedident, _) => {
                let codemap::BytePos(lo) = spannedident.span.lo;
                let codemap::BytePos(hi) = spannedident.span.hi;
                self.ident_points.push((lo as usize, hi as usize));
            }
            _ => {
                visit::walk_pat(self, p);
            }
        }
    }
}

pub struct PatVisitor {
    ident_points: Vec<(usize, usize)>
}

impl<'v> visit::Visitor<'v> for PatVisitor {
    fn visit_pat(&mut self, p: &'v ast::Pat) {
        match p.node {
            ast::PatIdent(_ , ref spannedident, _) => {
                let codemap::BytePos(lo) = spannedident.span.lo;
                let codemap::BytePos(hi) = spannedident.span.hi;
                self.ident_points.push((lo as usize, hi as usize));
            }
            _ => { visit::walk_pat(self, p); }
        }
    }
}

fn to_racer_ty(ty: &ast::Ty, scope: &Scope) -> Option<Ty> {
    match ty.node {
        ast::TyTup(ref items) => {
            let mut res = Vec::new();
            for t in items {
                res.push(match to_racer_ty(t, scope) {
                    Some(t) => t,
                    None => return None
                });
            }
            Some(TyTuple(res))
        },
        ast::TyRptr(_, ref ty) => {
            to_racer_ty(&*ty.ty, scope)
        },
        ast::TyPath(_, ref path) => {
            Some(TyPathSearch(to_racer_path(path), scope.clone()))
        }
        _ => None
    }
}

fn point_is_in_span(point: u32, span: &codemap::Span) -> bool {
    let codemap::BytePos(lo) = span.lo;
    let codemap::BytePos(hi) = span.hi;
    point >= lo && point < hi
}

// The point must point to an ident within the pattern.
fn destructure_pattern_to_ty(pat: &ast::Pat,
                             point: usize,
                             ty: &Ty,
                             scope: &Scope,
                             session: SessionRef) -> Option<Ty> {
    debug!("destructure_pattern_to_ty point {} ty {:?}    ||||||||    pat: {:?}", point, ty, pat);
    match pat.node {
        ast::PatIdent(_ , ref spannedident, _) => {
            if point_is_in_span(point as u32, &spannedident.span) {
                debug!("destructure_pattern_to_ty matched an ident!");
                Some(ty.clone())
            } else {
                panic!("Expecting the point to be in the patident span. pt: {}", point);
            }
        }
        ast::PatTup(ref tuple_elements) => {
            match *ty {
                TyTuple(ref typeelems) => {
                    let mut i = 0usize;
                    let mut res = None;
                    for p in tuple_elements {
                        if point_is_in_span(point as u32, &p.span) {
                            let ref ty = typeelems[i];
                            res = destructure_pattern_to_ty(p, point, ty, scope, session);
                            break;
                        }
                        i += 1;
                    }
                    res
                }
                _ => panic!("Expecting TyTuple")

            }
        }
        ast::PatEnum(ref path, ref children) => {
            let mut i = 0u32;

            let m = resolve_ast_path(path, &scope.filepath, scope.point, session);
            let contextty = path_to_match(ty.clone(), session);
            if let (Some(m), Some(children)) = (m, children.as_ref()) {
                let mut res = None;
                for p in children {
                    if point_is_in_span(point as u32, &p.span) {

                        res = typeinf::get_tuplestruct_field_type(i, &m, session)
                            .and_then(|ty|
                                // if context ty is a match, use its generics
                                if let Some(Ty::TyMatch(ref contextmatch)) = contextty {
                                    path_to_match_including_generics(ty, contextmatch, session)
                                } else {
                                    path_to_match(ty, session)
                                })
                            .and_then(|ty| destructure_pattern_to_ty(p, point, &ty, scope, session));

                        break;
                    }
                    i += 1;
                }
                res
            } else {
                None
            }
        }
        _ => {
            debug!("Could not destructure pattern {:?}", pat);
            None
        }
    }
}

struct LetTypeVisitor<'s> {
    scope: Scope,
    session: SessionRef<'s>,
    srctxt: String,
    pos: usize,        // pos is relative to the srctxt, scope is global
    result: Option<Ty>
}

impl<'s, 'v> visit::Visitor<'v> for LetTypeVisitor<'s> {
    fn visit_expr(&mut self, ex: &'v ast::Expr) {
        match ex.node {
            ast::ExprIfLet(ref pattern, ref expr, _, _) |
            ast::ExprWhileLet(ref pattern, ref expr, _, _) => {
                let mut v = ExprTypeVisitor{ scope: self.scope.clone(), result: None,
                                             session: self.session };
                v.visit_expr(expr);
                self.result = v.result.and_then(|ty|
                       destructure_pattern_to_ty(pattern, self.pos, &ty, &self.scope, self.session))
                    .and_then(|ty| path_to_match(ty, self.session));
            }
            _ => {
                visit::walk_expr(self, ex)
            }
        }
    }

    fn visit_local(&mut self, local: &'v ast::Local) {
        let mut ty = None;
        if let Some(ref local_ty) = local.ty {
            ty = to_racer_ty(local_ty, &self.scope);
        }

        if ty.is_none() {
            // oh, no type in the let expr. Try evalling the RHS
            ty = local.init.as_ref().and_then(|initexpr| {
                debug!("init node is {:?}", initexpr.node);
                let mut v = ExprTypeVisitor{ scope: self.scope.clone(), result: None,
                                             session: self.session };
                v.visit_expr(initexpr);
                v.result
            });
        }

        debug!("LetTypeVisitor: ty is {:?}. pos is {}, src is |{}|", ty, self.pos, self.srctxt);
        self.result = ty.and_then(|ty|
           destructure_pattern_to_ty(&*local.pat, self.pos, &ty, &self.scope, self.session))
            .and_then(|ty| path_to_match(ty, self.session));
    }
}

struct MatchTypeVisitor<'s> {
    scope: Scope,
    session: SessionRef<'s>,
    pos: usize,        // pos is relative to the srctxt, scope is global
    result: Option<Ty>
}

impl<'s, 'v> visit::Visitor<'v> for MatchTypeVisitor<'s> {
    fn visit_expr(&mut self, ex: &'v ast::Expr) {
        if let ast::ExprMatch(ref subexpression, ref arms, _) = ex.node {
            debug!("PHIL sub expr is {:?}", subexpression);

            let mut v = ExprTypeVisitor{ scope: self.scope.clone(), result: None,
                                         session: self.session };
            v.visit_expr(subexpression);

            debug!("PHIL sub type is {:?}", v.result);

            for arm in arms {
                for pattern in &arm.pats {
                    if point_is_in_span(self.pos as u32, &pattern.span) {
                        debug!("PHIL point is in pattern |{:?}|", pattern);
                        self.result = v.result.as_ref().and_then(|ty|
                               destructure_pattern_to_ty(pattern, self.pos, ty, &self.scope, self.session))
                            .and_then(|ty| path_to_match(ty, self.session));
                    }
                }
            }
        }
    }
}

fn resolve_ast_path(path: &ast::Path, filepath: &Path, pos: usize, session: SessionRef) -> Option<Match> {
    debug!("resolve_ast_path {:?}", to_racer_path(path));
    nameres::resolve_path_with_str(&to_racer_path(path), filepath, pos, core::SearchType::ExactMatch,
                                   core::Namespace::BothNamespaces, session).nth(0)
}

fn to_racer_path(pth: &ast::Path) -> core::Path {
    let mut v = Vec::new();
    for seg in &pth.segments {
        let name = seg.identifier.name.to_string();
        let mut types = Vec::new();
        for ty in seg.parameters.types() {
            if let ast::TyPath(_, ref path) = ty.node {
                types.push(to_racer_path(path));
            }
        }
        v.push(core::PathSegment{ name: name, types: types });
    }
    core::Path{ global: pth.global, segments: v }
}

fn path_to_match(ty: Ty, session: SessionRef) -> Option<Ty> {
    match ty {
        TyPathSearch(ref path, ref scope) =>
            find_type_match(path, &scope.filepath, scope.point, session),
        _ => Some(ty)
    }
}

fn find_type_match(path: &core::Path, fpath: &Path, pos: usize, session: SessionRef) -> Option<Ty> {
    debug!("find_type_match {:?}", path);
    let res = resolve_path_with_str(path, fpath, pos, core::SearchType::ExactMatch,
               core::Namespace::TypeNamespace, session).nth(0).and_then(|m| {
                   match m.mtype {
                       MatchType::Type => get_type_of_typedef(m, session),
                       _ => Some(m)
                   }
               });

    res.and_then(|m| {
        // add generic types to match (if any)
        let types: Vec<core::PathSearch> = path.generic_types()
            .map(|typepath|
                 core::PathSearch{
                     path: typepath.clone(),
                     filepath: fpath.to_path_buf(),
                     point: pos
                 }).collect();

        if types.is_empty() {
            Some(TyMatch(m))
        } else {
            Some(TyMatch(m.with_generic_types(types.clone())))
        }
    })
}

fn get_type_of_typedef(m: Match, session: SessionRef) -> Option<Match> {
    debug!("get_type_of_typedef match is {:?}", m);
    let msrc = session.load_file_and_mask_comments(&m.filepath);
    let blobstart = m.point - 5;  // - 5 because 'type '
    let blob = msrc.from(blobstart);

    blob.iter_stmts().nth(0).and_then(|(start, end)| {
        let blob = msrc[blobstart + start..blobstart+end].to_owned();
        debug!("get_type_of_typedef blob string {}", blob);
        let res = parse_type(blob);
        debug!("get_type_of_typedef parsed type {:?}", res.type_);
        res.type_
    }).and_then(|type_| {
        nameres::resolve_path_with_str(&type_, &m.filepath, m.point, core::SearchType::ExactMatch,
                                       core::Namespace::TypeNamespace, session).nth(0)
    })
}


struct ExprTypeVisitor<'s> {
    scope: Scope,
    session: SessionRef<'s>,
    result: Option<Ty>,
}

impl<'s, 'v> visit::Visitor<'v> for ExprTypeVisitor<'s> {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        debug!("visit_expr {:?}", expr);
        //walk_expr(self, ex, e)
        match expr.node {
            ast::ExprUnary(_, ref expr) => {
                self.visit_expr(expr);
            }
            ast::ExprAddrOf(_, ref expr) => {
                self.visit_expr(expr);
            }
            ast::ExprPath(_, ref path) => {
                debug!("expr is a path {:?}", to_racer_path(path));
                self.result = resolve_ast_path(path,
                                 &self.scope.filepath,
                                 self.scope.point,
                                 self.session).and_then(|m| {
                                     let msrc = self.session.load_file_and_mask_comments(&m.filepath);
                                     typeinf::get_type_of_match(m, msrc, self.session)
                                 });
            }
            ast::ExprCall(ref callee_expression, _/*ref arguments*/) => {
                self.visit_expr(callee_expression);

                self.result = self.result.as_ref().and_then(|m|
                    match *m {
                        TyMatch(ref m) =>  {

                            match m.mtype {
                                MatchType::Function => typeinf::get_return_type_of_function(m, self.session)
                                    .and_then(|ty| path_to_match(ty, self.session)),
                                MatchType::Struct => Some(TyMatch(m.clone())),
                                _ => {
                                    debug!("ExprTypeVisitor: Cannot handle ExprCall of {:?} type", m.mtype);
                                    None
                                }
                            }
                        },
                        _ => None
                    }
                );
            }
            ast::ExprStruct(ref path, _, _) => {
                let pathvec = to_racer_path(path);
                self.result = find_type_match(&pathvec,
                                              &self.scope.filepath,
                                              self.scope.point,
                                              self.session);
            }

            ast::ExprMethodCall(ref spannedident, ref types, ref arguments) => {
                // spannedident.node is an ident I think
                let methodname = spannedident.node.name.to_string();
                debug!("method call ast name {}", methodname);
                debug!("method call ast types {:?} {}", types, types.len());

                let objexpr = &arguments[0];
                //println!("obj expr is {:?}",objexpr);
                self.visit_expr(objexpr);

                self.result = self.result.as_ref().and_then(|contextm| {
                    match *contextm {
                        TyMatch(ref contextm) => {
                            let omethod = nameres::search_for_impl_methods(
                                &contextm.matchstr,
                                &methodname,
                                contextm.point,
                                &contextm.filepath,
                                contextm.local,
                                core::SearchType::ExactMatch,
                                self.session).nth(0);
                            omethod
                                .and_then(|method|
                                          typeinf::get_return_type_of_function(&method, self.session))
                                .and_then(|ty| path_to_match_including_generics(ty, contextm, self.session))
                        }
                        _ => None
                    }
                });
            }

            ast::ExprField(ref subexpression, spannedident) => {
                let fieldname = spannedident.node.name.to_string();
                debug!("exprfield {}", fieldname);
                self.visit_expr(subexpression);
                self.result = self.result.as_ref()
                      .and_then(|structm|
                                match *structm {
                                    TyMatch(ref structm) => {
                                typeinf::get_struct_field_type(&fieldname, structm, self.session)
                                .and_then(|fieldtypepath|
                                          find_type_match_including_generics(&fieldtypepath,
                                                                             &structm.filepath,
                                                                             structm.point,
                                                                             structm,
                                                                             self.session))
                                    },
                                    _ => None
                                });
            }

            ast::ExprTup(ref exprs) => {
                let mut v = Vec::new();
                for expr in exprs {
                    self.visit_expr(expr);
                    match self.result {
                        Some(ref t) => v.push(t.clone()),
                        None => {
                            self.result = None;
                            return;
                        }
                    };
                }
                self.result = Some(TyTuple(v));
            }

            ast::ExprLit(_) => {
                self.result = Some(TyUnsupported);
            }

            _ => {
                println!("- Could not match expr node type: {:?}",expr.node);
            }
        }
    }
}

// gets generics info from the context match
fn path_to_match_including_generics(ty: Ty, contextm: &Match, session: SessionRef) -> Option<Ty> {
    match ty {
        TyPathSearch(ref fieldtypepath, ref scope) => {

            if fieldtypepath.segments.len() == 1 {
                // could be a generic arg! - try and resolve it
                let ref typename = fieldtypepath.segments[0].name;
                let it = contextm.generic_args.iter()
                    .zip(contextm.generic_types.iter());
                for (name, typesearch) in it {
                    if name == typename {
                        // yes! a generic type match!
                        return find_type_match(&typesearch.path,
                                               &typesearch.filepath,
                                               typesearch.point,
                                               session);
                    }
                }
            }

            find_type_match(fieldtypepath, &scope.filepath, scope.point, session)
        }
        _ => Some(ty)
    }
}


fn find_type_match_including_generics(fieldtype: &core::Ty,
                                      filepath: &Path,
                                      pos: usize,
                                      structm: &Match,
                                      session: SessionRef) -> Option<Ty>{
    assert_eq!(&structm.filepath, &filepath.to_path_buf());
    let fieldtypepath = match *fieldtype {
        TyPathSearch(ref path, _) => path,
        _ => {
            debug!("EXPECTING A PATH!! Cannot handle other types yet. {:?}", fieldtype);
            return None
        }
    };

    if fieldtypepath.segments.len() == 1 {
        // could be a generic arg! - try and resolve it
        let ref typename = fieldtypepath.segments[0].name;
        let it = structm.generic_args.iter()
            .zip(structm.generic_types.iter());
        for (name, typesearch) in it {
            if name == typename {
                // yes! a generic type match!
                return find_type_match(&typesearch.path,
                                       &typesearch.filepath,
                                       typesearch.point,
                                       session);
            }
        }
    }

    find_type_match(fieldtypepath, filepath, pos, session)
}


struct StructVisitor {
    pub scope: Scope,
    pub fields: Vec<(String, usize, Option<core::Ty>)>
}

impl<'v> visit::Visitor<'v> for StructVisitor {
    fn visit_struct_def(&mut self, struct_definition: &ast::StructDef, _: ast::Ident, _: &ast::Generics, _: ast::NodeId) {
        for field in &struct_definition.fields {
            let codemap::BytePos(point) = field.span.lo;

            match field.node.kind {
                ast::NamedField(name, _) => {
                    let ty = to_racer_ty(&*field.node.ty, &self.scope);
                    self.fields.push(((&name).to_string(), point as usize, ty));
                }
                ast::UnnamedField(_) => {
                    let ty = to_racer_ty(&*field.node.ty, &self.scope);
                    // name unnamed field by its ordinal, since self.0 works
                    let name = format!("{}",self.fields.len());
                    self.fields.push((name, point as usize, ty));
                }
            }
        }
    }
}

pub struct TypeVisitor {
    pub name: Option<String>,
    pub type_: Option<core::Path>
}

impl<'v> visit::Visitor<'v> for TypeVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ast::ItemTy(ref ty, _) = item.node {
            self.name = Some(item.ident.name.to_string());

            let typepath = match ty.node {
                ast::TyRptr(_, ref ty) => {
                    match ty.ty.node {
                        ast::TyPath(_, ref path) => {
                            let type_ = to_racer_path(path);
                            debug!("type type is {:?}", type_);
                            Some(type_)
                        }
                        _ => None
                    }
                }
                ast::TyPath(_, ref path) => {
                    let type_ = to_racer_path(path);
                    debug!("type type is {:?}", type_);
                    Some(type_)
                }
                _ => None
            };
            self.type_ = typepath;
            debug!("typevisitor type is {:?}", self.type_);
        }
    }
}

pub struct TraitVisitor {
    pub name: Option<String>
}

impl<'v> visit::Visitor<'v> for TraitVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ast::ItemTrait(_, _, _, _) = item.node {
            self.name = Some(item.ident.name.to_string());
        }
    }
}

#[derive(Debug)]
pub struct ImplVisitor {
    pub name_path: Option<core::Path>,
    pub trait_path: Option<core::Path>,
}

impl<'v> visit::Visitor<'v> for ImplVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ast::ItemImpl(_, _, _, ref otrait, ref typ, _) = item.node {
            match typ.node {
                ast::TyPath(_, ref path) => {
                    self.name_path = Some(to_racer_path(path));
                }
                ast::TyRptr(_, ref ty) => {
                    // HACK for now, treat refs the same as unboxed types
                    // so that we can match '&str' to 'str'
                    if let ast::TyPath(_, ref path) = ty.ty.node {
                        self.name_path = Some(to_racer_path(path));
                    }
                }
                _ => {}
            }
            otrait.as_ref().map(|ref t| {
                self.trait_path = Some(to_racer_path(&t.path));
            });
        }
    }
}

pub struct ModVisitor {
    pub name: Option<String>
}

impl<'v> visit::Visitor<'v> for ModVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ast::ItemMod(_) = item.node {
            self.name = Some((&item.ident.name).to_string());
        }
    }
}

pub struct ExternCrateVisitor {
    pub name: Option<String>,
    pub realname: Option<String>
}

impl<'v> visit::Visitor<'v> for ExternCrateVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ast::ItemExternCrate(ref optional_s) = item.node {
            self.name = Some((&item.ident.name).to_string());
            if let &Some(ref istr) = optional_s {
                self.realname = Some(istr.to_string());
            }
        }
    }
}


pub struct GenericsVisitor {
    pub generic_args: Vec<String>
}

impl<'v> visit::Visitor<'v> for GenericsVisitor {
    fn visit_generics(&mut self, g: &ast::Generics) {
        for ty in g.ty_params.iter() {
            self.generic_args.push((&ty.ident.name).to_string());
        }
    }
}

pub struct StructDefVisitor {
    pub name: Option<(String,usize)>,
    pub generic_args: Vec<String>
}

impl<'v> visit::Visitor<'v> for StructDefVisitor {
    fn visit_generics(&mut self, g: &ast::Generics) {
        for ty in g.ty_params.iter() {
            self.generic_args.push((&ty.ident.name).to_string());
        }
    }

    fn visit_ident(&mut self, _sp: codemap::Span, _ident: ast::Ident) {
        /*! Visit the idents */
        let codemap::BytePos(point) = _sp.lo;
        let name = (&_ident.name).to_string();
        self.name = Some((name,point as usize));
    }
}

pub struct EnumVisitor {
    pub name: String,
    pub values: Vec<(String, usize)>
}

impl<'v> visit::Visitor<'v> for EnumVisitor {
    fn visit_item(&mut self, i: &ast::Item) {
        if let ast::ItemEnum(ref enum_definition, _) = i.node {
            self.name = (&i.ident.name).to_string();
            //visitor.visit_generics(type_parameters, env.clone());
            //visit::walk_enum_def(self, enum_definition, type_parameters, e)

            let codemap::BytePos(point) = i.span.lo;
            let codemap::BytePos(point2) = i.span.hi;
            debug!("name point is {} {}", point, point2);

            for variant in &enum_definition.variants {
                let codemap::BytePos(point) = variant.span.lo;
                self.values.push(((&variant.node.name).to_string(), point as usize));
            }
        }
    }
}

pub fn parse_use(s: String) -> UseVisitor {
    let mut v = UseVisitor{ ident: None, paths: Vec::new(), is_glob: false };
    if let Some(cr) = string_to_crate(s) {
        visit::walk_crate(&mut v, &cr);
    }
    v
}

pub fn parse_pat_bind_stmt(s: String) -> Vec<(usize, usize)> {
    let mut v = PatBindVisitor{ ident_points: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.ident_points
}

pub fn parse_struct_fields(s: String, scope: Scope) -> Vec<(String, usize, Option<core::Ty>)> {
    let mut v = StructVisitor{ scope: scope, fields: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.fields
}

pub fn parse_impl(s: String) -> ImplVisitor {
    let mut v = ImplVisitor { name_path: None, trait_path: None };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_trait(s: String) -> TraitVisitor {
    let mut v = TraitVisitor { name: None };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_struct_def(s: String) -> StructDefVisitor {
    let mut v = StructDefVisitor { name: None, generic_args: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_generics(s: String) -> GenericsVisitor {
    let mut v = GenericsVisitor { generic_args: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_type(s: String) -> TypeVisitor {
    let mut v = TypeVisitor { name: None, type_: None };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_fn_args(s: String) -> Vec<(usize, usize)> {
    parse_pat_idents(s)
}

pub fn parse_pat_idents(s: String) -> Vec<(usize, usize)> {
    let mut v = PatVisitor{ ident_points: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        debug!("parse_pat_idents stmt is {:?}", stmt);
        visit::walk_stmt(&mut v, &*stmt);
        debug!("ident points are {:?}", v.ident_points);
    }
    v.ident_points
}


pub fn parse_fn_output(s: String, scope: Scope) -> Option<core::Ty> {
    let mut v = FnOutputVisitor { result: None, scope: scope };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.result
}

pub fn parse_fn_arg_type(s: String, argpos: usize, scope: Scope, session: SessionRef) -> Option<core::Ty> {
    debug!("parse_fn_arg {} |{}|", argpos, s);
    let mut v = FnArgTypeVisitor { argpos: argpos, scope: scope, result: None,
                                   session: session };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.result
}

pub fn parse_mod(s: String) -> ModVisitor {
    let mut v = ModVisitor { name: None };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_extern_crate(s: String) -> ExternCrateVisitor {
    let mut v = ExternCrateVisitor { name: None, realname: None };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn parse_enum(s: String) -> EnumVisitor {
    let mut v = EnumVisitor { name: String::new(), values: Vec::new() };
    if let Some(stmt) = string_to_stmt(s) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v
}

pub fn get_type_of(exprstr: String, fpath: &Path, pos: usize, session: SessionRef) -> Option<Ty> {
    let myfpath = fpath.clone();
    let startscope = Scope {
        filepath: myfpath.to_path_buf(),
        point: pos
    };

    let mut v = ExprTypeVisitor{ scope: startscope, result: None, session: session };

    if let Some(stmt) = string_to_stmt(exprstr) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.result
}

// pos points to an ident in the lhs of the stmtstr
pub fn get_let_type(stmtstr: String, pos: usize, scope: Scope, session: SessionRef) -> Option<Ty> {
    let mut v = LetTypeVisitor {
        scope: scope,
        session: session,
        srctxt: stmtstr.clone(),
        pos: pos, result: None
    };
    if let Some(stmt) = string_to_stmt(stmtstr) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.result
}

pub fn get_match_arm_type(stmtstr: String, pos: usize, scope: Scope, session: SessionRef) -> Option<Ty> {
    let mut v = MatchTypeVisitor {
        scope: scope,
        session: session,
        pos: pos,
        result: None
    };
    if let Some(stmt) = string_to_stmt(stmtstr) {
        visit::walk_stmt(&mut v, &*stmt);
    }
    v.result
}

pub struct FnOutputVisitor {
    scope: Scope,
    pub result: Option<Ty>
}

impl<'v> visit::Visitor<'v> for FnOutputVisitor {
    fn visit_fn(&mut self,  _: visit::FnKind, fd: &ast::FnDecl, _: &ast::Block, _: codemap::Span, _: ast::NodeId) {
        self.result = match fd.output {
            ast::Return(ref ty) => to_racer_ty(ty, &self.scope),
            ast::NoReturn(_) => None,
            ast::DefaultReturn(_) => None
        };
    }
}

pub struct FnArgTypeVisitor<'s> {
    argpos: usize,
    scope: Scope,
    session: SessionRef<'s>,
    pub result: Option<Ty>
}

impl<'s, 'v> visit::Visitor<'v> for FnArgTypeVisitor<'s> {
    fn visit_fn(&mut self, _: visit::FnKind, fd: &ast::FnDecl, _: &ast::Block, _: codemap::Span, _: ast::NodeId) {
        for arg in &fd.inputs {
            let codemap::BytePos(lo) = arg.pat.span.lo;
            let codemap::BytePos(hi) = arg.pat.span.hi;
            if self.argpos >= (lo as usize) && self.argpos <= (hi as usize) {
                debug!("fn arg visitor found type {:?}", arg.ty);
                self.result = to_racer_ty(&*arg.ty, &self.scope)
                    .and_then(|ty| destructure_pattern_to_ty(&*arg.pat, self.argpos,
                                                             &ty, &self.scope, self.session))
                    .and_then(|ty| path_to_match(ty, self.session));
                break;
            }
        }
    }
}


#[test]
fn ast_sandbox() {
    // let stmt = string_to_stmt(String::from_str(src));
    // println!("stmt {:?} ", stmt);
    // // get_match_arm_type(src.to_string(), 17, Scope {filepath: Path::new("./foo"), point: 0});
    // panic!("");

    //let src = "if let Foo(a) = b {}";

    //let src = "let (a,b): (Foo,Bar);";

    //let src = "let (a,b) = (2,3);";

    // let src = "let (a,b) : (usize,usize);";

    // //let src = "fn foo(a: int, b: |int|->int) {};";

    // let src = "let myvar = |blah| {};";

    // let src = "(myvar, foo) = (3,4);";

    // let src = "fn myfn((a,b) : (usize, usize)) {}";
    // //let src = "impl blah {pub fn another_method() {}}";

    // let stmt = string_to_stmt(String::from_str(src));
    // println!("stmt {} ", stmt);
    // panic!("");
    // let mut v = PatVisitor { ident_points: Vec::new() };
    // visit::walk_stmt(&mut v, &*stmt);

    // panic!("BLAH {}",v.result);

    // let mut v = LetTypeVisitor { scope: Scope {filepath: Path::new("./foo"), point: 0 },
    //                           srctxt: src.to_string(),
    //                           pos: 5, result: None
    // };
    // visit::walk_stmt(&mut v, &*stmt);

    // let out = parse_let2(src.to_string());

    // for &(l,h) in out.iter() {
    //     println!("{}",src.slice(l,h));
    // }

    // let src = "let (a,b) : (usize,usize);";

    // let result = task::try(move || {
    //     return string_to_stmt(String::from_str(src));
    // });


    //println!("out {} ", result);
    //panic!();

    //parse_let("let l : Vec<Blah>;".to_string(), Path::new("./ast.rs"), 0, true);
    //parse_let("let l = Vec<Blah>::new();".to_string(), Path::new("./ast.rs"), 0, true);

    //get_type_of("let l : Vec<Blah>;".to_string(), &Path::new("./ast.rs"), 0);
    //panic!();


    // let src = "pub struct Foo<T>;";
    // let s = parse_generics(src.to_string());
    // println!("out {} ", s.generic_args);
    // let stmt = string_to_stmt(String::from_str(src));
    // println!("stmt is {:?}",stmt);
    // panic!();
    // // let mut v = LetVisitor{ scope: Scope {filepath: Path::new("./foo"), point: 0} , result: None, parseinit: true};
    // let mut v = StructDefVisitor {nam}
    // visit::walk_stmt(&mut v, &*stmt, ());

    // println!("{:?}", stmt);
    // panic!();
    // println!("{}", v.name);
    // panic!("");
    // let mut v = ExprTypeVisitor{ scope: startscope,
    //                              result: None};


    // let src = ~"fn myfn(a: usize) -> Foo {}";
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

    // println!("v {} {}",v.ident, v.paths);

    //visit::walk_stmt(&mut v, stmt, ());

    // println!("stmt {:?}",stmt);


    // let mut v = ImplVisitor{ name_path: Vec::new() };
    // visit::walk_stmt(&mut v, stmt, ());


    // println!("v {}",v.name_path);

// pub struct Match {
//     pub matchstr: ~str,
//     pub filepath: Path,
//     pub point: usize,
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
    //     mtype: core::Module
    // };

    // let startscope = Scope {
    //     filepath: Path::new("./ast.rs"),
    //     point: 0
    // };

    // let mut v = ExprTypeVisitor{ scope: startscope,
    //                              result: None};
    // visit::walk_stmt(&mut v, stmt, ());

    // println!("result was {:?}",v.result);
    //return v.result;

    // let mut v = EnumVisitor { name: String::new(), values: Vec::new()};
    // visit::walk_stmt(&mut v, cr, ());
    // println!("{} {}", v.name, v.values);

    // let src = "let v = Foo::new();".to_owned();

    // let res = parse_let(src);
    // debug!("res {}",res.unwrap().init);
}
