use ast_types::Path as RacerPath;
use ast_types::{self, GenericsArgs, ImplHeader, Pat, PathAlias, PathAliasKind, TraitBounds, Ty};
use core::{self, BytePos, ByteRange, Match, MatchType, Scope, Session, SessionExt};
use nameres::{self, resolve_path_with_str};
use scopes;
use typeinf;

use std::path::Path;
use std::rc::Rc;

use syntax::ast::{
    self, ExprKind, FunctionRetTy, ItemKind, LitKind, PatKind, TyKind, UseTree, UseTreeKind,
};
use syntax::errors::{emitter::ColorConfig, Handler};
use syntax::parse::parser::Parser;
use syntax::parse::{self, ParseSess};
use syntax::source_map::{self, FileName, SourceMap, Span};
use syntax::{self, visit};

/// construct parser from string
// From syntax/util/parser_testing.rs
pub fn string_to_parser(ps: &ParseSess, source_str: String) -> Parser {
    parse::new_parser_from_source_str(ps, FileName::Custom("racer-file".to_owned()), source_str)
}

/// Get parser from string s and then apply closure f to it
// TODO: use Result insated of Option
pub fn with_error_checking_parse<F, T>(s: String, f: F) -> Option<T>
where
    F: FnOnce(&mut Parser) -> Option<T>,
{
    syntax::with_globals(|| {
        let codemap = Rc::new(SourceMap::new(source_map::FilePathMapping::empty()));
        // setting of how we display errors in console
        // here we set can_emit_warnings=false, treat_err_as_bug=false
        let handler =
            Handler::with_tty_emitter(ColorConfig::Never, false, false, Some(codemap.clone()));
        let parse_sess = ParseSess::with_span_handler(handler, codemap);

        let mut p = string_to_parser(&parse_sess, s);
        f(&mut p)
    })
}

/// parse string source_str as statement and then apply f to it
/// return false if we can't parse s as statement
// TODO: make F FnOnce(&ast::Stmt) -> Result<Something, Error>
pub fn with_stmt<F>(source_str: String, f: F) -> bool
where
    F: FnOnce(&ast::Stmt),
{
    with_error_checking_parse(source_str, |p| {
        let stmt = match p.parse_stmt() {
            Ok(Some(stmt)) => stmt,
            _ => return None,
        };
        f(&stmt);
        Some(())
    }).is_some()
}

pub(crate) fn destruct_span(span: Span) -> (u32, u32) {
    let source_map::BytePos(lo) = span.lo();
    let source_map::BytePos(hi) = span.hi();
    (lo, hi)
}

pub(crate) fn get_span_start(span: Span) -> u32 {
    let source_map::BytePos(lo) = span.lo();
    lo
}

/// collect paths from syntax::ast::UseTree
#[derive(Debug)]
pub struct UseVisitor {
    pub path_list: Vec<PathAlias>,
    pub contains_glob: bool,
}

impl<'ast> visit::Visitor<'ast> for UseVisitor {
    fn visit_item(&mut self, i: &ast::Item) {
        // collect items from use tree recursively
        // returns (Paths, contains_glab)
        fn collect_nested_items(
            use_tree: &UseTree,
            parent_path: Option<&ast_types::Path>,
        ) -> (Vec<PathAlias>, bool) {
            let mut res = vec![];
            let mut path = if let Some(parent) = parent_path {
                let relative_path = RacerPath::from_ast_nogen(&use_tree.prefix);
                let mut path = parent.clone();
                path.extend(relative_path);
                path
            } else {
                RacerPath::from_ast_nogen(&use_tree.prefix)
            };
            let mut contains_glob = false;
            match use_tree.kind {
                UseTreeKind::Simple(_, _, _) => {
                    let ident = use_tree.ident().name.to_string();
                    let kind = if let Some(last_seg) = path.segments.last() {
                        //` self` is treated normaly in libsyntax,
                        //  but we distinguish it here to make completion easy
                        if last_seg.name == "self" {
                            PathAliasKind::Self_(ident)
                        } else {
                            PathAliasKind::Ident(ident)
                        }
                    } else {
                        PathAliasKind::Ident(ident)
                    };
                    if let PathAliasKind::Self_(_) = kind {
                        path.segments.pop();
                    }
                    res.push(PathAlias { kind, path });
                }
                UseTreeKind::Nested(ref nested) => {
                    nested.iter().for_each(|(ref tree, _)| {
                        let (items, has_glob) = collect_nested_items(tree, Some(&path));
                        res.extend(items);
                        contains_glob |= has_glob;
                    });
                }
                UseTreeKind::Glob => {
                    res.push(PathAlias {
                        kind: PathAliasKind::Glob,
                        path,
                    });
                    contains_glob = true;
                }
            }
            (res, contains_glob)
        }
        if let ItemKind::Use(ref use_tree) = i.node {
            let (path_list, contains_glob) = collect_nested_items(use_tree, None);
            self.path_list = path_list;
            self.contains_glob = contains_glob;
        }
    }
}

pub struct PatBindVisitor {
    ident_points: Vec<ByteRange>,
}

impl<'ast> visit::Visitor<'ast> for PatBindVisitor {
    fn visit_local(&mut self, local: &ast::Local) {
        // don't visit the RHS (init) side of the let stmt
        self.visit_pat(&local.pat);
    }

    fn visit_expr(&mut self, ex: &ast::Expr) {
        // don't visit the RHS or block of an 'if let' or 'for' stmt
        match ex.node {
            ExprKind::IfLet(ref pat, ..) | ExprKind::WhileLet(ref pat, ..) => {
                pat.iter().for_each(|pat| self.visit_pat(pat))
            }
            ExprKind::ForLoop(ref pat, ..) => self.visit_pat(pat),
            _ => visit::walk_expr(self, ex),
        }
    }

    fn visit_pat(&mut self, p: &ast::Pat) {
        match p.node {
            PatKind::Ident(_, ref spannedident, _) => {
                self.ident_points.push(spannedident.span.into());
            }
            _ => {
                visit::walk_pat(self, p);
            }
        }
    }
}

pub struct PatVisitor {
    ident_points: Vec<ByteRange>,
}

impl<'ast> visit::Visitor<'ast> for PatVisitor {
    fn visit_pat(&mut self, p: &ast::Pat) {
        match p.node {
            PatKind::Ident(_, ref spannedident, _) => {
                self.ident_points.push(spannedident.span.into());
            }
            _ => {
                visit::walk_pat(self, p);
            }
        }
    }
}

pub struct PatAndGenVisitor<'f> {
    ident_points: Vec<ByteRange>,
    generics: GenericsArgs,
    fpath: &'f Path,
    offset: i32,
}

impl<'ast, 'f> visit::Visitor<'ast> for PatAndGenVisitor<'f> {
    fn visit_pat(&mut self, p: &ast::Pat) {
        match p.node {
            PatKind::Ident(_, ref spannedident, _) => {
                self.ident_points.push(spannedident.span.into());
            }
            _ => {
                visit::walk_pat(self, p);
            }
        }
    }
    fn visit_generics(&mut self, g: &'ast ast::Generics) {
        let generics = GenericsArgs::from_generics(g, self.fpath, self.offset);
        self.generics.extend(generics);
    }
}

fn point_is_in_span(point: BytePos, span: &Span) -> bool {
    let point: u32 = point.0 as u32;
    let (lo, hi) = destruct_span(*span);
    point >= lo && point < hi
}

// The point must point to an ident within the pattern.
fn destructure_pattern_to_ty(
    pat: &ast::Pat,
    point: BytePos,
    ty: &Ty,
    scope: &Scope,
    session: &Session,
) -> Option<Ty> {
    debug!(
        "destructure_pattern_to_ty point {:?} ty {:?} pat: {:?}",
        point, ty, pat.node
    );
    match pat.node {
        PatKind::Ident(_, ref spannedident, _) => {
            if point_is_in_span(point, &spannedident.span) {
                debug!("destructure_pattern_to_ty matched an ident!");
                Some(ty.clone())
            } else {
                panic!(
                    "Expecting the point to be in the patident span. pt: {:?}",
                    point
                );
            }
        }
        PatKind::Tuple(ref tuple_elements, _) => match *ty {
            Ty::Tuple(ref typeelems) => {
                let mut res = None;
                for (i, p) in tuple_elements.iter().enumerate() {
                    if !point_is_in_span(point, &p.span) {
                        continue;
                    }
                    if let Some(ref ty) = typeelems[i] {
                        res = destructure_pattern_to_ty(p, point, ty, scope, session);
                        break;
                    }
                }
                res
            }
            _ => panic!("Expecting TyTuple"),
        },
        PatKind::TupleStruct(ref path, ref children, _) => {
            let m = resolve_ast_path(path, &scope.filepath, scope.point, session);
            let contextty = path_to_match(ty.clone(), session);
            if let Some(m) = m {
                let mut res = None;

                for (i, p) in children.iter().enumerate() {
                    if point_is_in_span(point, &p.span) {
                        res = typeinf::get_tuplestruct_field_type(i, &m, session)
                            .and_then(|ty|
                                // if context ty is a match, use its generics
                                if let Some(Ty::Match(ref contextmatch)) = contextty {
                                    path_to_match_including_generics(ty, contextmatch, session)
                                } else {
                                    path_to_match(ty, session)
                                })
                            .and_then(|ty| destructure_pattern_to_ty(p, point, &ty, scope, session));

                        break;
                    }
                }
                res
            } else {
                None
            }
        }
        PatKind::Struct(ref path, ref children, _) => {
            let m = resolve_ast_path(path, &scope.filepath, scope.point, session);
            let contextty = path_to_match(ty.clone(), session);
            if let Some(m) = m {
                let mut res = None;

                for child in children {
                    if point_is_in_span(point, &child.span) {
                        res = typeinf::get_struct_field_type(
                            &child.node.ident.name.as_str(),
                            &m,
                            session,
                        ).and_then(|ty| {
                            if let Some(Ty::Match(ref contextmatch)) = contextty {
                                path_to_match_including_generics(ty, contextmatch, session)
                            } else {
                                path_to_match(ty, session)
                            }
                        }).and_then(|ty| {
                            destructure_pattern_to_ty(&child.node.pat, point, &ty, scope, session)
                        });

                        break;
                    }
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

struct LetTypeVisitor<'c: 's, 's> {
    scope: Scope,
    session: &'s Session<'c>,
    srctxt: String,
    pos: BytePos, // pos is relative to the srctxt, scope is global
    result: Option<Ty>,
}

impl<'c, 's, 'ast> visit::Visitor<'ast> for LetTypeVisitor<'c, 's> {
    fn visit_expr(&mut self, ex: &ast::Expr) {
        match ex.node {
            ExprKind::IfLet(ref pattern, ref expr, _, _)
            | ExprKind::WhileLet(ref pattern, ref expr, _, _) => {
                let mut v = ExprTypeVisitor::new(self.scope.clone(), self.session);
                v.visit_expr(expr);
                // TODO: too ugly
                self.result = v
                    .result
                    .and_then(|ty| {
                        pattern
                            .iter()
                            .filter_map(|pat| {
                                destructure_pattern_to_ty(
                                    pat,
                                    self.pos,
                                    &ty,
                                    &self.scope,
                                    self.session,
                                )
                            }).nth(0)
                    }).and_then(|ty| path_to_match(ty, self.session));
            }
            _ => visit::walk_expr(self, ex),
        }
    }

    fn visit_local(&mut self, local: &ast::Local) {
        let mut ty = None;
        if let Some(ref local_ty) = local.ty {
            ty = Ty::from_ast(local_ty, &self.scope);
        }

        if ty.is_none() {
            // oh, no type in the let expr. Try evalling the RHS
            ty = local.init.as_ref().and_then(|initexpr| {
                debug!("init node is {:?}", initexpr.node);
                let mut v = ExprTypeVisitor::new(self.scope.clone(), self.session);
                v.visit_expr(initexpr);
                v.result
            });
        }

        debug!(
            "LetTypeVisitor: ty is {:?}. pos is {:?}, src is |{}|",
            ty, self.pos, self.srctxt
        );
        self.result = ty
            .and_then(|ty| {
                destructure_pattern_to_ty(&local.pat, self.pos, &ty, &self.scope, self.session)
            }).and_then(|ty| path_to_match(ty, self.session));
    }
}

struct MatchTypeVisitor<'c: 's, 's> {
    scope: Scope,
    session: &'s Session<'c>,
    pos: BytePos, // pos is relative to the srctxt, scope is global
    result: Option<Ty>,
}

impl<'c, 's, 'ast> visit::Visitor<'ast> for MatchTypeVisitor<'c, 's> {
    fn visit_expr(&mut self, ex: &ast::Expr) {
        if let ExprKind::Match(ref subexpression, ref arms) = ex.node {
            debug!("PHIL sub expr is {:?}", subexpression);

            let mut v = ExprTypeVisitor::new(self.scope.clone(), self.session);
            v.visit_expr(subexpression);

            debug!("PHIL sub type is {:?}", v.result);

            for arm in arms {
                for pattern in &arm.pats {
                    if point_is_in_span(self.pos, &pattern.span) {
                        debug!("PHIL point is in pattern |{:?}|", pattern);
                        self.result = v
                            .result
                            .as_ref()
                            .and_then(|ty| {
                                destructure_pattern_to_ty(
                                    pattern,
                                    self.pos,
                                    ty,
                                    &self.scope,
                                    self.session,
                                )
                            }).and_then(|ty| path_to_match(ty, self.session));
                    }
                }
            }
        }
    }
}

fn resolve_ast_path(
    path: &ast::Path,
    filepath: &Path,
    pos: BytePos,
    session: &Session,
) -> Option<Match> {
    let scope = Scope::new(filepath.to_owned(), pos);
    let path = RacerPath::from_ast(path, &scope);
    debug!("resolve_ast_path {:?} {:?}", path, scope);
    nameres::resolve_path_with_str(
        &path,
        filepath,
        pos,
        core::SearchType::ExactMatch,
        core::Namespace::Both,
        session,
    ).nth(0)
}

fn path_to_match(ty: Ty, session: &Session) -> Option<Ty> {
    match ty {
        Ty::PathSearch(paths) => {
            find_type_match(&paths.path, &paths.filepath, paths.point, session).map(Ty::Match)
        }
        Ty::RefPtr(ty) => path_to_match(*ty, session),
        _ => Some(ty),
    }
}

pub(crate) fn find_type_match(
    path: &RacerPath,
    fpath: &Path,
    pos: BytePos,
    session: &Session,
) -> Option<Match> {
    debug!("find_type_match {:?}, {:?}", path, fpath);
    let mut res = resolve_path_with_str(
        path,
        fpath,
        pos,
        core::SearchType::ExactMatch,
        core::Namespace::Type,
        session,
    ).nth(0)
    .and_then(|m| match m.mtype {
        MatchType::Type => get_type_of_typedef(&m, session),
        _ => Some(m),
    })?;
    // TODO: 'Type' support
    // if res is Enum/Struct and has a generic type paramter, let's resolve it.
    for (param, typ) in res.generics_mut().zip(path.generic_types()) {
        param.resolve(typ.to_owned());
    }
    Some(res)
}

pub(crate) fn get_type_of_typedef(m: &Match, session: &Session) -> Option<Match> {
    debug!("get_type_of_typedef match is {:?}", m);
    let msrc = session.load_source_file(&m.filepath);
    let blobstart = m.point - BytePos(5); // 5 == "type ".len()
    let blob = msrc.get_src_from_start(blobstart);
    blob.iter_stmts()
        .nth(0)
        .and_then(|range| {
            let blob = msrc[range.shift(blobstart).to_range()].to_owned();
            debug!("get_type_of_typedef blob string {}", blob);
            let scope = Scope::new(m.filepath.clone(), range.start);
            let res = parse_type(blob, &scope);
            debug!("get_type_of_typedef parsed type {:?}", res.type_);
            res.type_
        }).and_then(|type_| {
            let src = session.load_source_file(&m.filepath);
            let scope_start = scopes::scope_start(src.as_src(), m.point);

            // Type of TypeDef cannot be inside the impl block so look outside
            let outer_scope_start = scope_start
                .0
                .checked_sub(1)
                .map(|sub| scopes::scope_start(src.as_src(), sub.into()))
                .and_then(|s| {
                    let blob = src.get_src_from_start(s);
                    let blob = blob.trim_left();
                    if blob.starts_with("impl")
                        || blob.starts_with("trait")
                        || blob.starts_with("pub trait")
                    {
                        Some(s)
                    } else {
                        None
                    }
                });

            nameres::resolve_path_with_str(
                &type_,
                &m.filepath,
                outer_scope_start.unwrap_or(scope_start),
                core::SearchType::ExactMatch,
                core::Namespace::Type,
                session,
            ).nth(0)
        })
}

struct ExprTypeVisitor<'c: 's, 's> {
    scope: Scope,
    session: &'s Session<'c>,
    // what we have before calling typeinf::get_type_of_match
    path_match: Option<Match>,
    result: Option<Ty>,
}

impl<'c: 's, 's> ExprTypeVisitor<'c, 's> {
    fn new(scope: Scope, session: &'s Session<'c>) -> Self {
        ExprTypeVisitor {
            scope,
            session,
            path_match: None,
            result: None,
        }
    }
    fn same_scope(&self) -> Self {
        Self {
            scope: self.scope.clone(),
            session: self.session,
            path_match: None,
            result: None,
        }
    }
}

impl<'c, 's, 'ast> visit::Visitor<'ast> for ExprTypeVisitor<'c, 's> {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        println!(
            "ExprTypeVisitor::visit_expr {:?}(kind: {:?})",
            expr, expr.node
        );
        //walk_expr(self, ex, e)
        match expr.node {
            ExprKind::Unary(_, ref expr) | ExprKind::AddrOf(_, ref expr) => {
                self.visit_expr(expr);
            }
            ExprKind::Path(_, ref path) => {
                let source_map::BytePos(lo) = path.span.lo();
                self.result = resolve_ast_path(
                    path,
                    &self.scope.filepath,
                    self.scope.point + lo.into(),
                    self.session,
                ).and_then(|m| {
                    let msrc = self.session.load_source_file(&m.filepath);
                    self.path_match = Some(m.clone());
                    typeinf::get_type_of_match(m, msrc.as_src(), self.session)
                });
            }
            ExprKind::Call(ref callee_expression, ref caller_expr) => {
                self.visit_expr(callee_expression);
                self.result = self.result.take().and_then(|m| {
                    if let Ty::Match(mut m) = m {
                        match m.mtype {
                            MatchType::Function | MatchType::Method(_) => {
                                typeinf::get_return_type_of_function(&m, &m, self.session)
                                    .and_then(|ty| path_to_match(ty, self.session))
                            }
                            // if we find tuple struct / enum variant, try to resolve its generics name
                            MatchType::Struct(ref mut gen) | MatchType::Enum(ref mut gen) => {
                                if gen.is_empty() {
                                    return Some(Ty::Match(m));
                                }
                                let tuple_fields = match self.path_match {
                                    Some(ref m) => typeinf::get_tuplestruct_fields(m, self.session),
                                    None => return Some(Ty::Match(m)),
                                };
                                // search what is in callee e.g. Some(String::new()<-) for generics
                                for ((_, _, ty), expr) in tuple_fields.into_iter().zip(caller_expr)
                                {
                                    let ty = try_continue!(ty).dereference();
                                    if let Ty::PathSearch(paths) = ty {
                                        let (id, _) =
                                            try_continue!(gen.search_param_by_path(&paths.path));
                                        let mut visitor = self.same_scope();
                                        visitor.visit_expr(expr);
                                        if let Some(ty) = visitor.result {
                                            gen.0[id].resolve(ty.dereference());
                                        }
                                    }
                                }
                                Some(Ty::Match(m))
                            }
                            _ => {
                                debug!(
                                    "ExprTypeVisitor: Cannot handle ExprCall of {:?} type",
                                    m.mtype
                                );
                                None
                            }
                        }
                    } else {
                        None
                    }
                });
            }
            ExprKind::Struct(ref path, _, _) => {
                let pathvec = RacerPath::from_ast(path, &self.scope);
                self.result = find_type_match(
                    &pathvec,
                    &self.scope.filepath,
                    self.scope.point,
                    self.session,
                ).map(Ty::Match);
            }
            ExprKind::MethodCall(ref method_def, ref arguments) => {
                let methodname = method_def.ident.name.as_str();
                debug!("method call ast name {}", methodname);

                // arguments[0] is receiver(e.g. self)
                let objexpr = &arguments[0];
                self.visit_expr(objexpr);

                self.result = self.result.as_ref().and_then(|contextm| match contextm {
                    Ty::Match(contextm) => {
                        let omethod = nameres::search_for_impl_methods(
                            contextm,
                            &methodname,
                            contextm.point,
                            &contextm.filepath,
                            contextm.local,
                            core::SearchType::ExactMatch,
                            self.session,
                        );
                        omethod
                            .map(|method| {
                                typeinf::get_return_type_of_function(
                                    &method,
                                    contextm,
                                    self.session,
                                )
                            }).filter_map(|ty| {
                                ty.and_then(|ty| {
                                    path_to_match_including_generics(ty, contextm, self.session)
                                })
                            }).nth(0)
                    }
                    _ => None,
                });
            }
            ExprKind::Field(ref subexpression, spannedident) => {
                let fieldname = spannedident.name.to_string();
                debug!("exprfield {}", fieldname);
                self.visit_expr(subexpression);
                self.result = self.result.take().and_then(|structm| match structm {
                    Ty::Match(structm) => {
                        typeinf::get_struct_field_type(&fieldname, &structm, self.session).and_then(
                            |fieldtypepath| {
                                find_type_match_including_generics(
                                    fieldtypepath,
                                    &structm.filepath,
                                    structm.point,
                                    &structm,
                                    self.session,
                                )
                            },
                        )
                    }
                    _ => None,
                });
            }
            ExprKind::Tup(ref exprs) => {
                let mut v = Vec::new();
                for expr in exprs {
                    self.visit_expr(expr);
                    v.push(self.result.take());
                }
                self.result = Some(Ty::Tuple(v));
            }
            ExprKind::Lit(ref lit) => {
                let ty_path = match lit.node {
                    LitKind::Str(_, _) => Some(RacerPath::from_vec(false, vec!["str"])),
                    // See https://github.com/phildawes/racer/issues/727 for
                    // information on why other literals aren't supported.
                    _ => None,
                };

                self.result = if let Some(lit_path) = ty_path {
                    find_type_match(
                        &lit_path,
                        &self.scope.filepath,
                        self.scope.point,
                        self.session,
                    ).map(Ty::Match)
                } else {
                    Some(Ty::Unsupported)
                };
            }
            ExprKind::Try(ref expr) => {
                self.visit_expr(&expr);
                debug!("ExprKind::Try result: {:?} expr: {:?}", self.result, expr);
                self.result = if let Some(&Ty::Match(ref m)) = self.result.as_ref() {
                    // HACK for speed up (kngwyu)
                    // Yeah there're many corner cases but it'll work well in most cases
                    if m.matchstr == "Result" || m.matchstr == "Option" {
                        debug!("Option or Result: {:?}", m);
                        m.resolved_generics().next().map(|x| x.to_owned())
                    } else {
                        debug!("Unable to desugar Try expression; type was {:?}", m);
                        None
                    }
                } else {
                    None
                };
            }
            ExprKind::Match(_, ref arms) => {
                debug!("match expr");

                for arm in arms {
                    self.visit_expr(&arm.body);

                    // All match arms need to return the same result, so if we found a result
                    // we can end the search.
                    if self.result.is_some() {
                        break;
                    }
                }
            }
            ExprKind::If(_, ref block, ref else_block)
            | ExprKind::IfLet(_, _, ref block, ref else_block) => {
                debug!("if/iflet expr");
                if let Some(stmt) = block.stmts.last() {
                    visit::walk_stmt(self, stmt);
                }
                if self.result.is_some() {
                    return;
                }
                // if the block does not resolve to a type, try the else block
                if let Some(expr) = else_block {
                    self.visit_expr(expr);
                }
            }
            ExprKind::Block(ref block, ref _label) => {
                debug!("block expr");
                if let Some(stmt) = block.stmts.last() {
                    visit::walk_stmt(self, stmt);
                }
            }
            ExprKind::Index(ref body, ref _index) => {
                self.visit_expr(body);
                // TODO(kngwyu) now we don't have support for literal so don't parse index
                // but in the future, we should handle index's type
                println!("{:?}", self.result);
                self.result = self
                    .result
                    .take()
                    .and_then(|ty| typeinf::get_type_of_indexed_value(ty, self.session));
            }
            ExprKind::Mac(ref m) => {
                if let Some(name) = m.node.path.segments.last().map(|seg| seg.ident) {
                    // use some ad-hoc rules
                    if name.as_str() == "vec" {
                        let path = RacerPath::from_iter(
                            true,
                            ["std", "vec", "Vec"].into_iter().map(|s| s.to_string()),
                        );
                        self.result = find_type_match(
                            &path,
                            &self.scope.filepath,
                            self.scope.point,
                            self.session,
                        ).map(Ty::Match);
                    }
                }
            }
            _ => {
                debug!("- Could not match expr node type: {:?}", expr.node);
            }
        };
    }

    fn visit_mac(&mut self, mac: &ast::Mac) {
        // Just do nothing if we see a macro, but also prevent the panic! in the default impl.
        debug!("ignoring visit_mac: {:?}", mac);
    }
}

// gets generics info from the context match
fn path_to_match_including_generics(ty: Ty, contextm: &Match, session: &Session) -> Option<Ty> {
    debug!("path_to_match_including_generics: {:?}  {:?}", ty, contextm);
    match ty {
        Ty::PathSearch(ref paths) => {
            let fieldtypepath = &paths.path;
            if fieldtypepath.segments.len() == 1 {
                let typename = &fieldtypepath.segments[0].name;
                // could have generic args! - try and resolve them
                for type_param in contextm.generics() {
                    let resolved = try_continue!(type_param.resolved());
                    if type_param.name() == typename {
                        return Some(resolved.to_owned());
                    }
                }
            }
            find_type_match(&fieldtypepath, &paths.filepath, paths.point, session).map(Ty::Match)
        }
        _ => Some(ty),
    }
}

fn find_type_match_including_generics(
    fieldtype: Ty,
    filepath: &Path,
    pos: BytePos,
    structm: &Match,
    session: &Session,
) -> Option<Ty> {
    assert_eq!(&structm.filepath, filepath);
    let fieldtypepath = match fieldtype {
        Ty::PathSearch(paths) => paths.path,
        Ty::RefPtr(ty) => match ty.dereference() {
            Ty::PathSearch(paths) => paths.path,
            Ty::Match(m) => return Some(Ty::Match(m)),
            _ => return None,
        },
        // already resolved
        Ty::Match(m) => return Some(Ty::Match(m)),
        _ => {
            return None;
        }
    };
    let generics = match &structm.mtype {
        MatchType::Struct(gen) => gen,
        _ => return None,
    };
    if fieldtypepath.segments.len() == 1 {
        // could be a generic arg! - try and resolve it
        if let Some((_, param)) = generics.search_param_by_path(&fieldtypepath) {
            if let Some(res) = param.resolved() {
                return Some(res.to_owned());
            }
            let mut m = param.to_owned().into_match();
            m.local = structm.local;
            return Some(Ty::Match(m));
        }
    }

    find_type_match(&fieldtypepath, filepath, pos, session).map(Ty::Match)
}

struct StructVisitor {
    pub scope: Scope,
    pub fields: Vec<(String, BytePos, Option<Ty>)>,
}

impl<'ast> visit::Visitor<'ast> for StructVisitor {
    fn visit_variant_data(
        &mut self,
        struct_definition: &ast::VariantData,
        _: ast::Ident,
        _: &ast::Generics,
        _: ast::NodeId,
        _: Span,
    ) {
        for field in struct_definition.fields() {
            let source_map::BytePos(point) = field.span.lo();

            let ty = Ty::from_ast(&field.ty, &self.scope);
            let name = match field.ident {
                Some(ref ident) => ident.to_string(),
                // name unnamed field by its ordinal, since self.0 works
                None => format!("{}", self.fields.len()),
            };

            self.fields.push((name, point.into(), ty));
        }
    }
}

#[derive(Debug)]
pub struct TypeVisitor<'s> {
    pub name: Option<String>,
    pub type_: Option<RacerPath>,
    scope: &'s Scope,
}

impl<'ast, 's> visit::Visitor<'ast> for TypeVisitor<'s> {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Ty(ref ty, _) = item.node {
            self.name = Some(item.ident.name.to_string());

            let typepath = match ty.node {
                TyKind::Rptr(_, ref ty) => match ty.ty.node {
                    TyKind::Path(_, ref path) => {
                        let type_ = RacerPath::from_ast(path, self.scope);
                        debug!("type type is {:?}", type_);
                        Some(type_)
                    }
                    _ => None,
                },
                TyKind::Path(_, ref path) => {
                    let type_ = RacerPath::from_ast(path, self.scope);
                    debug!("type type is {:?}", type_);
                    Some(type_)
                }
                _ => None,
            };
            self.type_ = typepath;
            debug!("typevisitor type is {:?}", self.type_);
        }
    }
}

pub struct TraitVisitor {
    pub name: Option<String>,
}

impl<'ast> visit::Visitor<'ast> for TraitVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Trait(..) = item.node {
            self.name = Some(item.ident.name.to_string());
        }
    }
}

#[derive(Debug)]
pub struct ImplVisitor<'p> {
    pub result: Option<ImplHeader>,
    filepath: &'p Path,
    offset: BytePos,
    block_start: BytePos, // the point { appears
    local: bool,
}

impl<'p> ImplVisitor<'p> {
    fn new(filepath: &'p Path, offset: BytePos, local: bool, block_start: BytePos) -> Self {
        ImplVisitor {
            result: None,
            filepath,
            offset,
            block_start,
            local,
        }
    }
}

impl<'ast, 'p> visit::Visitor<'ast> for ImplVisitor<'p> {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Impl(_, _, _, ref generics, ref otrait, ref self_typ, _) = item.node {
            let impl_start = self.offset + get_span_start(item.span).into();
            self.result = ImplHeader::new(
                generics,
                self.filepath,
                otrait,
                self_typ,
                self.offset,
                self.local,
                impl_start,
                self.block_start,
            );
        }
    }
}

pub struct ModVisitor {
    pub name: Option<String>,
}

impl<'ast> visit::Visitor<'ast> for ModVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Mod(_) = item.node {
            self.name = Some(item.ident.name.to_string());
        }
    }
}

pub struct ExternCrateVisitor {
    pub name: Option<String>,
    pub realname: Option<String>,
}

impl<'ast> visit::Visitor<'ast> for ExternCrateVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::ExternCrate(ref optional_s) = item.node {
            self.name = Some(item.ident.name.to_string());
            if let Some(ref istr) = *optional_s {
                self.realname = Some(istr.to_string());
            }
        }
    }
}

#[derive(Debug)]
struct GenericsVisitor<P> {
    result: GenericsArgs,
    filepath: P,
}

impl<'ast, P: AsRef<Path>> visit::Visitor<'ast> for GenericsVisitor<P> {
    fn visit_generics(&mut self, g: &ast::Generics) {
        let path = &self.filepath;
        if !self.result.0.is_empty() {
            warn!("[visit_generics] called for multiple generics!");
        }
        self.result.extend(GenericsArgs::from_generics(g, path, 0));
    }
}

pub struct EnumVisitor {
    pub name: String,
    pub values: Vec<(String, BytePos)>,
}

impl<'ast> visit::Visitor<'ast> for EnumVisitor {
    fn visit_item(&mut self, i: &ast::Item) {
        if let ItemKind::Enum(ref enum_definition, _) = i.node {
            self.name = i.ident.name.to_string();
            //visitor.visit_generics(type_parameters, env.clone());
            //visit::walk_enum_def(self, enum_definition, type_parameters, e)

            let (point1, point2) = destruct_span(i.span);
            debug!("name point is {} {}", point1, point2);

            for variant in &enum_definition.variants {
                let source_map::BytePos(point) = variant.span.lo();
                self.values
                    .push((variant.node.ident.to_string(), point.into()));
            }
        }
    }
}

pub fn parse_use(s: String) -> UseVisitor {
    let mut v = UseVisitor {
        path_list: Vec::new(),
        contains_glob: false,
    };

    // visit::walk_crate can be panic so we don't use it here
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

pub fn parse_pat_bind_stmt(s: String) -> Vec<ByteRange> {
    let mut v = PatBindVisitor {
        ident_points: Vec::new(),
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.ident_points
}

pub fn parse_struct_fields(s: String, scope: Scope) -> Vec<(String, BytePos, Option<Ty>)> {
    let mut v = StructVisitor {
        scope,
        fields: Vec::new(),
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.fields
}

pub fn parse_impl(
    s: String,
    path: &Path,
    offset: BytePos,
    local: bool,
    scope_start: BytePos,
) -> Option<ImplHeader> {
    let mut v = ImplVisitor::new(path, offset, local, scope_start);
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_trait(s: String) -> TraitVisitor {
    let mut v = TraitVisitor { name: None };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

/// parse traits and collect inherited traits as TraitBounds
pub fn parse_inherited_traits<P: AsRef<Path>>(
    s: String,
    filepath: P,
    offset: i32,
) -> Option<TraitBounds> {
    let mut v = InheritedTraitsVisitor {
        result: None,
        file_path: filepath,
        offset: offset,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_generics(s: String, filepath: &Path) -> GenericsArgs {
    let mut v = GenericsVisitor {
        result: GenericsArgs::default(),
        filepath: filepath,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_type<'s>(s: String, scope: &'s Scope) -> TypeVisitor<'s> {
    let mut v = TypeVisitor {
        name: None,
        type_: None,
        scope,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

pub fn parse_fn_args_and_generics(
    s: String,
    fpath: &Path,
    offset: i32,
) -> (Vec<ByteRange>, GenericsArgs) {
    let mut v = PatAndGenVisitor {
        ident_points: vec![],
        generics: GenericsArgs::default(),
        fpath,
        offset,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    (v.ident_points, v.generics)
}

pub fn parse_fn_args(s: String) -> Vec<ByteRange> {
    parse_pat_idents(s)
}

pub fn parse_pat_idents(s: String) -> Vec<ByteRange> {
    let mut v = PatVisitor {
        ident_points: Vec::new(),
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    debug!("ident points are {:?}", v.ident_points);
    v.ident_points
}

pub fn parse_fn_output(s: String, scope: Scope) -> Option<Ty> {
    let mut v = FnOutputVisitor {
        result: None,
        scope,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_fn_arg_type(
    s: String,
    argpos: BytePos,
    scope: Scope,
    session: &Session,
    offset: i32,
) -> Option<Ty> {
    debug!("parse_fn_arg {:?} |{}|", argpos, s);
    let mut v = FnArgTypeVisitor {
        argpos,
        scope,
        session,
        generics: GenericsArgs::default(),
        offset,
        result: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_mod(s: String) -> ModVisitor {
    let mut v = ModVisitor { name: None };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

pub fn parse_extern_crate(s: String) -> ExternCrateVisitor {
    let mut v = ExternCrateVisitor {
        name: None,
        realname: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

pub fn parse_enum(s: String) -> EnumVisitor {
    let mut v = EnumVisitor {
        name: String::new(),
        values: Vec::new(),
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

pub fn get_type_of(s: String, fpath: &Path, pos: BytePos, session: &Session) -> Option<Ty> {
    let startscope = Scope {
        filepath: fpath.to_path_buf(),
        point: pos,
    };

    let mut v = ExprTypeVisitor::new(startscope, session);

    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

// pos points to an ident in the lhs of the stmtstr
pub fn get_let_type(s: String, pos: BytePos, scope: Scope, session: &Session) -> Option<Ty> {
    let mut v = LetTypeVisitor {
        scope,
        session,
        srctxt: s.clone(),
        pos,
        result: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn get_match_arm_type(s: String, pos: BytePos, scope: Scope, session: &Session) -> Option<Ty> {
    let mut v = MatchTypeVisitor {
        scope,
        session,
        pos,
        result: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub struct FnOutputVisitor {
    scope: Scope,
    pub result: Option<Ty>,
}

impl<'ast> visit::Visitor<'ast> for FnOutputVisitor {
    fn visit_fn(
        &mut self,
        _: visit::FnKind,
        fd: &ast::FnDecl,
        _: source_map::Span,
        _: ast::NodeId,
    ) {
        self.result = match fd.output {
            FunctionRetTy::Ty(ref ty) => Ty::from_ast(ty, &self.scope),
            FunctionRetTy::Default(_) => None,
        };
    }
}

/// Visitor to detect type of fnarg
pub struct FnArgTypeVisitor<'c: 's, 's> {
    /// the code point arg appears in search string
    argpos: BytePos,
    scope: Scope,
    session: &'s Session<'c>,
    generics: GenericsArgs,
    /// the code point search string starts
    /// use i32 for the case `impl blah {` is inserted
    offset: i32,
    pub result: Option<Ty>,
}

impl<'c, 's, 'ast> visit::Visitor<'ast> for FnArgTypeVisitor<'c, 's> {
    fn visit_generics(&mut self, g: &'ast ast::Generics) {
        let filepath = &self.scope.filepath;
        let generics = GenericsArgs::from_generics(g, filepath, self.offset);
        debug!(
            "[FnArgTypeVisitor::visit_generics] {:?}",
            self.generics.get_idents()
        );
        self.generics.extend(generics);
    }

    fn visit_fn(
        &mut self,
        _fk: visit::FnKind,
        fd: &ast::FnDecl,
        _: source_map::Span,
        _: ast::NodeId,
    ) {
        debug!("[FnArgTypeVisitor::visit_fn] inputs: {:?}", fd.inputs);
        // Get generics arguments here (just for speed up)
        self.result = fd
            .inputs
            .iter()
            .find(|arg| point_is_in_span(self.argpos, &arg.pat.span))
            .and_then(|arg| {
                debug!("[FnArgTypeVisitor::visit_fn] type {:?} was found", arg.ty);
                let ty = Ty::from_ast(&arg.ty, &self.scope)
                    .and_then(|ty| {
                        destructure_pattern_to_ty(
                            &arg.pat,
                            self.argpos,
                            &ty,
                            &self.scope,
                            self.session,
                        )
                    }).map(Ty::dereference)?;
                if let Ty::PathSearch(ref paths) = ty {
                    let segments = &paths.path.segments;
                    // now we want to get 'T' from fn f<T>(){}, so segments.len() == 1
                    if segments.len() == 1 {
                        let name = &segments[0].name;
                        if let Some(bounds) = self.generics.find_type_param(name) {
                            let res = bounds.to_owned().into_match();
                            return Some(Ty::Match(res));
                        }
                    }
                    find_type_match(&paths.path, &paths.filepath, paths.point, self.session)
                        .map(Ty::Match)
                } else {
                    Some(ty)
                }
            });
    }
}

/// Visitor to collect Inherited Traits
pub struct InheritedTraitsVisitor<P> {
    /// search result(list of Inherited Traits)
    result: Option<TraitBounds>,
    /// the file trait appears
    file_path: P,
    /// thecode point 'trait' statement starts
    offset: i32,
}

impl<'ast, P> visit::Visitor<'ast> for InheritedTraitsVisitor<P>
where
    P: AsRef<Path>,
{
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Trait(_, _, _, ref bounds, _) = item.node {
            self.result = Some(TraitBounds::from_generic_bounds(
                bounds,
                &self.file_path,
                self.offset,
            ));
        }
    }
}

/// Visitor for for ~ in .. statement
pub(crate) struct ForStmtVisitor<'r, 's: 'r> {
    pub(crate) for_pat: Option<Pat>,
    pub(crate) in_expr: Option<Ty>,
    scope: Scope,
    session: &'r Session<'s>,
}

impl<'ast, 'r, 's> visit::Visitor<'ast> for ForStmtVisitor<'r, 's> {
    fn visit_expr(&mut self, ex: &'ast ast::Expr) {
        if let ExprKind::ForLoop(ref pat, ref expr, _, _) = ex.node {
            let for_pat = Pat::from_ast(&pat.node, &self.scope);
            let mut expr_visitor = ExprTypeVisitor::new(self.scope.clone(), self.session);
            expr_visitor.visit_expr(expr);
            self.in_expr = expr_visitor.result;
            self.for_pat = Some(for_pat);
        }
    }
}

pub(crate) fn parse_for_stmt<'r, 's: 'r>(
    s: String,
    scope: Scope,
    session: &'r Session<'s>,
) -> ForStmtVisitor<'r, 's> {
    let mut v = ForStmtVisitor {
        for_pat: None,
        in_expr: None,
        scope,
        session,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}

/// Visitor for if let / while let statement
pub(crate) struct IfLetVisitor<'r, 's: 'r> {
    pub(crate) let_pat: Option<Pat>,
    pub(crate) rh_expr: Option<Ty>,
    scope: Scope,
    session: &'r Session<'s>,
}

impl<'ast, 'r, 's> visit::Visitor<'ast> for IfLetVisitor<'r, 's> {
    fn visit_expr(&mut self, ex: &'ast ast::Expr) {
        match &ex.node {
            ExprKind::IfLet(pats, expr, _, _) | ExprKind::WhileLet(pats, expr, _, _) => {
                if let Some(pat) = pats.get(0) {
                    self.let_pat = Some(Pat::from_ast(&pat.node, &self.scope));
                    let mut expr_visitor = ExprTypeVisitor::new(self.scope.clone(), self.session);
                    expr_visitor.visit_expr(expr);
                    self.rh_expr = expr_visitor.result;
                }
            }
            _ => {}
        }
    }
}

pub(crate) fn parse_if_let<'r, 's: 'r>(
    s: String,
    scope: Scope,
    session: &'r Session<'s>,
) -> IfLetVisitor<'r, 's> {
    let mut v = IfLetVisitor {
        let_pat: None,
        rh_expr: None,
        scope,
        session,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
}
