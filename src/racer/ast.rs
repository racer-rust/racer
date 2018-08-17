use core::{self, BytePos, ByteRange, Match, MatchType, Scope, Session, SessionExt, Ty};
use nameres::{self, resolve_path_with_str};
use scopes;
use typeinf;

use std::path::Path;
use std::rc::Rc;

use syntax::ast::{
    self, ExprKind, FunctionRetTy, GenericArg, GenericArgs, GenericBound, GenericBounds,
    GenericParamKind, ItemKind, LitKind, PatKind, TyKind, UseTree, UseTreeKind,
};
use syntax::codemap::{self, FileName, Span};
use syntax::errors::{emitter::ColorConfig, Handler};
use syntax::parse::parser::Parser;
use syntax::parse::{self, ParseSess};
use syntax::print::pprust;
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
        let codemap = Rc::new(codemap::CodeMap::new(codemap::FilePathMapping::empty()));
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
    let codemap::BytePos(lo) = span.lo();
    let codemap::BytePos(hi) = span.hi();
    (lo, hi)
}

/// The leaf of a `use` statement.
#[derive(Clone, Debug)]
pub struct PathAlias {
    /// the leaf of Use Tree
    /// it can be one of one of 3 types, e.g.
    /// use std::collections::{self, hashmap::*, HashMap};
    pub kind: PathAliasKind,
    /// The path.
    pub path: core::Path,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathAliasKind {
    Ident(String),
    Self_(String),
    Glob,
}

impl AsRef<core::Path> for PathAlias {
    fn as_ref(&self) -> &core::Path {
        &self.path
    }
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
            parent_path: Option<&core::Path>,
        ) -> (Vec<PathAlias>, bool) {
            let mut res = vec![];
            let mut path = if let Some(parent) = parent_path {
                let relative_path = to_racer_path(&use_tree.prefix);
                let mut path = parent.clone();
                path.extend(relative_path);
                path
            } else {
                to_racer_path(&use_tree.prefix)
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

fn to_racer_ty(ty: &ast::Ty, scope: &Scope) -> Option<Ty> {
    match ty.node {
        TyKind::Tup(ref items) => {
            let mut res = Vec::new();
            for t in items {
                res.push(match to_racer_ty(t, scope) {
                    Some(t) => t,
                    None => return None,
                });
            }
            Some(Ty::Tuple(res))
        }
        TyKind::Rptr(ref _lifetime, ref ty) => {
            to_racer_ty(&ty.ty, scope).map(|ref_ty| Ty::RefPtr(Box::new(ref_ty)))
        }
        TyKind::Path(_, ref path) => Some(Ty::PathSearch(to_racer_path(path), scope.clone())),
        TyKind::Array(ref ty, ref expr) => to_racer_ty(ty, scope).map(|racer_ty| {
            Ty::FixedLengthVec(Box::new(racer_ty), pprust::expr_to_string(&expr.value))
        }),
        TyKind::Slice(ref ty) => to_racer_ty(ty, scope).map(|ref_ty| Ty::Vec(Box::new(ref_ty))),
        TyKind::Never => None,
        _ => {
            trace!("unhandled Ty node: {:?}", ty.node);
            None
        }
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
        "destructure_pattern_to_ty point {:?} ty {:?}    ||||||||    pat: {:?}",
        point, ty, pat
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
                    if point_is_in_span(point, &p.span) {
                        let ty = &typeelems[i];
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
                let mut v = ExprTypeVisitor {
                    scope: self.scope.clone(),
                    result: None,
                    session: self.session,
                };
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
            ty = to_racer_ty(local_ty, &self.scope);
        }

        if ty.is_none() {
            // oh, no type in the let expr. Try evalling the RHS
            ty = local.init.as_ref().and_then(|initexpr| {
                debug!("init node is {:?}", initexpr.node);
                let mut v = ExprTypeVisitor {
                    scope: self.scope.clone(),
                    result: None,
                    session: self.session,
                };
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

            let mut v = ExprTypeVisitor {
                scope: self.scope.clone(),
                result: None,
                session: self.session,
            };
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
    debug!("resolve_ast_path {:?}", to_racer_path(path));
    nameres::resolve_path_with_str(
        &to_racer_path(path),
        filepath,
        pos,
        core::SearchType::ExactMatch,
        core::Namespace::Both,
        session,
    ).nth(0)
}

fn to_racer_path(path: &ast::Path) -> core::Path {
    let mut segments = Vec::new();
    for seg in path.segments.iter() {
        let name = seg.ident.name.to_string();
        let mut types = Vec::new();
        // TODO: support GenericArgs::Parenthesized (A path like `Foo(A,B) -> C`)
        if let Some(ref params) = seg.args {
            if let GenericArgs::AngleBracketed(ref angle_args) = **params {
                angle_args.args.iter().for_each(|arg| {
                    if let GenericArg::Type(ty) = arg {
                        if let TyKind::Path(_, ref path) = ty.node {
                            types.push(to_racer_path(path));
                        }
                    }
                })
            }
        }
        segments.push(core::PathSegment::new(name, types));
    }
    core::Path {
        prefix: None,
        segments,
    }
}

fn path_to_match(ty: Ty, session: &Session) -> Option<Ty> {
    match ty {
        Ty::PathSearch(ref path, ref scope) => {
            find_type_match(path, &scope.filepath, scope.point, session)
        }
        Ty::RefPtr(ty) => path_to_match(*ty, session),
        _ => Some(ty),
    }
}

fn find_type_match(path: &core::Path, fpath: &Path, pos: BytePos, session: &Session) -> Option<Ty> {
    debug!("find_type_match {:?}, {:?}", path, fpath);
    let res = resolve_path_with_str(
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
    });

    res.and_then(|mut m| {
        // add generic types to match (if any)
        let mut types: Vec<core::PathSearch> = path
            .generic_types()
            .map(|typepath| core::PathSearch {
                path: typepath.clone(),
                filepath: fpath.to_path_buf(),
                point: pos,
            }).collect();
        m.generic_types.append(&mut types);
        Some(Ty::Match(m))
    })
}

pub(crate) fn get_type_of_typedef(m: &Match, session: &Session) -> Option<Match> {
    debug!("get_type_of_typedef match is {:?}", m);
    let msrc = session.load_file_and_mask_comments(&m.filepath);
    let blobstart = m.point - BytePos(5); // 5 == "type ".len()
    let blob = msrc.get_src_from_start(blobstart);

    blob.iter_stmts()
        .nth(0)
        .and_then(|range| {
            let blob = msrc[range.shift(blobstart).to_range()].to_owned();
            debug!("get_type_of_typedef blob string {}", blob);
            let res = parse_type(blob);
            debug!("get_type_of_typedef parsed type {:?}", res.type_);
            res.type_
        }).and_then(|type_| {
            let src = session.load_file(&m.filepath);
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
    result: Option<Ty>,
}

impl<'c, 's, 'ast> visit::Visitor<'ast> for ExprTypeVisitor<'c, 's> {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        debug!(
            "ExprTypeVisitor::visit_expr {:?}(kind: {:?})",
            expr, expr.node
        );
        //walk_expr(self, ex, e)
        match expr.node {
            ExprKind::Unary(_, ref expr) | ExprKind::AddrOf(_, ref expr) => {
                self.visit_expr(expr);
            }
            ExprKind::Path(_, ref path) => {
                debug!("expr is a path {:?}", to_racer_path(path));
                let codemap::BytePos(lo) = path.span.lo();
                self.result = resolve_ast_path(
                    path,
                    &self.scope.filepath,
                    self.scope.point + lo.into(),
                    self.session,
                ).and_then(|m| {
                    let msrc = self.session.load_file_and_mask_comments(&m.filepath);
                    typeinf::get_type_of_match(m, msrc.as_src(), self.session)
                });
            }
            ExprKind::Call(ref callee_expression, _ /*ref arguments*/) => {
                self.visit_expr(callee_expression);

                self.result = self.result.take().and_then(|m| {
                    if let Ty::Match(m) = m {
                        match m.mtype {
                            MatchType::Function => {
                                typeinf::get_return_type_of_function(&m, &m, self.session)
                                    .and_then(|ty| path_to_match(ty, self.session))
                            }
                            MatchType::Struct | MatchType::Enum => Some(Ty::Match(m)),
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
                let pathvec = to_racer_path(path);
                self.result = find_type_match(
                    &pathvec,
                    &self.scope.filepath,
                    self.scope.point,
                    self.session,
                );
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
                self.result = self.result.as_ref().and_then(|structm| match *structm {
                    Ty::Match(ref structm) => {
                        typeinf::get_struct_field_type(&fieldname, structm, self.session).and_then(
                            |fieldtypepath| {
                                find_type_match_including_generics(
                                    &fieldtypepath,
                                    &structm.filepath,
                                    structm.point,
                                    structm,
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
                    match self.result {
                        Some(ref t) => v.push(t.clone()),
                        None => {
                            self.result = None;
                            return;
                        }
                    };
                }
                self.result = Some(Ty::Tuple(v));
            }

            ExprKind::Lit(ref lit) => {
                let ty_path = match lit.node {
                    LitKind::Str(_, _) => Some(core::Path::from_vec(false, vec!["str"])),
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
                    )
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
                        m.generic_types.get(0).and_then(|ok_var| {
                            find_type_match(
                                &ok_var.path,
                                &ok_var.filepath,
                                ok_var.point,
                                self.session,
                            )
                        })
                    } else {
                        debug!(
                            "Unable to desugar Try expression; type was {} with arity {} of {}",
                            m.matchstr,
                            m.generic_types.len(),
                            m.generic_args.len()
                        );
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

                visit::walk_block(self, &block);

                // if the block does not resolve to a type, try the else block
                if self.result.is_none() && else_block.is_some() {
                    self.visit_expr(&else_block.as_ref().unwrap());
                }
            }

            ExprKind::Block(ref block, ref _label) => {
                debug!("block expr");
                visit::walk_block(self, &block);
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
    match ty {
        Ty::PathSearch(ref fieldtypepath, ref scope) => {
            debug!("path_to_match_including_generics: {:?}  {:?}", ty, contextm);
            if fieldtypepath.segments.len() == 1 {
                // could have generic args! - try and resolve them
                let typename = fieldtypepath.segments[0].name.clone();
                let it = contextm
                    .generic_args
                    .iter()
                    .cloned()
                    .zip(contextm.generic_types.iter().cloned());
                let mut typepath = fieldtypepath.clone();
                let mut gentypefound = false;

                for (name, typesearch) in it.clone() {
                    if name == typename {
                        // yes! a generic type match!
                        return find_type_match(
                            &typesearch.path,
                            &typesearch.filepath,
                            typesearch.point,
                            session,
                        );
                    }

                    for typ in &mut typepath.segments[0].types {
                        let gentypename = typ.segments[0].name.clone();
                        if name == gentypename {
                            // A generic type on ty matches one on contextm
                            *typ = typesearch.path.clone(); // Overwrite the type with the one from contextm
                            gentypefound = true;
                        }
                    }
                }

                if gentypefound {
                    let mut out = find_type_match(&typepath, &scope.filepath, scope.point, session);

                    // Fix the paths on the generic types in out
                    if let Some(Ty::Match(ref mut m)) = out {
                        for (_, typesearch) in it {
                            for gentypematch in m.generic_types.iter_mut().filter(|ty| {
                                ty.path.segments[0].name == typesearch.path.segments[0].name
                            }) {
                                *gentypematch = typesearch.clone();
                            }
                        }
                    }
                    return out;
                }
            }

            find_type_match(fieldtypepath, &scope.filepath, scope.point, session)
        }
        _ => Some(ty),
    }
}

fn find_type_match_including_generics(
    fieldtype: &core::Ty,
    filepath: &Path,
    pos: BytePos,
    structm: &Match,
    session: &Session,
) -> Option<Ty> {
    assert_eq!(&structm.filepath, filepath);
    let fieldtypepath = match *fieldtype {
        Ty::PathSearch(ref path, _) => path,
        Ty::RefPtr(ref ty) => match *ty.as_ref() {
            Ty::PathSearch(ref path, _) => path,
            _ => {
                debug!(
                    "EXPECTING A PATH!! Cannot handle other types yet. {:?}",
                    fieldtype
                );
                return None;
            }
        },
        _ => {
            debug!(
                "EXPECTING A PATH!! Cannot handle other types yet. {:?}",
                fieldtype
            );
            return None;
        }
    };

    if fieldtypepath.segments.len() == 1 {
        // could be a generic arg! - try and resolve it
        let typename = &fieldtypepath.segments[0].name;
        let it = structm
            .generic_args
            .iter()
            .zip(structm.generic_types.iter());
        for (name, typesearch) in it {
            if name == typename {
                // yes! a generic type match!
                return find_type_match(
                    &typesearch.path,
                    &typesearch.filepath,
                    typesearch.point,
                    session,
                );
            }
        }
    }

    find_type_match(fieldtypepath, filepath, pos, session)
}

struct StructVisitor {
    pub scope: Scope,
    pub fields: Vec<(String, BytePos, Option<core::Ty>)>,
}

impl<'ast> visit::Visitor<'ast> for StructVisitor {
    fn visit_variant_data(
        &mut self,
        struct_definition: &ast::VariantData,
        _: ast::Ident,
        _: &ast::Generics,
        _: ast::NodeId,
        _: codemap::Span,
    ) {
        for field in struct_definition.fields() {
            let codemap::BytePos(point) = field.span.lo();

            let ty = to_racer_ty(&field.ty, &self.scope);
            let name = match field.ident {
                Some(ref ident) => ident.to_string(),
                // name unnamed field by its ordinal, since self.0 works
                None => format!("{}", self.fields.len()),
            };

            self.fields.push((name, point.into(), ty));
        }
    }
}

pub struct TypeVisitor {
    pub name: Option<String>,
    pub type_: Option<core::Path>,
}

impl<'ast> visit::Visitor<'ast> for TypeVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Ty(ref ty, _) = item.node {
            self.name = Some(item.ident.name.to_string());

            let typepath = match ty.node {
                TyKind::Rptr(_, ref ty) => match ty.ty.node {
                    TyKind::Path(_, ref path) => {
                        let type_ = to_racer_path(path);
                        debug!("type type is {:?}", type_);
                        Some(type_)
                    }
                    _ => None,
                },
                TyKind::Path(_, ref path) => {
                    let type_ = to_racer_path(path);
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
pub struct ImplVisitor {
    pub name_path: Option<core::Path>,
    pub trait_path: Option<core::Path>,
}

impl<'ast> visit::Visitor<'ast> for ImplVisitor {
    fn visit_item(&mut self, item: &ast::Item) {
        if let ItemKind::Impl(_, _, _, _, ref otrait, ref typ, _) = item.node {
            match typ.node {
                TyKind::Path(_, ref path) => {
                    self.name_path = Some(to_racer_path(path));
                }
                TyKind::Rptr(_, ref ty) => {
                    // HACK for now, treat refs the same as unboxed types
                    // so that we can match '&str' to 'str'
                    if let TyKind::Path(_, ref path) = ty.ty.node {
                        self.name_path = Some(to_racer_path(path));
                    }
                }
                _ => {}
            }
            if let Some(ref t) = otrait {
                self.trait_path = Some(to_racer_path(&t.path));
            }
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

/// Wrapper struct for representing trait bounds.
/// Its usages are
/// - for generic types like T: Debug + Clone
/// - for trait inheritance like trait A: Debug + Clone
/// - for impl_trait like fn f(a: impl Debug + Clone)
/// - for dynamic traits(dyn_trait) like Box<Debug + Clone> or Box<dyn Debug + Clone>
#[derive(Clone, Debug, PartialEq)]
pub struct TraitBounds(Vec<core::PathSearch>);

impl TraitBounds {
    /// checks if it contains a trait, whick its name is 'name'
    pub fn find_by_name(&self, name: &str) -> Option<&core::PathSearch> {
        Some(self.0.iter().find(|path_search| {
            let seg = &path_search.path.segments;
            if seg.len() != 1 {
                return false;
            }
            &seg[0].name == name
        })?)
    }
    /// Search traits included in bounds and return Matches
    pub fn get_traits(&self, session: &Session) -> Vec<Match> {
        self.0
            .iter()
            .filter_map(|ps| {
                resolve_path_with_str(
                    &ps.path,
                    &ps.filepath,
                    ps.point,
                    core::SearchType::ExactMatch,
                    core::Namespace::Type,
                    session,
                ).nth(0)
            }).collect()
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    fn from_generic_bounds<P: AsRef<Path>>(
        bounds: &GenericBounds,
        filepath: P,
        offset: i32,
    ) -> TraitBounds {
        let vec = bounds
            .iter()
            .filter_map(|bound| {
                if let GenericBound::Trait(ref ptrait_ref, _) = *bound {
                    let ast_path = &ptrait_ref.trait_ref.path;
                    let codemap::BytePos(point) = ast_path.span.lo();
                    let path = to_racer_path(&ast_path);
                    let path_search = core::PathSearch {
                        path: path,
                        filepath: filepath.as_ref().to_path_buf(),
                        point: BytePos::from((point as i32 + offset) as u32),
                    };
                    Some(path_search)
                } else {
                    None
                }
            }).collect();
        TraitBounds(vec)
    }
}

/// Argument of generics like T: From<String>
/// It's intended to use this type only for declaration of type parameter.
// TODO: impl trait's name
#[derive(Clone, Debug, PartialEq)]
pub struct TypeParameter {
    /// the name of type parameter declared in generics, like 'T'
    pub name: String,
    /// The point 'T' appears
    pub point: BytePos,
    /// bounds
    pub bounds: TraitBounds,
}

impl TypeParameter {
    pub fn name(&self) -> &str {
        &(*self.name)
    }
    fn into_match<P: AsRef<Path>>(self, filepath: &P) -> Option<Match> {
        // TODO: contextstr, local
        Some(Match {
            matchstr: self.name,
            filepath: filepath.as_ref().to_path_buf(),
            point: self.point,
            coords: None,
            local: false,
            mtype: MatchType::TypeParameter(Box::new(self.bounds)),
            contextstr: String::new(),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: String::new(),
        })
    }
}

/// List of Args in generics, e.g. <T: Clone, U, P>
/// Now it's intended to use only for type parameters
// TODO: should we extend this type enable to handle both type parameters and true types?
#[derive(Clone, Debug, Default)]
pub struct GenericsArgs(pub Vec<TypeParameter>);

impl GenericsArgs {
    fn find_type_param(&self, name: &str) -> Option<&TypeParameter> {
        self.0.iter().find(|v| &v.name == name)
    }
    fn extend(&mut self, other: GenericsArgs) {
        self.0.extend(other.0);
    }
    fn from_generics<'a, P: AsRef<Path>>(
        generics: &'a ast::Generics,
        filepath: P,
        offset: i32,
    ) -> Self {
        let res = generics
            .params
            .iter()
            .filter_map(|param| {
                match param.kind {
                    // TODO: lifetime support
                    GenericParamKind::Lifetime => None,
                    // TODO: should we handle default type here?
                    GenericParamKind::Type { default: _ } => {
                        let param_name = param.ident.name.to_string();
                        let codemap::BytePos(point) = param.ident.span.lo();
                        let bounds =
                            TraitBounds::from_generic_bounds(&param.bounds, &filepath, offset);
                        Some(TypeParameter {
                            name: param_name,
                            point: BytePos::from((point as i32 + offset) as u32),
                            bounds,
                        })
                    }
                }
            }).collect();
        GenericsArgs(res)
    }
    pub fn get_idents(&self) -> Vec<String> {
        self.0.iter().map(|g| g.name.clone()).collect()
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
                let codemap::BytePos(point) = variant.span.lo();
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

pub fn parse_struct_fields(s: String, scope: Scope) -> Vec<(String, BytePos, Option<core::Ty>)> {
    let mut v = StructVisitor {
        scope,
        fields: Vec::new(),
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.fields
}

pub fn parse_impl(s: String) -> ImplVisitor {
    let mut v = ImplVisitor {
        name_path: None,
        trait_path: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
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

pub fn parse_generics<P: AsRef<Path>>(s: String, filepath: P) -> GenericsArgs {
    let mut v = GenericsVisitor {
        result: GenericsArgs::default(),
        filepath: filepath,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v.result
}

pub fn parse_generics_and_impl<P: AsRef<Path>>(
    s: String,
    filepath: P,
) -> (GenericsArgs, ImplVisitor) {
    let mut v = GenericsVisitor {
        result: GenericsArgs::default(),
        filepath: filepath,
    };
    let mut w = ImplVisitor {
        name_path: None,
        trait_path: None,
    };
    with_stmt(s, |stmt| {
        visit::walk_stmt(&mut v, stmt);
        visit::walk_stmt(&mut w, stmt);
    });
    (v.result, w)
}

pub fn parse_type(s: String) -> TypeVisitor {
    let mut v = TypeVisitor {
        name: None,
        type_: None,
    };
    with_stmt(s, |stmt| visit::walk_stmt(&mut v, stmt));
    v
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

pub fn parse_fn_output(s: String, scope: Scope) -> Option<core::Ty> {
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
) -> Option<core::Ty> {
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

    let mut v = ExprTypeVisitor {
        scope: startscope,
        result: None,
        session,
    };

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
    fn visit_fn(&mut self, _: visit::FnKind, fd: &ast::FnDecl, _: codemap::Span, _: ast::NodeId) {
        self.result = match fd.output {
            FunctionRetTy::Ty(ref ty) => to_racer_ty(ty, &self.scope),
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

    fn visit_fn(&mut self, _fk: visit::FnKind, fd: &ast::FnDecl, _: codemap::Span, _: ast::NodeId) {
        debug!("[FnArgTypeVisitor::visit_fn] inputs: {:?}", fd.inputs);
        // Get generics arguments here (just for speed up)
        let filepath = &self.scope.filepath;
        self.result = fd
            .inputs
            .iter()
            .find(|arg| point_is_in_span(self.argpos, &arg.pat.span))
            .and_then(|arg| {
                debug!("[FnArgTypeVisitor::visit_fn] type {:?} was found", arg.ty);
                let ty = to_racer_ty(&arg.ty, &self.scope)
                    .and_then(|ty| {
                        destructure_pattern_to_ty(
                            &arg.pat,
                            self.argpos,
                            &ty,
                            &self.scope,
                            self.session,
                        )
                    }).map(destruct_ty_refptr)?;
                if let Ty::PathSearch(ref path, ref scope) = ty {
                    let segments = &path.segments;
                    // now we want to get 'T' from fn f<T>(){}, so segments.len() == 1
                    if segments.len() == 1 {
                        let name = &segments[0].name;
                        if let Some(bounds) = self.generics.find_type_param(name) {
                            let res = bounds.to_owned().into_match(filepath)?;
                            return Some(Ty::Match(res));
                        }
                    }
                    find_type_match(path, &scope.filepath, scope.point, self.session)
                } else {
                    Some(ty)
                }
            });
    }
}

fn destruct_ty_refptr(ty_arg: Ty) -> Ty {
    if let Ty::RefPtr(ty) = ty_arg {
        destruct_ty_refptr(*ty)
    } else {
        ty_arg
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

impl<'ast, P: AsRef<Path>> visit::Visitor<'ast> for InheritedTraitsVisitor<P> {
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
