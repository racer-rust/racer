//! Type inference
//! THIS MODULE IS ENTIRELY TOO UGLY SO REALLY NEADS REFACTORING(kngwyu)
use ast;
use ast_types::{Pat, Ty};
use core;
use core::{BytePos, Match, MatchType, Namespace, Scope, SearchType, Session, SessionExt, Src};
use matchers;
use nameres;
use scopes;
use std::path::Path;
use util::{self, txt_matches};

// TODO: Option
fn find_start_of_function_body(src: &str) -> BytePos {
    // TODO: this should ignore anything inside parens so as to skip the arg list
    src.find('{')
        .map(|u| BytePos::from(u))
        .expect("Function body should have a beginning")
}

// Removes the body of the statement (anything in the braces {...}), leaving just
// the header
// TODO: this should skip parens (e.g. function arguments)
pub fn generate_skeleton_for_parsing(src: &str) -> String {
    let n = find_start_of_function_body(src);
    src[..n.0 + 1].to_owned() + "}"
}

// TODO(kngwyu): use libsyntax parser
pub fn first_param_is_self(blob: &str) -> bool {
    // Restricted visibility introduces the possibility of `pub(in ...)` at the start
    // of a method declaration. To counteract this, we restrict the search to only
    // look at text _after_ the visibility declaration.
    //
    // Having found the end of the visibility declaration, we now start the search
    // for method parameters.
    let blob = util::trim_visibility(blob);

    // skip generic arg
    // consider 'pub fn map<U, F: FnOnce(T) -> U>(self, f: F)'
    // we have to match the '>'
    match blob.find('(') {
        None => false,
        Some(probable_param_start) => {
            let skip_generic = match blob.find('<') {
                None => 0,
                Some(generic_start) if generic_start < probable_param_start => {
                    let mut level = 0;
                    let mut prev = ' ';
                    let mut skip_generic = 0;
                    for (i, c) in blob[generic_start..].char_indices() {
                        match c {
                            '<' => level += 1,
                            '>' if prev == '-' => (),
                            '>' => level -= 1,
                            _ => (),
                        }
                        prev = c;
                        if level == 0 {
                            skip_generic = i;
                            break;
                        }
                    }
                    skip_generic
                }
                Some(..) => 0,
            };
            if let Some(start) = blob[skip_generic..].find('(') {
                let start = BytePos::from(start).increment();
                let end = scopes::find_closing_paren(blob, start);
                let is_self = txt_matches(SearchType::ExactMatch, "self", &blob[start.0..end.0]);
                trace!(
                    "searching fn args for self: |{}| {}",
                    &blob[start.0..end.0],
                    is_self
                );
                return is_self;
            }
            false
        }
    }
}

#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah }";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {}", out);
}

fn get_type_of_self_arg(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    debug!("get_type_of_self_arg {:?}", m);
    get_type_of_self(m.point, &m.filepath, m.local, msrc, session)
}

// TODO(kngwyu): parse correctly
pub fn get_type_of_self(
    point: BytePos,
    filepath: &Path,
    local: bool,
    msrc: Src,
    session: &Session,
) -> Option<Ty> {
    let start = scopes::find_impl_start(msrc, point, BytePos::ZERO)?;
    let decl = generate_skeleton_for_parsing(&msrc.shift_start(start));
    debug!("get_type_of_self_arg impl skeleton |{}|", decl);

    if decl.starts_with("impl") {
        // we have to do 2 operations around generics here
        // 1. Checks if self's type is T
        // 2. Checks if self's type contains T
        let scope_start = start + decl.len().into();
        let implres = ast::parse_impl(decl, filepath, start, local, scope_start)?;
        if let Some((_, param)) = implres.generics().search_param_by_path(implres.self_path()) {
            if let Some(resolved) = param.resolved() {
                return Some(resolved.to_owned());
            }
            let mut m = param.to_owned().into_match();
            m.local = local;
            return Some(Ty::Match(m));
        }
        debug!("get_type_of_self_arg implres |{:?}|", implres);
        nameres::resolve_path(
            implres.self_path(),
            filepath,
            start,
            SearchType::ExactMatch,
            Namespace::Type,
            session,
            &matchers::ImportInfo::default(),
        ).nth(0)
        .map(|mut m| {
            match &mut m.mtype {
                MatchType::Enum(gen) | MatchType::Struct(gen) => {
                    for (i, param) in implres.generics.0.into_iter().enumerate() {
                        gen.add_bound(i, param.bounds);
                    }
                }
                _ => {}
            }
            Ty::Match(m)
        })
    } else {
        // // must be a trait
        ast::parse_trait(decl).name.and_then(|name| {
            Some(Ty::Match(Match {
                matchstr: name,
                filepath: filepath.into(),
                point: start,
                coords: None,
                local: local,
                mtype: core::MatchType::Trait,
                contextstr: matchers::first_line(&msrc[start.0..]),
                docs: String::new(),
            }))
        })
    }
}

fn is_closure(src: &str) -> Option<bool> {
    let s = src.matches(|c| c == '{' || c == '|').nth(0)?;
    Some(s == "|")
}

fn find_start_of_closure_body(src: &str) -> Option<BytePos> {
    let mut cnt = 0;
    for (i, c) in src.chars().enumerate() {
        if c == '|' {
            cnt += 1;
        }
        if cnt == 2 {
            return Some(BytePos::from(i).increment());
        }
    }
    warn!(
        "[find_start_of_closure_body] start of closure body not found!: {}",
        src
    );
    None
}

fn get_type_of_fnarg(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    if m.matchstr == "self" {
        return get_type_of_self_arg(m, msrc, session);
    }

    let stmtstart = match scopes::find_stmt_start(msrc, m.point) {
        Some(s) => s,
        None => {
            warn!(
                "[get_type_of_fnarg] start of statement was not found for {:?}",
                m
            );
            return None;
        }
    };
    let block = msrc.shift_start(stmtstart);
    let range = block.iter_stmts().nth(0)?;
    let blob = &msrc[range.shift(stmtstart).to_range()];
    let is_closure = is_closure(blob)?;
    if is_closure {
        let start_of_body = find_start_of_closure_body(blob)?;
        let s = format!("{}{{}}", &blob[..start_of_body.0]);
        let argpos = m.point - (stmtstart + range.start);
        let offset = (stmtstart + range.start).0 as i32;
        ast::parse_fn_arg_type(s, argpos, Scope::from_match(m), session, offset)
    } else {
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let start_blah = "impl blah {";
        let s = format!(
            "{}{}}}}}",
            start_blah,
            &blob[..find_start_of_function_body(blob).increment().0]
        );
        let argpos = m.point - (stmtstart + range.start) + start_blah.len().into();
        let offset = (stmtstart + range.start).0 as i32 - start_blah.len() as i32;
        ast::parse_fn_arg_type(s, argpos, Scope::from_match(m), session, offset)
    }
}

fn get_type_of_let_expr(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    // ASSUMPTION: this is being called on a let decl
    let point = scopes::find_let_start(msrc, m.point).expect("`let` should have a beginning");
    let src = msrc.shift_start(point);

    if let Some(range) = src.iter_stmts().next() {
        let blob = &src[range.to_range()];
        debug!("get_type_of_let_expr calling parse_let |{}|", blob);

        let pos = m.point - point - range.start;
        let scope = Scope {
            filepath: m.filepath.clone(),
            point,
        };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

/// Decide l_value's type given r_value and ident query
fn resolve_lvalue_ty<'a>(
    l_value: Pat,
    r_value: Option<Ty>,
    query: &str,
    fpath: &Path,
    pos: BytePos,
    session: &Session,
) -> Option<Ty> {
    match l_value {
        Pat::Ident(_bi, name) => {
            if name != query {
                return None;
            }
            r_value
        }
        Pat::Tuple(pats) => {
            if let Ty::Tuple(ty) = r_value? {
                for (p, t) in pats.into_iter().zip(ty) {
                    let ret = try_continue!(resolve_lvalue_ty(p, t, query, fpath, pos, session,));
                    return Some(ret);
                }
            }
            None
        }
        Pat::Ref(pat, _) => {
            if let Some(ty) = r_value {
                if let Ty::RefPtr(ty, _) = ty {
                    resolve_lvalue_ty(*pat, Some(*ty), query, fpath, pos, session)
                } else {
                    resolve_lvalue_ty(*pat, Some(ty), query, fpath, pos, session)
                }
            } else {
                resolve_lvalue_ty(*pat, None, query, fpath, pos, session)
            }
        }
        Pat::TupleStruct(path, pats) => {
            let ma = ast::find_type_match(&path, fpath, pos, session)?;
            match &ma.mtype {
                MatchType::Struct(_generics) => {
                    for (pat, (_, _, t)) in
                        pats.into_iter().zip(get_tuplestruct_fields(&ma, session))
                    {
                        let ret =
                            try_continue!(resolve_lvalue_ty(pat, t, query, fpath, pos, session));
                        return Some(ret);
                    }
                    None
                }
                MatchType::EnumVariant(enum_) => {
                    let generics = if let Some(Ty::Match(match_)) = r_value.map(Ty::dereference) {
                        match_.into_generics()
                    } else {
                        enum_.to_owned().and_then(|ma| ma.into_generics())
                    };
                    for (pat, (_, _, mut t)) in
                        pats.into_iter().zip(get_tuplestruct_fields(&ma, session))
                    {
                        debug!(
                            "Hi! I'm in enum and l: {:?}\n  r: {:?}\n gen: {:?}",
                            pat, t, generics
                        );
                        if let Some(ref gen) = generics {
                            t = t.map(|ty| ty.replace_by_generics(&gen));
                        }
                        let ret =
                            try_continue!(resolve_lvalue_ty(pat, t, query, fpath, pos, session));
                        return Some(ret);
                    }
                    None
                }
                _ => None,
            }
        }
        // Let's implement after #946 solved
        Pat::Struct(path, _) => {
            let item = ast::find_type_match(&path, fpath, pos, session)?;
            if !item.mtype.is_struct() {
                return None;
            }
            None
        }
        _ => None,
    }
}

fn get_type_of_for_arg(m: &Match, session: &Session) -> Option<Ty> {
    let for_start = match &m.mtype {
        MatchType::For(pos) => *pos,
        _ => {
            warn!("[get_type_of_for_expr] invalid match type: {:?}", m.mtype);
            return None;
        }
    };
    // HACK: use outer scope when getting in ~ expr's type
    let scope = Scope::new(m.filepath.clone(), for_start);
    let ast::ForStmtVisitor {
        for_pat, in_expr, ..
    } = ast::parse_for_stmt(m.contextstr.clone(), scope, session);
    debug!(
        "[get_type_of_for_expr] match: {:?}, for: {:?}, in: {:?},",
        m, for_pat, in_expr
    );
    fn get_item(ty: Ty, session: &Session) -> Option<Ty> {
        match ty {
            Ty::Match(ma) => nameres::get_iter_item(&ma, session),
            Ty::PathSearch(paths) => {
                nameres::get_iter_item(&paths.resolve_as_match(session)?, session)
            }
            Ty::RefPtr(ty, _) => get_item(*ty, session),
            _ => None,
        }
    }
    resolve_lvalue_ty(
        for_pat?,
        in_expr.and_then(|ty| get_item(ty, session)),
        &m.matchstr,
        &m.filepath,
        m.point,
        session,
    )
}

fn get_type_of_if_let(m: &Match, session: &Session, start: BytePos) -> Option<Ty> {
    // HACK: use outer scope when getting r-value's type
    let scope = Scope::new(m.filepath.clone(), start);
    let ast::IfLetVisitor {
        let_pat, rh_expr, ..
    } = ast::parse_if_let(m.contextstr.clone(), scope, session);
    debug!(
        "[get_type_of_if_let] match: {:?}\n  let: {:?}\n  rh: {:?},",
        m, let_pat, rh_expr,
    );
    resolve_lvalue_ty(
        let_pat?,
        rh_expr,
        &m.matchstr,
        &m.filepath,
        m.point,
        session,
    )
}

pub fn get_struct_field_type(
    fieldname: &str,
    structmatch: &Match,
    session: &Session,
) -> Option<Ty> {
    // temporary fix for https://github.com/rust-lang-nursery/rls/issues/783
    if !structmatch.mtype.is_struct() {
        warn!(
            "get_struct_filed_type is called for {:?}",
            structmatch.mtype
        );
        return None;
    }
    debug!("[get_struct_filed_type]{}, {:?}", fieldname, structmatch);

    let src = session.load_source_file(&structmatch.filepath);

    let opoint = scopes::expect_stmt_start(src.as_src(), structmatch.point);
    // HACK: if scopes::end_of_next_scope returns empty struct, it's maybe tuple struct
    let structsrc = if let Some(end) = scopes::end_of_next_scope(&src[opoint.0..]) {
        src[opoint.0..=(opoint + end).0].to_owned()
    } else {
        (*get_first_stmt(src.as_src().shift_start(opoint))).to_owned()
    };
    let fields = ast::parse_struct_fields(structsrc.to_owned(), Scope::from_match(structmatch));
    for (field, _, ty) in fields {
        if fieldname != field {
            continue;
        }
        return ty;
    }
    None
}

pub(crate) fn get_tuplestruct_fields(
    structmatch: &Match,
    session: &Session,
) -> Vec<(String, BytePos, Option<Ty>)> {
    let src = session.load_source_file(&structmatch.filepath);
    let structsrc = if let core::MatchType::EnumVariant(_) = structmatch.mtype {
        // decorate the enum variant src to make it look like a tuple struct
        let to = src[structmatch.point.0..]
            .find('(')
            .map(|n| {
                scopes::find_closing_paren(&src, structmatch.point + BytePos::from(n).increment())
            }).expect("Tuple enum variant should have `(` in definition");
        "struct ".to_owned() + &src[structmatch.point.0..to.increment().0] + ";"
    } else {
        assert!(structmatch.mtype.is_struct());
        let opoint = scopes::expect_stmt_start(src.as_src(), structmatch.point);
        (*get_first_stmt(src.as_src().shift_start(opoint))).to_owned()
    };

    debug!("[tuplestruct_fields] structsrc=|{}|", structsrc);

    ast::parse_struct_fields(structsrc, Scope::from_match(structmatch))
}

pub fn get_tuplestruct_field_type(
    fieldnum: usize,
    structmatch: &Match,
    session: &Session,
) -> Option<Ty> {
    let fields = get_tuplestruct_fields(structmatch, session);

    for (i, (_, _, ty)) in fields.into_iter().enumerate() {
        if i == fieldnum {
            return ty;
        }
    }
    None
}

pub fn get_first_stmt(src: Src) -> Src {
    match src.iter_stmts().next() {
        Some(range) => src.shift_range(range),
        None => src,
    }
}

pub fn get_type_of_match(m: Match, msrc: Src, session: &Session) -> Option<Ty> {
    debug!("get_type_of match {:?} ", m);

    match m.mtype {
        core::MatchType::Let => get_type_of_let_expr(&m, msrc, session),
        core::MatchType::IfLet(start) | core::MatchType::WhileLet(start) => {
            get_type_of_if_let(&m, session, start)
        }
        core::MatchType::For(_) => get_type_of_for_arg(&m, session),
        core::MatchType::FnArg => get_type_of_fnarg(&m, msrc, session),
        core::MatchType::MatchArm => get_type_from_match_arm(&m, msrc, session),
        core::MatchType::Struct(_)
        | core::MatchType::Enum(_)
        | core::MatchType::Function
        | core::MatchType::Method(_)
        | core::MatchType::Module => Some(Ty::Match(m)),
        core::MatchType::EnumVariant(Some(boxed_enum)) => {
            if boxed_enum.mtype.is_enum() {
                Some(Ty::Match(*boxed_enum))
            } else {
                debug!("EnumVariant has not-enum type: {:?}", boxed_enum.mtype);
                None
            }
        }
        _ => {
            debug!("!!! WARNING !!! Can't get type of {:?}", m.mtype);
            None
        }
    }
}

pub fn get_type_from_match_arm(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    // We construct a faux match stmt and then parse it. This is because the
    // match stmt may be incomplete (half written) in the real code

    // skip to end of match arm pattern so we can search backwards
    let arm = BytePos(msrc[m.point.0..].find("=>")?) + m.point;
    let scopestart = scopes::scope_start(msrc, arm);

    let stmtstart = scopes::find_stmt_start(msrc, scopestart.decrement())?;
    debug!("PHIL preblock is {:?} {:?}", stmtstart, scopestart);
    let preblock = &msrc[stmtstart.0..scopestart.0];
    let matchstart = stmtstart + preblock.rfind("match ")?.into();

    let lhs_start = scopes::get_start_of_pattern(&msrc, arm);
    let lhs = &msrc[lhs_start.0..arm.0];
    // construct faux match statement and recreate point
    let mut fauxmatchstmt = msrc[matchstart.0..scopestart.0].to_owned();
    let faux_prefix_size = BytePos::from(fauxmatchstmt.len());
    fauxmatchstmt = fauxmatchstmt + lhs + " => () };";
    let faux_point = faux_prefix_size + (m.point - lhs_start);

    debug!(
        "fauxmatchstmt for parsing is pt:{:?} src:|{}|",
        faux_point, fauxmatchstmt
    );

    ast::get_match_arm_type(
        fauxmatchstmt,
        faux_point,
        // scope is used to locate expression, so send
        // it the start of the match expr
        Scope {
            filepath: m.filepath.clone(),
            point: matchstart,
        },
        session,
    )
}

pub fn get_function_declaration(fnmatch: &Match, session: &Session) -> String {
    let src = session.load_source_file(&fnmatch.filepath);
    let start = scopes::expect_stmt_start(src.as_src(), fnmatch.point);
    let def_end: &[_] = &['{', ';'];
    let end = src[start.0..]
        .find(def_end)
        .expect("Definition should have an end (`{` or `;`)");
    src[start.0..start.0 + end].to_owned()
}

pub fn get_return_type_of_function(
    fnmatch: &Match,
    contextm: &Match,
    session: &Session,
) -> Option<Ty> {
    let src = session.load_source_file(&fnmatch.filepath);
    let point = scopes::expect_stmt_start(src.as_src(), fnmatch.point);
    let out = src[point.0..].find('{').and_then(|n| {
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let decl = "impl blah {".to_string() + &src[point.0..point.0 + n + 1] + "}}";
        debug!("get_return_type_of_function: passing in |{}|", decl);
        ast::parse_fn_output(decl, Scope::from_match(fnmatch))
    });
    // Convert output arg of type Self to the correct type
    if let Some(Ty::PathSearch(ref paths)) = out {
        let path = &paths.path;
        if let Some(ref path_seg) = path.segments.get(0) {
            if "Self" == path_seg.name {
                return get_type_of_self_arg(fnmatch, src.as_src(), session);
            }
            if path.segments.len() == 1 && path_seg.generics.is_empty() {
                for type_param in fnmatch.generics() {
                    if type_param.name() == &path_seg.name {
                        return Some(Ty::Match(contextm.clone()));
                    }
                }
            }
        }
    }
    out
}

pub(crate) fn get_type_of_indexed_value(body: Ty, session: &Session) -> Option<Ty> {
    // TODO(kngwyu): slice support
    match body {
        Ty::Match(m) => nameres::get_index_output(&m, session),
        Ty::PathSearch(p) => p
            .resolve_as_match(session)
            .and_then(|m| nameres::get_index_output(&m, session)),
        _ => None,
    }
}
