// Type inference

use core::{Match, Src, SessionRef, Scope};
use nameres::resolve_path_with_str;
use core::Namespace::TypeNamespace;
use core;
use ast;
use scopes;
use matchers;
use core::SearchType::ExactMatch;
use util::txt_matches;

fn find_start_of_function_body(src: &str) -> usize {
    // TODO: this should ignore anything inside parens so as to skip the arg list
    src.find("{").unwrap()
}

// Removes the body of the statement (anything in the braces {...}), leaving just
// the header
// TODO: this should skip parens (e.g. function arguments)
pub fn generate_skeleton_for_parsing(src: &str) -> String {
    let mut s = String::new();
    let n = src.find("{").unwrap();
    s.push_str(&src[..n+1]);
    s.push_str("};");
    s
}

pub fn first_param_is_self(blob: &str) -> bool {
    blob.find("(").map_or(false, |start| {
        let end = scopes::find_closing_paren(blob, start+1);
        let is_self = txt_matches(ExactMatch, "self", &blob[(start+1)..end]);
        debug!("searching fn args: |{}| {}", &blob[(start+1)..end], is_self);
        is_self
    })
}

#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {};", out);
}

fn get_type_of_self_arg(m: &Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    debug!("get_type_of_self_arg {:?}", m);
    scopes::find_impl_start(msrc, m.point, 0).and_then(|start| {
        let decl = generate_skeleton_for_parsing(&msrc.from(start));
        debug!("get_type_of_self_arg impl skeleton |{}|", decl);

        if decl.starts_with("impl") {
            let implres = ast::parse_impl(decl);
            debug!("get_type_of_self_arg implres |{:?}|", implres);
            resolve_path_with_str(&implres.name_path.expect("failed parsing impl name"),
                                  &m.filepath, start,
                                  ExactMatch, TypeNamespace,
                                  session).nth(0).map(core::Ty::TyMatch)
        } else {
            // // must be a trait
            ast::parse_trait(decl).name.and_then(|name| {
                Some(core::Ty::TyMatch(Match {
                           matchstr: name,
                           filepath: m.filepath.clone(),
                           point: start,
                           local: m.local,
                           mtype: core::MatchType::Trait,
                           contextstr: matchers::first_line(&msrc[start..]),
                           generic_args: Vec::new(), generic_types: Vec::new()
                }))
            })
        }
    })
}

fn get_type_of_fnarg(m: &Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    if m.matchstr == "self" {
        return get_type_of_self_arg(m, msrc, session);
    }

    let stmtstart = scopes::find_stmt_start(msrc, m.point).unwrap();
    let block = msrc.from(stmtstart);
    if let Some((start, end)) = block.iter_stmts().next() {
        let blob = &msrc[(stmtstart+start)..(stmtstart+end)];
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut s = String::new();
        s.push_str("impl blah {");
        let impl_header_len = s.len();
        s.push_str(&blob[..(find_start_of_function_body(blob)+1)]);
        s.push_str("}}");
        let argpos = m.point - (stmtstart+start) + impl_header_len;
        return ast::parse_fn_arg_type(s, argpos, Scope::from_match(m), session);
    }
    None
}

fn get_type_of_let_expr(m: &Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    // ASSUMPTION: this is being called on a let decl
    let point = scopes::find_stmt_start(msrc, m.point).unwrap();
    let src = msrc.from(point);

    if let Some((start, end)) = src.iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_let_expr calling parse_let |{}|", blob);

        let pos = m.point - point - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: m.point };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

fn get_type_of_let_block_expr(m: &Match, msrc: Src, session: SessionRef, prefix: &str) -> Option<core::Ty> {
    // ASSUMPTION: this is being called on an if let or while let decl
    let stmtstart = scopes::find_stmt_start(msrc, m.point).unwrap();
    let stmt = msrc.from(stmtstart);
    let point = stmt.find(prefix).unwrap();
    let src = core::new_source(generate_skeleton_for_parsing(&stmt[point..]));

    if let Some((start, end)) = src.as_ref().iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_let_block_expr calling get_let_type |{}|", blob);

        let pos = m.point - stmtstart - point - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: m.point };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

fn get_type_of_for_expr(m: &Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    let stmtstart = scopes::find_stmt_start(msrc, m.point).unwrap();
    let stmt = msrc.from(stmtstart);
    let forpos = stmt.find("for ").unwrap();
    let inpos = stmt.find(" in ").unwrap();
    // XXX: this need not be the correct brace, see generate_skeleton_for_parsing
    let bracepos = stmt.find("{").unwrap();
    let mut src = stmt[..forpos].to_owned();
    src.push_str("if let Some(");
    src.push_str(&stmt[forpos+4..inpos]);
    src.push_str(") = ");
    src.push_str(&stmt[inpos+4..bracepos]);
    src.push_str(".into_iter().next() { }}");
    let src = core::new_source(src);

    if let Some((start, end)) = src.as_ref().iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_for_expr: |{}| {} {} {} {}", blob, m.point, stmtstart, forpos, start);

        let pos = m.point - stmtstart - forpos - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: m.point };

        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

pub fn get_struct_field_type(fieldname: &str, structmatch: &Match, session: SessionRef) -> Option<core::Ty> {
    assert!(structmatch.mtype == core::MatchType::Struct);

    let src = session.load_file(&structmatch.filepath);

    let opoint = scopes::find_stmt_start(src, structmatch.point);
    let structsrc = scopes::end_of_next_scope(&src[opoint.unwrap()..]);

    let fields = ast::parse_struct_fields(structsrc.to_owned(), Scope::from_match(structmatch));
    for (field, _, ty) in fields.into_iter() {
        if fieldname == field {
            return ty;
        }
    }
    None
}

pub fn get_tuplestruct_field_type(fieldnum: u32, structmatch: &Match, session: SessionRef) -> Option<core::Ty> {
    let src = session.load_file(&structmatch.filepath);

    let structsrc = if let core::MatchType::EnumVariant = structmatch.mtype {
        // decorate the enum variant src to make it look like a tuple struct
        let to = (&src[structmatch.point..]).find("(")
            .map(|n| scopes::find_closing_paren(&src, structmatch.point + n+1))
            .unwrap();
        "struct ".to_owned() + &src[structmatch.point..(to+1)] + ";"
    } else {
        assert!(structmatch.mtype == core::MatchType::Struct);
        let opoint = scopes::find_stmt_start(src, structmatch.point);
        (*get_first_stmt(src.from(opoint.unwrap()))).to_owned()
    };

    debug!("get_tuplestruct_field_type structsrc=|{}|", structsrc);

    let fields = ast::parse_struct_fields(structsrc, Scope::from_match(structmatch));
    let mut i = 0u32;
    for (_, _, ty) in fields.into_iter() {
        if i == fieldnum {
            return ty;
        }
        i+=1;
    }
    None
}

pub fn get_first_stmt(src: Src) -> Src {
    match src.iter_stmts().next() {
        Some((from, to)) => src.from_to(from, to),
        None => src
    }
}

pub fn get_type_of_match(m: Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    debug!("get_type_of match {:?} ", m);

    match m.mtype {
        core::MatchType::Let => get_type_of_let_expr(&m, msrc, session),
        core::MatchType::IfLet => get_type_of_let_block_expr(&m, msrc, session, "if let"),
        core::MatchType::WhileLet => get_type_of_let_block_expr(&m, msrc, session, "while let"),
        core::MatchType::For => get_type_of_for_expr(&m, msrc, session),
        core::MatchType::FnArg => get_type_of_fnarg(&m, msrc, session),
        core::MatchType::MatchArm => get_type_from_match_arm(&m, msrc, session),
        core::MatchType::Struct => Some(core::Ty::TyMatch(m)),
        core::MatchType::Enum => Some(core::Ty::TyMatch(m)),
        core::MatchType::Function => Some(core::Ty::TyMatch(m)),
        core::MatchType::Module => Some(core::Ty::TyMatch(m)),
        _ => { debug!("!!! WARNING !!! Can't get type of {:?}", m.mtype); None }
    }
}

macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None })
}

pub fn get_type_from_match_arm(m: &Match, msrc: Src, session: SessionRef) -> Option<core::Ty> {
    // We construct a faux match stmt and then parse it. This is because the
    // match stmt may be incomplete (half written) in the real code

    // skip to end of match arm pattern so we can search backwards
    let arm = otry!((&msrc[m.point..]).find("=>")) + m.point;
    let scopestart = scopes::scope_start(msrc, arm);

    let stmtstart = otry!(scopes::find_stmt_start(msrc, scopestart-1));
    debug!("PHIL preblock is {} {}", stmtstart, scopestart);
    let preblock = &msrc[stmtstart..scopestart];
    let matchstart = otry!(preblock.rfind("match ")) + stmtstart;

    let lhs_start = scopes::get_start_of_pattern(&msrc, arm);
    let lhs = &msrc[lhs_start..arm];
    // construct faux match statement and recreate point
    let mut fauxmatchstmt = (&msrc[matchstart..scopestart]).to_owned();
    let faux_prefix_size = fauxmatchstmt.len();
    fauxmatchstmt = fauxmatchstmt + lhs + " => () };";
    let faux_point = faux_prefix_size + (m.point - lhs_start);

    debug!("fauxmatchstmt for parsing is pt:{} src:|{}|", faux_point, fauxmatchstmt);

    ast::get_match_arm_type(fauxmatchstmt, faux_point,
                            // scope is used to locate expression, so send
                            // it the start of the match expr
                            Scope {
                                filepath: m.filepath.clone(),
                                point: matchstart,
                            }, session)
}

pub fn get_function_declaration(fnmatch: &Match, session: SessionRef) -> String {
    let src = session.load_file(&fnmatch.filepath);
    let start = scopes::find_stmt_start(src, fnmatch.point).unwrap();
    let end = (&src[start..]).find('{').unwrap();
    (&src[start..end+start]).to_owned()
}

pub fn get_return_type_of_function(fnmatch: &Match, session: SessionRef) -> Option<core::Ty> {
    let src = session.load_file(&fnmatch.filepath);
    let point = scopes::find_stmt_start(src, fnmatch.point).unwrap();
    (&src[point..]).find("{").and_then(|n| {
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(&src[point..(point+n+1)]);
        decl.push_str("}}");
        debug!("get_return_type_of_function: passing in |{}|", decl);
        ast::parse_fn_output(decl, Scope::from_match(fnmatch))
    })
}
