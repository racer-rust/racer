// Type inference

use racer::{Match};
use racer::nameres::{resolve_path_with_str};
use racer::Namespace::TypeNamespace;
use racer::{self, ast, codeiter, scopes, util};
use racer::SearchType::ExactMatch;
use racer::util::txt_matches;

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
    return blob.find("(").map_or(false, |start| {
        let end = scopes::find_closing_paren(blob, start+1);
        debug!("searching fn args: |{}| {}",&blob[(start+1)..end], txt_matches(ExactMatch, "self", &blob[(start+1)..end]));
        return txt_matches(ExactMatch, "self", &blob[(start+1)..end]);
    });
}

#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {};", out);
}

fn get_type_of_self_arg(m: &Match, msrc: &str) -> Option<racer::Ty> {
    debug!("get_type_of_self_arg {:?}", m);
    return scopes::find_impl_start(msrc, m.point, 0).and_then(|start| {
        let decl = generate_skeleton_for_parsing(&msrc[start..]);
        debug!("get_type_of_self_arg impl skeleton |{}|", decl);

        if (&decl).starts_with("impl") {
            let implres = ast::parse_impl(decl);
            debug!("get_type_of_self_arg implres |{:?}|", implres);
            return resolve_path_with_str(&implres.name_path.expect("failed parsing impl name"),
                                         &m.filepath, start,
                                         ExactMatch, TypeNamespace).nth(0).map(|m| racer::Ty::TyMatch(m));
        } else {
            // // must be a trait
            return ast::parse_trait(decl).name.and_then(|name| {
                Some(racer::Ty::TyMatch(Match {matchstr: name,
                           filepath: m.filepath.clone(),
                           point: start,
                           local: m.local,
                           mtype: racer::MatchType::Trait,
                           contextstr: racer::matchers::first_line(&msrc[start..]),
                           generic_args: Vec::new(), generic_types: Vec::new()
                }))
            });
        }
    });
}

fn get_type_of_fnarg(m: &Match, msrc: &str) -> Option<racer::Ty> {

    if m.matchstr == "self" {
        return get_type_of_self_arg(m, msrc);
    }

    let stmtstart = scopes::find_stmt_start(msrc, m.point).unwrap();
    let block = &msrc[stmtstart..];
    if let Some((start,end)) = codeiter::iter_stmts(block).next() {
        let blob = &msrc[(stmtstart+start)..(stmtstart+end)];
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut s = String::new();
        s.push_str("impl blah {");
        let impl_header_len = s.len();
        s.push_str(&blob[..(find_start_of_function_body(blob)+1)]);
        s.push_str("}}");
        let argpos = m.point - (stmtstart+start) + impl_header_len;
        return ast::parse_fn_arg_type(s, argpos, racer::Scope::from_match(m));
    }
    return None;
}

fn get_type_of_let_expr(m: &Match, msrc: &str) -> Option<racer::Ty> {
    // ASSUMPTION: this is being called on a let decl
    let point = scopes::find_stmt_start(msrc, m.point).unwrap();

    let src = &msrc[point..];
    if let Some((start,end)) = codeiter::iter_stmts(src).next() {
        let blob = &src[start..end];
        debug!("get_type_of_let_expr calling parse_let |{}|",blob);

        let pos = m.point - point - start;
        let scope = racer::Scope{ filepath: m.filepath.clone(), point: m.point };
        return ast::get_let_type(blob.to_string(), pos, scope);
    } else {
        return None;
    }
}

fn get_type_of_if_let_expr(m: &Match, msrc: &str) -> Option<racer::Ty> {
    // ASSUMPTION: this is being called on an if let decl
    let stmtstart = scopes::find_stmt_start(msrc, m.point).unwrap();
    let stmt = &msrc[stmtstart..];
    let point = stmt.find("if let").unwrap();
    let src = &stmt[point..];
    let src = generate_skeleton_for_parsing(src);

    if let Some((start, end)) = codeiter::iter_stmts(&*src).next() {
        let blob = &src[start..end];
        debug!("get_type_of_if_let_expr calling parse_if_let |{}|",blob);

        let pos = m.point - stmtstart - point - start;
        let scope = racer::Scope{ filepath: m.filepath.clone(), point: m.point };
        return ast::get_let_type(blob.to_string(), pos, scope);
    } else {
        return None;
    }
}


pub fn get_struct_field_type(fieldname: &str, structmatch: &Match) -> Option<racer::Ty> {
    assert!(structmatch.mtype == racer::MatchType::Struct);

    let src = racer::load_file(&structmatch.filepath);

    let opoint = scopes::find_stmt_start(&*src, structmatch.point);
    let structsrc = scopes::end_of_next_scope(&src[opoint.unwrap()..]);

    let fields = ast::parse_struct_fields(structsrc.to_string(),
                                          racer::Scope::from_match(structmatch));
    for (field, _, ty) in fields.into_iter() {

        if fieldname == field {
            return ty;
        }
    }
    return None;
}

pub fn get_tuplestruct_field_type(fieldnum: u32, structmatch: &Match) -> Option<racer::Ty> {
    let src = racer::load_file(&structmatch.filepath);

    let structsrc = if let racer::MatchType::EnumVariant = structmatch.mtype {
        // decorate the enum variant src to make it look like a tuple struct
        let to = (&src[structmatch.point..]).find("(")
            .map(|n| scopes::find_closing_paren(&*src, structmatch.point + n+1))
            .unwrap();
        "struct ".to_string() + &src[structmatch.point..(to+1)] + ";"
    } else {
        assert!(structmatch.mtype == racer::MatchType::Struct);
        let opoint = scopes::find_stmt_start(&*src, structmatch.point);
        get_first_stmt(&src[opoint.unwrap()..]).to_string()
    };

    debug!("get_tuplestruct_field_type structsrc=|{}|",structsrc);

    let fields = ast::parse_struct_fields(structsrc,
                                          racer::Scope::from_match(structmatch));
    let mut i = 0u32;
    for (_, _, ty) in fields.into_iter() {
        if i == fieldnum {
            return ty;
        }
        i+=1;
    }
    return None;
}

pub fn get_first_stmt(src: &str) -> &str {
    match codeiter::iter_stmts(src).next() {
        Some((from, to)) => &src[from..to],
        None => src
    }
}

pub fn get_type_of_match(m: Match, msrc: &str) -> Option<racer::Ty> {
    debug!("get_type_of match {:?} ",m);

    return match m.mtype {
        racer::MatchType::Let => get_type_of_let_expr(&m, msrc),
        racer::MatchType::IfLet => get_type_of_if_let_expr(&m, msrc),
        racer::MatchType::FnArg => get_type_of_fnarg(&m, msrc),
        racer::MatchType::MatchArm => get_type_from_match_arm(&m, msrc),
        racer::MatchType::Struct => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Enum => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Function => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Module => Some(racer::Ty::TyMatch(m)),
        _ => { debug!("!!! WARNING !!! Can't get type of {:?}",m.mtype); None }
    }
}

macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None})
}

pub fn get_type_from_match_arm(m: &Match, msrc: &str) -> Option<racer::Ty> {
    // We construct a faux match stmt and then parse it. This is because the
    // match stmt may be incomplete (half written) in the real code

    // skip to end of match arm pattern so we can search backwards
    let arm = otry!((&msrc[m.point..]).find("=>")) + m.point;
    let scopestart = scopes::scope_start(msrc, arm);

    let stmtstart = otry!(scopes::find_stmt_start(msrc, scopestart-1));
    debug!("PHIL preblock is {} {}", stmtstart, scopestart);
    let preblock = &msrc[stmtstart..scopestart];
    let matchstart = otry!(util::find_last_str("match ", preblock)) + stmtstart;

    let lhs_start = scopes::get_start_of_pattern(msrc, arm);
    let lhs = &msrc[lhs_start..arm];
    // construct faux match statement and recreate point
    let mut fauxmatchstmt = (&msrc[matchstart..scopestart]).to_string();
    let faux_prefix_size = fauxmatchstmt.len();
    fauxmatchstmt = fauxmatchstmt + lhs + " => () };";
    let faux_point = faux_prefix_size + (m.point - lhs_start);

    debug!("fauxmatchstmt for parsing is pt:{} src:|{}|", faux_point, fauxmatchstmt);

    return ast::get_match_arm_type(fauxmatchstmt, faux_point,
                                   // scope is used to locate expression, so send
                                   // it the start of the match expr
                                   racer::Scope {
                                       filepath: m.filepath.clone(),
                                       point: matchstart
                                   });
}

pub fn get_function_declaration(fnmatch: &Match) -> String {
    let src = racer::load_file(&fnmatch.filepath);
    let start = scopes::find_stmt_start(&*src, fnmatch.point).unwrap();
    let end = (&src[start..]).find('{').unwrap();
    (&src[start..end+start]).to_string()
}

pub fn get_return_type_of_function(fnmatch: &Match) -> Option<racer::Ty> {
    let src = racer::load_file(&fnmatch.filepath);
    let point = scopes::find_stmt_start(&*src, fnmatch.point).unwrap();
    return (&src[point..]).find("{").and_then(|n|{
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(&src[point..(point+n+1)]);
        decl.push_str("}}");
        debug!("get_return_type_of_function: passing in |{}|",decl);
        return ast::parse_fn_output(decl, racer::Scope::from_match(fnmatch));
    });
}
