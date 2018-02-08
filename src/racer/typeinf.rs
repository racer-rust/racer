// Type inference

use core::{Match, Src, Scope, Session, SessionExt, Point};
use nameres::resolve_path_with_str;
use core::Namespace;
use core;
use ast;
use scopes;
use matchers;
use core::SearchType::ExactMatch;
use util::{self, txt_matches};
use std::path::Path;

fn find_start_of_function_body(src: &str) -> Point {
    // TODO: this should ignore anything inside parens so as to skip the arg list
    src.find('{').expect("Function body should have a beginning")
}

// Removes the body of the statement (anything in the braces {...}), leaving just
// the header
// TODO: this should skip parens (e.g. function arguments)
pub fn generate_skeleton_for_parsing(src: &str) -> String {
    let mut s = String::new();
    let n = find_start_of_function_body(src);
    s.push_str(&src[..n+1]);
    s.push_str("};");
    s
}

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
                },
                Some(..) => 0,
            };
            while let Some(start) = blob[skip_generic..].find('(') {
                let end = scopes::find_closing_paren(blob, start + 1);
                let is_self = txt_matches(ExactMatch, "self", &blob[(start + 1)..end]);
                trace!("searching fn args for self: |{}| {}",
                       &blob[(start + 1)..end],
                       is_self);
                return is_self;
            }
            false
        }
    }
}

#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {};", out);
}

fn get_type_of_self_arg(m: &Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    debug!("get_type_of_self_arg {:?}", m);
    get_type_of_self(m.point, &m.filepath, m.local, msrc, session)
}

pub fn get_type_of_self(point: Point, filepath: &Path, local: bool, msrc: Src, session: &Session) -> Option<core::Ty> {
    scopes::find_impl_start(msrc, point, 0).and_then(|start| {
        let decl = generate_skeleton_for_parsing(&msrc.from(start));
        debug!("get_type_of_self_arg impl skeleton |{}|", decl);

        if decl.starts_with("impl") {
            let implres = ast::parse_impl(decl);
            debug!("get_type_of_self_arg implres |{:?}|", implres);
            resolve_path_with_str(&implres.name_path.expect("failed parsing impl name"),
                                  filepath, start,
                                  ExactMatch, Namespace::Type,
                                  session).nth(0).map(core::Ty::Match)
        } else {
            // // must be a trait
            ast::parse_trait(decl).name.and_then(|name| {
                Some(core::Ty::Match(Match {
                    matchstr: name,
                    filepath: filepath.into(),
                    point: start,
                    coords: None,
                    local: local,
                    mtype: core::MatchType::Trait,
                    contextstr: matchers::first_line(&msrc[start..]),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: String::new(),
                }))
            })
        }
    })
}

fn get_type_of_fnarg(m: &Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    if m.matchstr == "self" {
        return get_type_of_self_arg(m, msrc, session);
    }

    let stmtstart = scopes::expect_stmt_start(msrc, m.point);
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

fn get_type_of_let_expr(m: &Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    // ASSUMPTION: this is being called on a let decl
    let point = scopes::find_let_start(msrc, m.point).expect("`let` should have a beginning");
    let src = msrc.from(point);

    if let Some((start, end)) = src.iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_let_expr calling parse_let |{}|", blob);

        let pos = m.point - point - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: point };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

fn get_type_of_let_block_expr(m: &Match, msrc: Src, session: &Session, prefix: &str) -> Option<core::Ty> {
    // ASSUMPTION: this is being called on an if let or while let decl
    let stmtstart = scopes::find_stmt_start(msrc, m.point).expect("`let` should have a beginning");
    let stmt = msrc.from(stmtstart);
    let point = stmt.find(prefix).expect("`prefix` should appear in statement");
    let src = core::new_source(generate_skeleton_for_parsing(&stmt[point..]));

    if let Some((start, end)) = src.as_src().iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_let_block_expr calling get_let_type |{}|", blob);

        let pos = m.point - stmtstart - point - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: stmtstart };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

fn get_type_of_for_expr(m: &Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    let stmtstart = scopes::expect_stmt_start(msrc, m.point);
    let stmt = msrc.from(stmtstart);
    let forpos = stmt.find("for ").expect("`for` should appear in for .. in loop");
    let inpos = stmt.find(" in ").expect("`in` should appear in for .. in loop");
    // XXX: this need not be the correct brace, see generate_skeleton_for_parsing
    let bracepos = stmt.find('{').expect("for-loop should have an opening brace");
    let mut src = stmt[..forpos].to_owned();
    src.push_str("if let Some(");
    src.push_str(&stmt[forpos+4..inpos]);
    src.push_str(") = ");
    let iter_stmt = &stmt[inpos+4..bracepos];

    // TODO: Remove these lines when iter()/iter_mut() method lookup on
    //       built in types is properly supported
    let mut iter_stmt_trimmed = iter_stmt.replace(".iter()", ".into_iter()");
    iter_stmt_trimmed = iter_stmt_trimmed.replace(".iter_mut()", ".into_iter()");

    src.push_str(&iter_stmt_trimmed);
    src = src.trim_right().to_owned();
    src.push_str(".into_iter().next() { }}");

    let src = core::new_source(src);

    if let Some((start, end)) = src.as_src().iter_stmts().next() {
        let blob = &src[start..end];
        debug!("get_type_of_for_expr: |{}| {} {} {} {}", blob, m.point, stmtstart, forpos, start);

        let pos = m.point + 8 - stmtstart - forpos - start;
        let scope = Scope{ filepath: m.filepath.clone(), point: stmtstart };

        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

pub fn get_struct_field_type(fieldname: &str, structmatch: &Match, session: &Session) -> Option<core::Ty> {
    assert!(structmatch.mtype == core::MatchType::Struct);

    let src = session.load_file(&structmatch.filepath);

    let opoint = scopes::expect_stmt_start(src.as_src(), structmatch.point);
    let structsrc = scopes::end_of_next_scope(&src[opoint..]);

    let fields = ast::parse_struct_fields(structsrc.to_owned(), Scope::from_match(structmatch));
    for (field, _, ty) in fields.into_iter() {
        if fieldname == field {
            return ty;
        }
    }
    None
}

pub fn get_tuplestruct_field_type(fieldnum: usize, structmatch: &Match, session: &Session) -> Option<core::Ty> {
    let src = session.load_file(&structmatch.filepath);

    let structsrc = if let core::MatchType::EnumVariant(_) = structmatch.mtype {
        // decorate the enum variant src to make it look like a tuple struct
        let to = src[structmatch.point..].find('(')
            .map(|n| scopes::find_closing_paren(&src, structmatch.point + n+1))
            .expect("Tuple enum variant should have `(` in definition");
        "struct ".to_owned() + &src[structmatch.point..(to+1)] + ";"
    } else {
        assert!(structmatch.mtype == core::MatchType::Struct);
        let opoint = scopes::expect_stmt_start(src.as_src(), structmatch.point);
        (*get_first_stmt(src.as_src().from(opoint))).to_owned()
    };

    debug!("get_tuplestruct_field_type structsrc=|{}|", structsrc);

    let fields = ast::parse_struct_fields(structsrc, Scope::from_match(structmatch));

    for (i, (_, _, ty)) in fields.into_iter().enumerate() {
        if i == fieldnum {
            return ty;
        }
    }
    None
}

pub fn get_first_stmt(src: Src) -> Src {
    match src.iter_stmts().next() {
        Some((from, to)) => src.from_to(from, to),
        None => src
    }
}

pub fn get_type_of_match(m: Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    debug!("get_type_of match {:?} ", m);

    match m.mtype {
        core::MatchType::Let => get_type_of_let_expr(&m, msrc, session),
        core::MatchType::IfLet => get_type_of_let_block_expr(&m, msrc, session, "if let"),
        core::MatchType::WhileLet => get_type_of_let_block_expr(&m, msrc, session, "while let"),
        core::MatchType::For => get_type_of_for_expr(&m, msrc, session),
        core::MatchType::FnArg => get_type_of_fnarg(&m, msrc, session),
        core::MatchType::MatchArm => get_type_from_match_arm(&m, msrc, session),
        core::MatchType::Struct |
        core::MatchType::Enum |
        core::MatchType::Function |
        core::MatchType::Module => Some(core::Ty::Match(m)),
        core::MatchType::EnumVariant(Some(boxed_enum)) => {
            if boxed_enum.mtype == core::MatchType::Enum {
                Some(core::Ty::Match(*boxed_enum))
            } else {
                debug!("EnumVariant has not-enum type: {:?}", boxed_enum.mtype);
                None
            }
        }
        _ => { debug!("!!! WARNING !!! Can't get type of {:?}", m.mtype); None }
    }
}

macro_rules! otry {
    ($e:expr) => (match $e { Some(e) => e, None => return None })
}

pub fn get_type_from_match_arm(m: &Match, msrc: Src, session: &Session) -> Option<core::Ty> {
    // We construct a faux match stmt and then parse it. This is because the
    // match stmt may be incomplete (half written) in the real code

    // skip to end of match arm pattern so we can search backwards
    let arm = otry!(msrc[m.point..].find("=>")) + m.point;
    let scopestart = scopes::scope_start(msrc, arm);

    let stmtstart = otry!(scopes::find_stmt_start(msrc, scopestart-1));
    debug!("PHIL preblock is {} {}", stmtstart, scopestart);
    let preblock = &msrc[stmtstart..scopestart];
    let matchstart = otry!(preblock.rfind("match ")) + stmtstart;

    let lhs_start = scopes::get_start_of_pattern(&msrc, arm);
    let lhs = &msrc[lhs_start..arm];
    // construct faux match statement and recreate point
    let mut fauxmatchstmt = msrc[matchstart..scopestart].to_owned();
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

pub fn get_function_declaration(fnmatch: &Match, session: &Session) -> String {
    let src = session.load_file(&fnmatch.filepath);
    let start = scopes::expect_stmt_start(src.as_src(), fnmatch.point);
    let def_end: &[_] = &['{', ';'];
    let end = src[start..].find(def_end).expect("Definition should have an end (`{` or `;`)");
    src[start..end+start].to_owned()
}

pub fn get_return_type_of_function(fnmatch: &Match, contextm: &Match, session: &Session) -> Option<core::Ty> {
    let src = session.load_file(&fnmatch.filepath);
    let point = scopes::expect_stmt_start(src.as_src(), fnmatch.point);
    let out = src[point..].find(|c| {c == '{' || c == ';'}).and_then(|n| {
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(&src[point..(point+n+1)]);
        if decl.ends_with(';') {
            decl.pop();
            decl.push_str("{}}");
        }
        else {
            decl.push_str("}}");
        }
        debug!("get_return_type_of_function: passing in |{}|", decl);
        ast::parse_fn_output(decl, Scope::from_match(fnmatch))
    });

    // Convert output arg of type Self to the correct type
    if let Some(core::Ty::PathSearch(ref path, _)) = out {
        if let Some(ref path_seg) = path.segments.get(0) {
            if "Self" == path_seg.name {
                return get_type_of_self_arg(fnmatch, src.as_src(), session);
            }
        }
    }

    // Convert a generic output arg to the correct type
    if let Some(core::Ty::PathSearch(ref path, _)) = out {
        if let Some(ref path_seg) = path.segments.get(0) {
            if path.segments.len() == 1 && path_seg.types.is_empty() {
                for type_name in &fnmatch.generic_args {
                    if type_name == &path_seg.name {
                        return Some(core::Ty::Match(contextm.clone()));
                    }
                }
            }
        }
    };
    out
}
