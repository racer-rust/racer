//! Type inference
//! THIS MODULE IS ENTIRELY TOO UGLY SO REALLY NEADS REFACTORING(kngwyu)
use ast;
use ast_types::Ty;
use core;
use core::Namespace;
use core::SearchType::ExactMatch;
use core::{BytePos, MaskedSource, Match, Scope, Session, SessionExt, Src};
use matchers;
use nameres::resolve_path_with_str;
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
                let is_self = txt_matches(ExactMatch, "self", &blob[start.0..end.0]);
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
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {}", out);
}

fn get_type_of_self_arg(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    debug!("get_type_of_self_arg {:?}", m);
    // match m.mtype {
    //     core::MatchType::Method(impl_header) => {}
    //     core::MatchType::Trait => {}
    //     _ => {}
    // }
    get_type_of_self(m.point, &m.filepath, m.local, msrc, session)
}

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
        let implres = ast::parse_impl(decl, filepath, start, local)?;
        debug!("get_type_of_self_arg implres |{:?}|", implres);
        resolve_path_with_str(
            implres.self_path(),
            filepath,
            start,
            ExactMatch,
            Namespace::Type,
            session,
        ).nth(0)
        .map(Ty::Match)
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

// ASSUMPTION: this is being called on an if let or while let decl
fn get_type_of_let_block_expr(m: &Match, msrc: Src, session: &Session, prefix: &str) -> Option<Ty> {
    let stmtstart = scopes::find_stmt_start(msrc, m.point).expect("`let` should have a beginning");
    let stmt = msrc.shift_start(stmtstart);
    let point: BytePos = stmt
        .find(prefix)
        .expect("`prefix` should appear in statement")
        .into();
    let src = MaskedSource::new(&generate_skeleton_for_parsing(&stmt[point.0..]));

    if let Some(range) = src.as_src().iter_stmts().next() {
        let blob = &src[range.to_range()];
        debug!("get_type_of_let_block_expr calling get_let_type |{}|", blob);

        let pos = m.point - stmtstart - point - range.start;
        let scope = Scope {
            filepath: m.filepath.clone(),
            point: stmtstart,
        };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
}

// TODO: it's inefficient(kngwyu)
fn get_type_of_for_expr(m: &Match, msrc: Src, session: &Session) -> Option<Ty> {
    let stmtstart = scopes::expect_stmt_start(msrc, m.point);
    let stmt = msrc.shift_start(stmtstart);
    let forpos = stmt
        .find("for ")
        .expect("`for` should appear in for .. in loop");
    let inpos = stmt
        .find(" in ")
        .expect("`in` should appear in for .. in loop");
    // XXX: this need not be the correct brace, see generate_skeleton_for_parsing
    let bracepos = stmt
        .find('{')
        .expect("for-loop should have an opening brace");
    let mut src = stmt[..forpos].to_owned();
    src.push_str("if let Some(");
    src.push_str(&stmt[forpos + 4..inpos]);
    src.push_str(") = ");
    let iter_stmt = &stmt[inpos + 4..bracepos];

    // TODO: Remove these lines when iter()/iter_mut() method lookup on
    //       built in types is properly supported
    let mut iter_stmt_trimmed = iter_stmt.replace(".iter()", ".into_iter()");
    iter_stmt_trimmed = iter_stmt_trimmed.replace(".iter_mut()", ".into_iter()");

    src.push_str(&iter_stmt_trimmed);
    src = src.trim_right().to_owned();
    src.push_str(".into_iter().next() { }}");

    let src = MaskedSource::new(&src);

    if let Some(range) = src.as_src().iter_stmts().next() {
        let blob = &src[range.to_range()];
        debug!(
            "get_type_of_for_expr: |{}| {:?} {:?} {} {:?}",
            blob, m.point, stmtstart, forpos, range.start
        );

        let pos = m.point + BytePos(8) - stmtstart - forpos.into() - range.start;
        let scope = Scope {
            filepath: m.filepath.clone(),
            point: stmtstart,
        };
        ast::get_let_type(blob.to_owned(), pos, scope, session)
    } else {
        None
    }
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
        if fieldname == field {
            return ty;
        }
    }
    None
}

pub fn get_tuplestruct_field_type(
    fieldnum: usize,
    structmatch: &Match,
    session: &Session,
) -> Option<Ty> {
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
        Some(range) => src.shift_range(range),
        None => src,
    }
}

pub fn get_type_of_match(m: Match, msrc: Src, session: &Session) -> Option<Ty> {
    debug!("get_type_of match {:?} ", m);

    match m.mtype {
        core::MatchType::Let => get_type_of_let_expr(&m, msrc, session),
        core::MatchType::IfLet => get_type_of_let_block_expr(&m, msrc, session, "if let"),
        core::MatchType::WhileLet => get_type_of_let_block_expr(&m, msrc, session, "while let"),
        core::MatchType::For => get_type_of_for_expr(&m, msrc, session),
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
    let out = src[point.0..].find(|c| c == '{' || c == ';').and_then(|n| {
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(&src[point.0..point.0 + n + 1]);
        if decl.ends_with(';') {
            decl.pop();
            decl.push_str("{}}");
        } else {
            decl.push_str("}}");
        }
        debug!("get_return_type_of_function: passing in |{}|", decl);
        ast::parse_fn_output(decl, Scope::from_match(fnmatch))
    });
    // Convert output arg of type Self to the correct type
    if let Some(Ty::PathSearch(ref path, _)) = out {
        if let Some(ref path_seg) = path.segments.get(0) {
            if "Self" == path_seg.name {
                return get_type_of_self_arg(fnmatch, src.as_src(), session);
            }
            if path.segments.len() == 1 && path_seg.types.is_empty() {
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
