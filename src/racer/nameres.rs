//! Name resolving

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{self, vec};

use ast_types::{
    GenericsArgs, ImplHeader, Path as RacerPath, PathPrefix, PathSearch, PathSegment, Ty,
};
use core::Namespace;
use core::SearchType::{self, ExactMatch, StartsWith};
use core::{BytePos, ByteRange, Coordinate, Match, MatchType, Session, SessionExt, Src};
use fileres::{get_crate_file, get_module_file, get_std_file};
use matchers::{find_doc, ImportInfo, MatchCxt};
use util::{
    self, calculate_str_hash, closure_valid_arg_scope, find_ident_end, get_rust_src_path,
    symbol_matches, txt_matches,
};
use {ast, core, matchers, scopes, typeinf};

lazy_static! {
    pub static ref RUST_SRC_PATH: Option<PathBuf> = get_rust_src_path().ok();
}

fn search_struct_fields(
    searchstr: &str,
    structmatch: &Match,
    search_type: SearchType,
    session: &Session,
) -> vec::IntoIter<Match> {
    let src = session.load_source_file(&structmatch.filepath);
    let mut out = Vec::new();

    let struct_start = scopes::expect_stmt_start(src.as_src(), structmatch.point);
    let struct_range = if let Some(end) = scopes::end_of_next_scope(&src[struct_start.0..]) {
        struct_start.0..=(struct_start + end).0
    } else {
        return out.into_iter();
    };
    let structsrc = &src[struct_range.clone()];
    let fields =
        ast::parse_struct_fields(structsrc.to_owned(), core::Scope::from_match(structmatch));

    for (field, field_point, ty) in fields {
        if symbol_matches(search_type, searchstr, &field) {
            let contextstr = if let Some(t) = ty {
                t.to_string()
            } else {
                field.clone()
            };
            let raw_src = session.load_raw_file(&structmatch.filepath);
            out.push(Match {
                matchstr: field,
                filepath: structmatch.filepath.clone(),
                point: field_point + struct_start,
                coords: None,
                local: structmatch.local,
                mtype: MatchType::StructField,
                contextstr: contextstr,
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: find_doc(&raw_src[struct_range.clone()], field_point),
            });
        }
    }
    out.into_iter()
}

pub fn search_for_impl_methods(
    match_request: &Match,
    fieldsearchstr: &str,
    point: BytePos,
    fpath: &Path,
    local: bool,
    search_type: SearchType,
    session: &Session,
) -> vec::IntoIter<Match> {
    let implsearchstr: &str = &match_request.matchstr;

    debug!(
        "searching for impl methods |{:?}| |{}| {:?}",
        match_request,
        fieldsearchstr,
        fpath.display()
    );

    let mut out = Vec::new();

    for m in search_for_impls(
        point,
        implsearchstr,
        fpath,
        local,
        true,
        session,
        &ImportInfo::default(),
    ) {
        debug!("found impl!! |{:?}| looking for methods", m);

        if m.matchstr == "Deref" {
            out.extend(search_for_deref_matches(
                &m,
                match_request,
                fieldsearchstr,
                fpath,
                session,
            ));
        }

        let src = session.load_source_file(&m.filepath);

        // find the opening brace and skip to it.
        if let Some(n) = src[m.point.0..].find('{') {
            let point = m.point.increment() + n.into();
            for m in search_scope_for_methods(
                point,
                src.as_src(),
                fieldsearchstr,
                &m.filepath,
                search_type,
            ) {
                out.push(m);
            }
        }
        for gen_m in
            search_for_generic_impls(m.point, &m.matchstr, match_request, &m.filepath, session)
        {
            debug!("found generic impl!! {:?}", gen_m);
            let src = session.load_source_file(&gen_m.filepath);
            // find the opening brace and skip to it.
            if let Some(n) = src[gen_m.point.0..].find('{') {
                let point = gen_m.point.increment() + n.into();
                for gen_method in search_generic_impl_scope_for_methods(
                    point,
                    src.as_src(),
                    fieldsearchstr,
                    &gen_m,
                    search_type,
                ) {
                    out.push(gen_method);
                }
            }
        }

        if m.matchstr == "Iterator" && fieldsearchstr == "into_iter" {
            let mut m_copy = m.clone();
            if let Ok(mut m_filestring) = m_copy.filepath.into_os_string().into_string() {
                m_filestring = m_filestring.replace("iterator.rs", "traits.rs");
                m_copy.filepath = PathBuf::from(&m_filestring);
                for m in search_for_generic_impls(
                    m_copy.point,
                    &m_copy.matchstr,
                    match_request,
                    &m_copy.filepath,
                    session,
                ) {
                    debug!("found generic impl!! {:?}", m);
                    let src = session.load_source_file(&m.filepath);
                    // find the opening brace and skip to it.
                    if let Some(n) = src[m.point.0..].find('{') {
                        let point = m.point.increment() + n.into();
                        for m in search_generic_impl_scope_for_methods(
                            point,
                            src.as_src(),
                            fieldsearchstr,
                            &m,
                            search_type,
                        ) {
                            out.push(m);
                        }
                    }
                }
            }
        }
    }
    out.into_iter()
}

fn search_scope_for_methods(
    point: BytePos,
    src: Src,
    searchstr: &str,
    filepath: &Path,
    search_type: SearchType,
) -> vec::IntoIter<Match> {
    debug!(
        "searching scope for methods {:?} |{}| {:?}",
        point,
        searchstr,
        filepath.display()
    );

    let scopesrc = src.shift_start(point);
    let mut out = Vec::new();
    for blob_range in scopesrc.iter_stmts() {
        let blob = &scopesrc[blob_range.to_range()];
        if let Some(n) = blob.find(|c| c == '{' || c == ';') {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                && typeinf::first_param_is_self(blob)
            {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly, or, txt_matches should return match pos?
                let start = BytePos::from(blob.find(&format!("fn {}", searchstr)).unwrap() + 3);
                let end = find_ident_end(blob, start);
                let l = &blob[start.0..end.0];
                // TODO: make a better context string for functions
                let m = Match {
                    matchstr: l.to_owned(),
                    filepath: filepath.to_path_buf(),
                    point: point + blob_range.start + start,
                    coords: None,
                    local: true,
                    mtype: MatchType::Function,
                    contextstr: signature.to_owned(),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: find_doc(&scopesrc, blob_range.start + start),
                };
                out.push(m);
            }
        }
    }
    out.into_iter()
}

fn search_generic_impl_scope_for_methods(
    point: BytePos,
    src: Src,
    searchstr: &str,
    contextm: &Match,
    search_type: SearchType,
) -> vec::IntoIter<Match> {
    debug!(
        "searching generic impl scope for methods {:?} |{}| {:?}",
        point,
        searchstr,
        contextm.filepath.display()
    );

    let scopesrc = src.shift_start(point);
    let mut out = Vec::new();
    for blob_range in scopesrc.iter_stmts() {
        let blob = &scopesrc[blob_range.to_range()];
        if let Some(n) = blob.find(|c| c == '{' || c == ';') {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                && typeinf::first_param_is_self(blob)
            {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly, or, txt_matches should return match pos?
                let start = BytePos::from(blob.find(&format!("fn {}", searchstr)).unwrap() + 3);
                let end = find_ident_end(blob, start);
                let l = &blob[start.0..end.0];
                // TODO: make a better context string for functions
                let m = Match {
                    matchstr: l.to_owned(),
                    filepath: contextm.filepath.clone(),
                    point: point + blob_range.start + start,
                    coords: None,
                    local: true,
                    mtype: MatchType::Function,
                    contextstr: signature.to_owned(),
                    generic_args: contextm.generic_args.clone(), // Attach impl generic args
                    generic_types: contextm.generic_types.clone(), // Attach impl generic types
                    docs: find_doc(&scopesrc, blob_range.start + start),
                };
                out.push(m);
            }
        }
    }
    out.into_iter()
}

/// Look for static trait functions. This fn doesn't search for _method_ declarations
/// or implementations as `search_scope_for_methods` already handles that.
fn search_scope_for_static_trait_fns(
    point: BytePos,
    src: Src,
    searchstr: &str,
    filepath: &Path,
    search_type: SearchType,
) -> vec::IntoIter<Match> {
    debug!(
        "searching scope for trait fn declarations {:?} |{}| {:?}",
        point,
        searchstr,
        filepath.display()
    );

    let scopesrc = src.shift_start(point);
    let mut out = Vec::new();
    for blob_range in scopesrc.iter_stmts() {
        let blob = &scopesrc[blob_range.to_range()];
        if let Some(n) = blob.find(|c| c == '{' || c == ';') {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                // filtering out methods here prevents duplicate results with
                // `search_scope_for_methods`
                && !typeinf::first_param_is_self(blob)
            {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly
                // TODO: parse this properly, or, txt_matches should return match pos?
                let start = BytePos::from(blob.find(&format!("fn {}", searchstr)).unwrap() + 3);
                let end = find_ident_end(blob, start);
                let l = &blob[start.0..end.0];
                // TODO: make a better context string for functions
                let m = Match {
                    matchstr: l.to_owned(),
                    filepath: filepath.to_path_buf(),
                    point: point + blob_range.start + start,
                    coords: None,
                    local: true,
                    mtype: MatchType::Function,
                    contextstr: signature.to_owned(),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: find_doc(&scopesrc, blob_range.start + start),
                };
                out.push(m);
            }
        }
    }
    out.into_iter()
}

// this function shouldn't be looked from outer
fn search_for_impls(
    pos: BytePos,
    searchstr: &str,
    filepath: &Path,
    local: bool,
    include_traits: bool,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    debug!(
        "search_for_impls {:?}, {}, {:?}",
        pos,
        searchstr,
        filepath.display()
    );
    let s = session.load_source_file(filepath);
    let scope_start = scopes::scope_start(s.as_src(), pos);
    let src = s.get_src_from_start(scope_start);

    let mut out = Vec::new();
    for blob_range in src.iter_stmts() {
        let blob = &src[blob_range.to_range()];
        if !blob.starts_with("impl") {
            continue;
        }
        blob.find('{').map(|n| {
            let decl = &blob[..n + 1];
            if decl.contains('!') {
                // Guard against macros
                debug!(
                    "impl was probably a macro: {} {:?}",
                    filepath.display(),
                    blob_range.start
                );
                return;
            }
            let mut decl = decl.to_owned();
            decl.push_str("}");
            if txt_matches(ExactMatch, searchstr, &decl) {
                debug!("impl decl {}", decl);
                let implres = ast::parse_impl(decl, filepath);
                let (self_path, trait_path, _) = implres.destruct();
                let is_trait_impl = trait_path.is_some();
                let mtype = if is_trait_impl {
                    MatchType::TraitImpl
                } else {
                    MatchType::Impl
                };

                if let Some(name_path) = self_path {
                    if let Some(name) = name_path.segments.last() {
                        if symbol_matches(ExactMatch, searchstr, &name.name) {
                            let m = Match {
                                matchstr: name.name.clone(),
                                filepath: filepath.to_path_buf(),
                                point: scope_start + blob_range.start + BytePos(5), // 5 = "impl ".len()
                                coords: None,
                                // items in trait impls have no "pub" but are
                                // still accessible from other modules
                                local: local || is_trait_impl,
                                mtype: mtype,
                                contextstr: "".into(),
                                generic_args: Vec::new(),
                                generic_types: Vec::new(),
                                docs: String::new(),
                            };
                            out.push(m);
                        }
                    }
                }

                // find trait
                if include_traits && is_trait_impl {
                    let trait_path = trait_path.unwrap();
                    let m = resolve_path(
                        &trait_path,
                        filepath,
                        scope_start + blob_range.start,
                        ExactMatch,
                        Namespace::Type,
                        session,
                        import_info,
                    ).nth(0);
                    debug!("found trait |{:?}| {:?}", trait_path, m);

                    if let Some(mut m) = m {
                        if m.matchstr == "Deref" {
                            let impl_block = &blob[n..];

                            if let Some(pos) = impl_block.find('=') {
                                let deref_type_start = n + pos + 1;

                                if let Some(pos) = blob[deref_type_start..].find(';') {
                                    let deref_type_end = deref_type_start + pos;
                                    let deref_type = blob[deref_type_start..deref_type_end].trim();
                                    debug!("Deref to {} found", deref_type);

                                    m.generic_args = vec![deref_type.to_owned()];
                                };
                            };
                        }
                        out.push(m);
                    }
                }
            }
        });
    }
    out.into_iter()
}

fn cached_generic_impls(
    filepath: &Path,
    session: &Session,
    scope_start: BytePos,
) -> Rc<Vec<(BytePos, String, GenericsArgs, ImplHeader)>> {
    // the cache is keyed by path and the scope we search in
    session
        .generic_impls
        .borrow_mut()
        .entry((filepath.into(), scope_start))
        .or_insert_with(|| {
            let s = session.load_source_file(&filepath);
            let src = s.get_src_from_start(scope_start);
            let mut out = Vec::new();
            for blob_range in src.iter_stmts() {
                let blob = &src[blob_range.to_range()];

                if blob.starts_with("impl") {
                    blob.find('{').map(|n| {
                        let decl = &blob[..n + 1];
                        if decl.contains('!') {
                            // Guard against macros
                            debug!(
                                "impl was probably a macro: {} {:?}",
                                filepath.display(),
                                blob_range.start
                            );
                            return;
                        }
                        let mut decl = decl.to_owned();
                        decl.push_str("}");
                        let (generics, impls) = ast::parse_generics_and_impl(decl, filepath);
                        out.push((blob_range.start, blob.into(), generics, impls));
                    });
                }
            }
            Rc::new(out)
        }).clone()
}

pub fn search_for_generic_impls(
    pos: BytePos,
    searchstr: &str,
    contextm: &Match,
    filepath: &Path,
    session: &Session,
) -> vec::IntoIter<Match> {
    debug!(
        "search_for_generic_impls {:?}, {}, {:?}",
        pos,
        searchstr,
        filepath.display()
    );
    let s = session.load_source_file(filepath);
    let scope_start = scopes::scope_start(s.as_src(), pos);

    let mut out = Vec::new();
    for &(start, ref blob, ref generics, ref implres) in
        cached_generic_impls(filepath, session, scope_start).iter()
    {
        if let (Some(name_path), Some(trait_path)) = (implres.self_path(), implres.trait_path()) {
            if let (Some(name), Some(trait_name)) =
                (name_path.segments.last(), trait_path.segments.last())
            {
                for gen_arg in &generics.0 {
                    if symbol_matches(ExactMatch, gen_arg.name(), &name.name)
                        && gen_arg.bounds.find_by_name(searchstr).is_some()
                    {
                        debug!("generic impl decl {}", blob);

                        let trait_pos: BytePos = blob
                            .find(&trait_name.name)
                            .expect("[search_for_generic_impls] trait was not found")
                            .into();
                        let self_path = RacerPath::from_vec(false, vec![&contextm.matchstr]);
                        let self_pathsearch = PathSearch {
                            path: self_path,
                            filepath: contextm.filepath.clone(),
                            point: contextm.point,
                        };
                        let m = Match {
                            matchstr: trait_name.name.clone(),
                            filepath: filepath.to_path_buf(),
                            point: start + trait_pos,
                            coords: None,
                            local: true,
                            mtype: MatchType::TraitImpl,
                            contextstr: "".into(),
                            generic_args: vec![gen_arg.name().to_owned()],
                            generic_types: vec![self_pathsearch],
                            docs: String::new(),
                        };
                        debug!("Found a trait! {:?}", m);
                        out.push(m);
                    }
                }
            }
        }
    }
    out.into_iter()
}

// scope headers include fn decls, if let, while let etc..
fn search_scope_headers(
    point: BytePos,
    scopestart: BytePos,
    msrc: Src,
    search_str: &str,
    filepath: &Path,
    search_type: SearchType,
) -> vec::IntoIter<Match> {
    debug!(
        "search_scope_headers for |{}| pt: {:?}",
        search_str, scopestart
    );

    let get_cxt = |len| MatchCxt {
        filepath,
        search_type,
        search_str,
        range: ByteRange::new(0, len),
        is_local: true,
    };
    if let Some(stmtstart) = scopes::find_stmt_start(msrc, scopestart) {
        let preblock = &msrc[stmtstart.0..scopestart.0];
        debug!("search_scope_headers preblock is |{}|", preblock);

        if preblock_is_fn(preblock) {
            return search_fn_args(
                stmtstart,
                scopestart,
                &msrc,
                search_str,
                filepath,
                search_type,
                true,
            );

        // 'if let' can be an expression, so might not be at the start of the stmt
        } else if let Some(n) = preblock.find("if let") {
            let ifletstart = stmtstart + n.into();
            let src = msrc[ifletstart.0..scopestart.increment().0].to_owned() + "}";
            if txt_matches(search_type, search_str, &src) {
                let match_cxt = get_cxt(src.len());
                let mut out = matchers::match_if_let(&src, &match_cxt);
                for m in &mut out {
                    m.point += ifletstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("while let") {
            let src = msrc[stmtstart.0..scopestart.increment().0].to_owned() + "}";
            if txt_matches(search_type, search_str, &src) {
                let match_cxt = get_cxt(src.len());
                let mut out = matchers::match_while_let(&src, &match_cxt);
                for m in &mut out {
                    m.point += stmtstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("for ") {
            let src = msrc[stmtstart.0..scopestart.increment().0].to_owned() + "}";
            if txt_matches(search_type, search_str, &msrc[..scopestart.0]) {
                let match_cxt = get_cxt(src.len());
                let mut out = matchers::match_for(&src, &match_cxt);
                for m in &mut out {
                    m.point += stmtstart;
                }
                return out.into_iter();
            }
        } else if let Some(n) = preblock.rfind("match ") {
            // TODO: this code is crufty. refactor me!
            let matchstart = stmtstart + n.into();
            let matchstmt = typeinf::get_first_stmt(msrc.shift_start(matchstart));
            // The definition could be in the match LHS arms. Try to find this
            let masked_matchstmt = mask_matchstmt(&matchstmt, scopestart.increment() - matchstart);
            debug!(
                "found match stmt, masked is len {} |{}|",
                masked_matchstmt.len(),
                masked_matchstmt
            );

            // Locate the match arm LHS by finding the => just before point and then backtracking
            // be sure to be on the right side of the ... => ... arm
            let arm = match masked_matchstmt[..(point - matchstart).0].rfind("=>") {
                None =>
                // we are in the first arm enum
                {
                    return Vec::new().into_iter()
                }
                Some(arm) => {
                    // be sure not to be in the next arm enum
                    if let Some(next_arm) = masked_matchstmt[arm + 2..].find("=>") {
                        let enum_start = scopes::get_start_of_pattern(
                            &masked_matchstmt,
                            BytePos(arm + next_arm + 1),
                        );
                        if point > matchstart + enum_start {
                            return Vec::new().into_iter();
                        }
                    }
                    BytePos(arm)
                }
            };

            debug!("PHIL matched arm rhs is |{}|", &masked_matchstmt[arm.0..]);

            let lhs_start = scopes::get_start_of_pattern(&msrc, matchstart + arm);
            let lhs = &msrc[lhs_start.0..(matchstart + arm).0];

            // Now create a pretend match expression with just the one match arm in it
            let faux_prefix_size = scopestart.increment() - matchstart;
            let fauxmatchstmt = format!("{}{{{} => () }};", &msrc[matchstart.0..scopestart.0], lhs);

            debug!("PHIL arm lhs is |{}|", lhs);
            debug!(
                "PHIL arm fauxmatchstmt is |{}|, {:?}",
                fauxmatchstmt, faux_prefix_size
            );
            let mut out = Vec::new();
            for pat_range in ast::parse_pat_idents(fauxmatchstmt) {
                let (start, end) = (
                    lhs_start + pat_range.start - faux_prefix_size,
                    lhs_start + pat_range.end - faux_prefix_size,
                );
                let s = &msrc[start.0..end.0];

                if symbol_matches(search_type, search_str, s) {
                    out.push(Match {
                        matchstr: s.to_owned(),
                        filepath: filepath.to_path_buf(),
                        point: start,
                        coords: None,
                        local: true,
                        mtype: MatchType::MatchArm,
                        contextstr: lhs.trim().to_owned(),
                        generic_args: Vec::new(),
                        generic_types: Vec::new(),
                        docs: String::new(),
                    });
                    if let SearchType::ExactMatch = search_type {
                        break;
                    }
                }
            }
            return out.into_iter();
        } else if let Some(vec) =
            search_closure_args(search_str, preblock, stmtstart, filepath, search_type)
        {
            return vec.into_iter();
        }
    }

    Vec::new().into_iter()
}

/// Checks if a scope preblock is a function declaration.
///
/// TODO: Handle `extern` functions
fn preblock_is_fn(preblock: &str) -> bool {
    // Perform simple checks
    if preblock.starts_with("fn")
        || preblock.starts_with("pub fn")
        || preblock.starts_with("const fn")
    {
        return true;
    }

    // Remove visibility declarations, such as restricted visibility
    let trimmed = if preblock.starts_with("pub") {
        util::trim_visibility(preblock)
    } else {
        preblock
    };

    trimmed.starts_with("fn") || trimmed.starts_with("const fn")
}

#[test]
fn is_fn() {
    assert!(preblock_is_fn("pub fn bar()"));
    assert!(preblock_is_fn("fn foo()"));
    assert!(preblock_is_fn("const fn baz()"));
    assert!(preblock_is_fn("pub(crate) fn bar()"));
    assert!(preblock_is_fn("pub(in foo::bar) fn bar()"));
}

fn mask_matchstmt(matchstmt_src: &str, innerscope_start: BytePos) -> String {
    let s = scopes::mask_sub_scopes(&matchstmt_src[innerscope_start.0..]);
    matchstmt_src[..innerscope_start.0].to_owned() + &s
}

#[test]
fn test_mask_match_stmt() {
    let src = "
    match foo {
        Some(a) => { something }
    }";
    let res = mask_matchstmt(src, BytePos(src.find('{').unwrap() + 1));
    debug!("PHIL res is |{}|", res);
}

fn search_fn_args(
    fnstart: BytePos,
    open_brace_pos: BytePos,
    msrc: &str,
    searchstr: &str,
    filepath: &Path,
    search_type: SearchType,
    local: bool,
) -> vec::IntoIter<Match> {
    let mut out = Vec::new();
    let mut fndecl = String::new();
    // wrap in 'impl blah {}' so that methods get parsed correctly too
    fndecl.push_str("impl blah {");
    let impl_header_len = fndecl.len();
    fndecl.push_str(&msrc[fnstart.0..open_brace_pos.increment().0]);
    fndecl.push_str("}}");
    debug!(
        "search_fn_args: found start of fn!! {:?} |{}| {}",
        fnstart, fndecl, searchstr
    );
    if txt_matches(search_type, searchstr, &fndecl) {
        let coords = ast::parse_fn_args(fndecl.clone());

        for arg_range in coords {
            let s = &fndecl[arg_range.to_range()];
            debug!("search_fn_args: arg str is |{}|", s);

            if symbol_matches(search_type, searchstr, s) {
                let m = Match {
                    matchstr: s.to_owned(),
                    filepath: filepath.to_path_buf(),
                    point: fnstart + arg_range.start - impl_header_len.into(),
                    coords: None,
                    local: local,
                    mtype: MatchType::FnArg,
                    contextstr: s.to_owned(),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: String::new(),
                };
                debug!("search_fn_args matched: {:?}", m);
                out.push(m);
            }
        }
    }
    out.into_iter()
}

#[test]
fn test_do_file_search_std() {
    let cache = core::FileCache::default();
    let session = Session::new(&cache);
    let mut matches = do_file_search("std", &Path::new("."), &session);
    assert!(matches.any(|m| m.filepath.ends_with("src/libstd/lib.rs")));
}

#[test]
fn test_do_file_search_local() {
    let cache = core::FileCache::default();
    let session = Session::new(&cache);
    let mut matches = do_file_search("submodule", &Path::new("fixtures/arst/src"), &session);
    assert!(matches.any(|m| m.filepath.ends_with("fixtures/arst/src/submodule/mod.rs")));
}

pub fn do_file_search(
    searchstr: &str,
    currentdir: &Path,
    session: &Session,
) -> vec::IntoIter<Match> {
    debug!("do_file_search with search string \"{}\"", searchstr);
    let mut out = Vec::new();

    let std_path = RUST_SRC_PATH.as_ref();
    debug!("do_file_search std_path: {:?}", std_path);

    let (v_1, v_2);
    let v = if let Some(std_path) = std_path {
        v_2 = [std_path, currentdir];
        &v_2[..]
    } else {
        v_1 = [currentdir];
        &v_1[..]
    };

    debug!("do_file_search v: {:?}", v);
    for srcpath in v {
        if let Ok(iter) = std::fs::read_dir(srcpath) {
            for fpath_buf in iter.filter_map(|res| res.ok().map(|entry| entry.path())) {
                // skip filenames that can't be decoded
                let fname = match fpath_buf.file_name().and_then(|n| n.to_str()) {
                    Some(fname) => fname,
                    None => continue,
                };
                if fname.starts_with(&format!("lib{}", searchstr)) {
                    let filepath = fpath_buf.join("lib.rs");
                    if filepath.exists() || session.contains_file(&filepath) {
                        let m = Match {
                            matchstr: fname[3..].to_owned(),
                            filepath: filepath.to_path_buf(),
                            point: BytePos::ZERO,
                            coords: Some(Coordinate::start()),
                            local: false,
                            mtype: MatchType::Module,
                            contextstr: fname[3..].to_owned(),
                            generic_args: Vec::new(),
                            generic_types: Vec::new(),
                            docs: String::new(),
                        };
                        out.push(m);
                    }
                }

                if fname.starts_with(searchstr) {
                    for name in &[&format!("{}.rs", fname)[..], "mod.rs", "lib.rs"] {
                        let filepath = fpath_buf.join(name);
                        if filepath.exists() || session.contains_file(&filepath) {
                            let m = Match {
                                matchstr: fname.to_owned(),
                                filepath: filepath.to_path_buf(),
                                point: BytePos::ZERO,
                                coords: Some(Coordinate::start()),
                                local: false,
                                mtype: MatchType::Module,
                                contextstr: filepath.to_str().unwrap().to_owned(),
                                generic_args: Vec::new(),
                                generic_types: Vec::new(),
                                docs: String::new(),
                            };
                            out.push(m);
                        }
                    }
                    // try just <name>.rs
                    if fname.ends_with(".rs")
                        && (fpath_buf.exists() || session.contains_file(&fpath_buf))
                    {
                        let m = Match {
                            matchstr: fname[..(fname.len() - 3)].to_owned(),
                            filepath: fpath_buf.clone(),
                            point: BytePos::ZERO,
                            coords: Some(Coordinate::start()),
                            local: false,
                            mtype: MatchType::Module,
                            contextstr: fpath_buf.to_str().unwrap().to_owned(),
                            generic_args: Vec::new(),
                            generic_types: Vec::new(),
                            docs: String::new(),
                        };
                        out.push(m);
                    }
                }
            }
        }
    }
    out.into_iter()
}

pub fn search_crate_root(
    pathseg: &PathSegment,
    modfpath: &Path,
    searchtype: SearchType,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    debug!("search_crate_root |{:?}| {:?}", pathseg, modfpath.display());

    let crateroots = find_possible_crate_root_modules(modfpath.parent().unwrap(), session);
    let mut out = Vec::new();
    for crateroot in crateroots {
        if *modfpath == *crateroot {
            continue;
        }
        debug!(
            "going to search for {:?} in crateroot {:?}",
            pathseg,
            crateroot.display()
        );
        for m in resolve_name(
            pathseg,
            &crateroot,
            BytePos::ZERO,
            searchtype,
            namespace,
            session,
            import_info,
        ) {
            out.push(m);
            if let ExactMatch = searchtype {
                break;
            }
        }
        break;
    }
    out.into_iter()
}

pub fn find_possible_crate_root_modules(currentdir: &Path, session: &Session) -> Vec<PathBuf> {
    let mut res = Vec::new();

    for root in &["lib.rs", "main.rs"] {
        let filepath = currentdir.join(root);
        if filepath.exists() || session.contains_file(&filepath) {
            res.push(filepath);
            return res; // for now stop at the first match
        }
    }
    // recurse up the directory structure
    if let Some(parentdir) = currentdir.parent() {
        if parentdir != currentdir {
            // PD: this was using the vec.push_all() api, but that is now unstable
            res.extend(
                find_possible_crate_root_modules(parentdir, session)
                    .iter()
                    .cloned(),
            );
            return res; // for now stop at the first match
        }
    }
    res
}

pub fn search_next_scope(
    mut startpoint: BytePos,
    pathseg: &PathSegment,
    filepath: &Path,
    search_type: SearchType,
    local: bool,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    let filesrc = session.load_source_file(filepath);
    if startpoint != BytePos::ZERO {
        // is a scope inside the file. Point should point to the definition
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = &filesrc[startpoint.0..];
        //debug!("search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it.
        if let Some(n) = src.find('{') {
            startpoint += BytePos(n + 1);
        }
    }
    search_scope(
        startpoint,
        startpoint,
        filesrc.as_src(),
        pathseg,
        filepath,
        search_type,
        local,
        namespace,
        session,
        import_info,
    )
}

pub fn search_scope(
    start: BytePos,
    point: BytePos,
    src: Src,
    pathseg: &PathSegment,
    filepath: &Path,
    search_type: SearchType,
    is_local: bool,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    let search_str = &pathseg.name;
    let mut out = Vec::new();

    debug!(
        "searching scope {:?} start: {:?} point: {:?} '{}' {:?} {:?} local: {}, session: {:?}",
        namespace,
        start,
        point,
        search_str,
        filepath.display(),
        search_type,
        is_local,
        session
    );

    let scopesrc = src.shift_start(start);
    let mut skip_next_block = false;
    let mut delayed_single_imports = Vec::new();
    let mut delayed_glob_imports = Vec::new();
    let mut codeit = scopesrc.iter_stmts();
    let mut v = Vec::new();

    // collect up to point so we can search backwards for let bindings
    //  (these take precidence over local fn declarations etc..
    for blob_range in &mut codeit {
        //  (e.g. #[cfg(test)])
        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = &scopesrc[blob_range.to_range()];

        // for now skip stuff that's meant for testing. Often the test
        // module hierarchy is incompatible with the non-test
        // hierarchy and we get into recursive loops
        if blob.starts_with("#[cfg(test)") {
            skip_next_block = true;
            continue;
        }

        v.push(blob_range);

        if blob_range.start > point {
            break;
        }
    }
    let get_match_cxt = |range| MatchCxt {
        filepath,
        search_str,
        search_type,
        is_local,
        range,
    };
    // search backwards from point for let bindings
    for &blob_range in v.iter().rev() {
        if (start + blob_range.end) >= point {
            continue;
        }
        let match_cxt = get_match_cxt(blob_range.shift(start));
        for m in matchers::match_let(&src, &match_cxt) {
            out.push(m);
            if let ExactMatch = search_type {
                return out.into_iter();
            }
        }
    }

    // since we didn't find a `let` binding, now search from top of scope for items etc..
    let mut codeit = v.into_iter().chain(codeit);
    for blob_range in &mut codeit {
        // sometimes we need to skip blocks of code if the preceeding attribute disables it
        //  (e.g. #[cfg(test)])
        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = &scopesrc[blob_range.to_range()];
        // for now skip stuff that's meant for testing. Often the test
        // module hierarchy is incompatible with the non-test
        // hierarchy and we get into recursive loops
        if blob.starts_with("#[cfg(test)") {
            skip_next_block = true;
            continue;
        }

        let is_an_import = blob.starts_with("use") || blob.starts_with("pub use");

        if is_an_import {
            // A `use` item can import a value
            // with the same name as a "type" (type/module/etc.) in the same scope.
            // However, that type might appear after the `use`,
            // so we need to process the type first and the `use` later (if necessary).
            // If we didn't delay imports,
            // we'd try to resolve such a `use` item by recursing onto itself.

            // Optimisation: if the search string is not in the blob and it is not
            // a glob import, this cannot match so fail fast!
            let is_glob_import = blob.contains("::*");
            if !is_glob_import && !blob.contains(search_str.trim_right_matches('!')) {
                continue;
            }

            if is_glob_import {
                delayed_glob_imports.push(blob_range);
            } else {
                delayed_single_imports.push(blob_range);
            }

            continue;
        }

        if search_str == "core" && blob.starts_with("#![no_std]") {
            debug!("Looking for core and found #![no_std], which implicitly imports it");
            if let Some(cratepath) = get_crate_file("core", filepath, session) {
                let context = cratepath.to_str().unwrap().to_owned();
                out.push(Match {
                    matchstr: "core".into(),
                    filepath: cratepath,
                    point: BytePos::ZERO,
                    coords: Some(Coordinate::start()),
                    local: false,
                    mtype: MatchType::Module,
                    contextstr: context,
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: String::new(),
                });
            }
        }

        // Optimisation: if the search string is not in the blob,
        // this cannot match so fail fast!
        if !blob.contains(search_str.trim_right_matches('!')) {
            continue;
        }

        // if we find extern block, let's look up inner scope
        if blob.starts_with("extern") {
            if let Some(block_start) = blob[7..].find('{') {
                debug!("[search_scope] found extern block!");
                // move to the point next to {
                let src = src
                    .shift_range(blob_range)
                    .shift_start(BytePos(block_start + 8));
                out.extend(search_scope(
                    BytePos::ZERO,
                    BytePos::ZERO,
                    src,
                    pathseg,
                    filepath,
                    search_type,
                    is_local,
                    namespace,
                    session,
                    import_info,
                ));
                continue;
            }
        }
        // There's a good chance of a match. Run the matchers
        let match_cxt = get_match_cxt(blob_range.shift(start));
        out.extend(run_matchers_on_blob(
            src,
            &match_cxt,
            namespace,
            session,
            import_info,
        ));
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    let delayed_import_len = delayed_single_imports.len() + delayed_glob_imports.len();

    if delayed_import_len > 0 {
        trace!(
            "Searching {} delayed imports for `{}`",
            delayed_import_len,
            search_str
        );
    }

    // Finally, process the imports that we skipped before.
    // Process single imports first, because they shadow glob imports.
    for blob_range in delayed_single_imports
        .into_iter()
        .chain(delayed_glob_imports)
    {
        // There's a good chance of a match. Run the matchers
        let match_cxt = get_match_cxt(blob_range.shift(start));
        for m in run_matchers_on_blob(src, &match_cxt, namespace, session, import_info) {
            out.push(m);
            if let ExactMatch = search_type {
                return out.into_iter();
            }
        }
    }

    if let Some(vec) = search_closure_args(search_str, &scopesrc[0..], start, filepath, search_type)
    {
        for mat in vec {
            out.push(mat)
        }

        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }

    debug!("search_scope found matches {:?} {:?}", search_type, out);
    out.into_iter()
}

fn search_closure_args(
    search_str: &str,
    scope_src: &str,
    scope_src_pos: BytePos,
    filepath: &Path,
    search_type: SearchType,
) -> Option<Vec<Match>> {
    if search_str.is_empty() {
        return None;
    }

    trace!(
        "Closure definition match is looking for `{}` in {} characters",
        search_str,
        scope_src.len()
    );

    if let Some((pipe_range, pipe_str)) = closure_valid_arg_scope(scope_src) {
        debug!(
            "search_closure_args found valid closure arg scope: {}",
            pipe_str
        );

        if txt_matches(search_type, search_str, pipe_str) {
            // Add a fake body for parsing
            let closure_def = String::from(pipe_str) + "{}";

            let coords = ast::parse_fn_args(closure_def.clone());

            let mut out: Vec<Match> = Vec::new();

            for arg_range in coords {
                let s = &closure_def[arg_range.to_range()];

                if symbol_matches(search_type, search_str, s) {
                    let m = Match {
                        matchstr: s.to_owned(),
                        filepath: filepath.to_path_buf(),
                        point: scope_src_pos + pipe_range.start + arg_range.start,
                        coords: None,
                        local: true,
                        mtype: MatchType::FnArg,
                        contextstr: pipe_str.to_owned(),
                        generic_args: Vec::new(),
                        generic_types: Vec::new(),
                        docs: String::new(),
                    };
                    debug!("search_closure_args matched: {:?}", m);
                    out.push(m);
                }
            }

            return Some(out);
        }
    }

    None
}

fn run_matchers_on_blob(
    src: Src,
    context: &MatchCxt,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> Vec<Match> {
    debug!(
        "[run_matchers_on_blob] src: {}, cxt: {:?}, namespace: {:?}",
        &src[context.range.to_range()],
        context,
        namespace
    );
    let mut out = Vec::new();
    if namespace.contains(Namespace::Type) {
        for m in matchers::match_types(src, context, session, import_info) {
            out.push(m);
            if context.search_type == ExactMatch {
                return out;
            }
        }
    }
    if namespace.contains(Namespace::Value) {
        for m in matchers::match_values(src, context, session) {
            out.push(m);
            if context.search_type == ExactMatch {
                return out;
            }
        }
    }
    if namespace.contains(Namespace::Macro) {
        if let Some(m) = matchers::match_macro(src, context, session) {
            out.push(m);
        }
    }
    out
}

fn search_local_scopes(
    pathseg: &PathSegment,
    filepath: &Path,
    msrc: Src,
    point: BytePos,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    debug!(
        "search_local_scopes {:?} {:?} {:?} {:?} {:?}",
        pathseg,
        filepath.display(),
        point,
        search_type,
        namespace
    );

    if point == BytePos::ZERO {
        // search the whole file
        search_scope(
            BytePos::ZERO,
            BytePos::ZERO,
            msrc,
            pathseg,
            filepath,
            search_type,
            true,
            namespace,
            session,
            import_info,
        )
    } else {
        let mut out = Vec::new();
        let mut start = point;
        // search each parent scope in turn
        while start > BytePos::ZERO {
            start = scopes::scope_start(msrc, start);
            for m in search_scope(
                start,
                point,
                msrc,
                pathseg,
                filepath,
                search_type,
                true,
                namespace,
                session,
                import_info,
            ) {
                out.push(m);
                if search_type == ExactMatch {
                    return out.into_iter();
                }
            }
            if start == BytePos::ZERO {
                break;
            }
            start = start.decrement();
            let searchstr = &pathseg.name;

            // scope headers = fn decls, if let, match, etc..
            for m in search_scope_headers(point, start, msrc, searchstr, filepath, search_type) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out.into_iter();
                }
            }
        }
        out.into_iter()
    }
}

pub fn search_prelude_file(
    pathseg: &PathSegment,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    debug!(
        "search_prelude file {:?} {:?} {:?}",
        pathseg, search_type, namespace
    );
    let mut out: Vec<Match> = Vec::new();

    // find the prelude file from the search path and scan it
    if let Some(ref std_path) = *RUST_SRC_PATH {
        let filepath = std_path.join("libstd").join("prelude").join("v1.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            let msrc = session.load_source_file(&filepath);
            let is_local = true;
            for m in search_scope(
                BytePos::ZERO,
                BytePos::ZERO,
                msrc.as_src(),
                pathseg,
                &filepath,
                search_type,
                is_local,
                namespace,
                session,
                import_info,
            ) {
                out.push(m);
            }
        }
    }
    out.into_iter()
}

pub fn resolve_path_with_str(
    path: &RacerPath,
    filepath: &Path,
    pos: BytePos,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
) -> vec::IntoIter<Match> {
    debug!("resolve_path_with_str {:?}", path);

    let mut out = Vec::new();

    // HACK
    if path.segments.len() == 1 && path.segments[0].name == "str" {
        debug!("{:?} == {:?}", path.segments[0], "str");

        if let Some(module) = resolve_path(
            &RacerPath::from_vec(true, vec!["std", "str"]),
            filepath,
            pos,
            search_type,
            namespace,
            session,
            &ImportInfo::default(),
        ).nth(0)
        {
            out.push(Match {
                matchstr: "str".into(),
                filepath: module.filepath,
                point: BytePos::ZERO,
                coords: Some(Coordinate::start()),
                local: false,
                mtype: MatchType::Builtin,
                contextstr: "str".into(),
                generic_args: vec![],
                generic_types: vec![],
                docs: String::new(),
            });
        }
    } else {
        for m in resolve_path(
            path,
            filepath,
            pos,
            search_type,
            namespace,
            session,
            &ImportInfo::default(),
        ) {
            out.push(m);
            if let ExactMatch = search_type {
                break;
            }
        }
    }
    out.into_iter()
}

#[derive(PartialEq, Debug)]
pub struct Search {
    path: Vec<String>,
    filepath: String,
    pos: BytePos,
}

/// Attempt to resolve a name which occurs in a given file.
pub fn resolve_name(
    pathseg: &PathSegment,
    filepath: &Path,
    pos: BytePos,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    let mut out = Vec::new();
    let searchstr = &pathseg.name;

    let msrc = session.load_source_file(filepath);
    let is_exact_match = search_type == ExactMatch;

    if is_exact_match && &searchstr[..] == "Self" {
        if let Some(Ty::Match(m)) =
            typeinf::get_type_of_self(pos, filepath, true, msrc.as_src(), session)
        {
            out.push(m.clone());
        }
    }

    if (is_exact_match && &searchstr[..] == "std")
        || (!is_exact_match && "std".starts_with(searchstr))
    {
        if let Some(cratepath) = get_std_file("std", session) {
            let context = cratepath.to_str().unwrap().to_owned();
            out.push(Match {
                matchstr: "std".into(),
                filepath: cratepath,
                point: BytePos::ZERO,
                coords: Some(Coordinate::start()),
                local: false,
                mtype: MatchType::Module,
                contextstr: context,
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: String::new(),
            });
        }

        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    for m in search_local_scopes(
        pathseg,
        filepath,
        msrc.as_src(),
        pos,
        search_type,
        namespace,
        session,
        import_info,
    ) {
        out.push(m);
        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }

    for m in search_crate_root(
        pathseg,
        filepath,
        search_type,
        namespace,
        session,
        import_info,
    ) {
        out.push(m);
        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }

    for m in search_prelude_file(pathseg, search_type, namespace, session, import_info) {
        out.push(m);
        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }
    // filesearch. Used to complete e.g. extern crate blah or mod foo
    if let StartsWith = search_type {
        for m in do_file_search(searchstr, filepath.parent().unwrap(), session) {
            out.push(m);
        }
    }

    if namespace.contains(Namespace::Macro) {
        get_std_macros(searchstr, search_type, session, &mut out);
    }
    out.into_iter()
}

// Get the scope corresponding to super::
pub fn get_super_scope(
    filepath: &Path,
    pos: BytePos,
    session: &Session,
    import_info: &ImportInfo,
) -> Option<core::Scope> {
    let msrc = session.load_source_file(filepath);
    let mut path = scopes::get_local_module_path(msrc.as_src(), pos);
    debug!(
        "get_super_scope: path: {:?} filepath: {:?} {:?} {:?}",
        path, filepath, pos, session
    );
    if path.is_empty() {
        let moduledir = if filepath.ends_with("mod.rs") || filepath.ends_with("lib.rs") {
            // Need to go up to directory above
            // TODO(PD): fix: will crash if mod.rs is in the root fs directory
            filepath.parent()?.parent()?
        } else {
            // module is in current directory
            filepath.parent()?
        };

        for filename in &["mod.rs", "lib.rs"] {
            let f_path = moduledir.join(&filename);
            if f_path.exists() || session.contains_file(&f_path) {
                return Some(core::Scope {
                    filepath: f_path,
                    point: BytePos::ZERO,
                });
            }
        }
        None
    } else if path.len() == 1 {
        Some(core::Scope {
            filepath: filepath.to_path_buf(),
            point: BytePos::ZERO,
        })
    } else {
        path.pop();
        let path = RacerPath::from_svec(false, path);
        debug!("get_super_scope looking for local scope {:?}", path);
        resolve_path(
            &path,
            filepath,
            BytePos::ZERO,
            SearchType::ExactMatch,
            Namespace::Type,
            session,
            import_info,
        ).nth(0)
        .and_then(|m| {
            msrc[m.point.0..].find('{').map(|p| core::Scope {
                filepath: filepath.to_path_buf(),
                point: m.point + BytePos(p + 1),
            })
        })
    }
}

fn get_impls(
    search_path: &PathSegment,
    namespace: Namespace,
    search_type: SearchType,
    context: &Match,
    session: &Session,
    import_info: &ImportInfo,
) -> Vec<Match> {
    let mut out = Vec::new();
    match context.mtype {
        MatchType::Enum => {
            let filesrc = session.load_source_file(&context.filepath);
            let scopestart = scopes::find_stmt_start(filesrc.as_src(), context.point)
                .expect("[resolve_path] statement start was not found");
            let scopesrc = filesrc.get_src_from_start(scopestart);
            if let Some(blob_range) = scopesrc.iter_stmts().nth(0) {
                let match_cxt = MatchCxt {
                    filepath: &context.filepath,
                    search_str: &search_path.name,
                    search_type,
                    range: blob_range.shift(scopestart),
                    is_local: true,
                };
                for mut enum_var in matchers::match_enum_variants(&filesrc, &match_cxt) {
                    debug!(
                        "Found enum variant {} with enum type {}",
                        enum_var.matchstr, context.matchstr
                    );
                    // return Match which has enum simultaneously, for method completion
                    enum_var.mtype = MatchType::EnumVariant(Some(Box::new(context.clone())));
                    out.push(enum_var);
                }
            }
        }
        MatchType::Struct => {}
        _ => return out,
    }

    for m_impl in search_for_impls(
        context.point,
        &context.matchstr,
        &context.filepath,
        context.local,
        true,
        session,
        import_info,
    ) {
        debug!("found impl!! {:?}", m_impl);
        let src = session.load_source_file(&m_impl.filepath);
        // find the opening brace and skip to it.
        if let Some(n) = src[m_impl.point.0..].find('{') {
            let point = m_impl.point + BytePos(n + 1);
            out.extend(search_scope(
                point,
                point,
                src.as_src(),
                &search_path,
                &m_impl.filepath,
                search_type,
                m_impl.local,
                namespace,
                session,
                import_info,
            ));
        }
        for m_gen in search_for_generic_impls(
            m_impl.point,
            &m_impl.matchstr,
            &context,
            &m_impl.filepath,
            session,
        ) {
            debug!("found generic impl!! {:?}", m_gen);
            let src = session.load_source_file(&m_gen.filepath);
            // find the opening brace and skip to it.
            if let Some(n) = src[m_gen.point.0..].find('{') {
                let point = m_gen.point + BytePos(n + 1);
                out.extend(search_scope(
                    point,
                    point,
                    src.as_src(),
                    search_path,
                    &m_gen.filepath,
                    search_type,
                    m_gen.local,
                    namespace,
                    session,
                    import_info,
                ));
            }
        }
    }
    out
}

pub fn resolve_path(
    path: &RacerPath,
    filepath: &Path,
    pos: BytePos,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
    import_info: &ImportInfo,
) -> vec::IntoIter<Match> {
    debug!(
        "resolve_path {:?} {:?} {:?} {:?}",
        path,
        filepath.display(),
        pos,
        search_type
    );
    let len = path.len();
    if let Some(ref prefix) = path.prefix {
        match prefix {
            // TODO: Crate, Self,..
            PathPrefix::Super => {
                if let Some(scope) = get_super_scope(filepath, pos, session, import_info) {
                    debug!("PHIL super scope is {:?}", scope);
                    let mut newpath = path.clone();
                    newpath.prefix = None;
                    return resolve_path(
                        &newpath,
                        &scope.filepath,
                        scope.point,
                        search_type,
                        namespace,
                        session,
                        import_info,
                    );
                } else {
                    // can't find super scope. Return no matches
                    debug!("can't resolve path {:?}, returning no matches", path);
                    return Vec::new().into_iter();
                }
            }
            _ => {}
        }
    }
    if len == 1 {
        let pathseg = &path.segments[0];
        resolve_name(
            pathseg,
            filepath,
            pos,
            search_type,
            namespace,
            session,
            import_info,
        )
    } else if len != 0 {
        let mut out = Vec::new();
        let mut parent_path = path.clone();
        parent_path.segments.pop();
        let context = resolve_path(
            &parent_path,
            filepath,
            pos,
            ExactMatch,
            Namespace::Type,
            session,
            import_info,
        ).nth(0);
        context.map(|m| match m.mtype {
            MatchType::Module => {
                let mut searchstr: &str = &path.segments[len - 1].name;
                if let Some(i) = searchstr.rfind(',') {
                    searchstr = searchstr[i + 1..].trim();
                }
                if searchstr.starts_with('{') {
                    searchstr = &searchstr[1..];
                }
                let pathseg = PathSegment {
                    name: searchstr.to_owned(),
                    types: Vec::new(),
                };
                debug!(
                    "searching a module '{}' for {} (whole path: {:?})",
                    m.matchstr, pathseg.name, path
                );
                out.extend(search_next_scope(
                    m.point,
                    &pathseg,
                    &m.filepath,
                    search_type,
                    false,
                    namespace,
                    session,
                    import_info,
                ));
            }
            MatchType::Enum | MatchType::Struct => {
                out.extend(get_impls(
                    &path.segments[len - 1],
                    namespace,
                    search_type,
                    &m,
                    session,
                    import_info,
                ));
            }
            MatchType::Type => {
                if let Some(match_) = ast::get_type_of_typedef(&m, session) {
                    out.extend(get_impls(
                        &path.segments[len - 1],
                        namespace,
                        search_type,
                        &match_,
                        session,
                        import_info,
                    ));
                }
            }
            _ => (),
        });
        debug!("resolve_path returning {:?}", out);
        out.into_iter()
    } else {
        // TODO: Should this better be an assertion ? Why do we have a core::Path
        // with empty segments in the first place ?
        Vec::new().into_iter()
    }
}

pub fn resolve_method(
    point: BytePos,
    msrc: Src,
    searchstr: &str,
    filepath: &Path,
    search_type: SearchType,
    session: &Session,
    import_info: &ImportInfo,
) -> Vec<Match> {
    let scopestart = scopes::scope_start(msrc, point);
    debug!(
        "resolve_method for |{}| pt: {:?}; scopestart: {:?}",
        searchstr, point, scopestart,
    );

    if let Some(stmtstart) = scopes::find_stmt_start(msrc, scopestart.decrement()) {
        let preblock = &msrc[stmtstart.0..scopestart.0];
        debug!("search_scope_headers preblock is |{}|", preblock);

        if preblock.starts_with("impl") {
            if let Some(n) = preblock.find(" for ") {
                let start = scopes::get_start_of_search_expr(preblock, n.into());
                let expr = &preblock[start.0..n];

                debug!("found impl of trait : expr is |{}|", expr);
                let path = RacerPath::from_vec(false, expr.split("::").collect::<Vec<_>>());
                let m = resolve_path(
                    &path,
                    filepath,
                    stmtstart + BytePos(n - 1),
                    SearchType::ExactMatch,
                    Namespace::Both,
                    session,
                    import_info,
                ).filter(|m| m.mtype == MatchType::Trait)
                .nth(0);
                if let Some(m) = m {
                    debug!("found trait : match is |{:?}|", m);
                    let mut out = Vec::new();
                    let src = session.load_source_file(&m.filepath);
                    if let Some(n) = src[m.point.0..].find('{') {
                        let point = m.point + BytePos(n + 1);
                        for m in search_scope_for_static_trait_fns(
                            point,
                            src.as_src(),
                            searchstr,
                            &m.filepath,
                            search_type,
                        ) {
                            out.push(m);
                        }
                        for m in search_scope_for_methods(
                            point,
                            src.as_src(),
                            searchstr,
                            &m.filepath,
                            search_type,
                        ) {
                            out.push(m);
                        }
                    }

                    trace!(
                        "Found {} methods matching `{}` for trait `{}`",
                        out.len(),
                        searchstr,
                        m.matchstr
                    );

                    return out;
                }
            }
        }
    }

    Vec::new()
}

pub fn do_external_search(
    path: &[&str],
    filepath: &Path,
    pos: BytePos,
    search_type: SearchType,
    namespace: Namespace,
    session: &Session,
) -> vec::IntoIter<Match> {
    debug!(
        "do_external_search path {:?} {:?}",
        path,
        filepath.display()
    );
    let mut out = Vec::new();
    if path.len() == 1 {
        let searchstr = path[0];
        // hack for now
        let pathseg = PathSegment {
            name: searchstr.to_owned(),
            types: Vec::new(),
        };
        out.extend(search_next_scope(
            pos,
            &pathseg,
            filepath,
            search_type,
            false,
            namespace,
            session,
            &ImportInfo::default(),
        ));

        if let Some(path) = get_module_file(searchstr, filepath.parent().unwrap(), session) {
            let context = path.to_str().unwrap().to_owned();
            out.push(Match {
                matchstr: searchstr.to_owned(),
                filepath: path,
                point: BytePos::ZERO,
                coords: Some(Coordinate::start()),
                local: false,
                mtype: MatchType::Module,
                contextstr: context,
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: String::new(),
            });
        }
    } else {
        let parent_path = &path[..(path.len() - 1)];
        let context = do_external_search(
            parent_path,
            filepath,
            pos,
            ExactMatch,
            Namespace::Type,
            session,
        ).nth(0);
        context.map(|m| {
            let import_info = &ImportInfo::default();
            match m.mtype {
                MatchType::Module => {
                    debug!("found an external module {}", m.matchstr);
                    // deal with started with "{", so that "foo::{bar" will be same as "foo::bar"
                    let searchstr = match path[path.len() - 1].chars().next() {
                        Some('{') => &path[path.len() - 1][1..],
                        _ => path[path.len() - 1],
                    };
                    let pathseg = PathSegment {
                        name: searchstr.to_owned(),
                        types: Vec::new(),
                    };
                    for m in search_next_scope(
                        m.point,
                        &pathseg,
                        &m.filepath,
                        search_type,
                        false,
                        namespace,
                        session,
                        import_info,
                    ) {
                        out.push(m);
                    }
                }

                MatchType::Struct => {
                    debug!("found a pub struct. Now need to look for impl");
                    for m in search_for_impls(
                        m.point,
                        &m.matchstr,
                        &m.filepath,
                        m.local,
                        false,
                        session,
                        import_info,
                    ) {
                        debug!("found  impl2!! {}", m.matchstr);
                        // deal with started with "{", so that "foo::{bar" will be same as "foo::bar"
                        let searchstr = match path[path.len() - 1].chars().next() {
                            Some('{') => &path[path.len() - 1][1..],
                            _ => path[path.len() - 1],
                        };
                        let pathseg = PathSegment {
                            name: searchstr.to_owned(),
                            types: Vec::new(),
                        };
                        debug!("about to search impl scope...");
                        for m in search_next_scope(
                            m.point,
                            &pathseg,
                            &m.filepath,
                            search_type,
                            m.local,
                            namespace,
                            session,
                            import_info,
                        ) {
                            out.push(m);
                        }
                    }
                }
                _ => (),
            }
        });
    }
    out.into_iter()
}

/// collect inherited traits by Depth First Search
fn collect_inherited_traits(trait_match: Match, s: &Session) -> Vec<Match> {
    // search node
    struct Node {
        target_str: String,
        offset: i32,
        filepath: PathBuf,
    }
    impl Node {
        fn from_match(m: &Match) -> Self {
            let mut target_str = m.contextstr.to_owned();
            target_str.push_str("{}");
            let offset = m.point.0 as i32 - "trait ".len() as i32;
            Node {
                target_str: target_str,
                offset: offset,
                filepath: m.filepath.clone(),
            }
        }
    }
    // DFS stack
    let mut stack = vec![Node::from_match(&trait_match)];
    // we have to store hashes of trait names to prevent infinite loop!
    let mut trait_names = HashSet::new();
    trait_names.insert(calculate_str_hash(&trait_match.matchstr));
    let mut res = vec![trait_match];
    // DFS
    while let Some(t) = stack.pop() {
        if let Some(bounds) = ast::parse_inherited_traits(t.target_str, t.filepath, t.offset) {
            let traits = bounds.get_traits(s);
            let filtered = traits.into_iter().filter(|tr| {
                let hash = calculate_str_hash(&tr.matchstr);
                if trait_names.contains(&hash) {
                    return false;
                }
                trait_names.insert(hash);
                let tr_info = Node::from_match(&tr);
                stack.push(tr_info);
                true
            });
            res.extend(filtered);
        }
    }
    res
}

pub fn search_for_field_or_method(
    context: Match,
    searchstr: &str,
    search_type: SearchType,
    session: &Session,
) -> vec::IntoIter<Match> {
    let m = context;
    let mut out = Vec::new();
    // for Trait and TypeParameter
    let find_inherited_traits = |m: Match| {
        let traits = collect_inherited_traits(m, session);
        traits
            .into_iter()
            .filter_map(|tr| {
                let src = session.load_source_file(&tr.filepath);
                src[tr.point.0..].find('{').map(|start| {
                    search_scope_for_methods(
                        tr.point + BytePos(start + 1),
                        src.as_src(),
                        searchstr,
                        &tr.filepath,
                        search_type,
                    )
                })
            }).flatten()
    };
    match m.mtype {
        MatchType::Struct => {
            debug!(
                "got a struct, looking for fields and impl methods!! {}",
                m.matchstr
            );
            for m in search_struct_fields(searchstr, &m, search_type, session) {
                out.push(m);
            }
            for m in search_for_impl_methods(
                &m,
                searchstr,
                m.point,
                &m.filepath,
                m.local,
                search_type,
                session,
            ) {
                out.push(m);
            }
        }
        MatchType::Builtin => {
            for m in search_for_impl_methods(
                &m,
                searchstr,
                m.point,
                &m.filepath,
                m.local,
                search_type,
                session,
            ) {
                out.push(m);
            }
        }
        MatchType::Enum => {
            debug!("got an enum, looking for impl methods {}", m.matchstr);
            for m in search_for_impl_methods(
                &m,
                searchstr,
                m.point,
                &m.filepath,
                m.local,
                search_type,
                session,
            ) {
                out.push(m);
            }
        }
        MatchType::Trait => {
            debug!("got a trait, looking for methods {}", m.matchstr);
            out.extend(find_inherited_traits(m));
        }
        MatchType::TypeParameter(bounds) => {
            debug!("got a trait bound, looking for methods {}", m.matchstr);
            let traits = bounds.get_traits(session);
            traits.into_iter().for_each(|m| {
                out.extend(find_inherited_traits(m));
            });
        }
        _ => {
            debug!(
                "WARN!! context wasn't a Struct, Enum, Builtin or Trait {:?}",
                m
            );
        }
    };
    out.into_iter()
}

fn search_for_deref_matches(
    impl_match: &Match,
    type_match: &Match,
    fieldsearchstr: &str,
    fpath: &Path,
    session: &Session,
) -> vec::IntoIter<Match> {
    debug!(
        "Found a Deref Implementation for {}, Searching for Methods on the Deref Type",
        type_match.matchstr
    );
    let mut out = Vec::new();

    if let Some(type_arg) = impl_match.generic_args.first() {
        // If Deref to a generic type
        if let Some(inner_type_path) = generic_arg_to_path(&type_arg, type_match) {
            let type_match = resolve_path_with_str(
                &inner_type_path.path,
                &inner_type_path.filepath,
                BytePos::ZERO,
                SearchType::ExactMatch,
                Namespace::Type,
                session,
            ).nth(0);
            let subpath = get_subpathsearch(&inner_type_path);
            if let Some(mut m) = type_match {
                if let Some(path) = subpath {
                    m.generic_types.push(path);
                }
                let methods =
                    search_for_field_or_method(m, fieldsearchstr, SearchType::StartsWith, session);
                out.extend(methods);
            };
        }
        // If Deref to an ordinary type
        else {
            let deref_type_path = RacerPath::single(impl_match.generic_args[0].clone().into());
            let type_match = resolve_path_with_str(
                &deref_type_path,
                fpath,
                BytePos::ZERO,
                SearchType::ExactMatch,
                Namespace::Type,
                session,
            ).nth(0);
            if let Some(m) = type_match {
                let methods =
                    search_for_field_or_method(m, fieldsearchstr, SearchType::StartsWith, session);
                out.extend(methods);
            }
        }
    }

    out.into_iter()
}

fn generic_arg_to_path(type_str: &str, m: &Match) -> Option<PathSearch> {
    debug!("Attempting to find type match for {} in {:?}", type_str, m);
    if let Some(match_pos) = m.generic_args.iter().position(|x| *x == type_str) {
        if let Some(gen_type) = m.generic_types.get(match_pos) {
            return Some(gen_type.clone());
        }
    }
    None
}

fn get_subpathsearch(pathsearch: &PathSearch) -> Option<PathSearch> {
    pathsearch.path.segments.get(0).and_then(|seg| {
        seg.types.get(0).and_then(|first_type| {
            Some(PathSearch {
                path: first_type.clone(),
                filepath: pathsearch.filepath.clone(),
                point: pathsearch.point,
            })
        })
    })
}

fn get_std_macros(
    searchstr: &str,
    search_type: SearchType,
    session: &Session,
    out: &mut Vec<Match>,
) {
    let std_path = if let Some(ref p) = *RUST_SRC_PATH {
        p
    } else {
        return;
    };
    let searchstr = if searchstr.ends_with("!") {
        let len = searchstr.len();
        &searchstr[..len - 1]
    } else {
        searchstr
    };
    for macro_file in &[
        "libstd/macros.rs",
        "libcore/macros.rs",
        "liballoc/macros.rs",
    ] {
        let macro_path = std_path.join(macro_file);
        if !macro_path.exists() {
            return;
        }
        get_std_macros_(
            &macro_path,
            searchstr,
            macro_file == &"libcore/macros.rs",
            search_type,
            session,
            out,
        );
    }
}

fn get_std_macros_(
    macro_path: &Path,
    searchstr: &str,
    is_core: bool,
    search_type: SearchType,
    session: &Session,
    out: &mut Vec<Match>,
) {
    let raw_src = session.load_raw_file(&macro_path);
    let src = session.load_source_file(&macro_path);
    let mut export = false;
    let mut get_macro_def = |blob: &str| -> Option<(BytePos, String)> {
        if blob.starts_with("#[macro_export]") | blob.starts_with("#[rustc_doc_only_macro]") {
            export = true;
            return None;
        }
        if !export {
            return None;
        }
        if !blob.starts_with("macro_rules!") {
            return None;
        }
        export = false;
        let mut start = BytePos(12);
        for &b in blob[start.0..].as_bytes() {
            match b {
                b if util::is_whitespace_byte(b) => start = start.increment(),
                _ => break,
            }
        }
        if !blob[start.0..].starts_with(searchstr) {
            return None;
        }
        let end = find_ident_end(blob, start + BytePos(searchstr.len()));
        let mut matchstr = blob[start.0..end.0].to_owned();
        if search_type == SearchType::ExactMatch && searchstr != matchstr {
            return None;
        }
        matchstr.push_str("!");
        Some((start, matchstr))
    };
    let mut builtin_start = None;
    out.extend(src.as_src().iter_stmts().filter_map(|range| {
        let blob = &src[range.to_range()];
        // for builtin macros in libcore/macros.rs
        if is_core && blob.starts_with("mod builtin") {
            builtin_start = blob.find("#").map(|u| range.start + u.into());
        }
        let (offset, matchstr) = get_macro_def(blob)?;
        let start = range.start + offset;
        Some(Match {
            matchstr,
            filepath: macro_path.to_owned(),
            point: start,
            coords: raw_src.point_to_coords(start),
            local: false,
            mtype: MatchType::Macro,
            contextstr: matchers::first_line(blob),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: matchers::find_doc(&raw_src, range.start),
        })
    }));
    if let Some(builtin_start) = builtin_start {
        let mod_src = src.get_src_from_start(builtin_start);
        out.extend(mod_src.iter_stmts().filter_map(|range| {
            let blob = &mod_src[range.to_range()];
            let (offset, matchstr) = get_macro_def(blob)?;
            let start = builtin_start + range.start + offset;
            Some(Match {
                matchstr,
                filepath: macro_path.to_owned(),
                point: start,
                coords: raw_src.point_to_coords(start),
                local: false,
                mtype: MatchType::Macro,
                contextstr: matchers::first_line(blob),
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: matchers::find_doc(&raw_src, range.start),
            })
        }));
    }
}
