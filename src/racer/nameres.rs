// Name resolution

use {core, ast, matchers, scopes, typeinf};
use core::SearchType::{self, ExactMatch, StartsWith};
use core::{Match, Src, Session, Coordinate, SessionExt, Ty, Point};
use core::MatchType::{Module, Function, Struct, Enum, EnumVariant, FnArg, Trait, StructField,
    Impl, TraitImpl, MatchArm, Builtin};
use core::Namespace;


use util::{self, closure_valid_arg_scope, symbol_matches, txt_matches,
    find_ident_end, get_rust_src_path};
use matchers::find_doc;
use cargo;
use std::path::{Path, PathBuf};
use std::{self, vec};
use matchers::PendingImports;

lazy_static! {
    pub static ref RUST_SRC_PATH: PathBuf = get_rust_src_path().unwrap();
}

fn search_struct_fields(searchstr: &str, structmatch: &Match,
                        search_type: SearchType, session: &Session) -> vec::IntoIter<Match> {
    let src = session.load_file(&structmatch.filepath);
    let struct_start = scopes::expect_stmt_start(src.as_src(), structmatch.point);
    let structsrc = scopes::end_of_next_scope(&src[struct_start..]);

    let fields = ast::parse_struct_fields(structsrc.to_owned(),
                                          core::Scope::from_match(structmatch));

    let mut out = Vec::new();

    for (field, field_point, ty) in fields.into_iter() {
        if symbol_matches(search_type, searchstr, &field) {
            let contextstr = if let Some(t) = ty {
                t.to_string()
            } else {
                field.clone()
            };

            out.push(Match { matchstr: field,
                                filepath: structmatch.filepath.clone(),
                                point: field_point + struct_start,
                                coords: None,
                                local: structmatch.local,
                                mtype: StructField,
                                contextstr: contextstr,
                                generic_args: Vec::new(),
                                generic_types: Vec::new(),
                                docs: find_doc(structsrc, field_point),
            });
        }
    }
    out.into_iter()
}

pub fn search_for_impl_methods(match_request: &Match,
                           fieldsearchstr: &str, point: Point,
                           fpath: &Path, local: bool,
                           search_type: SearchType,
                               session: &Session) -> vec::IntoIter<Match> {

    let implsearchstr: &str = &match_request.matchstr;

    debug!("searching for impl methods |{:?}| |{}| {:?}", match_request, fieldsearchstr, fpath.display());

    let mut out = Vec::new();

    for m in search_for_impls(point, implsearchstr, fpath, local, true, session, &PendingImports::empty()) {
        debug!("found impl!! |{:?}| looking for methods", m);

        if m.matchstr == "Deref" {
            out.extend(search_for_deref_matches(&m, match_request, fieldsearchstr, fpath, session));
        }

        let src = session.load_file(&m.filepath);

        // find the opening brace and skip to it.
        src[m.point..].find('{').map(|n| {
            let point = m.point + n + 1;
            for m in search_scope_for_methods(point, src.as_src(), fieldsearchstr, &m.filepath, search_type) {
                out.push(m);
            }
        });
        for gen_m in search_for_generic_impls(m.point, &m.matchstr, match_request, &m.filepath, session) {
            debug!("found generic impl!! {:?}", gen_m);
            let src = session.load_file(&gen_m.filepath);
            // find the opening brace and skip to it.
            src[gen_m.point..].find('{').map(|n| {
                let point = gen_m.point + n + 1;
                for gen_method in search_generic_impl_scope_for_methods(point, src.as_src(), fieldsearchstr, &gen_m, search_type) {
                    out.push(gen_method);
                }
            });
        }

        if m.matchstr == "Iterator" && fieldsearchstr == "into_iter" {
            let mut m_copy = m.clone();
            if let Ok(mut m_filestring) = m_copy.filepath.into_os_string().into_string() {
                m_filestring = m_filestring.replace("iterator.rs", "traits.rs");
                m_copy.filepath = PathBuf::from(&m_filestring);
                for m in search_for_generic_impls(m_copy.point, &m_copy.matchstr, match_request, &m_copy.filepath, session) {
                    debug!("found generic impl!! {:?}", m);
                    let src = session.load_file(&m.filepath);
                    // find the opening brace and skip to it.
                    src[m.point..].find('{').map(|n| {
                        let point = m.point + n + 1;
                        for m in search_generic_impl_scope_for_methods(point, src.as_src(), fieldsearchstr, &m, search_type) {
                            out.push(m);
                        }
                    });
                }
            }
        }
    };
    out.into_iter()
}

fn search_scope_for_methods(point: Point, src: Src, searchstr: &str, filepath: &Path,
                            search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("searching scope for methods {} |{}| {:?}", point, searchstr, filepath.display());

    let scopesrc = src.from(point);
    let mut out = Vec::new();
    for (blobstart,blobend) in scopesrc.iter_stmts() {
        let blob = &scopesrc[blobstart..blobend];
        blob.find(|c| {c == '{' || c == ';'}).map(|n| {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                && typeinf::first_param_is_self(blob) {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly
                let start = blob.find(&format!("fn {}", searchstr)).unwrap() + 3;
                let end = find_ident_end(blob, start);
                let l = &blob[start..end];
                // TODO: make a better context string for functions
                let m = Match {
                           matchstr: l.to_owned(),
                           filepath: filepath.to_path_buf(),
                           point: point + blobstart + start,
                           coords: None,
                           local: true,
                           mtype: Function,
                           contextstr: signature.to_owned(),
                           generic_args: Vec::new(),
                           generic_types: Vec::new(),
                           docs: find_doc(&scopesrc, blobstart + start),
                };
                out.push(m);
            }
        });
    }
    out.into_iter()
}

fn search_generic_impl_scope_for_methods(point: Point, src: Src, searchstr: &str, contextm: &Match,
                            search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("searching generic impl scope for methods {} |{}| {:?}", point, searchstr, contextm.filepath.display());

    let scopesrc = src.from(point);
    let mut out = Vec::new();
    for (blobstart,blobend) in scopesrc.iter_stmts() {
        let blob = &scopesrc[blobstart..blobend];
        blob.find(|c| {c == '{' || c == ';'}).map(|n| {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                && typeinf::first_param_is_self(blob) {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly
                let start = blob.find(&format!("fn {}", searchstr)).unwrap() + 3;
                let end = find_ident_end(blob, start);
                let l = &blob[start..end];
                // TODO: make a better context string for functions
                let m = Match {
                           matchstr: l.to_owned(),
                           filepath: contextm.filepath.clone(),
                           point: point + blobstart + start,
                           coords: None,
                           local: true,
                           mtype: Function,
                           contextstr: signature.to_owned(),
                           generic_args: contextm.generic_args.clone(),  // Attach impl generic args
                           generic_types: contextm.generic_types.clone(), // Attach impl generic types
                           docs: find_doc(&scopesrc, blobstart + start),
                };
                out.push(m);
            }
        });
    }
    out.into_iter()
}

/// Look for static trait functions. This fn doesn't search for _method_ declarations
/// or implementations as `search_scope_for_methods` already handles that.
fn search_scope_for_static_trait_fns(point: Point, src: Src, searchstr: &str, filepath: &Path,
                            search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("searching scope for trait fn declarations {} |{}| {:?}", point, searchstr, filepath.display());

    let scopesrc = src.from(point);
    let mut out = Vec::new();
    for (blobstart,blobend) in scopesrc.iter_stmts() {
        let blob = &scopesrc[blobstart..blobend];
        blob.find(|c| c == '{' || c == ';').map(|n| {
            let signature = blob[..n].trim_right();

            if txt_matches(search_type, &format!("fn {}", searchstr), signature)
                // filtering out methods here prevents duplicate results with
                // `search_scope_for_methods`
                && !typeinf::first_param_is_self(blob) {
                debug!("found a method starting |{}| |{}|", searchstr, blob);
                // TODO: parse this properly
                let start = blob.find(&format!("fn {}", searchstr)).unwrap() + 3;
                let end = find_ident_end(blob, start);
                let l = &blob[start..end];
                // TODO: make a better context string for functions
                let m = Match {
                           matchstr: l.to_owned(),
                           filepath: filepath.to_path_buf(),
                           point: point + blobstart + start,
                           coords: None,
                           local: true,
                           mtype: Function,
                           contextstr: signature.to_owned(),
                           generic_args: Vec::new(),
                           generic_types: Vec::new(),
                           docs: find_doc(&scopesrc, blobstart + start),
                };
                out.push(m);
            }
        });
    }
    out.into_iter()
}


pub fn search_for_impls(pos: Point, searchstr: &str, filepath: &Path, local: bool, include_traits: bool,
                        session: &Session, pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    debug!("search_for_impls {}, {}, {:?}", pos, searchstr, filepath.display());
    let s = session.load_file(filepath);
    let scope_start = scopes::scope_start(s.as_src(), pos);
    let src = s.from(scope_start);

    let mut out = Vec::new();
    for (start, end) in src.iter_stmts() {
        let blob = &src[start..end];

        if blob.starts_with("impl") {
            blob.find('{').map(|n| {
                let ref decl = blob[..n+1];
                if decl.contains('!') {
                    // Guard against macros
                    debug!("impl was probably a macro: {} {}", filepath.display(), start);
                    return;
                }
                let mut decl = decl.to_owned();
                decl.push_str("}");
                if txt_matches(ExactMatch, searchstr, &decl) {
                    debug!("impl decl {}", decl);
                    let implres = ast::parse_impl(decl);
                    let is_trait_impl = implres.trait_path.is_some();
                    let mtype = if is_trait_impl { TraitImpl } else { Impl };

                    implres.name_path.map(|name_path| {
                        name_path.segments.last().map(|name| {
                            if symbol_matches(ExactMatch, searchstr, &name.name) {
                                let m = Match {
                                    matchstr: name.name.clone(),
                                    filepath: filepath.to_path_buf(),
                                    point: scope_start + start + 5,
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
                        });
                    });

                    // find trait
                    if include_traits && is_trait_impl {
                        let trait_path = implres.trait_path.unwrap();
                        let mut m = resolve_path(&trait_path,
                                                 filepath, scope_start + start, ExactMatch, Namespace::Type,
                                                 session, pending_imports).nth(0);
                        debug!("found trait |{:?}| {:?}", trait_path, m);

                        if let Some(ref mut m) = m {
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
                        }

                        m.map(|m| out.push(m));
                    }
                }
            });
        }
    }
    out.into_iter()
}

pub fn search_for_generic_impls(pos: Point, searchstr: &str, contextm: &Match, filepath: &Path, session: &Session) -> vec::IntoIter<Match> {
    debug!("search_for_generic_impls {}, {}, {:?}", pos, searchstr, filepath.display());
    let s = session.load_file(filepath);
    let scope_start = scopes::scope_start(s.as_src(), pos);
    let src = s.from(scope_start);

    let mut out = Vec::new();
    for (start, end) in src.iter_stmts() {
        let blob = &src[start..end];

        if blob.starts_with("impl") {
            blob.find('{').map(|n| {
                let ref decl = blob[..n+1];
                if decl.contains('!') {
                    // Guard against macros
                    debug!("impl was probably a macro: {} {}", filepath.display(), start);
                    return;
                }
                let mut decl = decl.to_owned();
                decl.push_str("}");
                let generics = ast::parse_generics(decl.clone());
                let implres = ast::parse_impl(decl.clone());
                if let (Some(name_path), Some(trait_path)) = (implres.name_path, implres.trait_path) {
                    if let (Some(name), Some(trait_name)) = (name_path.segments.last(), trait_path.segments.last()) {
                        for gen_arg in generics.generic_args {
                        if symbol_matches(ExactMatch, &gen_arg.name, &name.name)
                           && gen_arg.bounds.len() == 1
                           && gen_arg.bounds[0] == searchstr {
                                  debug!("generic impl decl {}", decl);

                                  let trait_pos = blob.find(&trait_name.name).unwrap();
                                  let self_path = core::Path::from_vec(false, vec![&contextm.matchstr]);
                                  let self_pathsearch = core::PathSearch {
                                      path: self_path,
                                      filepath: contextm.filepath.clone(),
                                      point: contextm.point
                                  };

                                  let m = Match {
                                      matchstr: trait_name.name.clone(),
                                      filepath: filepath.to_path_buf(),
                                      point: scope_start + start + trait_pos,
                                      coords: None,
                                      local: true,
                                      mtype: TraitImpl,
                                      contextstr: "".into(),
                                      generic_args: vec![gen_arg.name],
                                      generic_types: vec![self_pathsearch],
                                      docs: String::new(),
                                  };
                                  debug!("Found a trait! {:?}", m);
                                  out.push(m);
                              }
                        }
                    }
                }
            });
        }
    }
    out.into_iter()
}

// scope headers include fn decls, if let, while let etc..
fn search_scope_headers(point: Point, scopestart: Point, msrc: Src, searchstr: &str,
                        filepath: &Path, search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("search_scope_headers for |{}| pt: {}", searchstr, scopestart);

    if let Some(stmtstart) = scopes::find_stmt_start(msrc, scopestart) {
        let preblock = &msrc[stmtstart..scopestart];
        debug!("search_scope_headers preblock is |{}|", preblock);

        if preblock_is_fn(preblock) {
            return search_fn_args(stmtstart, scopestart, &msrc, searchstr, filepath, search_type, true);

        // 'if let' can be an expression, so might not be at the start of the stmt
        } else if let Some(n) = preblock.find("if let") {
            let ifletstart = stmtstart + n;
            let src = msrc[ifletstart..scopestart+1].to_owned() + "}";
            if txt_matches(search_type, searchstr, &src) {
                let mut out = matchers::match_if_let(&src, 0, src.len(), searchstr,
                                                     filepath, search_type, true);
                for m in &mut out {
                    m.point += ifletstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("while let") {
            let src = msrc[stmtstart..scopestart+1].to_owned() + "}";
            if txt_matches(search_type, searchstr, &src) {
                let mut out = matchers::match_while_let(&src, 0, src.len(), searchstr,
                                                        filepath, search_type, true);
                for m in &mut out {
                    m.point += stmtstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("for ") {
            let src = msrc[stmtstart..scopestart+1].to_owned() + "}";
            if txt_matches(search_type, searchstr, &msrc[..scopestart]) {
                let mut out = matchers::match_for(&src, 0, src.len(), searchstr,
                                                  filepath, search_type, true);
                for m in &mut out {
                    m.point += stmtstart;
                }
                return out.into_iter();
            }
        } else if let Some(n) = preblock.rfind("match ") {
            // TODO: this code is crufty. refactor me!
            let matchstart = stmtstart + n;
            let matchstmt = typeinf::get_first_stmt(msrc.from(matchstart));
            // The definition could be in the match LHS arms. Try to find this
            let masked_matchstmt = mask_matchstmt(&matchstmt, scopestart + 1 - matchstart);
            debug!("found match stmt, masked is len {} |{}|",
                   masked_matchstmt.len(), masked_matchstmt);

            // Locate the match arm LHS by finding the => just before point and then backtracking
            // be sure to be on the right side of the ... => ... arm
            let arm = match masked_matchstmt[..point-matchstart].rfind("=>") {
                None =>
                    // we are in the first arm enum
                    return Vec::new().into_iter(),
                Some(arm) => {
                    // be sure not to be in the next arm enum
                    if let Some(next_arm) = masked_matchstmt[arm+2..].find("=>") {
                        let enum_start = scopes::get_start_of_pattern(&masked_matchstmt, arm+next_arm+1);
                        if point > matchstart+enum_start { return Vec::new().into_iter(); }
                    }
                    arm
                }
            };

            debug!("PHIL matched arm rhs is |{}|", &masked_matchstmt[arm..]);

            let lhs_start = scopes::get_start_of_pattern(&msrc, matchstart + arm);
            let lhs = &msrc[lhs_start..matchstart + arm];

            // Now create a pretend match expression with just the one match arm in it
            let faux_prefix_size = scopestart - matchstart + 1;
            let fauxmatchstmt = format!("{}{{{} => () }};", &msrc[matchstart..scopestart], lhs);

            debug!("PHIL arm lhs is |{}|", lhs);
            debug!("PHIL arm fauxmatchstmt is |{}|, {}", fauxmatchstmt, faux_prefix_size);
            let mut out = Vec::new();
            for (start,end) in ast::parse_pat_idents(fauxmatchstmt) {
                let (start,end) = (lhs_start + start - faux_prefix_size,
                                   lhs_start + end - faux_prefix_size);
                let s = &msrc[start..end];

                if symbol_matches(search_type, searchstr, s) {
                    out.push(Match {
                                    matchstr: s.to_owned(),
                                    filepath: filepath.to_path_buf(),
                                    point: start,
                                    coords: None,
                                    local: true,
                                    mtype: MatchArm,
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
        } else if let Some(vec) = search_closure_args(searchstr, preblock, stmtstart, filepath, search_type) {
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
    if preblock.starts_with("fn") || preblock.starts_with("pub fn") || preblock.starts_with("const fn") {
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

fn mask_matchstmt(matchstmt_src: &str, innerscope_start: Point) -> String {
    let s = scopes::mask_sub_scopes(&matchstmt_src[innerscope_start..]);
    matchstmt_src[..innerscope_start].to_owned() + &s
}

#[test]
fn does_it() {
    let src = "
    match foo {
        Some(a) => { something }
    }";
    let res = mask_matchstmt(src, src.find('{').unwrap()+1);
    debug!("PHIL res is |{}|",res);
}

fn search_fn_args(fnstart: Point, open_brace_pos: Point, msrc: &str,
                  searchstr: &str, filepath: &Path,
                  search_type: SearchType, local: bool) -> vec::IntoIter<Match> {
    let mut out = Vec::new();
    let mut fndecl = String::new();
    // wrap in 'impl blah {}' so that methods get parsed correctly too
    fndecl.push_str("impl blah {");
    let impl_header_len = fndecl.len();
    fndecl.push_str(&msrc[fnstart..(open_brace_pos+1)]);
    fndecl.push_str("}}");
    debug!("search_fn_args: found start of fn!! {} |{}| {}", fnstart, fndecl, searchstr);
    if txt_matches(search_type, searchstr, &fndecl) {
        let coords = ast::parse_fn_args(fndecl.clone());

        for (start,end) in coords {
            let s = &fndecl[start..end];
            debug!("search_fn_args: arg str is |{}|", s);

            if symbol_matches(search_type, searchstr, s) {
                let m = Match {
                                matchstr: s.to_owned(),
                                filepath: filepath.to_path_buf(),
                                point: fnstart + start - impl_header_len,
                                coords: None,
                                local: local,
                                mtype: FnArg,
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
fn test_do_file_search() {
    let cache = core::FileCache::default();
    let session = Session::new(&cache);
    let mut matches = do_file_search("std", &Path::new("."), &session);

    assert!(matches.len() > 1);

    assert!(matches.any(|ma| ma.filepath.ends_with("src/libstd/lib.rs")));
}

pub fn do_file_search(
    searchstr: &str,
    currentdir: &Path,
    session: &Session
) -> vec::IntoIter<Match> {
    debug!("do_file_search with search string \"{}\"", searchstr);
    let mut out = Vec::new();

    let srcpath = RUST_SRC_PATH.as_ref();
    debug!("do_file_search srcpath: {:?}", srcpath);
    let v = &[srcpath, currentdir][..];
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
                            point: 0,
                            coords: Some(Coordinate { line: 1, column: 1 }),
                            local: false,
                            mtype: Module,
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
                                point: 0,
                                coords: Some(Coordinate { line: 1, column: 1 }),
                                local: false,
                                mtype: Module,
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
                            matchstr: fname[..(fname.len()-3)].to_owned(),
                            filepath: fpath_buf.clone(),
                            point: 0,
                            coords: Some(Coordinate { line: 1, column: 1 }),
                            local: false,
                            mtype: Module,
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

pub fn search_crate_root(pathseg: &core::PathSegment, modfpath: &Path,
                         searchtype: SearchType, namespace: Namespace,
                         session: &Session, pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    debug!("search_crate_root |{:?}| {:?}", pathseg, modfpath.display());

    let crateroots = find_possible_crate_root_modules(modfpath.parent().unwrap(), session);
    let mut out = Vec::new();
    for crateroot in crateroots {
        if *modfpath == *crateroot {
            continue;
        }
        debug!("going to search for {:?} in crateroot {:?}", pathseg, crateroot.display());
        for m in resolve_name(pathseg, &crateroot, 0, searchtype, namespace, session, pending_imports) {
            out.push(m);
            if let ExactMatch = searchtype {
                break;
            }
        }
        break
    }
    out.into_iter()
}

pub fn find_possible_crate_root_modules(currentdir: &Path, session: &Session) -> Vec<PathBuf> {
    let mut res = Vec::new();

    for root in &["lib.rs", "main.rs"] {
        let filepath = currentdir.join(root);
        if filepath.exists() || session.contains_file(&filepath) {
            res.push(filepath);
            return res;   // for now stop at the first match
        }
    }
    // recurse up the directory structure
    if let Some(parentdir) = currentdir.parent() {
        if parentdir != currentdir {
            // PD: this was using the vec.push_all() api, but that is now unstable
            res.extend(find_possible_crate_root_modules(parentdir, session).iter().cloned());
            return res;   // for now stop at the first match
        }
    }
    res
}

pub fn search_next_scope(mut startpoint: Point, pathseg: &core::PathSegment,
                         filepath:&Path, search_type: SearchType, local: bool,
                         namespace: Namespace, session: &Session,
                         pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    let filesrc = session.load_file(filepath);
    if startpoint != 0 {
        // is a scope inside the file. Point should point to the definition
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = &filesrc[startpoint..];
        //debug!("search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it.
        src.find('{').map(|n| {
            startpoint += n + 1;
        });
    }
    search_scope(startpoint, startpoint, filesrc.as_src(), pathseg, filepath, search_type, local, namespace, session, pending_imports)
}

pub fn get_crate_file(name: &str, from_path: &Path, session: &Session) -> Option<PathBuf> {
    debug!("get_crate_file {}, {:?}", name, from_path);
    if let Some(p) = cargo::get_crate_file(name, from_path) {
        debug!("get_crate_file  - found the crate file! {:?}", p);
        return Some(p);
    }

    let srcpath = &*RUST_SRC_PATH;
    {
        // try lib<name>/lib.rs, like in the rust source dir
        let cratelibname = format!("lib{}", name);
        let filepath = srcpath.join(cratelibname).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    {
        // try <name>/lib.rs
        let filepath = srcpath.join(name).join("lib.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    None
}

pub fn get_module_file(name: &str, parentdir: &Path, session: &Session) -> Option<PathBuf> {
    {
        // try just <name>.rs
        let filepath = parentdir.join(format!("{}.rs", name));
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    {
        // try <name>/mod.rs
        let filepath = parentdir.join(name).join("mod.rs");
        if filepath.exists() || session.contains_file(&filepath) {
            return Some(filepath);
        }
    }
    None
}

pub fn search_scope(start: Point, point: Point, src: Src,
                    pathseg: &core::PathSegment,
                    filepath:&Path, search_type: SearchType, local: bool,
                    namespace: Namespace,
                    session: &Session,
                    pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    let searchstr = &pathseg.name;
    let mut out = Vec::new();

    debug!("searching scope {:?} start: {} point: {} '{}' {:?} {:?} local: {}, session: {:?}",
           namespace, start, point, searchstr, filepath.display(), search_type, local, session);

    let scopesrc = src.from(start);
    let mut skip_next_block = false;
    let mut delayed_single_imports = Vec::new();
    let mut delayed_glob_imports = Vec::new();
    let mut codeit = scopesrc.iter_stmts();
    let mut v = Vec::new();

    // collect up to point so we can search backwards for let bindings
    //  (these take precidence over local fn declarations etc..
    for (blobstart, blobend) in &mut codeit {
        //  (e.g. #[cfg(test)])
        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = &scopesrc[blobstart..blobend];

        // for now skip stuff that's meant for testing. Often the test
        // module hierarchy is incompatible with the non-test
        // hierarchy and we get into recursive loops
        if blob.starts_with("#[cfg(test)") {
            skip_next_block = true;
            continue;
        }

        v.push((blobstart,blobend));

        if blobstart > point {
            break;
        }
    }

    // search backwards from point for let bindings
    for &(blobstart, blobend) in v.iter().rev() {
        if (start+blobend) >= point {
            continue;
        }

        for m in matchers::match_let(&src, start+blobstart,
                                     start+blobend,
                                     searchstr,
                                     filepath, search_type, local).into_iter() {
            out.push(m);
            if let ExactMatch = search_type {
                return out.into_iter();
            }
        }
    }

    // since we didn't find a `let` binding, now search from top of scope for items etc..
    let mut codeit = v.into_iter().chain(codeit);
    for (blobstart, blobend) in &mut codeit {
        // sometimes we need to skip blocks of code if the preceeding attribute disables it
        //  (e.g. #[cfg(test)])
        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = &scopesrc[blobstart..blobend];

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
            if !is_glob_import {
                if !blob.contains(searchstr.trim_right_matches('!')) {
                    continue;
                }
            }

            if is_glob_import {
                delayed_glob_imports.push((blobstart, blobend));
            } else {
                delayed_single_imports.push((blobstart, blobend));
            }

            continue;
        }

        if searchstr == "core" && blob.starts_with("#![no_std]") {
            debug!("Looking for core and found #![no_std], which implicitly imports it");
            get_crate_file("core", filepath, session).map(|cratepath| {
                let context = cratepath.to_str().unwrap().to_owned();
                out.push(Match { matchstr: "core".into(),
                                  filepath: cratepath,
                                  point: 0,
                                  coords: Some(Coordinate { line: 1, column: 1 }),
                                  local: false,
                                  mtype: Module,
                                  contextstr: context,
                                  generic_args: Vec::new(),
                                  generic_types: Vec::new(),
                                  docs: String::new(),

                });
            });
        }

        // Optimisation: if the search string is not in the blob,
        // this cannot match so fail fast!
        if !blob.contains(searchstr.trim_right_matches('!')) {
            continue;
        }

        // There's a good chance of a match. Run the matchers
        out.extend(run_matchers_on_blob(src, start+blobstart, start+blobend,
                                        searchstr,
                                        filepath, search_type, local, namespace, session, pending_imports));
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    let delayed_import_len =  delayed_single_imports.len() + delayed_glob_imports.len();

    if delayed_import_len > 0 {
        trace!("Searching {} delayed imports for `{}`", delayed_import_len, searchstr);
    }

    // Finally, process the imports that we skipped before.
    // Process single imports first, because they shadow glob imports.
    for (blobstart, blobend) in delayed_single_imports.into_iter().chain(delayed_glob_imports) {
        // There's a good chance of a match. Run the matchers
        for m in run_matchers_on_blob(src, start+blobstart, start+blobend,
                                      searchstr, filepath, search_type,
                                      local, namespace, session, pending_imports).into_iter() {
            out.push(m);
            if let ExactMatch = search_type {
                return out.into_iter();
            }
        }
    }

    if let Some(vec) = search_closure_args(
        searchstr, &scopesrc[0..], start, filepath, search_type) {
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

fn search_closure_args(searchstr: &str, scope_src: &str, scope_src_pos: Point,
                       filepath: &Path, search_type: SearchType) -> Option<Vec<Match>> {
    if searchstr.is_empty() {
        return None;
    }

    trace!("Closure definition match is looking for `{}` in {} characters", searchstr, scope_src.len());

    if let Some((left_pipe, _, pipe_scope)) = closure_valid_arg_scope(scope_src) {
        debug!("search_closure_args found valid closure arg scope: {}", pipe_scope);

        if txt_matches(search_type, searchstr, pipe_scope) {
            // Add a fake body for parsing
            let closure_def = String::from(pipe_scope) + "{}";

            let coords = ast::parse_fn_args(closure_def.clone());

            let mut out: Vec<Match> = Vec::new();

            for (start,end) in coords {
                let s = &closure_def[start..end];

                if symbol_matches(search_type, searchstr, s) {
                    let m = Match {
                        matchstr: s.to_owned(),
                        filepath: filepath.to_path_buf(),
                        point: scope_src_pos + left_pipe + start,
                        coords: None,
                        local: true,
                        mtype: FnArg,
                        contextstr: pipe_scope.to_owned(),
                        generic_args: Vec::new(),
                        generic_types: Vec::new(),
                        docs: String::new(),
                    };
                    debug!("search_closure_args matched: {:?}", m);
                    out.push(m);
                }
            }

            return Some(out)
        }
    }

    None
}

fn run_matchers_on_blob(src: Src, start: Point, end: Point, searchstr: &str,
                        filepath: &Path, search_type: SearchType, local: bool,
                        namespace: Namespace, session: &Session,
                        pending_imports: &PendingImports) -> Vec<Match> {
    let mut out = Vec::new();
    match namespace {
        Namespace::Type =>
            for m in matchers::match_types(src, start,
                                           end, searchstr,
                                           filepath, search_type, local, session, pending_imports) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            },
        Namespace::Value =>
            for m in matchers::match_values(src, start,
                                            end, searchstr,
                                            filepath, search_type, local) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            },
        Namespace::Both => {
            for m in matchers::match_types(src, start,
                                           end, searchstr,
                                           filepath, search_type, local, session, pending_imports) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            }
            for m in matchers::match_values(src, start,
                                            end, searchstr,
                                            filepath, search_type, local) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            }
        }
    }
    out
}

fn search_local_scopes(pathseg: &core::PathSegment, filepath: &Path,
                       msrc: Src, point: Point, search_type: SearchType,
                       namespace: Namespace, session: &Session,
                       pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    debug!("search_local_scopes {:?} {:?} {} {:?} {:?}", pathseg, filepath.display(), point,
           search_type, namespace);

    if point == 0 {
        // search the whole file
        search_scope(0, 0, msrc, pathseg, filepath, search_type, true, namespace, session, pending_imports)
    } else {
        let mut out = Vec::new();
        let mut start = point;
        // search each parent scope in turn
        while start > 0 {
            start = scopes::scope_start(msrc, start);
            for m in search_scope(start, point, msrc, pathseg, filepath, search_type, true, namespace, session, pending_imports) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out.into_iter();
                }
            }
            if start == 0 {
                break;
            }
            start -= 1;
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

pub fn search_prelude_file(pathseg: &core::PathSegment, search_type: SearchType,
                           namespace: Namespace, session: &Session,
                           pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    debug!("search_prelude file {:?} {:?} {:?}", pathseg, search_type, namespace);
    let mut out : Vec<Match> = Vec::new();

    // find the prelude file from the search path and scan it
    let srcpath = &*RUST_SRC_PATH;
    let filepath = srcpath.join("libstd").join("prelude").join("v1.rs");
    if filepath.exists() || session.contains_file(&filepath) {
        let msrc = session.load_file_and_mask_comments(&filepath);
        let is_local = true;
        for m in search_scope(0, 0, msrc.as_src(), pathseg, &filepath, search_type, is_local, namespace, session, pending_imports) {
            out.push(m);
        }
    }
    out.into_iter()
}

pub fn resolve_path_with_str(path: &core::Path, filepath: &Path, pos: Point,
                             search_type: SearchType, namespace: Namespace,
                             session: &Session) -> vec::IntoIter<Match> {
    debug!("resolve_path_with_str {:?}", path);

    let mut out = Vec::new();

    // HACK
    if path.segments.len() == 1 && path.segments[0].name == "str" {
        debug!("{:?} == {:?}", path.segments[0], "str");

        if let Some(module) = resolve_path(&core::Path::from_vec(true, vec!["std","str"]),
                                           filepath, pos, search_type, namespace,
                                           session, &PendingImports::empty()).nth(0) {
            out.push(Match {
                matchstr: "str".into(),
                filepath: module.filepath,
                point: 0,
                coords: Some(Coordinate { line: 1, column: 1 }),
                local: false,
                mtype: Builtin,
                contextstr: "str".into(),
                generic_args: vec![],
                generic_types: vec![],
                docs: String::new(),

            });
        }

    } else {
        for m in resolve_path(path, filepath, pos, search_type, namespace, session, &PendingImports::empty()) {
            out.push(m);
            if let ExactMatch = search_type {
                break;
            }
        }
    }
    out.into_iter()
}

#[derive(PartialEq,Debug)]
pub struct Search {
    path: Vec<String>,
    filepath: String,
    pos: Point
}

/// Attempt to resolve a name which occurs in a given file.
pub fn resolve_name(pathseg: &core::PathSegment, filepath: &Path, pos: Point,
                    search_type: SearchType, namespace: Namespace,
                    session: &Session, pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    let mut out = Vec::new();
    let searchstr = &pathseg.name;

    debug!("resolve_name {} {:?} {} {:?} {:?}", searchstr, filepath.display(), pos, search_type, namespace);

    let msrc = session.load_file(filepath);
    let is_exact_match = match search_type { ExactMatch => true, StartsWith => false };

    if is_exact_match && &searchstr[..] == "Self" {
        if let Some(Ty::Match(m)) = typeinf::get_type_of_self(pos, filepath, true, msrc.as_src(), session) {
            out.push(m.clone());
        }
    }

    if (is_exact_match && &searchstr[..] == "std") ||
       (!is_exact_match && "std".starts_with(searchstr)) {
        get_crate_file("std", filepath, session).map(|cratepath| {
            let context = cratepath.to_str().unwrap().to_owned();
            out.push(Match {
                        matchstr: "std".into(),
                        filepath: cratepath,
                        point: 0,
                        coords: Some(Coordinate { line: 1, column: 1 }),
                        local: false,
                        mtype: Module,
                        contextstr: context,
                        generic_args: Vec::new(),
                        generic_types: Vec::new(),
                        docs: String::new(),
            });
        });

        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    for m in search_local_scopes(pathseg, filepath, msrc.as_src(), pos, search_type, namespace, session, pending_imports) {
        out.push(m);
        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }

    for m in search_crate_root(pathseg, filepath, search_type, namespace, session, pending_imports) {
        out.push(m);
        if let ExactMatch = search_type {
            return out.into_iter();
        }
    }

    for m in search_prelude_file(pathseg, search_type, namespace, session, pending_imports) {
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
    out.into_iter()
}

// Get the scope corresponding to super::
pub fn get_super_scope(filepath: &Path, pos: Point, session: &Session,
                       pending_imports: &PendingImports) -> Option<core::Scope> {
    let msrc = session.load_file_and_mask_comments(filepath);
    let mut path = scopes::get_local_module_path(msrc.as_src(), pos);
    debug!("get_super_scope: path: {:?} filepath: {:?} {} {:?}", path, filepath, pos, session);
    if path.is_empty() {
        let moduledir = if filepath.ends_with("mod.rs") || filepath.ends_with("lib.rs") {
            // Need to go up to directory above
            // TODO(PD): fix: will crash if mod.rs is in the root fs directory
            filepath.parent().unwrap().parent().unwrap()
        } else {
            // module is in current directory
            filepath.parent().unwrap()
        };

        for filename in &[ "mod.rs", "lib.rs" ] {
            let f_path = moduledir.join(&filename);
            if f_path.exists() || session.contains_file(&f_path) {
                return Some(core::Scope{ filepath: f_path, point: 0 })
            }
        }
        None
    } else if path.len() == 1 {
        Some(core::Scope{ filepath: filepath.to_path_buf(), point: 0 })
    } else {
        path.pop();
        let path = core::Path::from_svec(false, path);
        debug!("get_super_scope looking for local scope {:?}", path);
        resolve_path(&path, filepath, 0, SearchType::ExactMatch,
                            Namespace::Type, session, pending_imports).nth(0)
            .and_then(|m| msrc[m.point..].find('{')
                      .map(|p| core::Scope{ filepath: filepath.to_path_buf(),
                                             point:m.point + p + 1 }))
    }
}

pub fn resolve_path(path: &core::Path, filepath: &Path, pos: Point,
                    search_type: SearchType, namespace: Namespace,
                    session: &Session, pending_imports: &PendingImports) -> vec::IntoIter<Match> {
    debug!("resolve_path {:?} {:?} {} {:?}", path, filepath.display(), pos, search_type);
    let len = path.segments.len();
    if len == 1 {
        let pathseg = &path.segments[0];
        resolve_name(pathseg, filepath, pos, search_type, namespace, session, pending_imports)
    } else if len != 0 {
        if path.segments[0].name == "self" {
            // just remove self
            let mut newpath: core::Path = path.clone();
            newpath.segments.remove(0);
            return resolve_path(&newpath, filepath, pos, search_type, namespace, session, pending_imports);
        }

        if path.segments[0].name == "super" {
            if let Some(scope) = get_super_scope(filepath, pos, session, pending_imports) {
                debug!("PHIL super scope is {:?}", scope);

                let mut newpath: core::Path = path.clone();
                newpath.segments.remove(0);
                return resolve_path(&newpath, &scope.filepath,
                                    scope.point, search_type, namespace, session, pending_imports);
            } else {
                // can't find super scope. Return no matches
                debug!("can't resolve path {:?}, returning no matches", path);
                return Vec::new().into_iter();
            }
        }

        let mut out = Vec::new();
        let mut parent_path: core::Path = path.clone();
        parent_path.segments.remove(len-1);
        let context = resolve_path(&parent_path, filepath, pos, ExactMatch, Namespace::Type, session, pending_imports).nth(0);
        context.map(|m| {
            match m.mtype {
                Module => {
                    let mut searchstr: &str = &path.segments[len-1].name;
                    if let Some(i) = searchstr.rfind(',') {
                        searchstr = searchstr[i+1..].trim();
                    }
                    if searchstr.starts_with('{') {
                        searchstr = &searchstr[1..];
                    }
                    let pathseg = core::PathSegment{name: searchstr.to_owned(), types: Vec::new()};
                    debug!("searching a module '{}' for {} (whole path: {:?})", m.matchstr, pathseg.name, path);
                    for m in search_next_scope(m.point, &pathseg, &m.filepath, search_type, false, namespace, session, pending_imports) {
                        out.push(m);
                    }
                }
                Enum => {
                    let pathseg = &path.segments[len-1];
                    debug!("searching an enum '{}' (whole path: {:?}) searchtype: {:?}", m.matchstr, path, search_type);
                    let filesrc = session.load_file(&m.filepath);
                    let scopestart = scopes::find_stmt_start(filesrc.as_src(), m.point).unwrap();
                    let scopesrc = filesrc.from(scopestart);
                    scopesrc.iter_stmts().nth(0).map(|(blobstart,blobend)| {
                        for mut enum_var in matchers::match_enum_variants(&filesrc,
                                                               scopestart+blobstart,
                                                               scopestart+blobend,
                                                      &pathseg.name, &m.filepath, search_type, true) {
                            debug!("Found enum variant {} with enum type {}", enum_var.matchstr, m.matchstr);
                            // return Match which has enum simultaneously, for method completion
                            enum_var.mtype = EnumVariant(Some(Box::new(m.clone())));
                            out.push(enum_var);
                        }
                    });

                    // TODO remove code duplication with the struct branch below. The two implementations are identical.

                    for m_impl in search_for_impls(m.point, &m.matchstr, &m.filepath, m.local, true, session, pending_imports) {
                        debug!("found impl!! {:?}", m_impl);
                        let pathseg = &path.segments[len-1];
                        let src = session.load_file(&m_impl.filepath);
                        // find the opening brace and skip to it.
                        src[m_impl.point..].find('{').map(|n| {
                            let point = m_impl.point + n + 1;
                            for m_impl in search_scope(point, point, src.as_src(), pathseg, &m_impl.filepath, search_type, m_impl.local, namespace, session, pending_imports) {
                                out.push(m_impl);
                            }
                        });
                        for m_gen in search_for_generic_impls(m_impl.point, &m_impl.matchstr, &m, &m_impl.filepath, session) {
                            debug!("found generic impl!! {:?}", m_gen);
                            let pathseg = &path.segments[len-1];
                            let src = session.load_file(&m_gen.filepath);
                            // find the opening brace and skip to it.
                            src[m_gen.point..].find('{').map(|n| {
                                let point = m_gen.point + n + 1;
                                for m_gen in search_scope(point, point, src.as_src(), pathseg, &m_gen.filepath, search_type, m_gen.local, namespace, session, pending_imports) {
                                    out.push(m_gen);
                                }
                            });
                        }
                    };
                }
                Struct => {
                    debug!("found a struct. Now need to look for impl");
                    for m_impl in search_for_impls(m.point, &m.matchstr, &m.filepath, m.local, true, session, pending_imports) {
                        debug!("found impl!! {:?}", m_impl);
                        let pathseg = &path.segments[len-1];
                        let src = session.load_file(&m_impl.filepath);
                        // find the opening brace and skip to it.
                        src[m_impl.point..].find('{').map(|n| {
                            let point = m_impl.point + n + 1;
                            for m_impl in search_scope(point, point, src.as_src(), pathseg, &m_impl.filepath, search_type, m_impl.local, namespace, session, pending_imports) {
                                out.push(m_impl);
                            }
                        });
                        for m_gen in search_for_generic_impls(m_impl.point, &m_impl.matchstr, &m, &m_impl.filepath, session) {
                            debug!("found generic impl!! {:?}", m_gen);
                            let pathseg = &path.segments[len-1];
                            let src = session.load_file(&m_gen.filepath);
                            // find the opening brace and skip to it.
                            src[m_gen.point..].find('{').map(|n| {
                                let point = m_gen.point + n + 1;
                                for m_gen in search_scope(point, point, src.as_src(), pathseg, &m_gen.filepath, search_type, m_gen.local, namespace, session, pending_imports) {
                                    out.push(m_gen);
                                }
                            });
                        }
                    };
                }
                _ => ()
            }
        });
        debug!("resolve_path returning {:?}", out);
        out.into_iter()
    } else {
        // TODO: Should this better be an assertion ? Why do we have a core::Path
        // with empty segments in the first place ?
        Vec::new().into_iter()
    }
}

pub fn resolve_method(point: Point, msrc: Src, searchstr: &str,
                        filepath: &Path, search_type: SearchType, session: &Session,
                        pending_imports: &PendingImports) -> Vec<Match> {

    let scopestart = scopes::scope_start(msrc, point);
    debug!("resolve_method for |{}| pt: {} ({:?}); scopestart: {} ({:?})", 
        searchstr, 
        point, 
        msrc.src.point_to_coords(point), 
        scopestart,
        msrc.src.point_to_coords(scopestart));

    if let Some(stmtstart) = scopes::find_stmt_start(msrc, (scopestart - 1)) {
        let preblock = &msrc[stmtstart..scopestart];
        debug!("search_scope_headers preblock is |{}|", preblock);

        if preblock.starts_with("impl") {
            if let Some(n) = preblock.find(" for ") {
                let start = scopes::get_start_of_search_expr(preblock, n);
                let expr = &preblock[start..n];

                debug!("found impl of trait : expr is |{}|", expr);
                let path = core::Path::from_vec(false, expr.split("::").collect::<Vec<_>>());
                let m = resolve_path(&path,
                                     filepath,
                                     stmtstart + n - 1,
                                     SearchType::ExactMatch,
                                     Namespace::Both,
                                     session,
                                     pending_imports)
                    .filter(|m| m.mtype == Trait)
                    .nth(0);
                if let Some(m) = m {
                    debug!("found trait : match is |{:?}|", m);
                    let mut out = Vec::new();
                    let src = session.load_file(&m.filepath);
                    src[m.point..].find('{').map(|n| {
                        let point = m.point + n + 1;
                        for m in search_scope_for_static_trait_fns(point, src.as_src(), searchstr, &m.filepath, search_type) {
                            out.push(m);
                        }
                        for m in search_scope_for_methods(point, src.as_src(), searchstr, &m.filepath, search_type) {
                            out.push(m);
                        }
                    });

                    trace!(
                        "Found {} methods matching `{}` for trait `{}`", 
                        out.len(), 
                        searchstr, 
                        m.matchstr);

                    return out;
                }

            }
        }
    }

    Vec::new()
}

pub fn do_external_search(path: &[&str], filepath: &Path, pos: Point, search_type: SearchType, namespace: Namespace,
                          session: &Session) -> vec::IntoIter<Match> {
    debug!("do_external_search path {:?} {:?}", path, filepath.display());
    let mut out = Vec::new();
    if path.len() == 1 {
        let searchstr = path[0];
        // hack for now
        let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};

        for m in search_next_scope(pos, &pathseg, filepath, search_type, false, namespace, session, &PendingImports::empty()) {
            out.push(m);
        }

        get_module_file(searchstr, filepath.parent().unwrap(), session).map(|path| {
            let context = path.to_str().unwrap().to_owned();
            out.push(Match {
                           matchstr: searchstr.to_owned(),
                           filepath: path,
                           point: 0,
                           coords: Some(Coordinate { line: 1, column: 1 }),
                           local: false,
                           mtype: Module,
                           contextstr: context,
                           generic_args: Vec::new(),
                           generic_types: Vec::new(),
                           docs: String::new(),
                       });
        });
    } else {
        let parent_path = &path[..(path.len()-1)];
        let context = do_external_search(parent_path, filepath, pos, ExactMatch, Namespace::Type, session).nth(0);
        context.map(|m| {
            let pending_imports = &PendingImports::empty();
            match m.mtype {
                Module => {
                    debug!("found an external module {}", m.matchstr);
                    // deal with started with "{", so that "foo::{bar" will be same as "foo::bar"
                    let searchstr = match path[path.len()-1].chars().next() {
                        Some('{') => &path[path.len()-1][1..],
                        _ => path[path.len()-1]
                    };
                    let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};
                    for m in search_next_scope(m.point, &pathseg, &m.filepath, search_type, false, namespace, session, pending_imports) {
                        out.push(m);
                    }
                }

                Struct => {
                    debug!("found a pub struct. Now need to look for impl");
                    for m in search_for_impls(m.point, &m.matchstr, &m.filepath, m.local, false, session, pending_imports) {
                        debug!("found  impl2!! {}", m.matchstr);
                        // deal with started with "{", so that "foo::{bar" will be same as "foo::bar"
                        let searchstr = match path[path.len()-1].chars().next() {
                            Some('{') => &path[path.len()-1][1..],
                            _ => path[path.len()-1]
                        };
                        let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};
                        debug!("about to search impl scope...");
                        for m in search_next_scope(m.point, &pathseg, &m.filepath, search_type, m.local, namespace, session, pending_imports) {
                            out.push(m);
                        }
                    };
                }
                _ => ()
            }
        });
    }
    out.into_iter()
}

pub fn search_for_field_or_method(context: Match, searchstr: &str, search_type: SearchType,
                                  session: &Session) -> vec::IntoIter<Match> {
    let m = context;
    let mut out = Vec::new();
    match m.mtype {
        Struct => {
            debug!("got a struct, looking for fields and impl methods!! {}", m.matchstr);
            for m in search_struct_fields(searchstr, &m, search_type, session) {
                out.push(m);
            }
            for m in search_for_impl_methods(&m,
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    session) {
                out.push(m);
            }
        },
        Builtin => {
            for m in search_for_impl_methods(&m,
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    session) {
                out.push(m);
            }
        },
        Enum => {
            debug!("got an enum, looking for impl methods {}", m.matchstr);
            for m in search_for_impl_methods(&m,
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    session) {
                out.push(m);
            }
        },
        Trait => {
            debug!("got a trait, looking for methods {}", m.matchstr);
            let src = session.load_file(&m.filepath);
            src[m.point..].find('{').map(|n| {
                let point = m.point + n + 1;
                for m in search_scope_for_methods(point, src.as_src(), searchstr, &m.filepath, search_type) {
                    out.push(m);
                }
            });
        }
        _ => { debug!("WARN!! context wasn't a Struct, Enum, Builtin or Trait {:?}",m);}
    };
    out.into_iter()
}

fn search_for_deref_matches(impl_match: &Match, type_match: &Match, fieldsearchstr: &str, fpath: &Path, session: &Session) -> vec::IntoIter<Match>
{
    debug!("Found a Deref Implementation for {}, Searching for Methods on the Deref Type", type_match.matchstr);
    let mut out = Vec::new();

    if let Some(type_arg) = impl_match.generic_args.first() {
        // If Deref to a generic type
        if let Some(inner_type_path) = generic_arg_to_path(&type_arg, type_match) {
            let type_match = resolve_path_with_str(&inner_type_path.path,
                                                   &inner_type_path.filepath,
                                                   0, SearchType::ExactMatch,
                                                   Namespace::Type,
                                                   session).nth(0);
            let subpath = get_subpathsearch(&inner_type_path);
            if let Some(mut m) = type_match {
                if let Some(path) = subpath {
                    m.generic_types.push(path);
                }
                let methods = search_for_field_or_method(m, fieldsearchstr, SearchType::StartsWith, session);
                out.extend(methods);
            };

        }
        // If Deref to an ordinary type
        else {
            let deref_type_path = core::Path {
                global: false,
                segments: vec![core::PathSegment {
                    name: impl_match.generic_args.first().unwrap().clone(),
                    types: Vec::new()
                }]
            };
            let type_match = resolve_path_with_str(&deref_type_path, fpath, 0, SearchType::ExactMatch, Namespace::Type, session).nth(0);
            if let Some(m) = type_match {
                let methods = search_for_field_or_method(m, fieldsearchstr, SearchType::StartsWith, session);
                out.extend(methods);
            }
        }
    }

    out.into_iter()
}

fn generic_arg_to_path(type_str: &str, m: &Match) -> Option<core::PathSearch>
{
    debug!("Attempting to find type match for {} in {:?}", type_str, m);
    if let Some(match_pos) = m.generic_args.iter().position(|x| *x == type_str) {
        if let Some(gen_type) = m.generic_types.get(match_pos) {
            return Some(gen_type.clone());
        }
    }
    None
}

fn get_subpathsearch(pathsearch: &core::PathSearch) -> Option<core::PathSearch> {
    pathsearch.path.segments.get(0)
        .and_then(|seg| {seg.types.get(0)
                         .and_then(|first_type| {
                                     Some(core::PathSearch {
                                         path: first_type.clone(),
                                         filepath: pathsearch.filepath.clone(),
                                         point: pathsearch.point
                                     })
                         })
        })
}
