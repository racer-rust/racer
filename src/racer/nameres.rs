// Name resolution

use {core, ast, matchers, scopes, typeinf};
use core::SearchType::{self, ExactMatch, StartsWith};
use core::{Match, Src, SessionRef};
use core::MatchType::{Module, Function, Struct, Enum, FnArg, Trait, StructField, Impl, MatchArm};
use core::Namespace::{self, TypeNamespace, ValueNamespace, BothNamespaces};
use util::{symbol_matches, txt_matches, find_ident_end, path_exists};
use cargo;
use std::path::{Path, PathBuf};
use std::{self, vec};

#[cfg(unix)]
pub const PATH_SEP: &'static str = ":";
#[cfg(windows)]
pub const PATH_SEP: &'static str = ";";

fn search_struct_fields(searchstr: &str, structmatch: &Match,
                        search_type: SearchType, session: SessionRef) -> vec::IntoIter<Match> {
    let src = session.load_file(&structmatch.filepath);
    let opoint = scopes::find_stmt_start(src, structmatch.point);
    let structsrc = scopes::end_of_next_scope(&src[opoint.unwrap()..]);

    let fields = ast::parse_struct_fields(structsrc.to_owned(),
                                          core::Scope::from_match(structmatch));

    let mut out = Vec::new();

    for (field, fpos, _) in fields.into_iter() {
        if symbol_matches(search_type, searchstr, &field) {
            out.push(Match { matchstr: field.clone(),
                                filepath: structmatch.filepath.to_path_buf(),
                                point: fpos + opoint.unwrap(),
                                local: structmatch.local,
                                mtype: StructField,
                                contextstr: field,
                                generic_args: Vec::new(), generic_types: Vec::new()
            });
        }
    }
    out.into_iter()
}

pub fn search_for_impl_methods(implsearchstr: &str,
                           fieldsearchstr: &str, point: usize,
                           fpath: &Path, local: bool,
                           search_type: SearchType,
                               session: SessionRef) -> vec::IntoIter<Match> {
    debug!("searching for impl methods |{}| |{}| {:?}", implsearchstr, fieldsearchstr, fpath.to_str());

    let mut out = Vec::new();

    for m in search_for_impls(point, implsearchstr, fpath, local, true, session) {
        debug!("found impl!! |{:?}| looking for methods", m);
        let src = session.load_file(&m.filepath);

        // find the opening brace and skip to it.
        (&src[m.point..]).find("{").map(|n| {
            let point = m.point + n + 1;
            for m in search_scope_for_methods(point, src, fieldsearchstr, &m.filepath, search_type) {
                out.push(m);
            }
        });
    };
    out.into_iter()
}

fn search_scope_for_methods(point: usize, src: Src, searchstr: &str, filepath: &Path,
                            search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("searching scope for methods {} |{}| {:?}", point, searchstr, filepath.to_str());

    let scopesrc = src.from(point);
    let mut out = Vec::new();
    for (blobstart,blobend) in scopesrc.iter_stmts() {
        let blob = &scopesrc[blobstart..blobend];
        blob.find("{").map(|n| { // only matches if is a method implementation
            let signature = &blob[..n -1];

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
                           local: true,
                           mtype: Function,
                           contextstr: signature.to_owned(),
                           generic_args: Vec::new(), generic_types: Vec::new()
                };
                out.push(m);
            }
        });
    }
    out.into_iter()
}


pub fn search_for_impls(pos: usize, searchstr: &str, filepath: &Path, local: bool, include_traits: bool,
                        session: SessionRef) -> vec::IntoIter<Match> {
    debug!("search_for_impls {}, {}, {:?}", pos, searchstr, filepath.to_str());
    let s = session.load_file(filepath);
    let src = s.from(pos);

    let mut out = Vec::new();
    for (start, end) in src.iter_stmts() {
        let blob = &src[start..end];

        if blob.starts_with("impl") {
            blob.find("{").map(|n| {
                let mut decl = (&blob[..n+1]).to_owned();
                decl.push_str("}");
                if txt_matches(ExactMatch, searchstr, &decl) {
                    debug!("impl decl {}", decl);
                    let implres = ast::parse_impl(decl);
                    let is_trait_impl = implres.trait_path.is_some();

                    implres.name_path.map(|name_path| {
                        name_path.segments.last().map(|name| {
                            if symbol_matches(ExactMatch, searchstr, &name.name) {
                                let m = Match {
                                    matchstr: name.name.clone(),
                                    filepath: filepath.to_path_buf(),
                                    point: pos + start + 5,
                                    // items in trait impls have no "pub" but are
                                    // still accessible from other modules
                                    local: local || is_trait_impl,
                                    mtype: Impl,
                                    contextstr: "".into(),
                                    generic_args: Vec::new(),
                                    generic_types: Vec::new()
                                };
                                out.push(m);
                            }
                        });
                    });

                    // find trait
                    if include_traits && is_trait_impl {
                        let trait_path = implres.trait_path.unwrap();
                        let m = resolve_path(&trait_path,
                                             filepath, pos + start, ExactMatch, TypeNamespace,
                                             session).nth(0);
                        debug!("found trait |{:?}| {:?}", trait_path, m);
                        m.map(|m| out.push(m));
                    }
                }
            });
        }
    }
    out.into_iter()
}

// scope headers include fn decls, if let, while let etc..
fn search_scope_headers(point: usize, scopestart: usize, msrc: Src, searchstr: &str,
                        filepath: &Path, search_type: SearchType) -> vec::IntoIter<Match> {
    debug!("search_scope_headers for |{}| pt: {}", searchstr, scopestart);
    if let Some(stmtstart) = scopes::find_stmt_start(msrc, scopestart) {
        let preblock = &msrc[stmtstart..scopestart];
        debug!("search_scope_headers preblock is |{}|", preblock);

        if preblock.starts_with("fn") || preblock.starts_with("pub fn") || preblock.starts_with("pub const fn") {
            return search_fn_args(stmtstart, scopestart, &msrc, searchstr, filepath, search_type, true);

        // 'if let' can be an expression, so might not be at the start of the stmt
        } else if let Some(n) = preblock.find("if let") {
            let ifletstart = stmtstart + n;
            let src = (&msrc[ifletstart..scopestart+1]).to_owned() + "}";
            if txt_matches(search_type, searchstr, &src) {
                let mut out = matchers::match_if_let(&src, 0, src.len(), searchstr,
                                                     filepath, search_type, true);
                for m in &mut out {
                    m.point += ifletstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("while let") {
            let src = (&msrc[stmtstart..scopestart+1]).to_owned() + "}";
            if txt_matches(search_type, searchstr, &src[..scopestart]) {
                let mut out = matchers::match_while_let(&src, 0, src.len(), searchstr,
                                                        filepath, search_type, true);
                for m in &mut out {
                    m.point += stmtstart;
                }
                return out.into_iter();
            }
        } else if preblock.starts_with("for ") {
            let src = (&msrc[stmtstart..scopestart+1]).to_owned() + "}";
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
                                    local: true,
                                    mtype: MatchArm,
                                    contextstr: lhs.trim().to_owned(),
                                    generic_args: Vec::new(),
                                    generic_types: Vec::new()
                    });
                    if let SearchType::ExactMatch = search_type {
                        break;
                    }
                }
            }
            return out.into_iter();
        }
    }

    Vec::new().into_iter()
}

fn mask_matchstmt(matchstmt_src: &str, innerscope_start: usize) -> String {
    let s = scopes::mask_sub_scopes(&matchstmt_src[innerscope_start..]);
    (&matchstmt_src[..innerscope_start]).to_owned() + &s
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

fn search_fn_args(fnstart: usize, open_brace_pos: usize, msrc: &str,
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
                                local: local,
                                mtype: FnArg,
                                contextstr: s.to_owned(),
                                generic_args: Vec::new(),
                                generic_types: Vec::new()
                };
                debug!("search_fn_args matched: {:?}", m);
                out.push(m);
            }
        }
    }
    out.into_iter()
}

pub fn do_file_search(searchstr: &str, currentdir: &Path) -> vec::IntoIter<Match> {
    debug!("do_file_search {}", searchstr);
    let mut out = Vec::new();

    let srcpaths = std::env::var("RUST_SRC_PATH").unwrap_or("".into());
    debug!("do_file_search srcpaths {}", srcpaths);
    let mut v = srcpaths.split(PATH_SEP).collect::<Vec<_>>();
    v.push(currentdir.to_str().unwrap());
    debug!("do_file_search v is {:?}", v);
    for srcpath in v.into_iter() {
        if let Ok(iter) = std::fs::read_dir(&Path::new(srcpath)) {
            for fpath_buf in iter.filter_map(|res| res.ok().map(|entry| entry.path())) {
                // skip filenames that can't be decoded
                let fname = match fpath_buf.file_name().and_then(|n| n.to_str()) {
                    Some(fname) => fname,
                    None => continue,
                };
                if fname.starts_with(&format!("lib{}", searchstr)) {
                    let filepath = fpath_buf.join("lib.rs");
                    if path_exists(&filepath) {
                        let m = Match {
                                       matchstr: (&fname[3..]).to_owned(),
                                       filepath: filepath.to_path_buf(),
                                       point: 0,
                                       local: false,
                                       mtype: Module,
                                       contextstr: (&fname[3..]).to_owned(),
                                       generic_args: Vec::new(),
                                       generic_types: Vec::new()
                        };
                        out.push(m);
                    }
                }

                if fname.starts_with(searchstr) {
                    for name in &[&format!("{}.rs", fname)[..], "mod.rs", "lib.rs"] {
                        let filepath = fpath_buf.join(name);

                        if path_exists(&filepath) {
                            let m = Match {
                                           matchstr: fname.to_owned(),
                                           filepath: filepath.to_path_buf(),
                                           point: 0,
                                           local: false,
                                           mtype: Module,
                                           contextstr: filepath.to_str().unwrap().to_owned(),
                                           generic_args: Vec::new(),
                                           generic_types: Vec::new()
                            };
                            out.push(m);
                        }
                    }
                    // try just <name>.rs
                    if fname.ends_with(".rs") && path_exists(&fpath_buf) {
                        let m = Match {
                                       matchstr: (&fname[..(fname.len()-3)]).to_owned(),
                                       filepath: fpath_buf.clone(),
                                       point: 0,
                                       local: false,
                                       mtype: Module,
                                       contextstr: fpath_buf.to_str().unwrap().to_owned(),
                                       generic_args: Vec::new(),
                                       generic_types: Vec::new()
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
                         session: SessionRef) -> vec::IntoIter<Match> {
    debug!("search_crate_root |{:?}| {:?}", pathseg, modfpath.to_str());

    let crateroots = find_possible_crate_root_modules(modfpath.parent().unwrap());
    let mut out = Vec::new();
    for crateroot in crateroots {
        if *modfpath == *crateroot {
            continue;
        }
        debug!("going to search for {:?} in crateroot {:?}", pathseg, crateroot.to_str());
        for m in resolve_name(pathseg, &crateroot, 0, searchtype, namespace, session) {
            out.push(m);
            if let ExactMatch = searchtype {
                break;
            }
        }
        break
    }
    out.into_iter()
}

pub fn find_possible_crate_root_modules(currentdir: &Path) -> Vec<PathBuf> {
    let mut res = Vec::new();

    {
        let filepath = currentdir.join("lib.rs");
        if path_exists(&filepath) {
            res.push(filepath.to_path_buf());
            return res;   // for now stop at the first match
        }
    }
    {
        let filepath = currentdir.join("main.rs");
        if path_exists(&filepath) {
            res.push(filepath.to_path_buf());
            return res;   // for now stop at the first match
        }
    }
    {
        // recurse up the directory structure
        if let Some(parentdir) = currentdir.parent() {
            if parentdir != currentdir {
                // PD: this was using the vec.push_all() api, but that is now unstable
                res.extend(find_possible_crate_root_modules(&parentdir).iter().cloned());
                return res;   // for now stop at the first match
            }
        }
    }
    res
}

pub fn search_next_scope(mut startpoint: usize, pathseg: &core::PathSegment,
                         filepath:&Path, search_type: SearchType, local: bool,
                         namespace: Namespace,
                         session: SessionRef) -> vec::IntoIter<Match> {
    let filesrc = session.load_file(filepath);
    if startpoint != 0 {
        // is a scope inside the file. Point should point to the definition
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = &filesrc[startpoint..];
        //debug!("search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it.
        src.find("{").map(|n| {
            startpoint += n + 1;
        });
    }
    search_scope(startpoint, startpoint, filesrc, pathseg, filepath, search_type, local, namespace, session)
}

pub fn get_crate_file(name: &str, from_path: &Path) -> Option<PathBuf> {
    debug!("get_crate_file {}", name);
    if let Some(p) = cargo::get_crate_file(name, from_path) {
        debug!("nameres::get_crate_file  - found the crate file! {:?}", p);
        return Some(p);
    }

    let srcpaths = std::env::var("RUST_SRC_PATH").unwrap();
    let v = (&srcpaths).split(PATH_SEP).collect::<Vec<_>>();
    for srcpath in v.into_iter() {
        {
            // try lib<name>/lib.rs, like in the rust source dir
            let cratelibname = format!("lib{}", name);
            let filepath = Path::new(srcpath).join(cratelibname).join("lib.rs");
            if path_exists(&filepath) {
                return Some(filepath.to_path_buf());
            }
        }
        {
            // try <name>/lib.rs
            let filepath = Path::new(srcpath).join(name).join("lib.rs");
            if path_exists(&filepath) {
                return Some(filepath.to_path_buf());
            }
        }
    }
    None
}

pub fn get_module_file(name: &str, parentdir: &Path) -> Option<PathBuf> {
    {
        // try just <name>.rs
        let filepath = parentdir.join(format!("{}.rs", name));
        if path_exists(&filepath) {
            return Some(filepath.to_path_buf());
        }
    }
    {
        // try <name>/mod.rs
        let filepath = parentdir.join(name).join("mod.rs");
        if path_exists(&filepath) {
            return Some(filepath.to_path_buf());
        }
    }
    None
}

pub fn search_scope(start: usize, point: usize, src: Src,
                    pathseg: &core::PathSegment,
                    filepath:&Path, search_type: SearchType, local: bool,
                    namespace: Namespace,
                    session: SessionRef) -> vec::IntoIter<Match> {
    let searchstr = &pathseg.name;
    let mut out = Vec::new();

    debug!("searching scope {:?} start: {} point: {} '{}' {:?} {:?} local: {}, session: {:?}",
           namespace, start, point, searchstr, filepath.to_str(), search_type, local, session);

    let scopesrc = src.from(start);
    let mut skip_next_block = false;
    let mut delayed_use_globs = Vec::new();
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
    // now search from top of scope for items etc..
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

        let is_a_use_glob = (blob.starts_with("use") || blob.starts_with("pub use"))
              && blob.find("::*").is_some();

        if is_a_use_glob {
            // globs are *really* expensive to process. delay them until later
            delayed_use_globs.push((blobstart, blobend));
            continue;
        }

        if searchstr == "core" && blob.starts_with("#![no_std]") {
            debug!("Looking for core and found #![no_std], which implicitly imports it");
            get_crate_file("core", filepath).map(|cratepath| {
                out.push(Match { matchstr: "core".into(),
                                  filepath: cratepath.to_path_buf(),
                                  point: 0,
                                  local: false,
                                  mtype: Module,
                                  contextstr: cratepath.to_str().unwrap().to_owned(),
                                  generic_args: Vec::new(),
                                  generic_types: Vec::new()
                });
            });
        }

        // Optimisation: if the search string is not in the blob and it is not
        // a 'use glob', this cannot match so fail fast!
        if blob.find(searchstr).is_none() {
            continue;
        }

        // There's a good chance of a match. Run the matchers
        out.extend(run_matchers_on_blob(src, start+blobstart, start+blobend,
                                        searchstr,
                                        filepath, search_type, local, namespace, session));
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    // finally process any use-globs that we skipped before
    for (blobstart, blobend) in delayed_use_globs {
        // There's a good chance of a match. Run the matchers
        for m in run_matchers_on_blob(src, start+blobstart, start+blobend,
                                      searchstr, filepath, search_type,
                                      local, namespace, session).into_iter() {
            out.push(m);
            if let ExactMatch = search_type {
                return out.into_iter();
            }
        }
    }

    debug!("search_scope found matches {:?} {:?}", search_type, out);
    out.into_iter()
}

fn run_matchers_on_blob(src: Src, start: usize, end: usize, searchstr: &str,
                         filepath: &Path, search_type: SearchType, local: bool,
                         namespace: Namespace, session: SessionRef) -> Vec<Match> {
    let mut out = Vec::new();
    match namespace {
        TypeNamespace =>
            for m in matchers::match_types(src, start,
                                           end, searchstr,
                                           filepath, search_type, local, session) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            },
        ValueNamespace =>
            for m in matchers::match_values(src, start,
                                            end, searchstr,
                                            filepath, search_type, local) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out;
                }
            },
        BothNamespaces => {
            for m in matchers::match_types(src, start,
                                           end, searchstr,
                                           filepath, search_type, local, session) {
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
                       msrc: Src, point: usize, search_type: SearchType,
                       namespace: Namespace,
                       session: SessionRef) -> vec::IntoIter<Match> {
    debug!("search_local_scopes {:?} {:?} {} {:?} {:?}", pathseg, filepath.to_str(), point,
           search_type, namespace);

    if point == 0 {
        // search the whole file
        search_scope(0, 0, msrc, pathseg, filepath, search_type, true, namespace, session)
    } else {
        let mut out = Vec::new();
        let mut start = point;
        // search each parent scope in turn
        while start > 0 {
            start = scopes::scope_start(msrc, start);
            for m in search_scope(start, point, msrc, pathseg, filepath, search_type, true, namespace, session) {
                out.push(m);
                if let ExactMatch = search_type {
                    return out.into_iter();
                }
            }
            if start == 0 {
                break;
            }
            start = start-1;
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
                           namespace: Namespace, session: SessionRef) -> vec::IntoIter<Match> {
    debug!("search_prelude file {:?} {:?} {:?}", pathseg, search_type, namespace);
//    debug!("PHIL searching prelude file, backtrace: {}",util::get_backtrace());

    let mut out : Vec<Match> = Vec::new();

    // find the prelude file from the search path and scan it
    let srcpaths = match std::env::var("RUST_SRC_PATH") {
        Ok(paths) => paths,
        Err(_) => return out.into_iter()
    };

    let v = srcpaths.split(PATH_SEP).collect::<Vec<_>>();

    for srcpath in v.into_iter() {
        let filepath = Path::new(srcpath).join("libstd").join("prelude").join("v1.rs");
        if path_exists(&filepath) {
            let msrc = session.load_file_and_mask_comments(&filepath);
            let is_local = true;
            for m in search_scope(0, 0, msrc, pathseg, &filepath, search_type, is_local, namespace, session) {
                out.push(m);
            }
        }
    }
    out.into_iter()
}

pub fn resolve_path_with_str(path: &core::Path, filepath: &Path, pos: usize,
                                   search_type: SearchType, namespace: Namespace,
                                   session: SessionRef) -> vec::IntoIter<Match> {
    debug!("resolve_path_with_str {:?}", path);

    let mut out = Vec::new();

    // HACK
    if path.segments.len() == 1 && path.segments[0].name == "str" {
        debug!("{:?} == {:?}", path.segments[0], "str");
        let str_pathseg = core::PathSegment{ name: "Str".into(), types: Vec::new() };
        let str_match = resolve_name(&str_pathseg, filepath, pos, ExactMatch, namespace, session).nth(0);
        debug!("str_match {:?}", str_match);

        str_match.map(|str_match| {
            debug!("found Str, converting to str");
            let m = Match {
                           matchstr: "str".into(),
                           filepath: str_match.filepath.to_path_buf(),
                           point: str_match.point,
                           local: false,
                           mtype: Struct,
                           contextstr: "str".into(),
                           generic_args: Vec::new(),
                           generic_types: Vec::new()
            };
            out.push(m);
        });
    } else {
        for m in resolve_path(path, filepath, pos, search_type, namespace, session) {
            out.push(m);
            if let ExactMatch = search_type {
                break;
            }
        }
    }
    out.into_iter()
}

thread_local!(pub static SEARCH_STACK: Vec<Search> = Vec::new());

#[derive(PartialEq,Debug)]
pub struct Search {
    path: Vec<String>,
    filepath: String,
    pos: usize
}

pub fn is_a_repeat_search(new_search: &Search) -> bool {
    SEARCH_STACK.with(|v| {
        for s in v {
            if s == new_search {
                debug!("is a repeat search {:?} Stack: {:?}", new_search, v);
                return true;
            }
        }
        false
    })
}

pub fn resolve_name(pathseg: &core::PathSegment, filepath: &Path, pos: usize,
                    search_type: SearchType, namespace: Namespace,
                    session: SessionRef) -> vec::IntoIter<Match> {
    let mut out = Vec::new();
    let searchstr = &pathseg.name;

    debug!("resolve_name {} {:?} {} {:?} {:?}", searchstr, filepath.to_str(), pos, search_type, namespace);

    let msrc = session.load_file_and_mask_comments(filepath);
    let is_exact_match = match search_type { ExactMatch => true, StartsWith => false };

    if (is_exact_match && &searchstr[..] == "std") ||
       (!is_exact_match && "std".starts_with(searchstr)) {
        get_crate_file("std", filepath).map(|cratepath| {
            out.push(Match {
                        matchstr: "std".into(),
                        filepath: cratepath.to_path_buf(),
                        point: 0,
                        local: false,
                        mtype: Module,
                        contextstr: cratepath.to_str().unwrap().to_owned(),
                        generic_args: Vec::new(), generic_types: Vec::new()
            });
        });

        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }



    for m in search_local_scopes(pathseg, filepath, msrc, pos, search_type, namespace, session) {
        out.push(m);
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    for m in search_crate_root(pathseg, &filepath, search_type, namespace, session) {
        out.push(m);
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }

    for m in search_prelude_file(pathseg, search_type, namespace, session) {
        out.push(m);
        if let ExactMatch = search_type {
            if !out.is_empty() {
                return out.into_iter();
            }
        }
    }
    // filesearch. Used to complete e.g. extern crate blah or mod foo
    if let StartsWith = search_type {
        for m in do_file_search(searchstr, &filepath.parent().unwrap()) {
            out.push(m);
        }
    }
    out.into_iter()
}

// Get the scope corresponding to super::
pub fn get_super_scope(filepath: &Path, pos: usize, session: SessionRef) -> Option<core::Scope> {
    let msrc = session.load_file_and_mask_comments(filepath);
    let mut path = scopes::get_local_module_path(msrc, pos);
    debug!("get_super_scope: path: {:?} filepath: {:?} {} {:?}", path, filepath, pos, session);
    if path.is_empty() {
        let moduledir;
        if filepath.ends_with("mod.rs") || filepath.ends_with("lib.rs"){
            // Need to go up to directory above
            // TODO(PD): fix: will crash if mod.rs is in the root fs directory 
            moduledir = filepath.parent().unwrap().parent().unwrap();
        } else {
            // module is in current directory
            moduledir = filepath.parent().unwrap(); 
        }

        for filename in &[ "mod.rs", "lib.rs" ] {
            let fpath = moduledir.join(&filename);
            if path_exists(&fpath) {
                return Some(core::Scope{ filepath: fpath, point: 0 })
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
                            Namespace::TypeNamespace, session).nth(0)
            .and_then(|m| msrc[m.point..].find("{")
                      .map(|p| core::Scope{ filepath: filepath.to_path_buf(),
                                             point:m.point + p + 1 }))
    }
}

pub fn resolve_path(path: &core::Path, filepath: &Path, pos: usize,
                    search_type: SearchType, namespace: Namespace,
                    session: SessionRef) -> vec::IntoIter<Match> {
    debug!("resolve_path {:?} {:?} {} {:?}", path, filepath.to_str(), pos, search_type);
    let len = path.segments.len();
    if len == 1 {
        let ref pathseg = path.segments[0];
        resolve_name(pathseg, filepath, pos, search_type, namespace, session)
    } else if len != 0 {
        if path.segments[0].name == "self" {
            // just remove self
            let mut newpath: core::Path = path.clone();
            newpath.segments.remove(0);
            return resolve_path(&newpath, filepath, pos, search_type, namespace, session);
        }

        if path.segments[0].name == "super" {
            if let Some(scope) = get_super_scope(filepath, pos, session) {
                debug!("PHIL super scope is {:?}", scope);

                let mut newpath: core::Path = path.clone();
                newpath.segments.remove(0);
                return resolve_path(&newpath, &scope.filepath,
                                    scope.point, search_type, namespace, session);
            } else {
                // can't find super scope. Return no matches
                debug!("can't resolve path {:?}, returning no matches", path);
                return Vec::new().into_iter();
            }
        }

        let mut out = Vec::new();
        let mut parent_path: core::Path = path.clone();
        parent_path.segments.remove(len-1);
        let context = resolve_path(&parent_path, filepath, pos, ExactMatch, TypeNamespace, session).nth(0);
        context.map(|m| {
            match m.mtype {
                Module => {
                    let ref pathseg = path.segments[len-1];
                    debug!("searching a module '{}' for {} (whole path: {:?})", m.matchstr, pathseg.name, path);
                    for m in search_next_scope(m.point, pathseg, &m.filepath, search_type, false, namespace, session) {
                        out.push(m);
                    }
                }
                Enum => {
                    let ref pathseg = path.segments[len-1];
                    debug!("searching an enum '{}' (whole path: {:?}) searchtype: {:?}", m.matchstr, path, search_type);

                    let filesrc = session.load_file(&m.filepath);
                    let scopestart = scopes::find_stmt_start(filesrc, m.point).unwrap();
                    let scopesrc = filesrc.from(scopestart);
                    scopesrc.iter_stmts().nth(0).map(|(blobstart,blobend)| {
                        for m in matchers::match_enum_variants(&filesrc,
                                                               scopestart+blobstart,
                                                               scopestart+blobend,
                                                      &pathseg.name, &m.filepath, search_type, true) {
                            debug!("Found enum variant: {}", m.matchstr);
                            out.push(m);
                        }
                    });
                }
                Struct => {
                    debug!("found a struct. Now need to look for impl");
                    for m in search_for_impls(m.point, &m.matchstr, &m.filepath, m.local, false, session) {
                        debug!("found impl!! {:?}", m);
                        let ref pathseg = path.segments[len-1];
                        let src = session.load_file(&m.filepath);
                        // find the opening brace and skip to it.
                        (&src[m.point..]).find("{").map(|n| {
                            let point = m.point + n + 1;
                            for m in search_scope(point, point, src, pathseg, &m.filepath, search_type, m.local, namespace, session) {
                                out.push(m);
                            }
                        });
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

pub fn do_external_search(path: &[&str], filepath: &Path, pos: usize, search_type: SearchType, namespace: Namespace,
                          session: SessionRef) -> vec::IntoIter<Match> {
    debug!("do_external_search path {:?} {:?}", path, filepath.to_str());
    let mut out = Vec::new();
    if path.len() == 1 {
        let searchstr = path[0];
        // hack for now
        let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};

        for m in search_next_scope(pos, &pathseg, filepath, search_type, false, namespace, session) {
            out.push(m);
        }

        get_module_file(searchstr, &filepath.parent().unwrap()).map(|path| {
            out.push(Match {
                           matchstr: searchstr.to_owned(),
                           filepath: path.to_path_buf(),
                           point: 0,
                           local: false,
                           mtype: Module,
                           contextstr: path.to_str().unwrap().to_owned(),
                           generic_args: Vec::new(),
                           generic_types: Vec::new()
                       });
        });
    } else {
        let parent_path = &path[..(path.len()-1)];
        let context = do_external_search(parent_path, filepath, pos, ExactMatch, TypeNamespace, session).nth(0);
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("found an external module {}", m.matchstr);
                    let searchstr = path[path.len()-1];
                    let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};
                    for m in search_next_scope(m.point, &pathseg, &m.filepath, search_type, false, namespace, session) {
                        out.push(m);
                    }
                }

                Struct => {
                    debug!("found a pub struct. Now need to look for impl");
                    for m in search_for_impls(m.point, &m.matchstr, &m.filepath, m.local, false, session) {
                        debug!("found  impl2!! {}", m.matchstr);
                        let searchstr = path[path.len()-1];
                        let pathseg = core::PathSegment{name: searchstr.to_owned(),
                                         types: Vec::new()};
                        debug!("about to search impl scope...");
                        for m in search_next_scope(m.point, &pathseg, &m.filepath, search_type, m.local, namespace, session) {
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
                                  session: SessionRef) -> vec::IntoIter<Match> {
    let m = context;
    let mut out = Vec::new();
    match m.mtype {
        Struct => {
            debug!("got a struct, looking for fields and impl methods!! {}", m.matchstr);
            for m in search_struct_fields(searchstr, &m, search_type, session) {
                out.push(m);
            }
            for m in search_for_impl_methods(&m.matchstr,
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
            for m in search_for_impl_methods(&m.matchstr,
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
            (&src[m.point..]).find("{").map(|n| {
                let point = m.point + n + 1;
                for m in search_scope_for_methods(point, src, searchstr, &m.filepath, search_type) {
                    out.push(m);
                }
            });
        }
        _ => { debug!("WARN!! context wasn't a Struct, Enum or Trait {:?}",m);}
    };
    out.into_iter()
}
