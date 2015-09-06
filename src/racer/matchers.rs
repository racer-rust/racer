use {scopes, typeinf, ast};
use core::{Match, PathSegment, Src, SessionRef};
use util::{symbol_matches, txt_matches, find_ident_end, is_ident_char, char_at};
use nameres::{get_module_file, get_crate_file, resolve_path};
use core::SearchType::{self, StartsWith, ExactMatch};
use core::MatchType::{self, Let, Module, Function, Struct, Type, Trait, Enum, EnumVariant,
                      Const, Static, IfLet, WhileLet, For};
use core::Namespace::BothNamespaces;
use std::cell::Cell;
use std::path::Path;
use std::{iter, option, vec};

pub type MIter = option::IntoIter<Match>;
pub type MChain<T> = iter::Chain<T, MIter>;

// Should I return a boxed trait object to make this signature nicer?
pub fn match_types(src: Src, blobstart: usize, blobend: usize,
                   searchstr: &str, filepath: &Path,
                   search_type: SearchType,
                   local: bool, session: SessionRef) -> iter::Chain<MChain<MChain<MChain<MChain<MChain<MIter>>>>>, vec::IntoIter<Match>> {
    let it = match_extern_crate(&src, blobstart, blobend, searchstr, filepath, search_type, session).into_iter();
    let it = it.chain(match_mod(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_struct(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_type(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_trait(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_enum(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    it.chain(match_use(&src, blobstart, blobend, searchstr, filepath, search_type, local, session).into_iter())
}

pub fn match_values(src: Src, blobstart: usize, blobend: usize,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> MChain<MChain<MIter>> {
    let it = match_const(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter();
    let it = it.chain(match_static(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    it.chain(match_fn(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter())
}

fn find_keyword(src: &str, pattern: &str, search: &str, search_type: SearchType, local: bool)
-> Option<usize> {
    // search for "^(pub\s+)?(unsafe\s+)?pattern\s+search"

    // if not local must start with pub
    if !local && !src.starts_with("pub") { return None; }

    let mut start = 0usize;

    // optional (pub\s+)?(unsafe\s+)?
    for pat in ["pub", "unsafe"].into_iter() {
        if src[start..].starts_with(pat) {
            // remove whitespaces ... must have one at least
            start += pat.len();
            let oldstart = start;
            for &b in src[start..].as_bytes() {
                match b {
                    b' '|b'\r'|b'\n'|b'\t' => start += 1,
                    _ => break
                }
            }
            if start == oldstart { return None; }
        }
    }

    // mandatory pattern\s+
    if src[start..].starts_with(pattern) {
        // remove whitespaces ... must have one at least
        start += pattern.len();
        let oldstart = start;
        for &b in src[start..].as_bytes() {
            match b {
                b' '|b'\r'|b'\n'|b'\t' => start += 1,
                _ => break
            }
        }
        if start == oldstart { return None; }
    } else {
        return None;
    }

    if src[start..].starts_with(search) {
        match search_type {
            StartsWith => Some(start),
            ExactMatch => {
                if src.len() > start+search.len() &&
                    !is_ident_char(char_at(src, start + search.len())) {
                    Some(start)
                } else {
                    None
                }
            }
        }
    } else {
        None
    }
}

fn is_const_fn(src: &str, blobstart: usize, blobend: usize) -> bool {
    src[blobstart..blobend].contains("const fn")
}

fn match_pattern_start(src: &str, blobstart: usize, blobend: usize,
                       searchstr: &str, filepath: &Path, search_type: SearchType,
                       local: bool, pattern: &str, mtype: MatchType) -> Option<Match> {
    // ast currently doesn't contain the ident coords, so match them with a hacky
    // string search

    let blob = &src[blobstart..blobend];
    if let Some(start) = find_keyword(blob, pattern, searchstr, search_type, local) {
        if let Some(end) = blob[start..].find(':') {
            let s = blob[start..start+end].trim_right();
            return Some(Match {
                matchstr: s.to_owned(),
                filepath: filepath.to_path_buf(),
                point: blobstart+start,
                local: local,
                mtype: mtype,
                contextstr: first_line(blob),
                generic_args: Vec::new(),
                generic_types: Vec::new()
            })
        }
    }
    None
}

pub fn match_const(msrc: &str, blobstart: usize, blobend: usize,
                   searchstr: &str, filepath: &Path, search_type: SearchType,
                   local: bool) -> Option<Match> {
    if is_const_fn(msrc, blobstart, blobend) {
        return None;
    }
    match_pattern_start(msrc, blobstart, blobend, searchstr, filepath,
                        search_type, local, "const", Const)
}

pub fn match_static(msrc: &str, blobstart: usize, blobend: usize,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> Option<Match> {
    match_pattern_start(msrc, blobstart, blobend, searchstr, filepath,
                        search_type, local, "static", Static)
}

fn match_pattern_let(msrc: &str, blobstart: usize, blobend: usize,
                     searchstr: &str, filepath: &Path, search_type: SearchType,
                     local: bool, pattern: &str, mtype: MatchType) -> Vec<Match> {
    let mut out = Vec::new();
    let blob = &msrc[blobstart..blobend];
    if blob.starts_with(pattern) && txt_matches(search_type, searchstr, blob) {
        let coords = ast::parse_pat_bind_stmt(blob.to_owned());
        for (start, end) in coords {
            let s = &blob[start..end];
            if symbol_matches(search_type, searchstr, s) {
                debug!("match_pattern_let point is {}", blobstart + start);
                out.push(Match { matchstr: s.to_owned(),
                                   filepath: filepath.to_path_buf(),
                                   point: blobstart + start,
                                   local: local,
                                   mtype: mtype,
                                   contextstr: first_line(blob),
                                   generic_args: Vec::new(),
                                   generic_types: Vec::new()
                         });
                if let ExactMatch = search_type {
                    break;
                }
            }
        }
    }
    out
}

pub fn match_if_let(msrc: &str, blobstart: usize, blobend: usize,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "if let ", IfLet)
}

pub fn match_while_let(msrc: &str, blobstart: usize, blobend: usize,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "while let ", WhileLet)
}

pub fn match_let(msrc: &str, blobstart: usize, blobend: usize,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "let ", Let)
}

pub fn match_for(msrc: &str, blobstart: usize, blobend: usize,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Vec<Match> {
    let mut out = Vec::new();
    let blob = &msrc[blobstart..blobend];
    let coords = ast::parse_pat_bind_stmt(blob.to_owned());
    for (start, end) in coords {
        let s = &blob[start..end];
        if symbol_matches(search_type, searchstr, s) {
            debug!("match_for point is {}, found ident {}", blobstart + start, s);
            out.push(Match { matchstr: s.to_owned(),
                             filepath: filepath.to_path_buf(),
                             point: blobstart + start,
                             local: local,
                             mtype: For,
                             contextstr: first_line(blob),
                             generic_args: Vec::new(),
                             generic_types: Vec::new() });
        }
    }
    out
}

pub fn first_line(blob: &str) -> String {
    (&blob[..blob.find('\n').unwrap_or(blob.len())]).to_owned()
}

pub fn match_extern_crate(msrc: &str, blobstart: usize, blobend: usize,
                          searchstr: &str, filepath: &Path, search_type: SearchType,
                          session: SessionRef) -> Option<Match> {
    let mut res = None;
    let blob = &msrc[blobstart..blobend];

    if txt_matches(search_type, &format!("extern crate {};", searchstr), blob) &&
        !(txt_matches(search_type, &format!("extern crate {} as", searchstr), blob))
        || (blob.starts_with("extern crate") &&
            txt_matches(search_type, &format!("as {}", searchstr), blob)) {

        debug!("found an extern crate: |{}|", blob);

        let extern_crate;
        if blob.contains("\"") {
            // Annoyingly the extern crate can use a string literal for the
            // real crate name (e.g. extern crate collections_core = "collections")
            // so we need to get the source text without scrubbed strings

            let rawsrc = session.load_file(filepath);
            let rawblob = &rawsrc[blobstart..blobend];
            debug!("found an extern crate (unscrubbed): |{}|", rawblob);

            extern_crate = ast::parse_extern_crate(rawblob.to_owned());
        } else {
            extern_crate = ast::parse_extern_crate(blob.to_owned());
        }

        if let Some(ref name) = extern_crate.name {
            debug!("extern crate {}", name);

            let realname =
                if let Some(ref realname) = extern_crate.realname {
                    realname
                } else {
                    name
                };
            get_crate_file(&realname, filepath).map(|cratepath| {
                res = Some(Match { matchstr: name.clone(),
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
    }
    res
}

pub fn match_mod(msrc: Src, blobstart: usize, blobend: usize,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "mod", searchstr, search_type, local) {
        debug!("found a module: |{}|", blob);
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };

        if blob.find('{').is_some() {
            debug!("found an inline module!");

            return Some(Match {
                matchstr: l.to_owned(),
                filepath: filepath.to_path_buf(),
                point: blobstart + start,
                local: false,
                mtype: Module,
                contextstr: filepath.to_str().unwrap().to_owned(),
                generic_args: Vec::new(),
                generic_types: Vec::new()
            })
        } else {
            // get internal module nesting
            // e.g. is this in an inline submodule?  mod foo{ mod bar; }
            // because if it is then we need to search further down the
            // directory hierarchy - e.g. <cwd>/foo/bar.rs
            let internalpath = scopes::get_local_module_path(msrc, blobstart);
            let mut searchdir = filepath.parent().unwrap().to_path_buf();
            for s in internalpath {
                searchdir.push(&s);
            }
            if let Some(modpath) = get_module_file(l, &searchdir) {
                return Some(Match {
                    matchstr: l.to_owned(),
                    filepath: modpath.to_path_buf(),
                    point: 0,
                    local: false,
                    mtype: Module,
                    contextstr: modpath.to_str().unwrap().to_owned(),
                    generic_args: Vec::new(),
                    generic_types: Vec::new()
                })
            }
        }
    }
    None
}

pub fn match_struct(msrc: &str, blobstart: usize, blobend: usize,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "struct", searchstr, search_type, local) {
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };
        debug!("found a struct |{}|", l);

        // Parse generics
        let end = match blob.find('{').or(blob.find(';')) {
            Some(e) => e,
            None => { 
                error!("Can't find end of struct header");
                return None;
            }
        };

        // structs with no values need to end in ';', not '{}'
        let generics = ast::parse_generics(format!("{};", &blob[..end]));

        Some(Match {
            matchstr: l.to_owned(),
            filepath: filepath.to_path_buf(),
            point: blobstart + start,
            local: local,
            mtype: Struct,
            contextstr: first_line(blob),
            generic_args: generics.generic_args,
            generic_types: Vec::new()
        })
    } else {
        None
    }
}

pub fn match_type(msrc: &str, blobstart: usize, blobend: usize,
                  searchstr: &str, filepath: &Path, search_type: SearchType,
                  local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "type", searchstr, search_type, local) {
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };
        debug!("found!! a type {}", l);
        Some(Match {
            matchstr: l.to_owned(),
            filepath: filepath.to_path_buf(),
            point: blobstart + start,
            local: local,
            mtype: Type,
            contextstr: first_line(blob),
            generic_args: Vec::new(),
            generic_types: Vec::new()
        })
    } else {
        None
    }
}

pub fn match_trait(msrc: &str, blobstart: usize, blobend: usize,
                   searchstr: &str, filepath: &Path, search_type: SearchType,
                   local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "trait", searchstr, search_type, local) {
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };
        debug!("found!! a trait {}", l);
        Some(Match {
            matchstr: l.to_owned(),
            filepath: filepath.to_path_buf(),
            point: blobstart + start,
            local: local,
            mtype: Trait,
            contextstr: first_line(blob),
            generic_args: Vec::new(),
            generic_types: Vec::new()
        })
    } else {
        None
    }
}

pub fn match_enum_variants(msrc: &str, blobstart: usize, blobend: usize,
                           searchstr: &str, filepath: &Path, search_type: SearchType,
                           local: bool) -> vec::IntoIter<Match> {
    let blob = &msrc[blobstart..blobend];
    let mut out = Vec::new();
    if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {
        if txt_matches(search_type, searchstr, blob) {
            // parse the enum
            let parsed_enum = ast::parse_enum(blob.to_owned());

            for (name, offset) in parsed_enum.values.into_iter() {
                if (&name).starts_with(searchstr) {
                    let m = Match {
                        matchstr: name.clone(),
                        filepath: filepath.to_path_buf(),
                        point: blobstart + offset,
                        local: local,
                        mtype: EnumVariant,
                        contextstr: first_line(&blob[offset..]),
                        generic_args: Vec::new(),
                        generic_types: Vec::new()
                    };
                    out.push(m);
                }
            }
        }
    }
    out.into_iter()
}

pub fn match_enum(msrc: &str, blobstart: usize, blobend: usize,
                  searchstr: &str, filepath: &Path, search_type: SearchType,
                  local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "enum", searchstr, search_type, local) {
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };
        debug!("found!! an enum |{}|", l);
        // Parse generics
        let end = blob.find('{').or(blob.find(';'))
            .expect("Can't find end of enum header");
        let generics = ast::parse_generics(format!("{}{{}}", &blob[..end]));

        Some(Match {
            matchstr: l.to_owned(),
            filepath: filepath.to_path_buf(),
            point: blobstart + start,
            local: local,
            mtype: Enum,
            contextstr: first_line(blob),
            generic_args: generics.generic_args,
            generic_types: Vec::new()
        })
    } else {
        None
    }
}

// HACK: recursion protection. With 'use glob' statements it's easy to
// get into a recursive loop and exchaust the stack. Currently we
// avoid this by not following a glob if we're already searching
// through one.
thread_local!(static ALREADY_GLOBBING: Cell<Option<bool>> = Cell::new(None));

pub fn match_use(msrc: &str, blobstart: usize, blobend: usize,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool, session: SessionRef) -> Vec<Match> {
    let mut out = Vec::new();
    let blob = &msrc[blobstart..blobend];

    if find_keyword(blob, "use", "", StartsWith, local).is_none() { return out; }

    if blob.contains("*") {
        // uh oh! a glob. Need to search the module for the searchstr
        let use_item = ast::parse_use(blob.to_owned());
        debug!("found a glob!! {:?}", use_item);

        if use_item.is_glob {
            let basepath = use_item.paths.into_iter().nth(0).unwrap();
            let mut follow_glob = true;
            {
                // don't follow glob if we are already following one otherwise
                // otherwise we get a recursive mess
                follow_glob &= ALREADY_GLOBBING.with(|c| { c.get().is_none() });

                // don't follow the glob if the path base is the searchstr
                follow_glob &= !(basepath.segments[0].name == searchstr ||
                    (basepath.segments[0].name == "self" && basepath.segments[1].name == searchstr));
            }

            if follow_glob {
                ALREADY_GLOBBING.with(|c| { c.set(Some(true)) });

                let seg = PathSegment{ name: searchstr.to_owned(), types: Vec::new() };
                let mut path = basepath.clone();
                path.segments.push(seg);
                debug!("found a glob: now searching for {:?}", path);
                let iter_path = resolve_path(&path, filepath, blobstart, search_type, BothNamespaces, session);
                if let StartsWith = search_type {
                    return iter_path.collect();
                }
                for m in iter_path {
                    out.push(m);
                    break;
                }
                ALREADY_GLOBBING.with(|c| { c.set(None) });
            } else {
                debug!("not following glob");
            }
        }
    } else if txt_matches(search_type, searchstr, blob) {
        debug!("found use: {} in |{}|", searchstr, blob);
        let use_item = ast::parse_use(blob.to_owned());

        let ident = use_item.ident.unwrap_or("".into());
        for path in use_item.paths.into_iter() {
            let len = path.segments.len();

            if symbol_matches(search_type, searchstr, &ident) { // i.e. 'use foo::bar as searchstr'
                if len == 1 && path.segments[0].name == searchstr {
                    // is an exact match of a single use stmt.
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else {
                    for m in resolve_path(&path, filepath, blobstart, ExactMatch, BothNamespaces, session) {
                        out.push(m);
                        if let ExactMatch = search_type  {
                            return out;
                        } else {
                            break;
                        }
                    }
                }
            } else if ident == "" {   // i.e. no 'as'. e.g. 'use foo::{bar, baz}'
                // if searching for a symbol and the last path segment
                // matches the symbol then find the fqn
                if len == 1 && path.segments[0].name == searchstr {
                    // is an exact match of a single use stmt.
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else if symbol_matches(search_type, searchstr,
                                         &path.segments.last().unwrap().name) {
                    // last path segment matches the path. find it!
                    for m in resolve_path(&path, filepath, blobstart, ExactMatch, BothNamespaces, session) {
                        out.push(m);
                        if let ExactMatch = search_type  {
                            return out;
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }
    out
}

pub fn match_fn(msrc: &str, blobstart: usize, blobend: usize,
                searchstr: &str, filepath: &Path, search_type: SearchType,
                local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    let keyword = if is_const_fn(msrc, blobstart, blobend) { "const fn" } else { "fn" };
    if let Some(start) = find_keyword(blob, keyword, searchstr, search_type, local) {
        if !typeinf::first_param_is_self(blob) {
            debug!("found a fn starting {}", searchstr);
            let l = match search_type {
                ExactMatch => searchstr, // already checked in find_keyword
                StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
            };
            debug!("found a fn {}", l);
            Some(Match {
                matchstr: l.to_owned(),
                filepath: filepath.to_path_buf(),
                point: blobstart + start,
                local: local,
                mtype: Function,
                contextstr: first_line(blob),
                generic_args: Vec::new(),
                generic_types: Vec::new()
            })
        } else {
            None
        }
    } else {
        None
    }
}
