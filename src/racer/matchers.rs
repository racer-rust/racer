use {scopes, typeinf, ast};
use core::{Match, PathSegment, Src, Session, Coordinate, SessionExt, Point};
use util::{StackLinkedListNode, symbol_matches, txt_matches, find_ident_end, is_ident_char, char_at};
use nameres::{get_module_file, get_crate_file, resolve_path};
use core::SearchType::{self, StartsWith, ExactMatch};
use core::MatchType::{self, Let, Module, Function, Struct, Type, Trait, Enum, EnumVariant,
                      Const, Static, IfLet, WhileLet, For, Macro};
use core::Namespace;
use std::path::Path;
use std::{iter, option, str, vec};

/// The location of an import (`use` item) currently being resolved.
#[derive(PartialEq, Eq)]
pub struct PendingImport<'fp> {
    filepath: &'fp Path,
    blobstart: Point,
    blobend: Point,
}

/// A stack of imports (`use` items) currently being resolved.
pub type PendingImports<'stack, 'fp> = StackLinkedListNode<'stack, PendingImport<'fp>>;

pub type MIter = option::IntoIter<Match>;
pub type MChain<T> = iter::Chain<T, MIter>;

// TODO change return type to `impl Iterator<Item = Match>`
pub fn match_types(src: Src, blobstart: Point, blobend: Point,
                   searchstr: &str, filepath: &Path,
                   search_type: SearchType,
                   local: bool, session: &Session,
                   pending_imports: &PendingImports) -> iter::Chain<MChain<MChain<MChain<MChain<MChain<MIter>>>>>, vec::IntoIter<Match>> {
    let it = match_extern_crate(&src, blobstart, blobend, searchstr, filepath, search_type, session).into_iter();
    let it = it.chain(match_mod(src, blobstart, blobend, searchstr, filepath, search_type, local, session).into_iter());
    let it = it.chain(match_struct(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_type(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_trait(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_enum(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    it.chain(match_use(&src, blobstart, blobend, searchstr, filepath, search_type, local, session, pending_imports).into_iter())
}

pub fn match_values(src: Src, blobstart: Point, blobend: Point,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> MChain<MChain<MChain<MIter>>> {
    let it = match_const(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter();
    let it = it.chain(match_static(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_fn(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    it.chain(match_macro(&src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter())
}

fn find_keyword(src: &str, pattern: &str, search: &str, search_type: SearchType, local: bool)
-> Option<Point> {
    // search for "^(pub\s+)?(unsafe\s+)?pattern\s+search"

    // if not local must start with pub
    if !local && !src.starts_with("pub") { return None; }

    let mut start = 0usize;

    // optional (pub\s+)?(unsafe\s+)?
    for pat in ["pub", "unsafe"].into_iter() {
        if src[start..].starts_with(pat) {
            // Rust added support for `pub(in codegen)`; we need to consume the visibility 
            // specifier for the rest of the code to keep working.
            let allow_scope = pat == &"pub";
            let mut levels = 0;

            // remove whitespaces ... must have one at least AFTER the visibility restriction
            start += pat.len();
            let oldstart = start;
            for &b in src[start..].as_bytes() {
                match b {
                    b'(' if allow_scope => {
                        levels += 1;
                        start += 1;
                    }
                    b')' if levels >= 1 => {
                        levels -= 1;
                        start += 1;
                    }
                    _ if levels >= 1 => { 
                        start += 1;
                    }
                    b' ' | b'\r' | b'\n' | b'\t' => start += 1,
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

fn is_const_fn(src: &str, blobstart: Point, blobend: Point) -> bool {
    src[blobstart..blobend].contains("const fn")
}

fn match_pattern_start(src: &str, blobstart: Point, blobend: Point,
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
                coords: None,
                local: local,
                mtype: mtype,
                contextstr: first_line(blob),
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: String::new(),
            })
        }
    }
    None
}

pub fn match_const(msrc: &str, blobstart: Point, blobend: Point,
                   searchstr: &str, filepath: &Path, search_type: SearchType,
                   local: bool) -> Option<Match> {
    if is_const_fn(msrc, blobstart, blobend) {
        return None;
    }
    match_pattern_start(msrc, blobstart, blobend, searchstr, filepath,
                        search_type, local, "const", Const)
}

pub fn match_static(msrc: &str, blobstart: Point, blobend: Point,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> Option<Match> {
    match_pattern_start(msrc, blobstart, blobend, searchstr, filepath,
                        search_type, local, "static", Static)
}

fn match_pattern_let(msrc: &str, blobstart: Point, blobend: Point,
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
                                   coords: None,
                                   local: local,
                                   mtype: mtype,
                                   contextstr: first_line(blob),
                                   generic_args: Vec::new(),
                                   generic_types: Vec::new(),
                                   docs: String::new(),
                         });
                if let ExactMatch = search_type {
                    break;
                }
            }
        }
    }
    out
}

pub fn match_if_let(msrc: &str, blobstart: Point, blobend: Point,
                    searchstr: &str, filepath: &Path, search_type: SearchType,
                    local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "if let ", IfLet)
}

pub fn match_while_let(msrc: &str, blobstart: Point, blobend: Point,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "while let ", WhileLet)
}

pub fn match_let(msrc: &str, blobstart: Point, blobend: Point,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool) -> Vec<Match> {
    match_pattern_let(msrc, blobstart, blobend, searchstr, filepath,
                      search_type, local, "let ", Let)
}

pub fn match_for(msrc: &str, blobstart: Point, blobend: Point,
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
                             coords: None,
                             local: local,
                             mtype: For,
                             contextstr: first_line(blob),
                             generic_args: Vec::new(),
                             generic_types: Vec::new(),
                             docs: String::new(),
            });
        }
    }
    out
}

pub fn first_line(blob: &str) -> String {
    blob[..blob.find('\n').unwrap_or(blob.len())].to_owned()
}

/// Get the match's cleaned up context string
///
/// Strip all whitespace, including newlines in order to have a single line
/// context string.
pub fn get_context(blob: &str, context_end: &str) -> String {
    blob[..blob.find(context_end).unwrap_or(blob.len())]
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn match_extern_crate(msrc: &str, blobstart: Point, blobend: Point,
                          searchstr: &str, filepath: &Path, search_type: SearchType,
                          session: &Session) -> Option<Match> {
    let mut res = None;
    let blob = &msrc[blobstart..blobend];

    if txt_matches(search_type, &format!("extern crate {}", searchstr), blob) &&
        !(txt_matches(search_type, &format!("extern crate {} as", searchstr), blob))
        || (blob.starts_with("extern crate") &&
            txt_matches(search_type, &format!("as {}", searchstr), blob)) {

        debug!("found an extern crate: |{}|", blob);

        let extern_crate = if blob.contains('\"') {
            // Annoyingly the extern crate can use a string literal for the
            // real crate name (e.g. extern crate collections_core = "collections")
            // so we need to get the source text without scrubbed strings

            let rawsrc = session.load_file(filepath);
            let rawblob = &rawsrc[blobstart..blobend];
            debug!("found an extern crate (unscrubbed): |{}|", rawblob);
            ast::parse_extern_crate(rawblob.to_owned())
        } else {
            ast::parse_extern_crate(blob.to_owned())
        };

        if let Some(ref name) = extern_crate.name {
            debug!("extern crate {}", name);

            let realname = extern_crate.realname.as_ref().unwrap_or(name);
            get_crate_file(realname, filepath, session).map(|cratepath| {
                let crate_src = session.load_file(&cratepath);
                res = Some(Match { matchstr: name.clone(),
                                  filepath: cratepath.to_path_buf(),
                                  point: 0,
                                  coords: Some(Coordinate { line: 1, column: 1 }),
                                  local: false,
                                  mtype: Module,
                                  contextstr: cratepath.to_str().unwrap().to_owned(),
                                  generic_args: Vec::new(),
                                  generic_types: Vec::new(),
                                  docs: find_mod_doc(&crate_src, 0),
                });
            });
        }
    }
    res
}

pub fn match_mod(msrc: Src, blobstart: Point, blobend: Point,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool, session: &Session) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    if let Some(start) = find_keyword(blob, "mod", searchstr, search_type, local) {
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };

        if blob.find('{').is_some() {
            debug!("found a module inline: |{}|", blob);

            return Some(Match {
                matchstr: l.to_owned(),
                filepath: filepath.to_path_buf(),
                point: blobstart + start,
                coords: None,
                local: false,
                mtype: Module,
                contextstr: filepath.to_str().unwrap().to_owned(),
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: String::new(),
            })
        } else {
            debug!("found a module declaration: |{}|", blob);

            // get module from path attribute
            if let Some(modpath) = scopes::get_module_file_from_path(msrc, blobstart,filepath.parent().unwrap()) {
                let msrc = session.load_file(&modpath);

                return Some(Match {
                    matchstr: l.to_owned(),
                    filepath: modpath.to_path_buf(),
                    point: 0,
                    coords: Some(Coordinate { line: 1, column: 1 }),
                    local: false,
                    mtype: Module,
                    contextstr: modpath.to_str().unwrap().to_owned(),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: find_mod_doc(&msrc, 0),
                })
            }
            // get internal module nesting
            // e.g. is this in an inline submodule?  mod foo{ mod bar; }
            // because if it is then we need to search further down the
            // directory hierarchy - e.g. <cwd>/foo/bar.rs
            let internalpath = scopes::get_local_module_path(msrc, blobstart);
            let mut searchdir = filepath.parent().unwrap().to_path_buf();
            for s in internalpath {
                searchdir.push(&s);
            }
            if let Some(modpath) = get_module_file(l, &searchdir, session) {
                let msrc = session.load_file(&modpath);
                let context = modpath.to_str().unwrap().to_owned();
                return Some(Match {
                    matchstr: l.to_owned(),
                    filepath: modpath,
                    point: 0,
                    coords: Some(Coordinate { line: 1, column: 1 }),
                    local: false,
                    mtype: Module,
                    contextstr: context,
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: find_mod_doc(&msrc, 0),
                })
            }
        }
    }
    None
}

pub fn match_struct(msrc: &str, blobstart: Point, blobend: Point,
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
            coords: None,
            local: local,
            mtype: Struct,
            contextstr: get_context(blob, "{"),
            generic_args: generics.generic_args.into_iter().map(|arg| {arg.name}).collect(),
            generic_types: Vec::new(),
            docs: find_doc(msrc, blobstart + start),
        })
    } else {
        None
    }
}

pub fn match_type(msrc: &str, blobstart: Point, blobend: Point,
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
            coords: None,
            local: local,
            mtype: Type,
            contextstr: first_line(blob),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: find_doc(msrc, blobstart + start),
        })
    } else {
        None
    }
}

pub fn match_trait(msrc: &str, blobstart: Point, blobend: Point,
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
            coords: None,
            local: local,
            mtype: Trait,
            contextstr: get_context(blob, "{"),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: find_doc(msrc, blobstart + start),
        })
    } else {
        None
    }
}

pub fn match_enum_variants(msrc: &str, blobstart: Point, blobend: Point,
                           searchstr: &str, filepath: &Path, search_type: SearchType,
                           local: bool) -> vec::IntoIter<Match> {
    let blob = &msrc[blobstart..blobend];
    let mut out = Vec::new();
    if (blob.starts_with("pub enum") || (local && blob.starts_with("enum"))) &&
       txt_matches(search_type, searchstr, blob) {
        // parse the enum
        let parsed_enum = ast::parse_enum(blob.to_owned());

        for (name, offset) in parsed_enum.values.into_iter() {
            if name.starts_with(searchstr) {
                let m = Match {
                    matchstr: name,
                    filepath: filepath.to_path_buf(),
                    point: blobstart + offset,
                    coords: None,
                    local: local,
                    mtype: EnumVariant,
                    contextstr: first_line(&blob[offset..]),
                    generic_args: Vec::new(),
                    generic_types: Vec::new(),
                    docs: find_doc(msrc, blobstart + offset),
                };
                out.push(m);
            }
        }
    }
    out.into_iter()
}

pub fn match_enum(msrc: &str, blobstart: Point, blobend: Point,
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
            coords: None,
            local: local,
            mtype: Enum,
            contextstr: first_line(blob),
            generic_args: generics.generic_args.into_iter().map(|arg| {arg.name}).collect(),
            generic_types: Vec::new(),
            docs: find_doc(msrc, blobstart + start),
        })
    } else {
        None
    }
}

pub fn match_use(msrc: &str, blobstart: Point, blobend: Point,
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool, session: &Session,
                 pending_imports: &PendingImports) -> Vec<Match> {
    let import = PendingImport {
        filepath: &filepath,
        blobstart: blobstart,
        blobend: blobend,
    };

    let blob = &msrc[blobstart..blobend];

    // If we're trying to resolve the same import recursively,
    // do not return any matches this time.
    if pending_imports.contains(&import) {
        debug!("import {} involved in a cycle; ignoring", blob);
        return Vec::new();
    }

    // Push this import on the stack of pending imports.
    let pending_imports = &pending_imports.push(import);

    let mut out = Vec::new();

    if find_keyword(blob, "use", "", StartsWith, local).is_none() { return out; }

    if blob.contains('*') {
        // uh oh! a glob. Need to search the module for the searchstr
        let use_item = ast::parse_use(blob.to_owned());
        debug!("found a glob!! {:?}", use_item);

        if use_item.is_glob {
            let basepath = use_item.paths.into_iter().nth(0).unwrap().path;
            let seg = PathSegment{ name: searchstr.to_owned(), types: Vec::new() };
            let mut path = basepath.clone();
            path.segments.push(seg);
            debug!("found a glob: now searching for {:?}", path);
            let iter_path = resolve_path(&path, filepath, blobstart, search_type, Namespace::Both, session, pending_imports);
            if let StartsWith = search_type {
                return iter_path.collect();
            }
            for m in iter_path {
                out.push(m);
                break;
            }
        }
    } else if txt_matches(search_type, searchstr, blob) {
        debug!("found use: {} in |{}|", searchstr, blob);
        let use_item = ast::parse_use(blob.to_owned());

        let ident = use_item.ident.unwrap_or("".into());
        for path in use_item.paths.into_iter() {
            let len = path.path.segments.len();
            if symbol_matches(search_type, searchstr, &path.ident) { // i.e. 'use foo::bar as searchstr'
                if len == 1 && path.path.segments[0].name == searchstr {
                    // is an exact match of a single use stmt.
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else {
                    for mut m in resolve_path(path.as_ref(), filepath, blobstart, ExactMatch, Namespace::Both, session, pending_imports) {
                        
                        // If the match was imported by a `use as` statement, racer
                        // should return the alias so that completions will produce
                        // valid code.
                        if m.matchstr != path.ident {
                            m.matchstr = path.ident.clone();
                        }

                        out.push(m);
                        if let ExactMatch = search_type  {
                            return out;
                        } else {
                            break;
                        }
                    }
                }
            } else if ident == "" || ident == "self" {   // i.e. no 'as'. e.g. 'use foo::{bar, baz}'
                // `use foo::bar::self;` is parsed as path = foo::bar, ident = self

                // if searching for a symbol and the last path segment
                // matches the symbol then find the fqn
                if len == 1 && path.path.segments[0].name == searchstr {
                    // is an exact match of a single use stmt.
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else {
                    let path = if &path.path.segments.last().unwrap().name == "self" {
                        // `use foo::bar::self` -> `use foo::bar`
                        let mut path = path;
                        path.path.segments.pop();
                        path
                    } else {
                        path
                    };

                    if path.path.segments.len() > 1 {
                        if symbol_matches(search_type, searchstr, &path.path.segments.last().unwrap().name) {
                            // last path segment matches the path. find it!
                            for m in resolve_path(path.as_ref(), filepath, blobstart,
                                                  ExactMatch, Namespace::Both, session, pending_imports) {
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
        }
    }
    out
}

pub fn match_fn(msrc: &str, blobstart: Point, blobend: Point,
                searchstr: &str, filepath: &Path, search_type: SearchType,
                local: bool) -> Option<Match> {

    let blob = &msrc[blobstart..blobend];
    let keyword = if is_const_fn(msrc, blobstart, blobend) { "const fn" } else { "fn" };
    if let Some(start) = find_keyword(blob, keyword, searchstr, search_type, local) {
        info!("{:?}", start);
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
                coords: None,
                local: local,
                mtype: Function,
                contextstr: get_context(blob, "{"),
                generic_args: Vec::new(),
                generic_types: Vec::new(),
                docs: find_doc(msrc, blobstart + start),
            })
        } else {
            None
        }
    } else {
        None
    }
}

pub fn match_macro(msrc: &str, blobstart: Point, blobend: Point,
                   searchstr: &str, filepath: &Path, search_type: SearchType,
                   local: bool) -> Option<Match> {
    let blob = &msrc[blobstart..blobend];
    let searchstr = searchstr.trim_right_matches('!');
    if let Some(start) = find_keyword(blob, "macro_rules!", searchstr, search_type, local) {
        debug!("found a macro starting {}", searchstr);
        let l = match search_type {
            ExactMatch => searchstr, // already checked in find_keyword
            StartsWith => &blob[start..find_ident_end(blob, start+searchstr.len())]
        };
        let l = format!("{}!", l);
        debug!("found a macro {}", l);
        Some(Match {
            matchstr: l.to_owned(),
            filepath: filepath.to_path_buf(),
            point: blobstart + start,
            coords: None,
            local: local,
            mtype: Macro,
            contextstr: first_line(blob),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: String::new(),
        })
    } else {
        None
    }
}

pub fn find_doc(msrc: &str, match_point: Point) -> String {
    let blob = &msrc[0..match_point];

    blob.lines()
        .rev()
        .skip(1) // skip the line that the match is on
        .map(|line| line.trim())
        .take_while(|line| line.starts_with("///") || line.starts_with("#[") || line.is_empty())
        .filter(|line| !(line.trim().starts_with("#[") || line.is_empty() ))  // remove the #[flags]
        .collect::<Vec<_>>()  // These are needed because
        .iter()               // you cannot `rev`an `iter` that
        .rev()                // has already been `rev`ed.
        .map(|line| if line.len() >= 4 { &line[4..] } else { "" })  // Remove "/// "
        .collect::<Vec<_>>()
        .join("\n")
}

fn find_mod_doc(msrc: &str, blobstart: Point) -> String {
    let blob = &msrc[blobstart..];
    let mut doc = String::new();

    let mut iter = blob.lines()
        .map(|line| line.trim())
        .take_while(|line| line.starts_with("//") || line.is_empty())
        // Skip over the copyright notice and empty lines until you find
        // the module's documentation (it will go until the end of the
        // file if the module doesn't have any docs).
        .filter(|line| line.starts_with("//!"))
        .peekable();

    // Use a loop to avoid unnecessary collect and String allocation
    while let Some(line) = iter.next() {
        // Remove "//! " and push to doc string to be returned
        doc.push_str(if line.len() >= 4 { &line[4..] } else { "" });
        if iter.peek() != None {
            doc.push_str("\n");
        }
    }
    doc
}
