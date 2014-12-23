use std::cell::{Cell};
use std::{iter,option};
use collections::vec;
use racer::nameres::resolve_path;
use racer::scopes;
use racer::util::{symbol_matches, txt_matches, find_ident_end};
use racer::nameres::{get_module_file, get_crate_file};
use racer::typeinf;
use racer;
use racer::{ast};
use racer::{SearchType, Match, PathSegment};
use racer::SearchType::{StartsWith, ExactMatch};
use racer::MatchType::{Let, Module, Function, Struct, Type, Trait, Enum, EnumVariant, Const, Static, IfLet};
use racer::Namespace::BothNamespaces;
use racer::util;


// Should I return a boxed trait object to make this signature nicer?
pub fn match_types(src: &str, blobstart: uint, blobend: uint, 
                   searchstr: &str, filepath: &Path, 
                   search_type: SearchType, 
                   local: bool) -> iter::Chain<iter::Chain<iter::Chain<iter::Chain<iter::Chain<iter::Chain<option::IntoIter<Match>,option::IntoIter<Match>>,option::IntoIter<Match>>,option::IntoIter<Match>>,option::IntoIter<Match>>,option::IntoIter<Match>>,vec::IntoIter<Match>> {
    
    let it = match_extern_crate(src, blobstart, blobend, searchstr, filepath, search_type).into_iter();
    
    let it = it.chain(match_mod(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    
    let it = it.chain(match_struct(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    
    let it = it.chain(match_type(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    
    let it = it.chain(match_trait(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    
    let it = it.chain(match_enum(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
        
    let it = it.chain(match_use(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());

    return it;
}

pub fn match_values(src: &str, blobstart: uint, blobend: uint, 
                  searchstr: &str, filepath: &Path, search_type: SearchType, 
                  local: bool) -> iter::Chain<iter::Chain<option::IntoIter<racer::Match>, option::IntoIter<racer::Match>>, option::IntoIter<racer::Match>> {
    let it = match_const(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter();
    let it = it.chain(match_static(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    let it = it.chain(match_fn(src, blobstart, blobend, searchstr, filepath, search_type, local).into_iter());
    return it;
}

pub fn match_const(msrc: &str, blobstart: uint, blobend: uint, 
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                  local: bool)  -> Option<Match> {
    // ast currently doesn't contain the ident coords, so match them with a hacky
    // string search
    let mut res = None;
    let blob = msrc.slice(blobstart, blobend);
    let start = if local && blob.starts_with("const ") { 
        6u 
    } else if blob.starts_with("pub const ") { 
        10u
    } else {
        0u
    };
    if start != 0 {
        blob.find_str(":").map(|end|{
            let s = blob.slice(start, end);
            if symbol_matches(search_type, searchstr, s) {
                res = Some(Match { matchstr: s.to_string(),
                                   filepath: filepath.clone(),
                                   point: blobstart + start,
                                   local: local,
                                   mtype: Static,
                                   contextstr: first_line(blob),
                                   generic_args: Vec::new(), 
                                   generic_types: Vec::new()
                });
            }
        });
    }
    return res;
}

pub fn match_static(msrc: &str, blobstart: uint, blobend: uint, 
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                  local: bool)  -> Option<Match> {
    // ast currently doesn't contain the ident coords, so match them with a hacky
    // string search
    let mut res = None;
    let blob = msrc.slice(blobstart, blobend);
    let start = if local && blob.starts_with("static ") { 
        7u 
    } else if blob.starts_with("pub static ") { 
        11u
    } else {
        0u
    };
    if start != 0 {
        blob.find_str(":").map(|end|{
            let s = blob.slice(start, end);
            if symbol_matches(search_type, searchstr, s) {
                res = Some(Match { matchstr: s.to_string(),
                                   filepath: filepath.clone(),
                                   point: blobstart + start,
                                   local: local,
                                   mtype: Const,
                                   contextstr: first_line(blob),
                                   generic_args: Vec::new(), 
                                   generic_types: Vec::new()
                });
            }
        });
    }
    return res;
}

pub fn match_if_let(msrc: &str, blobstart: uint, blobend: uint, 
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool)  -> Vec<Match> {
    let mut out = Vec::new();
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with("if let ") && txt_matches(search_type, searchstr, blob) {
        let coords = ast::parse_let(String::from_str(blob));
        for &(start,end) in coords.iter() {
            let s = blob.slice(start,end);
            if symbol_matches(search_type, searchstr, s) {
                out.push(Match { matchstr: s.to_string(),
                                   filepath: filepath.clone(),
                                   point: blobstart + start,
                                   local: local,
                                   mtype: IfLet,
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
    return out;
}

pub fn match_let(msrc: &str, blobstart: uint, blobend: uint, 
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                 local: bool)  -> Vec<Match> {
    let mut out = Vec::new();
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with("let ") && txt_matches(search_type, searchstr, blob) {
        let coords = ast::parse_let(String::from_str(blob));
        for &(start,end) in coords.iter() {
            let s = blob.slice(start,end);
            if symbol_matches(search_type, searchstr, s) {
                out.push(Match { matchstr: s.to_string(),
                                   filepath: filepath.clone(),
                                   point: blobstart + start,
                                   local: local,
                                   mtype: Let,
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
    return out;
}


pub fn first_line(blob: &str) -> String {
    return blob.slice_to(blob.find_str("\n").unwrap_or(blob.len())).to_string();
}

pub fn match_extern_crate(msrc: &str, blobstart: uint, blobend: uint, 
         searchstr: &str, filepath: &Path, search_type: SearchType) -> Option<Match> {
    let mut res = None;
    let blob = msrc.slice(blobstart, blobend);

    if blob.starts_with(format!("extern crate {}", searchstr).as_slice()) ||
         (blob.starts_with("extern crate") && 
          txt_matches(search_type, format!("as {}",searchstr).as_slice(), blob)) {

        debug!("found an extern crate: |{}|",blob);

        let view_item;
        if blob.contains("\"") {
            // Annoyingly the extern crate can use a string literal for the
            // real crate name (e.g. extern crate collections_core = "collections")
            // so we need to get the source text without scrubbed strings 

            let rawsrc = racer::load_file(filepath);
            let rawblob = rawsrc.slice(blobstart,blobend);
            debug!("found an extern crate (unscrubbed): |{}|", rawblob);
            
            view_item = ast::parse_view_item(String::from_str(rawblob));
        } else {
            view_item = ast::parse_view_item(String::from_str(blob));
        }

        if view_item.paths.is_empty() {
            // reference to a crate.

            view_item.ident.clone().map(|ident|{
                debug!("EXTERN CRATE {}",ident.as_slice());
                get_crate_file(ident.as_slice()).map(|cratepath|{
                    res = Some(Match {matchstr: ident.to_string(),
                                      filepath: cratepath.clone(), 
                                      point: 0,
                                      local: false,
                                      mtype: Module,
                                      contextstr: cratepath.as_str().unwrap().to_string(),
                                      generic_args: Vec::new(), 
                                      generic_types: Vec::new()
                    });
                });                
            });
        } else {

            view_item.ident.clone().map(|ident|{
                if symbol_matches(search_type, searchstr, ident.as_slice()) {
                    // e.g. extern core_collections = "collections";
                    let ref real_str = view_item.paths[0].segments[0].name;
                    get_crate_file(real_str.as_slice()).map(|modpath|{
                        res = Some(Match {matchstr: ident.to_string(),
                                       filepath: modpath.clone(), 
                                       point: 0,
                                       local: false,
                                       mtype: Module,
                                       contextstr: modpath.as_str().unwrap().to_string(),
                                       generic_args: Vec::new(), 
                                       generic_types: Vec::new()
                        });
                    });

                }
            });
        }
    }
    return res;
}

pub fn match_mod(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {


    let mut res = None;
    let blob = msrc.slice(blobstart, blobend);


    let exact_match = match search_type {
        ExactMatch => true,
        StartsWith => false
    };
    
    if local && blob.starts_with(format!("mod {}", searchstr).as_slice()) {
        debug!("found a module: |{}|",blob);
        // TODO: parse this properly
        let end = util::find_ident_end(blob, 4);
        let l = blob.slice(4, end);

        if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
            if blob.find_str("{").is_some() {
                debug!("found an inline module!");

                res = Some(Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: blobstart + 4, 
                               local: false,
                               mtype: Module,
                               contextstr: filepath.as_str().unwrap().to_string(),
                                  generic_args: Vec::new(), generic_types: Vec::new()
                });
                
            } else {

                // get internal module nesting  
                // e.g. is this in an inline submodule?  mod foo{ mod bar; } 
                // because if it is then we need to search further down the 
                // directory hierarchy
                let internalpath = scopes::get_local_module_path(msrc, 
                                                                 blobstart);
                let searchdir = filepath.dir_path().join_many(internalpath.as_slice());
                get_module_file(l, &searchdir).map(|modpath|{
                    res = Some(Match {matchstr: l.to_string(),
                                   filepath: modpath.clone(), 
                                   point: 0,
                                   local: false,
                                   mtype: Module,
                                   contextstr: modpath.as_str().unwrap().to_string(),
                                      generic_args: Vec::new(), generic_types: Vec::new()
                    });
                });
            }
        }
    }

    if blob.starts_with(format!("pub mod {}", searchstr).as_slice()) {
        // TODO: parse this properly
        let end = util::find_ident_end(blob, 8);
        let l = blob.slice(8, end);

        if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
            if blob.find_str("{").is_some() {
                debug!("found an inline module!");

                res = Some(Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: blobstart + 8,
                               local: false,
                               mtype: Module,
                               contextstr: blob.slice_to(blob.find_str("{").unwrap()).to_string(),
                                  generic_args: Vec::new(), generic_types: Vec::new()
                });
                
            } else {
                debug!("found a pub module: |{}|",blob);

                // get internal module nesting  
                // e.g. is this in an inline submodule?  mod foo{ mod bar; } 
                // because if it is then we need to search further down the 
                // directory hierarchy
                let internalpath = scopes::get_local_module_path(msrc, 
                                                                 blobstart);
                let searchdir = filepath.dir_path().join_many(internalpath.as_slice());
                get_module_file(l, &searchdir).map(|modpath|{
                    res = Some(Match {matchstr: l.to_string(),
                                      filepath: modpath.clone(), 
                                      point: 0,
                                      local: false,
                                      mtype: Module,
                                      contextstr: modpath.as_str().unwrap().to_string(),
                                      generic_args: Vec::new(), 
                                      generic_types: Vec::new()
                    });
                });
            }
        }
    }
    return res;
}

pub fn match_struct(msrc: &str, blobstart: uint, blobend: uint, 
                searchstr: &str, filepath: &Path, search_type: SearchType,
                local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if (local && txt_matches(search_type, format!("struct {}", searchstr).as_slice(), blob)) || 
        txt_matches(search_type, format!("pub struct {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("struct {}", searchstr).as_slice()).unwrap() + 7;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found a struct |{}|", l);

        // Parse generics
        let end = blob.find_str("{").or(blob.find_str(";"))
            .expect("Can't find end of struct header");
        // structs with no values need to end in ';', not '{}'
        let mut s = blob.slice_to(end).to_string();
        s.push_str(";");
        let generics = ast::parse_generics(s);

        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Struct,
                           contextstr: first_line(blob),
                           generic_args: generics.generic_args,
                           generic_types: Vec::new()
        });
    }
    return None;
}

pub fn match_type(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if local && txt_matches(search_type, format!("type {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("type {}", searchstr).as_slice()).unwrap() + 5;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found!! a type {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Type,
                           contextstr: first_line(blob),
                           generic_args: Vec::new(), generic_types: Vec::new()
        });
    }
    
    if txt_matches(search_type, format!("pub type {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("pub type {}", searchstr).as_slice()).unwrap() + 9;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found!! a pub type {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Type,
                           contextstr: first_line(blob),
                           generic_args: Vec::new(), generic_types: Vec::new()
        });
    }
    return None;
}

pub fn match_trait(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if local && txt_matches(search_type, format!("trait {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("trait {}", searchstr).as_slice()).unwrap() + 6;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found!! a trait {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Trait,
                           contextstr: first_line(blob),
                           generic_args: Vec::new(), generic_types: Vec::new()
        });
    }
     
    if txt_matches(search_type, format!("pub trait {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("pub trait {}", searchstr).as_slice()).unwrap() + 10;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found!! a pub trait {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Trait,
                           contextstr: first_line(blob),
                           generic_args: Vec::new(), generic_types: Vec::new()
        });
    }
    return None;
}

pub fn match_enum_variants(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> vec::IntoIter<Match> {
    let blob = msrc.slice(blobstart, blobend);
    let mut out = Vec::new();
    if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {
        if txt_matches(search_type, searchstr, blob) {
            // parse the enum
            let parsed_enum = ast::parse_enum(String::from_str(blob));
            if parsed_enum.name.as_slice().starts_with(searchstr) {
            }

            for (name, offset) in parsed_enum.values.into_iter() {
                if name.as_slice().starts_with(searchstr) {

                    let m = Match {matchstr: name.clone(),
                                   filepath: filepath.clone(), 
                                   point: blobstart + offset,
                                   local: local,
                                   mtype: EnumVariant,
                                   contextstr: first_line(blob.slice_from(offset)),
                                   generic_args: Vec::new(), 
                                   generic_types: Vec::new()
                    };
                    out.push(m);
                }
            }                
        }
    }
    return out.into_iter();
}

pub fn match_enum(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if (local && txt_matches(search_type, format!("enum {}", searchstr).as_slice(), blob)) || 
        txt_matches(search_type, format!("pub enum {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("enum {}", searchstr).as_slice()).unwrap() + 5;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found!! an enum |{}|", l);
        // Parse generics
        let end = blob.find_str("{").or(blob.find_str(";"))
            .expect("Can't find end of enum header");
        let mut s = blob.slice_to(end).to_string();
        s.push_str("{}");
        let generics = ast::parse_generics(s);

        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Enum,
                           contextstr: first_line(blob),
                           generic_args: generics.generic_args,
                           generic_types: Vec::new()
        });
    }
    return None;
}

// HACK: recursion protection. With 'use glob' statements it's easy to
// get into a recursive loop and exchaust the stack. Currently we
// avoid this by not following a glob if we're already searching
// through one.
thread_local!(static ALREADY_GLOBBING: Cell<Option<bool>> = Cell::new(None));

pub fn match_use(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Vec<Match> {

    let mut out = Vec::new();

    let blob = msrc.slice(blobstart, blobend);

    if ((local && blob.starts_with("use ")) || blob.starts_with("pub use ")) && blob.contains("*") {
        // uh oh! a glob. Need to search the module for the searchstr
        let view_item = ast::parse_view_item(String::from_str(blob));
        debug!("found a glob!! {}", view_item);

        if view_item.is_glob {

            let basepath = view_item.paths.into_iter().nth(0).unwrap();
            let mut follow_glob = true;
            {
                // don't follow glob if we are already following one otherwise
                // otherwise we get a recursive mess
                follow_glob &= ALREADY_GLOBBING.with(|c| { c.get().is_none() });

                // don't follow the glob if the path base is the searchstr
                follow_glob &= !(&*basepath.segments[0].name == searchstr || 
                    (&*basepath.segments[0].name == "self" && &*basepath.segments[1].name == searchstr));
            }

            if follow_glob {
                ALREADY_GLOBBING.with(|c| { c.set(Some(true)) });

                let seg = PathSegment{ name: searchstr.to_string(), types: Vec::new() };
                let mut path = basepath.clone();
                path.segments.push(seg);
                debug!("found a glob: now searching for {}", path);
                // TODO: pretty sure this isn't correct/complete, only works because
                //  we recurse backwards up modules when searching
                let path = hack_remove_self_and_super_in_modpaths(path);

                for m in resolve_path(&path, filepath, 0, search_type, BothNamespaces) {
                    out.push(m);
                    if let ExactMatch = search_type {
                        break;
                    }
                }
                ALREADY_GLOBBING.with(|c| { c.set(None) });
            } else {
                debug!("not following glob");
            }
        }
    } else if ((local && blob.starts_with("use ")) || blob.starts_with("pub use ")) && txt_matches(search_type, searchstr, blob) {     
        if searchstr.len() != 0 && blob.match_indices(searchstr).count() == 1 {
            if blob.find_str((searchstr.to_string() + "::").as_slice()).is_some() {
                // can't possibly match, fail fast!
                return out;
            }
        }

        debug!("found use: {} in |{}|", searchstr, blob);
        let view_item = ast::parse_view_item(String::from_str(blob));

        let ident = view_item.ident.unwrap_or("".to_string());
        for mut path in view_item.paths.into_iter() {
            let len = path.segments.len();

            // TODO: simplify this:
            if symbol_matches(search_type, searchstr, &*ident) { // i.e. 'use foo::bar as searchstr'
                if len == 1 && path.segments[0].name.as_slice() == searchstr {
                    // is an exact match of a single use stmt. 
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else {
                    let path = hack_remove_self_and_super_in_modpaths(path);
                    for m in resolve_path(&path, filepath, 0, ExactMatch, BothNamespaces) {
                        out.push(m);
                        if let ExactMatch = search_type  {
                            return out;
                        } else {
                            break;
                        }
                    }
                }

            } else if &*ident == "" {
                // if searching for a symbol and the last bit matches the symbol
                // then find the fqn
                if len == 1 && path.segments[0].name.as_slice() == searchstr {
                    // is an exact match of a single use stmt. 
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else if path.segments[len-1].name.as_slice().starts_with(searchstr) {
                    // TODO: pretty sure this isn't correct/complete, only works because
                    //  we recurse backwards up modules when searching
                    let path = hack_remove_self_and_super_in_modpaths(path);

                    for m in resolve_path(&path, filepath, 0, ExactMatch, BothNamespaces) {
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
    return out;
}

fn hack_remove_self_and_super_in_modpaths(mut path: racer::Path) -> racer::Path {
    if path.segments[0].name.as_slice() == "self" {
        path.segments.remove(0);
    }
    if path.segments[0].name.as_slice() == "super" {
        path.segments.remove(0);
    }
    return path;
} 

pub fn match_fn(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with("pub fn") && txt_matches(search_type, format!("pub fn {}", searchstr).as_slice(), blob) && !typeinf::first_param_is_self(blob) {
        debug!("found a pub fn starting {}",searchstr);
        // TODO: parse this properly
        let start = blob.find_str(format!("pub fn {}", searchstr).as_slice()).unwrap() + 7;
        let end = util::find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("found a pub fn {}",l);
        return Some(Match {matchstr: l.to_string(),
                       filepath: filepath.clone(), 
                       point: blobstart + start,
                       local: local,
                       mtype: Function,
                       contextstr: first_line(blob),
                           generic_args: Vec::new(), 
                           generic_types: Vec::new()
        });
    } else if local && blob.starts_with("fn") && txt_matches(search_type, format!("fn {}",searchstr).as_slice(), blob) && !typeinf::first_param_is_self(blob) {
        debug!("found a fn starting {}",searchstr);
        // TODO: parse this properly
        let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        return Some(Match {matchstr: l.to_string(),
                       filepath: filepath.clone(), 
                       point: blobstart + start,
                       local: local,
                       mtype: Function,
                       contextstr: first_line(blob),
                           generic_args: Vec::new(), 
                           generic_types: Vec::new()
        });
    }
    return None;
}
