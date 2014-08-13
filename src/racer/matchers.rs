use std::io::{File, BufferedReader};
use std::{iter,option,str};
use collections::vec;
use racer::nameres::resolve_path;
use racer::scopes;
use racer::util::{symbol_matches, txt_matches, find_ident_end, to_refs};
use racer::nameres::{get_module_file, get_crate_file};
use racer::typeinf;

use racer::{ast};
use racer::{SearchType, StartsWith, ExactMatch, Match, Let, Module, 
            Function, Struct, Type, Trait, Enum, EnumVariant, BothNamespaces};
use racer::util;


// Should I return a boxed trait object to make this signature nicer?
pub fn match_types(src: &str, blobstart: uint, blobend: uint, 
                  searchstr: &str, filepath: &Path, search_type: SearchType, 
                  local: bool) -> iter::Chain<iter::Chain<iter::Chain<iter::Chain<iter::Chain<iter::Chain<option::Item<Match>,option::Item<Match>>,option::Item<Match>>,option::Item<Match>>,option::Item<Match>>,option::Item<Match>>,vec::MoveItems<Match>> {
    
    let it = match_extern_crate(src, blobstart, blobend, searchstr, filepath, search_type).move_iter();
    
    let it = it.chain(match_mod(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
    
    let it = it.chain(match_struct(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
    
    let it = it.chain(match_type(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
    
    let it = it.chain(match_trait(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
    
    let it = it.chain(match_enum(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
        
    let it = it.chain(match_use(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());

    return it;
}

pub fn match_values(src: &str, blobstart: uint, blobend: uint, 
                  searchstr: &str, filepath: &Path, search_type: SearchType, 
                  local: bool) -> iter::Chain<iter::Chain<option::Item<Match>,option::Item<Match>>,vec::MoveItems<Match>> {
    let it = match_let(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter();
    let it = it.chain(match_fn(src, blobstart, blobend, searchstr, filepath, search_type, local).move_iter());
    let it = it.chain(match_enum_variants(src, blobstart, blobend, searchstr, filepath, search_type, local));
        
    return it;
}

pub fn match_let(msrc: &str, blobstart: uint, blobend: uint, 
                 searchstr: &str, filepath: &Path, search_type: SearchType,
                  local: bool)  -> Option<Match> {
    let mut res = None;
    let exact_match = match search_type {
        ExactMatch => true,
        StartsWith => false
    };
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with("let ") && blob.find_str(searchstr).is_some() {
        let letres = ast::parse_let(String::from_str(blob), filepath.clone(), blobstart, false);
        letres.map(|letresult| {
            
            let name = letresult.name.as_slice();
            if (exact_match && name == searchstr) || (!exact_match && name.starts_with(searchstr)) {
                res = Some(Match { matchstr: letresult.name.to_string(),
                                    filepath: filepath.clone(),
                                    point: blobstart + letresult.point,
                                    local: local,
                                    mtype: Let,
                                    contextstr: first_line(blob)
                });
            }
        });
    }
    return res;
}


pub fn first_line(blob: &str) -> String {
    return blob.slice_to(blob.find_str("\n").unwrap_or(blob.len())).to_string();
}

pub fn match_extern_crate(msrc: &str, blobstart: uint, blobend: uint, 
         searchstr: &str, filepath: &Path, search_type: SearchType) -> Option<Match> {
    let mut res = None;
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with(format!("extern crate {}", searchstr).as_slice()) {
        debug!("found an extern crate: |{}|",blob);

        let view_item;
        if blob.contains("\"") {
            // Annoyingly the extern crate can use a string literal for the
            // real crate name (e.g. extern crate collections_core = "collections")
            // so we need to get the source text without scrubbed strings 
            let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
            let rawsrc = str::from_utf8(filetxt.as_slice()).unwrap();
            let rawblob = rawsrc.slice(blobstart,blobend);
            debug!("found an extern crate (unscrubbed): |{}|", rawblob);
            
            view_item = ast::parse_view_item(String::from_str(rawblob));
        } else {
            view_item = ast::parse_view_item(String::from_str(blob));
        }

        if view_item.paths.is_empty() {
            // reference to a crate.

            view_item.ident.clone().map(|ident|{
                println!("PHIL EXTERN CRATE {}",ident.as_slice());
                get_crate_file(ident.as_slice()).map(|cratepath|{
                    res = Some(Match {matchstr: ident.to_string(),
                                      filepath: cratepath.clone(), 
                                      point: 0,
                                      local: false,
                                      mtype: Module,
                                      contextstr: cratepath.as_str().unwrap().to_string()
                    });
                });                
            });
        } else {

            view_item.ident.clone().map(|ident|{
                if symbol_matches(search_type, searchstr, ident.as_slice()) {
                    // e.g. extern core_collections = "collections";
                    let ref real_str = view_item.paths[0][0];
                    get_crate_file(real_str.as_slice()).map(|modpath|{
                        res = Some(Match {matchstr: ident.to_string(),
                                       filepath: modpath.clone(), 
                                       point: 0,
                                       local: false,
                                       mtype: Module,
                                       contextstr: modpath.as_str().unwrap().to_string()
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
                debug!("PHIL found an inline module!");

                res = Some(Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: blobstart + 4, 
                               local: false,
                               mtype: Module,
                               contextstr: filepath.as_str().unwrap().to_string()
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
                                   contextstr: modpath.as_str().unwrap().to_string()
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
                debug!("PHIL found an inline module!");

                res = Some(Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: blobstart + 8,
                               local: false,
                               mtype: Module,
                               contextstr: blob.slice_to(blob.find_str("{").unwrap()).to_string()
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
                                   contextstr: modpath.as_str().unwrap().to_string()
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
    if local && txt_matches(search_type, format!("struct {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("struct {}", searchstr).as_slice()).unwrap() + 7;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("PHIL found!! a local struct |{}|", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Struct,
                           contextstr: first_line(blob)
        });
    }

    if txt_matches(search_type, format!("pub struct {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("pub struct {}", searchstr).as_slice()).unwrap() + 11;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("PHIL found!! a pub struct |{}|", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Struct,
                           contextstr: first_line(blob)
        });
    }
    return None
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
        debug!("PHIL found!! a type {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Type,
                           contextstr: first_line(blob)
        });
    }
    
    if txt_matches(search_type, format!("pub type {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("pub type {}", searchstr).as_slice()).unwrap() + 9;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("PHIL found!! a pub type {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Type,
                           contextstr: first_line(blob)
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
        debug!("PHIL found!! a trait {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Trait,
                           contextstr: first_line(blob)
        });
    }
     
    if txt_matches(search_type, format!("pub trait {}", searchstr).as_slice(), blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("pub trait {}", searchstr).as_slice()).unwrap() + 10;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("PHIL found!! a pub trait {}", l);
        return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Trait,
                           contextstr: first_line(blob)
        });
    }
    return None;
}

pub fn match_enum_variants(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> vec::MoveItems<Match> {
    let blob = msrc.slice(blobstart, blobend);
    let mut out = Vec::new();
    if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {
        if txt_matches(search_type, searchstr, blob) {
            // parse the enum
            let parsedEnum = ast::parse_enum(String::from_str(blob));
            if parsedEnum.name.as_slice().starts_with(searchstr) {
            }

            for (name, offset) in parsedEnum.values.move_iter() {
                if name.as_slice().starts_with(searchstr) {

                    let m = Match {matchstr: name.clone(),
                                   filepath: filepath.clone(), 
                                   point: blobstart + offset,
                                   local: local,
                                   mtype: EnumVariant,
                                   contextstr: first_line(blob.slice_from(offset))
                    };
                    out.push(m);
                }
            }                
        }
    }
    return out.move_iter();
}

pub fn match_enum(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    let exact_match = match search_type {
        ExactMatch => true,
        StartsWith => false
    };
    if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {

        if blob.starts_with(format!("pub enum {}", searchstr).as_slice()) {
            // TODO: parse this properly
            let start = blob.find_str(format!("pub enum {}", searchstr).as_slice()).unwrap() + 9;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            if !exact_match || l == searchstr {
                debug!("PHIL found!! a pub enum {}", l);
                return Some(Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: blobstart + start,
                               local: local,
                               mtype: Enum,
                               contextstr: first_line(blob)
                });
            }
        } else if blob.starts_with(format!("enum {}", searchstr).as_slice()) {
            // TODO: parse this properly
            let start = blob.find_str(format!("enum {}", searchstr).as_slice()).unwrap() + 5;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a local enum {}", l);
            return Some(Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: blobstart + start,
                           local: local,
                           mtype: Enum,
                           contextstr: first_line(blob)
            });
        }
    }
    return None;
}

pub fn match_use(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Vec<Match> {

    let mut out = Vec::new();

    if searchstr.len() == 0 {
        return out;
    }

    let blob = msrc.slice(blobstart, blobend);

    if ((local && blob.starts_with("use ")) || blob.starts_with("pub use ")) && txt_matches(search_type, searchstr, blob) {     
        if blob.match_indices(searchstr).count() == 1 {
            if blob.find_str((searchstr.to_string() + "::").as_slice()).is_some() {
                // can't possible match, fail fast!
                return out;
            }
        }

        debug!("PHIL found use: {} in |{}|", searchstr, blob);
        let t0 = ::time::precise_time_s();
        let view_item = ast::parse_view_item(String::from_str(blob));
        let t1 = ::time::precise_time_s();
        debug!("PHIL ast use parse_view_item time {}",t1-t0);
        for fqn_ in view_item.paths.iter() {
            // HACK, convert from &[~str] to &[&str]
            let mut fqn = to_refs(fqn_);  
            //let fqn = v.as_slice();

            // if searching for a symbol and the last bit matches the symbol
            // then find the fqn
            if fqn.len() == 1 && fqn.as_slice()[0] == searchstr {
                // is an exact match of a single use stmt. 
                // Do nothing because this will be picked up by the module
                // search in a bit.
            } else if fqn.as_slice()[fqn.len()-1].starts_with(searchstr) {
                // TODO: pretty sure this isn't correct/complete
                if fqn.as_slice()[0] == "self" {
                    fqn.remove(0);
                }
                for m in resolve_path(fqn.as_slice(), filepath, 0, ExactMatch, BothNamespaces) {
                    out.push(m);
                }
            }
        }
    }
    return out;
}

pub fn match_fn(msrc: &str, blobstart: uint, blobend: uint, 
             searchstr: &str, filepath: &Path, search_type: SearchType,
             local: bool) -> Option<Match> {
    let blob = msrc.slice(blobstart, blobend);
    if blob.starts_with("pub fn") && txt_matches(search_type, format!("pub fn {}", searchstr).as_slice(), blob) && !typeinf::first_param_is_self(blob) {
        debug!("PHIL found a pub fn starting {}",searchstr);
        // TODO: parse this properly
        let start = blob.find_str(format!("pub fn {}", searchstr).as_slice()).unwrap() + 7;
        let end = util::find_ident_end(blob, start);
        let l = blob.slice(start, end);
        debug!("PHIL found a pub fn {}",l);
        return Some(Match {matchstr: l.to_string(),
                       filepath: filepath.clone(), 
                       point: blobstart + start,
                       local: local,
                       mtype: Function,
                       contextstr: first_line(blob)
        });
    } else if local && blob.starts_with("fn") && txt_matches(search_type, format!("fn {}",searchstr).as_slice(), blob) && !typeinf::first_param_is_self(blob) {
        // TODO: parse this properly
        let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
        let end = find_ident_end(blob, start);
        let l = blob.slice(start, end);
        return Some(Match {matchstr: l.to_string(),
                       filepath: filepath.clone(), 
                       point: blobstart + start,
                       local: local,
                       mtype: Function,
                       contextstr: first_line(blob)
        });
    }
    return None;
}
