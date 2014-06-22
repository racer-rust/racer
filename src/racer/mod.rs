extern crate std;
extern crate log;
extern crate collections;
extern crate time;

use std::io::File;
use std::io::BufferedReader;
use std::str;

pub mod scopes;
pub mod ast;
pub mod resolve;
pub mod codeiter;
pub mod codecleaner;
pub mod testutils;

#[cfg(test)] pub mod test;

pub enum MatchType {
    Struct,
    Module,
    Function,
    Crate,
    Let,
    StructField,
    Impl,
    Enum,
    Type,
    FnArg,
    Trait
}

pub enum SearchType {
    ExactMatch,
    StartsWith
}

pub enum CompletionType {
    Field,
    Path
}

pub struct Match {
    pub matchstr: String,
    pub filepath: Path,
    pub point: uint,
    pub local: bool,
    pub mtype: MatchType
}

pub fn getline(filepath : &Path, linenum : uint) -> String {
    let mut i = 0;
    let mut file = BufferedReader::new(File::open(filepath));
    for line in file.lines() {
        //print!("{}", line);
        i += 1;
        if i == linenum {
            return line.unwrap().to_string();
        }
    }
    return "not found".to_string();
}

fn is_path_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_ident_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_')
}

fn txt_matches(stype: SearchType, needle: &str, haystack: &str) -> bool { 
    return match stype {
        ExactMatch => {
            let nlen = needle.len();
            let hlen = haystack.len();

            if nlen == 0 {
                return true;
            }

            for (n,_) in haystack.match_indices(needle) {
                if (n == 0  || !is_ident_char(haystack.char_at(n-1))) && 
                    (n+nlen == hlen || !is_ident_char(haystack.char_at(n+nlen))) {
                    return true;
                }
            }
            return false;
        },
        StartsWith => {
            if needle.is_empty() {
                return true;
            }

            for (n,_) in haystack.match_indices(needle) {
                if n == 0  || !is_ident_char(haystack.char_at(n-1)) {
                    return true;
                }
            }
            return false;
        }
    }
}

fn symbol_matches(stype: SearchType, searchstr: &str, candidate: &str) -> bool {
   return match stype {
        ExactMatch => {
            return std::str::eq_slice(searchstr, candidate);
        },
        StartsWith => {
            return candidate.starts_with(searchstr);
        }
    }
}


#[test]
fn txt_matches_matches_stuff() {
    assert_eq!(true, txt_matches(ExactMatch, "Vec","Vec"));
    assert_eq!(true, txt_matches(StartsWith, "Vec","Vector"));
    assert_eq!(false, txt_matches(ExactMatch, "Vec","use Vector"));
    assert_eq!(true, txt_matches(StartsWith, "Vec","use Vector"));
    assert_eq!(false, txt_matches(StartsWith, "Vec","use aVector"));
    assert_eq!(true, txt_matches(ExactMatch, "Vec","use Vec"));
}

pub fn expand_ident(s : &str, pos : uint) -> (uint,uint) {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
        if !is_ident_char(c) {
            break;
        }
        start = i;
    }
    return (start, pos);
}

pub fn expand_fqn(s: &str, pos: uint) -> (uint,uint) {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
        if !is_path_char(c) {
            break;
        }
        start = i;
    }
    return (start, find_ident_end(s, pos));
}

pub fn expand_searchstr(s : &str, pos : uint) -> String {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
        if !is_path_char(c) {
            break;
        }
        start = i;
    }
    return s.slice(start,pos).to_string();
}

pub fn find_path_end(s : &str, pos : uint) -> uint {
    // find end of word
    let sa = s.slice_from(pos);
    let mut end = pos;
    for (i, c) in sa.char_indices() {
        if !is_path_char(c) {
            break;
        }
        end = pos + i + 1;
    }
    return end;
}

pub fn find_ident_end(s : &str, pos : uint) -> uint {
    // find end of word
    let sa = s.slice_from(pos);
    let mut end = pos;
    for (i, c) in sa.char_indices() {
        if !is_ident_char(c) {
            break;
        }
        end = pos + i + 1;
    }
    return end;
}

pub fn do_file_search(searchstr: &str, currentdir: &Path, outputfn: &mut |Match|) {
    debug!("PHIL do_file_search {}",searchstr);
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        match std::io::fs::readdir(&Path::new(srcpath)) {
            Ok(v) => {
                for fpath in v.iter() {
                    //debug!("PHIL fpath {}",fpath.as_str());
                    let fname = fpath.str_components().rev().next().unwrap().unwrap();
                    if fname.starts_with(format!("lib{}", searchstr).as_slice()) {
                        //debug!("PHIL Yeah found {}",fpath.as_str());
                        let filepath = Path::new(fpath).join_many([Path::new("lib.rs")]);
                        if File::open(&filepath).is_ok() {
                            let m = Match {matchstr: fname.slice_from(3).to_string(),
                                           filepath: filepath.clone(), 
                                           point: 0,
                                           local: false,
                                           mtype: Module};
                            (*outputfn)(m);
                        }
                    }

                    if fname.starts_with(searchstr) {
                        {
                            // try <name>/<name>.rs, like in the servo codebase
                            let filepath = Path::new(fpath).join_many([Path::new(format!("{}.rs", fname))]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/mod.rs
                            let filepath = Path::new(fpath).join_many([Path::new("mod.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/lib.rs
                            let filepath = Path::new(srcpath).join_many([Path::new("lib.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {            
                            // try just <name>.rs
                            if fname.ends_with(".rs") {
                                let m = Match {matchstr: fname.slice_to(fname.len()-3).to_string(),
                                               filepath: fpath.clone(),
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);                
                            }

                        }

                    }

                }
            }
            Err(_) => ()
        }
    }
}

pub fn search_crate_root(searchstr: &str, modfpath: &Path, 
                         searchtype: SearchType, outputfn: &mut |Match|) {
    let crateroots = find_possible_crate_root_modules(&modfpath.dir_path());
    for crateroot in crateroots.iter() {
        if crateroot == modfpath {
            continue;
        }
        debug!("PHIL going to search for {} in crateroot {}",searchstr, crateroot.as_str());
        do_local_search([searchstr], crateroot, 0, searchtype, outputfn);
    }
}

pub fn find_possible_crate_root_modules(currentdir: &Path) -> Vec<Path> {
    let mut res = Vec::new();
    {
        let filepath = currentdir.join_many([Path::new("lib.rs")]);
        if File::open(&filepath).is_ok() {
            res.push(filepath);
        }
    }
    return res;
}

pub fn get_module_file(name: &str, currentdir: &Path) -> Option<Path> {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        debug!("PHIL searching srcpath: {} for {}",srcpath, name);
        {
            // maybe path is from crate. 
            // try lib<name>/lib.rs, like in the rust source dir
            let cratelibname = format!("lib{}", name);
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/<name>.rs, like in the servo codebase
            let filepath = Path::new(srcpath).join_many([Path::new(name), 
                                                     Path::new(format!("{}.rs", name))]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/mod.rs
            let filepath = Path::new(srcpath).join_many([Path::new(name),
                                                     Path::new("mod.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/lib.rs
            let filepath = Path::new(srcpath).join_many([Path::new(name),
                                                     Path::new("lib.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {            
            // try just <name>.rs
            let filepath = Path::new(srcpath).join_many([Path::new(format!("{}.rs", name))]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
    }
    return None;
}


pub fn to_refs<'a>(v: &'a Vec<String>) -> Vec<&'a str> {
    let mut out = Vec::new();
    for item in v.iter() {
        out.push(item.as_slice()); 
    }
    return out;
}


fn search_next_scope(mut startpoint: uint, searchstr:&str, filepath:&Path, 
                     search_type: SearchType, local: bool, outputfn: &mut |Match|) {

    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let filesrc = str::from_utf8(filetxt.as_slice()).unwrap();
    if startpoint != 0 {
        // is a scope inside the file. Point should point to the definition 
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = filesrc.slice_from(startpoint);
        //debug!("PHIL search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it. 
        src.find_str("{").map(|n|{
            startpoint = startpoint + n + 1;
        });
    }

    search_scope(startpoint, filesrc, searchstr, filepath, search_type, local, outputfn);
}

fn first_param_is_self(blob: &str) -> bool {
    return blob.find_str("(").map_or(false, |start| {
        let end = scopes::find_closing_paren(blob, start+1);
        debug!("PHIL searching fn args: |{}| {}",blob.slice(start+1,end), txt_matches(ExactMatch, "self", blob.slice(start+1,end)));
        return txt_matches(ExactMatch, "self", blob.slice(start+1,end));
    });
}

fn search_scope_for_methods(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, 
                      outputfn: &mut |Match|) {
    debug!("PHIL searching scope for methods {} {} {}",point, searchstr, filepath.as_str());
    
    let scopesrc = src.slice_from(point);

    for (blobstart,blobend) in codeiter::iter_stmts(scopesrc) { 
        let blob = scopesrc.slice(blobstart, blobend);

        if txt_matches(search_type, format!("fn {}", searchstr).as_slice(), blob) 
            && first_param_is_self(blob) {
            debug!("PHIL found a method starting |{}| |{}|",searchstr,blob);
            // TODO: parse this properly
            let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: true,
                           mtype: Function
            };
            (*outputfn)(m);
        }
    }
}

fn search_scope(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL searching scope {} {} {}",point, searchstr, filepath.as_str());
    
    let exact_match = match search_type {
        ExactMatch => true,
        StartsWith => false
    };

    let scopesrc = src.slice_from(point);

    let mut skip_next_block = false;

    for (blobstart,blobend) in codeiter::iter_stmts(scopesrc) { 

        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = scopesrc.slice(blobstart,blobend);

        // for now skip stuff that's meant for testing. Often the test
        // module hierarchy is incompatible with the non-test
        // hierarchy and we get into recursive loops
        if blob.starts_with("#[cfg(test)") {
            skip_next_block = true;
            continue;
        }

        //debug!("PHIL search_scope BLOB |{}|",blob);
        if blob.starts_with("let ") && blob.find_str(searchstr).is_some() {
            let res = ast::parse_let(String::from_str(blob), filepath.clone(), blobstart, false);
            res.map(|letresult| {
                
                let name = letresult.name.as_slice();

                if (exact_match && name == searchstr) || (!exact_match && name.starts_with(searchstr)) {
                    (*outputfn)(Match { matchstr: letresult.name.to_string(),
                                        filepath: filepath.clone(),
                                        point: point + blobstart + letresult.point,
                                        local: local,
                                        mtype: Let});
                }
            });
        }

        if blob.starts_with(format!("extern crate {}", searchstr).as_slice()) {
            debug!("found an extern crate: |{}|",blob);


            let view_item;
            if blob.contains("\"") {
                // Annoyingly the extern crate can use a string literal for the
                // real crate name (e.g. extern crate collections_core = "collections")
                //so we need to get the source text without scrubbed strings 
                let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
                let rawsrc = str::from_utf8(filetxt.as_slice()).unwrap();
                let rawblob = rawsrc.slice(blobstart,blobend);
                debug!("found an extern crate (unscrubbed): |{}|", rawblob);
                
                view_item = ast::parse_view_item(String::from_str(rawblob));
            } else {
                view_item = ast::parse_view_item(String::from_str(blob));
            }


            if view_item.paths.is_empty() {
                // reference to a crate. For now do nothing since will
                // be picked up by file search. This will change
                // sometime in the future when racer has more accurate
                // crate handling
            } else {

                view_item.ident.clone().map(|ident|{
                    if symbol_matches(search_type, searchstr, ident.as_slice()) {
                        let real_str = view_item.paths.get(0).get(0);
                        get_module_file(real_str.as_slice(), &filepath.dir_path()).map(|modpath|{
                            let m = Match {matchstr: ident.to_string(),
                                           filepath: modpath.clone(), 
                                           point: 0,
                                           local: false,
                                           mtype: Module
                            };
                            (*outputfn)(m);
                        });

                    }
                });
            }
        }

        if local && blob.starts_with(format!("mod {}", searchstr).as_slice()) {
            debug!("found a module: |{}|",blob);
            // TODO: parse this properly
            let end = find_ident_end(blob, 4);
            let l = blob.slice(4, end);

            if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
                if blob.find_str("{").is_some() {
                    debug!("PHIL found an inline module!");

                    let m = Match {matchstr: l.to_string(),
                                   filepath: filepath.clone(), 
                                   point: point + blobstart + 4, 
                                   local: false,
                                   mtype: Module
                    };
                    (*outputfn)(m);
                    
                } else {
                    // reference to a local file
                    get_module_file(l, &filepath.dir_path()).map(|modpath|{
                        let m = Match {matchstr: l.to_string(),
                                       filepath: modpath.clone(), 
                                       point: 0,
                                       local: false,
                                       mtype: Module
                        };
                        (*outputfn)(m);
                    });
                }
            }
        }

        if blob.starts_with(format!("pub mod {}", searchstr).as_slice()) {
            debug!("found a pub module: |{}|",blob);
            // TODO: parse this properly
            let end = find_ident_end(blob, 8);
            let l = blob.slice(8, end);

            if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
                if blob.find_str("{").is_some() {
                    debug!("PHIL found an inline module!");

                    let m = Match {matchstr: l.to_string(),
                                   filepath: filepath.clone(), 
                                   point: point + blobstart + 8,
                                   local: false,
                                   mtype: Module
                    };
                    (*outputfn)(m);
                    
                } else {
                    // reference to a local file
                    get_module_file(l, &filepath.dir_path()).map(|modpath|{
                        let m = Match {matchstr: l.to_string(),
                                       filepath: modpath.clone(), 
                                       point: 0,
                                       local: false,
                                       mtype: Module
                        };
                        (*outputfn)(m);
                    });
                }
            }
        }

        if txt_matches(search_type, format!("pub fn {}", searchstr).as_slice(), blob) && !first_param_is_self(blob) {
            debug!("PHIL found a pub fn starting {}",searchstr);
            // TODO: parse this properly
            let start = blob.find_str(format!("pub fn {}", searchstr).as_slice()).unwrap() + 7;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found a pub fn {}",l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Function
            };
            (*outputfn)(m);
        } else if local && txt_matches(search_type, format!("fn {}",searchstr).as_slice(), blob) && !first_param_is_self(blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Function
            };
            (*outputfn)(m);
        }


        if local && txt_matches(search_type, format!("struct {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("struct {}", searchstr).as_slice()).unwrap() + 7;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a local struct |{}|", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Struct
            };
            (*outputfn)(m);
        }

        if txt_matches(search_type, format!("pub struct {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("pub struct {}", searchstr).as_slice()).unwrap() + 11;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a pub struct |{}|", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Struct
            };
            (*outputfn)(m);
        }

        if local && txt_matches(search_type, format!("type {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("type {}", searchstr).as_slice()).unwrap() + 5;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a type {}", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Type
            };
            (*outputfn)(m);
        }
        
        if txt_matches(search_type, format!("pub type {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("pub type {}", searchstr).as_slice()).unwrap() + 9;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a pub type {}", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Type
            };
            (*outputfn)(m);
        }
        
        if local && txt_matches(search_type, format!("trait {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("trait {}", searchstr).as_slice()).unwrap() + 6;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a type {}", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Trait
            };
            (*outputfn)(m);
        }
        
        if txt_matches(search_type, format!("pub trait {}", searchstr).as_slice(), blob) {
            // TODO: parse this properly
            let start = blob.find_str(format!("pub trait {}", searchstr).as_slice()).unwrap() + 10;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            debug!("PHIL found!! a pub type {}", l);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: local,
                           mtype: Trait
            };
            (*outputfn)(m);
        }


        if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {

            if blob.starts_with(format!("pub enum {}", searchstr).as_slice()) {
                // TODO: parse this properly
                let start = blob.find_str(format!("pub enum {}", searchstr).as_slice()).unwrap() + 9;
                let end = find_ident_end(blob, start);
                let l = blob.slice(start, end);
                if !exact_match || l == searchstr {
                    debug!("PHIL found!! a pub enum {}", l);
                    let m = Match {matchstr: l.to_string(),
                                   filepath: filepath.clone(), 
                                   point: point + blobstart + start,
                                   local: local,
                                   mtype: Enum
                    };
                    (*outputfn)(m);
                }
            } else if blob.starts_with(format!("enum {}", searchstr).as_slice()) {
                // TODO: parse this properly
                let start = blob.find_str(format!("enum {}", searchstr).as_slice()).unwrap() + 5;
                let end = find_ident_end(blob, start);
                let l = blob.slice(start, end);
                debug!("PHIL found!! a local enum {}", l);
                let m = Match {matchstr: l.to_string(),
                               filepath: filepath.clone(), 
                               point: point + blobstart + start,
                               local: local,
                               mtype: Enum
                };
                (*outputfn)(m);
            }

            if txt_matches(search_type, searchstr, blob) {
                // parse the enum
                let parsedEnum = ast::parse_enum(String::from_str(blob));
                if parsedEnum.name.as_slice().starts_with(searchstr) {
                }

                for (name, offset) in parsedEnum.values.move_iter() {
                    if name.as_slice().starts_with(searchstr) {
                        let m = Match {matchstr: name.into_string(),
                                       filepath: filepath.clone(), 
                                       point: point + blobstart + offset,
                                       local: local,
                                       mtype: Enum
                        };
                        (*outputfn)(m);
                    }
                }                
            }
        } 

        if ((local && blob.starts_with("use ")) || blob.starts_with("pub use ")) && txt_matches(search_type, searchstr, blob) {
            debug!("PHIL found use: {} in |{}|", searchstr, blob);
            let t0 = time::precise_time_s();
            let view_item = ast::parse_view_item(String::from_str(blob));
            let t1 = time::precise_time_s();
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
                    do_local_search(fqn.as_slice(), filepath, 0, ExactMatch, outputfn)
                }
            }
        }
    }
}

fn reverse_to_start_of_fn(point: uint, msrc: &str) -> Option<uint> {
    debug!("PHIL reverse to start of fn. {}", point);
    scopes::find_stmt_start(msrc, point).map_or(None, |n| {
        let block = msrc.slice_from(n);
        if block.starts_with("fn") || block.starts_with("pub fn") {
            return Some(n);
        } else {
            return None;
        }
    })
}

fn search_fn_args(point: uint, msrc:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL search_fn_args for |{}| pt: {}",searchstr, point);
    // 'point' points to the opening brace
    reverse_to_start_of_fn(point-1, msrc).map(|n| {
        let mut fndecl = String::new();
        // wrap in 'impl blah {}' so that methods get parsed correctly too
        fndecl.push_str("impl blah {");
        let impl_header = fndecl.len();
        fndecl.push_str(msrc.slice(n,point+1));
        fndecl.push_str("}}");
        debug!("PHIL found start of fn!! '{}' {} |{}|",searchstr, n, fndecl);
        if txt_matches(search_type, searchstr, fndecl.as_slice()) {
            let fn_ = ast::parse_fn(fndecl);
            debug!("PHIL parsed fn got {:?}",fn_);
            for (s, pos, _) in fn_.args.move_iter() {
                if match search_type {
                    ExactMatch => s.as_slice() == searchstr,
                    StartsWith => s.as_slice().starts_with(searchstr)
                    } {
                    (*outputfn)(Match { matchstr: s.to_string(),
                                        filepath: filepath.clone(),
                                        point: n + pos - impl_header,
                                        local: local,
                                        mtype: FnArg});
                };
            }
        }
    });
}

fn search_local_scopes(searchstr: &str, filepath: &Path, msrc: &str, mut point:uint,
                       search_type: SearchType, outputfn: &mut |Match|) {
    debug!("PHIL searching local scopes for {}",searchstr);

    let is_local = true;
    if point == 0 {
        // search the whole file
        search_scope(0, msrc, searchstr, filepath, search_type, is_local, outputfn);
    } else {
        // search each parent scope in turn
        while point > 0 {
            let n = scopes::scope_start(msrc, point);
               search_scope(n, msrc, searchstr, filepath, search_type, is_local, outputfn);
            if n == 0 { 
                break; 
            }
            point = n-1;

            search_fn_args(point, msrc, searchstr, filepath, search_type, is_local, outputfn);

        }
    }
}

pub fn load_file_and_mask_comments(filepath: &Path) -> String {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);
    return msrc;
}

fn search_local_text(searchstr: &str, filepath: &Path, point: uint,
                     search_type: SearchType, outputfn: &mut |Match|) {
    let msrc = load_file_and_mask_comments(filepath);

    if searchstr.contains(".") {
        println!("PHIL FIXME search_local_text searchstr contains a dot!!!!! |{}|",searchstr);
        fail!("PHIL FIXME search_local_text searchstr contains a dot!!!!!");
    }

    search_local_scopes(searchstr, filepath, msrc.as_slice(), point, search_type, outputfn);
}

fn search_struct_fields(searchstr: &str, m: &Match,
                        search_type: SearchType, outputfn: &mut |Match|) {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, m.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc));
    for (field, fpos) in fields.move_iter() {

        if symbol_matches(search_type, searchstr, field.as_slice()) {
            (*outputfn)(Match { matchstr: field.to_string(),
                                filepath: m.filepath.clone(),
                                point: fpos + opoint.unwrap(),
                                local: m.local,
                                mtype: StructField});
        }
    }
}

pub fn search_for_impl_methods(implsearchstr: &str,
                           fieldsearchstr: &str, point: uint, 
                           fpath: &Path, local: bool,
                           search_type: SearchType,
                           outputfn: &mut |Match|) {
    
    debug!("PHIL searching for impl methods |{}| |{}| {}",implsearchstr, fieldsearchstr, fpath.as_str());
    search_for_impls(point, implsearchstr, fpath, local, &mut |m|{
        debug!("PHIL found impl!! |{}| looking for methods",m.matchstr);
        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
        // find the opening brace and skip to it. 
        src.slice_from(m.point).find_str("{").map(|n|{
            let point = m.point + n + 1;
            search_scope_for_methods(point, src, fieldsearchstr, &m.filepath, search_type, outputfn);
        });
    });
}

pub fn first_match(myfn: |outputfn : &mut |Match||) -> Option<Match> {
    let mut result: Option<Match> = None;
    {
        let output_fn = &mut |m: Match| {
            if result.is_none() {
                result = Some(m);
            }
        };

        myfn(output_fn);
    }
    return result;
}

pub fn complete_from_file(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {

    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = src.slice(start,pos);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL contextstr is |{}|, searchstr is |{}|",contextstr, searchstr);

    if contextstr == "" {
        do_local_search([searchstr], filepath, pos, StartsWith, outputfn);
        return;
    }

    let context = ast::get_type_of(contextstr.to_string(), filepath, pos);

    context.map(|m| {
        match completetype {
            Field => {
                search_for_field(m, searchstr, StartsWith, outputfn);
            }
            Path => {
                search_for_path(m, searchstr, StartsWith, outputfn);
            }
        }
    });
}

pub fn search_for_path(context: Match, searchstr: &str, search_type: SearchType,  outputfn: &mut |Match|) {
    let m = context;
    match m.mtype {
        Module => {
            search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
        }
        Struct => {
            debug!("PHIL found a struct. Now need to look for impl");
            search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, &mut |m|{
                debug!("PHIL found impl!! {}",m.matchstr);
                let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                let src = str::from_utf8(filetxt.as_slice()).unwrap();
                
                // find the opening brace and skip to it. 
                src.slice_from(m.point).find_str("{").map(|n|{
                    let point = m.point + n + 1;
                    search_scope(point, src, searchstr, &m.filepath, search_type, m.local, outputfn);
                });
                
            });
        }
        _ => ()
    }
}

pub fn search_for_field(context: Match, searchstr: &str, search_type: SearchType, outputfn: &mut |Match|) {
    let m = context;
    match m.mtype {
        Struct => {
            debug!("PHIL got a struct, looking for fields and impl methods!! {}",m.matchstr);
            search_struct_fields(searchstr, &m, search_type, outputfn);
            search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    outputfn);
        }

        Enum => {
            search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    outputfn);
        }
        _ => ()
    };
}



pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return first_match(|m| find_definition_(src, filepath, pos, m));
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {

    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = src.slice(start,end);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL searching for |{}| |{}| {:?}",contextstr, searchstr, completetype);

    let find_definition_output_fn = &mut |m: Match| {
        if m.matchstr == searchstr.to_string() {  // only if is an exact match
            (*outputfn)(m);
        }
    };

    if contextstr == "" {
        do_local_search([searchstr], filepath, pos, ExactMatch, find_definition_output_fn);
        return;
    }

    let context = ast::get_type_of(contextstr.to_string(), filepath, pos);

    context.map(|m| {
        match completetype {
            Field => {
                search_for_field(m, searchstr, ExactMatch, find_definition_output_fn);
            }
            Path => {
                search_for_path(m, searchstr, ExactMatch, find_definition_output_fn);
            }
        }
    });
}

pub fn search_prelude_file(searchstr: &str, search_type: SearchType, outputfn: &mut |Match|) {
    // find the prelude file from the search path and scan it
    let srcpaths = match std::os::getenv("RUST_SRC_PATH") { 
        Some(paths) => paths,
        None => return
    };

    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    for srcpath in v.move_iter() {
        let filepath = Path::new(srcpath).join_many([Path::new("libstd/prelude.rs")]);
        if File::open(&filepath).is_ok() {
            search_local_text(searchstr, &filepath, 0, search_type, outputfn);
        }
    }
}

pub fn do_local_search_with_string(path: &[&str], filepath: &Path, pos: uint, 
                       search_type: SearchType,
                       outputfn: &mut |Match|) {
    debug!("PHIL: do_local_search_with_string {}", path);
    // HACK
    if path.len() == 1 && path[0] == "str" {
        debug!("PHIL {} == {}", path[0], "str");
        let str_match = first_match(|m| do_local_search(["Str"], filepath, pos, ExactMatch, m));
        debug!("PHIL: str_match {:?}", str_match);
        
        str_match.map(|str_match|{
            debug!("PHIL: found Str, converting to str");
            let m = Match {matchstr: "str".to_string(),
                           filepath: str_match.filepath.clone(), 
                           point: str_match.point,
                           local: false,
                           mtype: Struct
            };
            (*outputfn)(m);
        });
    } else {
        do_local_search(path, filepath, pos, search_type, outputfn);
    }
}

pub fn do_local_search(path: &[&str], filepath: &Path, pos: uint, 
                       search_type: SearchType,
                       outputfn: &mut |Match|) {
    debug!("PHIL do_local_search path {} in {}",path, filepath.as_str());

    if path.len() == 1 {
        let searchstr = path[0];
        search_local_text(searchstr, filepath, pos, search_type, outputfn);
        search_prelude_file(searchstr, search_type, outputfn);

        // don't need to match substrings here because substring matches are done
        // on the use stmts.
        search_crate_root(searchstr, filepath, search_type, outputfn);

        // don't need to match substrings here because substring matches are done
        // on the use stmts.
        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            let m = Match {matchstr: searchstr.to_string(),
                           filepath: path.clone(), 
                           point: 0,
                           local: false,
                           mtype: Module
            };
            (*outputfn)(m);
        });

        match search_type {
            StartsWith => do_file_search(searchstr, &filepath.dir_path(), outputfn),
            ExactMatch => ()
        };
    } else {
        if path[0] == "" {
            // match global searches starting with :: - e.g. ::std::blah::...
            return do_external_search(path.slice_from(1), filepath, pos, search_type, outputfn);
            
        }

        let parent_path = path.slice_to(path.len()-1);
        debug!("PHIL doing nested search: {} -> {}", path, parent_path);
        let context = first_match(|m| do_local_search(parent_path, filepath, pos, ExactMatch, m));
        context.map(|m| {
            debug!("PHIL context match is : {:?} {}", m, m.matchstr);
            match m.mtype {
                Module => {
                    debug!("PHIL searching a module '{}' (whole path: {})",m.matchstr, path);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                }
                Struct => {
                    debug!("PHIL found a struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found impl!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];

                        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
                        // find the opening brace and skip to it. 
                        src.slice_from(m.point).find_str("{").map(|n|{
                            let point = m.point + n + 1;
                            search_scope(point, src, searchstr, &m.filepath, search_type, m.local, outputfn);
                        });
                        
                    });
                }
                _ => ()
            }
        });
    }
}


fn search_for_impls(pos: uint, searchstr: &str, filepath: &Path, local: bool,
                    outputfn: &mut |Match|) {
    debug!("PHIL search_for_impls {}, {}, {}", pos, searchstr, filepath.as_str());
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let mut src = str::from_utf8(filetxt.as_slice()).unwrap();
    src = src.slice_from(pos);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);

        if blob.starts_with("impl") {
            blob.find_str("{").map(|n|{
                let mut decl = String::from_str(blob.slice_to(n+1));
                decl = decl.append("}");
                if txt_matches(ExactMatch, searchstr, decl.as_slice()) {
                    debug!("PHIL impl decl {}",decl);
                    let t0 = time::precise_time_s();
                    let implres = ast::parse_impl_name(decl);
                    let t1 = time::precise_time_s();
                    implres.map(|name|{
                        debug!("PHIL parsed an impl {}",name);
                        
                        let m = Match {matchstr: name.to_string(),
                                       filepath: filepath.clone(), 
                                       point: pos + start + 5,
                                       local: local,
                                       mtype: Impl
                        };
                        (*outputfn)(m);
                    });
                    debug!("PHIL ast parse impl {}s",t1-t0);
                }
            });
        }
    }
}

pub fn do_external_search(path: &[&str], filepath: &Path, pos: uint, search_type: SearchType, outputfn: &mut |Match|) {
    debug!("PHIL do_external_search path {} {}",path, filepath.as_str());
    if path.len() == 1 {
        let searchstr = path[0];
        search_next_scope(pos, searchstr, filepath, search_type, false, outputfn);

        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            let m = Match {matchstr: searchstr.to_string(),
                           filepath: path.clone(), 
                           point: 0,
                           local: false,
                           mtype: Module
            };
            (*outputfn)(m);
        });

    } else {
        let parent_path = path.slice_to(path.len()-1);
        let context = first_match(|m| do_external_search(parent_path, filepath, pos, ExactMatch, m));
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL found an external module {}",m.matchstr);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                }

                Struct => {
                    debug!("PHIL found a pub struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found  impl2!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];
                        debug!("PHIL about to search impl scope...");
                        search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                    });
                }
                _ => ()
            }
        });
    }
}
