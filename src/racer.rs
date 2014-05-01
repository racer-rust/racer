extern crate std;
extern crate log;
extern crate collections;
use std::io::File;
use std::io::BufferedReader;
use std::str;

pub mod scopes;
pub mod ast;
pub mod resolve;
pub mod codeiter;
pub mod codecleaner;
pub mod testutils;


pub enum MatchType {
    Struct,
    Module,
    Function,
    Crate,
    Let,
    StructField,
    Impl
}

pub struct Match {
    pub matchstr: ~str,
    pub filepath: Path,
    pub point: uint,
    pub linetxt: ~str,
    pub local: bool,
    pub mtype: MatchType
}

pub fn getline(filepath : &Path, linenum : uint) -> ~str {
    let mut i = 0;
    let mut file = BufferedReader::new(File::open(filepath));
    for line in file.lines() {
        //print!("{}", line);
        i += 1;
        if i == linenum {
            return line.unwrap().to_owned();
        }
    }
    return ~"not found";
}

fn is_path_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!') || (c == ':') || (c == '.')
}

fn is_ident_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!')
}

pub fn expand_ident(s : &str, pos : uint) -> (uint,uint) {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices_rev() {
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
    for (i, c) in sb.char_indices_rev() {
        if !is_path_char(c) {
            break;
        }
        start = i;
    }
    return (start, find_ident_end(s, pos));
}

pub fn expand_searchstr(s : &str, pos : uint) -> ~str {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices_rev() {
        if !is_path_char(c) {
            break;
        }
        start = i;
    }
    return s.slice(start,pos).to_owned();
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
    let v: Vec<&str> = srcpaths.split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        match std::io::fs::readdir(&Path::new(srcpath)) {
            Ok(v) => {
                for fpath in v.iter() {
                    //debug!("PHIL fpath {}",fpath.as_str());
                    let fname = fpath.rev_str_components().next().unwrap().unwrap();
                    if fname.starts_with("lib"+ searchstr) {
                        //debug!("PHIL Yeah found {}",fpath.as_str());
                        let filepath = Path::new(fpath).join_many([Path::new("lib.rs")]);
                        if File::open(&filepath).is_ok() {
                            let m = Match {matchstr: fname.slice_from(3).to_owned(), 
                                           filepath: filepath.clone(), 
                                           point: 0,
                                           linetxt: "".to_owned(),
                                           local: false,
                                           mtype: Module};
                            (*outputfn)(m);
                        }
                    }

                    if fname.starts_with(searchstr) {
                        {
                            // try <name>/<name>.rs, like in the servo codebase
                            let filepath = Path::new(fpath).join_many([Path::new(fname + ".rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_owned(), 
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               linetxt: "".to_owned(),
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/mod.rs
                            let filepath = Path::new(fpath).join_many([Path::new("mod.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_owned(), 
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               linetxt: "".to_owned(),
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/lib.rs
                            let filepath = Path::new(srcpath).join_many([Path::new("lib.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_owned(), 
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               linetxt: "".to_owned(),
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {            
                            // try just <name>.rs
                            if fname.ends_with(".rs") {
                                let m = Match {matchstr: fname.slice_to(fname.len()-3).to_owned(), 
                                               filepath: fpath.clone(),
                                               point: 0,
                                               linetxt: "".to_owned(),
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


pub fn get_module_file(name: &str, currentdir: &Path) -> Option<Path> {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        debug!("PHIL searching srcpath: {} for {}",srcpath, name);
        {
            // maybe path is from crate. 
            // try lib<name>/lib.rs, like in the rust source dir
            let cratelibname = "lib" + name;
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/<name>.rs, like in the servo codebase
            let filepath = Path::new(srcpath).join_many([Path::new(name), 
                                                     Path::new(name + ".rs")]);
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
            let filepath = Path::new(srcpath).join_many([Path::new(name+".rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
    }
    return None;
}


pub fn to_refs<'a>(v: &'a Vec<~str>) -> Vec<&'a str> {
    let mut out = Vec::new();
    for item in v.iter() {
        out.push(item.as_slice()); 
    }
    return out;
}


fn search_next_scope(mut startpoint: uint, searchstr:&str, filepath:&Path, 
                     local: bool, outputfn: &mut |Match|) {

    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let filesrc = str::from_utf8(filetxt.as_slice()).unwrap();
    if startpoint != 0 {
        // is a scope inside the file. Point should point to the definition 
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = filesrc.slice_from(startpoint);
        debug!("PHIL search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it. 
        src.find_str("{").map(|n|{
            startpoint = startpoint + n + 1;
        });
    }

    search_scope(startpoint, filesrc, searchstr, filepath, local, outputfn);
}

fn search_scope(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL searching scope {} {} {}",point, searchstr, filepath.as_str());
    
    let scopesrc = src.slice_from(point);
    for (start,end) in codeiter::iter_stmts(scopesrc) { 
        let blob = scopesrc.slice(start,end);
        //debug!("PHIL search_scope BLOB |{}|",blob);
        if blob.starts_with("let ") && blob.find_str(searchstr).is_some() {
            let res = ast::parse_let(blob.to_owned());
            res.map(|letresult| {
                if letresult.name.as_slice().starts_with(searchstr) {
                    (*outputfn)(Match { matchstr: letresult.name.to_owned(),
                                        filepath: filepath.clone(),
                                        point: point + start + letresult.point,
                                        linetxt: blob.to_owned(),
                                        local: local,
                                        mtype: Let});
                }
            });
        }

        if local && blob.starts_with("mod "+searchstr) {
            debug!("found a module: |{}|",blob);
            // TODO: parse this properly
            let end = find_path_end(blob, 4);
            let l = blob.slice(4, end);

            if blob.find_str("{").is_some() {
                debug!("PHIL found an inline module!");

                let m = Match {matchstr: l.to_owned(), 
                               filepath: filepath.clone(), 
                               point: point + start + 4, 
                               linetxt: blob.to_owned(),
                               local: false,
                               mtype: Module
                };
                (*outputfn)(m);
                
            } else {
                // reference to a local file
                get_module_file(l, &filepath.dir_path()).map(|modpath|{
                    let m = Match {matchstr: l.to_owned(), 
                                   filepath: modpath.clone(), 
                                   point: 0,
                                   linetxt: "".to_owned(),
                                   local: false,
                                   mtype: Module
                    };
                    (*outputfn)(m);
                });
            }
        }

        if blob.starts_with("pub mod "+searchstr) {
            debug!("found a pub module: |{}|",blob);
            // TODO: parse this properly
            let end = find_path_end(blob, 8);
            let l = blob.slice(8, end);

            if blob.find_str("{").is_some() {
                debug!("PHIL found an inline module!");

                let m = Match {matchstr: l.to_owned(), 
                               filepath: filepath.clone(), 
                               point: point + start + 8,
                               linetxt: blob.to_owned(),
                               local: false,
                               mtype: Module
                };
                (*outputfn)(m);
                
            } else {
                // reference to a local file
                get_module_file(l, &filepath.dir_path()).map(|modpath|{
                    let m = Match {matchstr: l.to_owned(), 
                                   filepath: modpath.clone(), 
                                   point: 0,
                                   linetxt: "".to_owned(),
                                   local: false,
                                   mtype: Module
                    };
                    (*outputfn)(m);
                });
            }
        }


        if local && blob.starts_with("fn "+searchstr) {
            // TODO: parse this properly
            let end = find_path_end(blob, 3);
            let l = blob.slice(3, end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 3,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Function
            };
            (*outputfn)(m);
        }

        if blob.starts_with("pub fn "+searchstr) {
            debug!("PHIL found a pub fn starting {}",searchstr);
            // TODO: parse this properly
            let end = find_path_end(blob, 7);
            let l = blob.slice(7, end);
            debug!("PHIL found a pub fn {}",l);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 7,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Function
            };
            (*outputfn)(m);
        }


        if local && blob.starts_with("struct "+searchstr) {
            // TODO: parse this properly
            let end = find_path_end(blob, 7);
            let l = blob.slice(7, end);
            debug!("PHIL found!! a local struct {}", l);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 7,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Struct
            };
            (*outputfn)(m);
        }

        if blob.starts_with("pub struct "+searchstr) {
            // TODO: parse this properly
            let end = find_path_end(blob, 11);
            let l = blob.slice(11, end);
            debug!("PHIL found!! a pub struct {}", l);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 11,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Struct
            };
            (*outputfn)(m);
        }


        if local && blob.starts_with("use ") && blob.find_str(searchstr).is_some() {
            debug!("PHIL in {} found use: |{}|", filepath.as_str(), blob);
            for fqn_ in ast::parse_view_item(blob.to_owned()).iter() {
                // HACK, convert from &[~str] to &[&str]
                let v = to_refs(fqn_);  
                let fqn = v.as_slice();
            
                // if searching for a symbol and the last bit matches the symbol
                // then find the fqn
                if fqn.len() == 1 && fqn[0] == searchstr {
                    // is an exact match of a single use stmt. 
                    // Do nothing because this will be picked up by the module
                    // search in a bit.
                } else if fqn[fqn.len()-1].starts_with(searchstr) {
                    do_local_search(fqn, filepath, 0, true, outputfn);
                }
            }
        }

        if blob.starts_with("pub use ") && blob.find_str(searchstr).is_some() {
            debug!("PHIL found pub use: |{}|", blob);
            for fqn_ in ast::parse_view_item(blob.to_owned()).iter() {
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
                    do_local_search(fqn.as_slice(), filepath, 0, true, outputfn);
                }
            }
        }
    }
}

fn search_local_scopes(searchstr: &str, filepath: &Path, msrc: &str, mut point:uint,
                          outputfn: &mut |Match|) {
    debug!("PHIL searching local scopes for {}",searchstr);

    if point == 0 {
        // search the whole file
        search_scope(0, msrc, searchstr, filepath, true, outputfn);
    } else {
        // search each parent scope in turn
        while point > 0 {
            let n = scopes::scope_start(msrc, point);
            //let s = msrc.slice_to(point);
            search_scope(n, msrc, searchstr, filepath, true, outputfn);
            if n == 0 { 
                break; 
            }
            point = n-1;
        }
    }
}

fn search_local_text(searchstr: &str, filepath: &Path, point: uint,
                          outputfn: &mut |Match|) {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);

    let mut l = searchstr.split_str(".");
    let field_expr: Vec<&str> = l.collect();

    search_local_text_(field_expr.as_slice(), filepath, msrc, point, outputfn);
}

fn search_local_text_(field_expr: &[&str], filepath: &Path, msrc: &str, point: uint, 
                          outputfn: &mut |Match|) {
    debug!("PHIL search_local_text_ {} {} {}",field_expr,filepath.as_str(),point);

    if field_expr.len() == 1 {
        search_local_scopes(field_expr[0], filepath, msrc, point, outputfn);
    } else {
        // field reference. 
        let parentexpr = field_expr.slice_to(field_expr.len()-1);
        let def = first_match(|m| search_local_text_(parentexpr, filepath, msrc, point, m));        
        def.map(|m| {
            let t = resolve::get_type_of(&m, filepath, msrc);
            t.map(|m| {
                match m.mtype {
                    Struct => {
                        for field in resolve::get_fields_of_struct(&m).iter() {
                            if field.starts_with(field_expr[field_expr.len()-1]) {
                                (*outputfn)(Match { matchstr: field.to_owned(),
                                                    filepath: m.filepath.clone(),
                                                    point: 0,
                                                    linetxt: ~"",
                                                    local: m.local,
                                                    mtype: StructField});
                            }
                        }
                    }
                    _ => ()
                }
            });
        });
    }
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
    let expr = expand_searchstr(src, pos);

    let mut l = expr.split_str("::");
    let path : ~[&str] = l.collect(); 

    do_local_search(path, filepath, pos, false, outputfn);
}

pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return first_match(|m| find_definition_(src, filepath, pos, m));
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {
    let (start, end) = expand_fqn(src, pos);
    let expr = src.slice(start, end);    

    let mut l = expr.split_str("::");
    let path : ~[&str] = l.collect(); 

    let lastbit = path[path.len()-1];

    let mut field_expr = lastbit.split_str(".");
    let field_expr : ~[&str] = field_expr.collect(); 

    let find_definition_output_fn = &mut |m: Match| {
        if m.matchstr == field_expr[field_expr.len()-1].to_owned() {  // only if is an exact match
            (*outputfn)(m);
        }
    };

    let mut l = expr.split_str("::");
    let path : ~[&str] = l.collect(); 

    do_local_search(path, filepath, pos, true, find_definition_output_fn);
}

pub fn do_local_search(path: &[&str], filepath: &Path, pos: uint, 
                       exact_match: bool,
                       outputfn: &mut |Match|) {

    debug!("PHIL do_local_search path {}",path);

    if path.len() == 1 {
        let searchstr = path[0];

        if exact_match {
            search_local_text(searchstr, filepath, pos, &mut |m: Match| {
                if m.matchstr == searchstr.to_owned() {  // only if is an exact match
                    (*outputfn)(m);
                }
            });
        } else {
            search_local_text(searchstr, filepath, pos, outputfn);
        }

        // don't need to match substrings here because substring matches are done
        // on the use stmts.
        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            let m = Match {matchstr: searchstr.to_owned(),
                           filepath: path.clone(), 
                           point: 0,
                           linetxt: "".to_owned(),
                           local: false,
                           mtype: Module
            };
            (*outputfn)(m);
        });

        if !exact_match {
            do_file_search(searchstr, &filepath.dir_path(), outputfn);
        }
    } else {
        let parent_path = path.slice_to(path.len()-1);
        debug!("PHIL doing nested search: {} -> {}", path, parent_path);
        let context = first_match(|m| do_local_search(parent_path, filepath, pos, true, m));
        debug!("PHIL context match is : {:?} ", context);
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL searching a module '{}' (whole path: {})",m.matchstr, path);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, false, outputfn);
                }
                Struct => {
                    debug!("PHIL found a struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr, &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found impl!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];

                        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
                        // find the opening brace and skip to it. 
                        src.slice_from(m.point).find_str("{").map(|n|{
                            let point = m.point + n + 1;
                            search_scope(point, src, searchstr, &m.filepath, m.local, outputfn);
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
                let mut decl = std::strbuf::StrBuf::from_str(blob.slice_to(n+1));
                decl = decl.append("}");
                if decl.as_slice().find_str(searchstr).is_some() {
                    debug!("PHIL decl {}",decl);
                    ast::parse_impl_name(decl.into_owned()).map(|name|{
                        debug!("PHIL parsed an impl {}",name);
                        
                        let m = Match {matchstr: name.to_owned(), 
                                       filepath: filepath.clone(), 
                                       point: pos + start + 5,
                                       linetxt: blob.to_owned(),
                                       local: local,
                                       mtype: Impl
                        };
                        (*outputfn)(m);
                    });
                }
            });
        }
    }
}

pub fn do_external_search(path: &[&str], filepath: &Path, pos: uint, outputfn: &mut |Match|) {
    if path.len() == 1 {
        let searchstr = path[0];
        search_next_scope(pos, searchstr, filepath, false, outputfn);

        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            let m = Match {matchstr: searchstr.to_owned(),
                           filepath: path.clone(), 
                           point: 0,
                           linetxt: "".to_owned(),
                           local: false,
                           mtype: Module
            };
            (*outputfn)(m);
        });

    } else {
        let parent_path = path.slice_to(path.len()-1);
        let context = first_match(|m| do_external_search(parent_path, filepath, pos, m));
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL found an external module {}",m.matchstr);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, false, outputfn);
                }


                Struct => {
                    debug!("PHIL found a pub struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr, &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found  impl2!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];
                        debug!("PHIL about to search impl scope...");
                        search_next_scope(m.point, searchstr, &m.filepath, false, outputfn);
                        
                    });
                }
                _ => ()
            }
        });
    }
}
