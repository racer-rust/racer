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
    Impl,
    Enum,
    Type,
    FnArg
}

pub enum SearchType {
    ExactMatch,
    StartsWith
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
    return "not found".to_owned();
}

fn is_path_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!') || (c == ':') || (c == '.')
}

fn is_ident_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!')
}

fn txt_matches(stype: SearchType, needle: &str, haystack: &str) -> bool { 
    return match stype {
        ExactMatch => {
            let nlen = needle.len();
            let hlen = haystack.len();

            return haystack.find_str(needle).map_or(false, |n|{
                (n == 0  || !is_ident_char(haystack.char_at(n-1))) && 
                    (n+nlen == hlen || !is_ident_char(haystack.char_at(n+nlen)))
            });
        },
        StartsWith => {
            return haystack.find_str(needle).map_or(false, |n|{
                (n == 0  || !is_ident_char(haystack.char_at(n-1)))
            });
        }
    }
}

#[test]
fn matches_matches_stuff() {
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

pub fn expand_searchstr(s : &str, pos : uint) -> ~str {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices().rev() {
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
                    let fname = fpath.str_components().rev().next().unwrap().unwrap();
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
                     search_type: SearchType, local: bool, outputfn: &mut |Match|) {

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

    search_scope(startpoint, filesrc, searchstr, filepath, search_type, local, outputfn);
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
    for (start,end) in codeiter::iter_stmts(scopesrc) { 
        let blob = scopesrc.slice(start,end);
        //debug!("PHIL search_scope BLOB |{}|",blob);
        if blob.starts_with("let ") && blob.find_str(searchstr).is_some() {
            let res = ast::parse_let(StrBuf::from_str(blob));
            res.map(|letresult| {
                
                let name = letresult.name.as_slice();

                if (exact_match && name == searchstr) || (!exact_match && name.starts_with(searchstr)) {
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

            if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
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
        }

        if blob.starts_with("pub mod "+searchstr) {
            debug!("found a pub module: |{}|",blob);
            // TODO: parse this properly
            let end = find_path_end(blob, 8);
            let l = blob.slice(8, end);

            if (exact_match && l == searchstr) || (!exact_match && l.starts_with(searchstr)) {
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

        if blob.starts_with("type "+searchstr) {
            // TODO: parse this properly
            let end = find_path_end(blob, 5);
            let l = blob.slice(5, end);
            debug!("PHIL found!! a type {}", l);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 5,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Type
            };
            (*outputfn)(m);
        }
        
        if blob.starts_with("pub type "+searchstr) {
            // TODO: parse this properly
            let end = find_path_end(blob, 9);
            let l = blob.slice(9, end);
            debug!("PHIL found!! a pub type {}", l);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: point + start + 9,
                           linetxt: blob.to_owned(),
                           local: local,
                           mtype: Type
            };
            (*outputfn)(m);
        }
        

        if blob.starts_with("pub enum") || (local && blob.starts_with("enum")) {

            if blob.starts_with("enum "+searchstr) {
                // TODO: parse this properly
                let end = find_path_end(blob, 5);
                let l = blob.slice(5, end);
                debug!("PHIL found!! a local enum {}", l);
                let m = Match {matchstr: l.to_owned(), 
                               filepath: filepath.clone(), 
                               point: point + start + 5,
                               linetxt: blob.to_owned(),
                               local: local,
                               mtype: Enum
                };
                (*outputfn)(m);
            }

            if blob.starts_with("pub enum "+searchstr) {
                // TODO: parse this properly
                let end = find_path_end(blob, 9);
                let l = blob.slice(9, end);
                if !exact_match || l == searchstr {
                    debug!("PHIL found!! a pub enum {}", l);
                    let m = Match {matchstr: l.to_owned(), 
                                   filepath: filepath.clone(), 
                                   point: point + start + 9,
                                   linetxt: blob.to_owned(),
                                   local: local,
                                   mtype: Enum
                    };
                    (*outputfn)(m);
                }
            }

            if txt_matches(search_type, searchstr, blob) {
                // parse the enum
                let parsedEnum = ast::parse_enum(StrBuf::from_str(blob));
                if parsedEnum.name.as_slice().starts_with(searchstr) {
                }

                for (name, offset) in parsedEnum.values.move_iter() {
                    if name.as_slice().starts_with(searchstr) {
                        let m = Match {matchstr: name.into_owned(), 
                                       filepath: filepath.clone(), 
                                       point: point + start + offset,
                                       linetxt: blob.to_owned(),
                                       local: local,
                                       mtype: Enum
                        };
                        (*outputfn)(m);
                    }
                }                
            }
        } 

        if (local && blob.starts_with("use ")) || blob.starts_with("pub use ") && txt_matches(search_type, searchstr, blob) {
            debug!("PHIL found use: |{}|", blob);
            for fqn_ in ast::parse_view_item(StrBuf::from_str(blob)).iter() {
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
                    do_local_search(fqn.as_slice(), filepath, 0, ExactMatch, outputfn);
                }
            }
        }
    }
}
fn reverse_to_start_of_fn(point: uint, msrc: &str) -> Option<uint> {
    let closeParen: u8 = ")"[0];
    let whitespace = " \t\n".as_bytes();
    let mut n = point;

    // is the char before the '{' a ')'? if so, assume this is a function
    while n > 5 {
        if whitespace.contains(&msrc[n]) {
            n -= 1;
        } else if msrc[n] == closeParen {
            
            break;
        } else {
            return None;
        }
    }
    // if got to here then we guess is a fn
    loop {
        if msrc.len() > 3 && msrc.slice(n,n+2) == "fn" &&
            whitespace.contains(&msrc[n+2]) && (n==0 || whitespace.contains(&msrc[n-1])) {
            return Some(n);
        }
        if n == 0 {
            break;
        }
        n-=1;
    }
    return None;
}

fn search_fn_args(point: uint, msrc:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL search_fn_args");
    // 'point' points to the opening brace
    reverse_to_start_of_fn(point-1, msrc).map(|n| {
        let mut fndecl = StrBuf::from_str(msrc.slice(n,point+1));
        fndecl.push_str("}");
        debug!("PHIL found start of fn!! {} {} {}",n, msrc.slice_from(n), fndecl);
        
        if txt_matches(search_type, searchstr, fndecl.as_slice()) {
            let fn_ = ast::parse_fn(fndecl);
            debug!("PHIL parsed fn got {:?}",fn_);
            for (s, pos) in fn_.args.move_iter() {
                if match search_type {
                    ExactMatch => s.as_slice() == searchstr,
                    StartsWith => s.as_slice().starts_with(searchstr)
                    } {
                    (*outputfn)(Match { matchstr: s.to_owned(),
                                        filepath: filepath.clone(),
                                        point: n+pos,
                                        linetxt: s.to_owned(),
                                        local: local,
                                        mtype: FnArg});
                }
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

fn search_local_text(searchstr: &str, filepath: &Path, point: uint,
                     search_type: SearchType, outputfn: &mut |Match|) {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);

    let mut l = searchstr.split_str(".");
    let field_expr: Vec<&str> = l.collect();
    let field_expr = field_expr.as_slice();

    match search_type {
        ExactMatch => {
        search_local_text_(field_expr, filepath, msrc.as_slice(), point, search_type, &mut |m: Match| {
            if m.matchstr == field_expr[field_expr.len()-1].to_owned() {  // only if is an exact match
                (*outputfn)(m);
            } else {
                debug!("PHIL got match '{}', but doesnt exact match '{}'",
                      m.matchstr, field_expr[field_expr.len()-1]);
            }
        });

        },
        StartsWith => search_local_text_(field_expr, filepath, msrc.as_slice(), point, search_type, outputfn)
    }
}

fn search_local_text_(field_expr: &[&str], filepath: &Path, msrc: &str, point: uint, 
                      search_type: SearchType,
                          outputfn: &mut |Match|) {
    debug!("PHIL search_local_text_ {} {} {}",field_expr,filepath.as_str(),point);

    if field_expr.len() == 1 {
        search_local_scopes(field_expr[0], filepath, msrc, point, search_type, outputfn);
    } else {
         // field reference. 
        let parentexpr = field_expr.slice_to(field_expr.len()-1);
        let def = first_match(|m| search_local_text_(parentexpr, filepath, msrc, point, ExactMatch, m));        
        def.map(|m| {
            let t = resolve::get_type_of_OLD(&m, filepath, msrc);
            t.map(|m| {
                debug!("PHIL got type match {:?}",m);
                    
                match m.mtype {
                    Struct => {
                        debug!("PHIL got a struct, looking for fields and impls!! {}",m.matchstr);

                        let fieldsearchstr = field_expr[field_expr.len()-1];

                        for field in resolve::get_fields_of_struct(&m).iter() {
                            if field.starts_with(fieldsearchstr) {
                                (*outputfn)(Match { matchstr: field.to_owned(),
                                                    filepath: m.filepath.clone(),
                                                    point: 0,
                                                    linetxt: "".to_owned(),
                                                    local: m.local,
                                                    mtype: StructField});
                            }
                        }

                        search_for_impls(m.point, m.matchstr, &m.filepath, m.local, &mut |m|{
                            debug!("PHIL found impl!! {}. looking for methods",m.matchstr);
                            let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                            let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
                            // find the opening brace and skip to it. 
                            src.slice_from(m.point).find_str("{").map(|n|{
                                let point = m.point + n + 1;
                                search_scope(point, src, fieldsearchstr, &m.filepath, search_type, m.local, outputfn);
                            });
                        
                        });
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
    let path : Vec<&str> = l.collect(); 

    do_local_search(path.as_slice(), filepath, pos, StartsWith, outputfn);
}

pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return first_match(|m| find_definition_(src, filepath, pos, m));
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {
    let (start, end) = expand_fqn(src, pos);
    let expr = src.slice(start, end);    

    let mut l = expr.split_str("::");
    let path : Vec<&str> = l.collect(); 

    let lastbit = path.as_slice()[path.len()-1];

    let mut field_expr = lastbit.split_str(".");
    let field_expr : Vec<&str> = field_expr.collect(); 

    let find_definition_output_fn = &mut |m: Match| {
        if m.matchstr == field_expr.as_slice()[field_expr.len()-1].to_owned() {  // only if is an exact match
            (*outputfn)(m);
        }
    };

    let mut l = expr.split_str("::");
    let path : Vec<&str> = l.collect(); 

    do_local_search(path.as_slice(), filepath, pos, ExactMatch, find_definition_output_fn);
}

pub fn search_prelude_file(searchstr: &str, search_type: SearchType, outputfn: &mut |Match|) {
    // find the prelude file from teh search path and scan it
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.split_str(":").collect();
    for srcpath in v.move_iter() {
        let filepath = Path::new(srcpath).join_many([Path::new("libstd/prelude.rs")]);
        if File::open(&filepath).is_ok() {
            search_local_text(searchstr, &filepath, 0, search_type, outputfn);
        }
    }
}

pub fn do_local_search(path: &[&str], filepath: &Path, pos: uint, 
                       search_type: SearchType,
                       outputfn: &mut |Match|) {

    debug!("PHIL do_local_search path {}",path);

    if path.len() == 1 {
        let searchstr = path[0];
        search_local_text(searchstr, filepath, pos, search_type, outputfn);
        search_prelude_file(searchstr, search_type, outputfn);

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

        match search_type {
            StartsWith => do_file_search(searchstr, &filepath.dir_path(), outputfn),
            ExactMatch => ()
        };
    } else {
        let parent_path = path.slice_to(path.len()-1);
        debug!("PHIL doing nested search: {} -> {}", path, parent_path);
        let context = first_match(|m| do_local_search(parent_path, filepath, pos, ExactMatch, m));
        debug!("PHIL context match is : {:?} ", context);
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL searching a module '{}' (whole path: {})",m.matchstr, path);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
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
                let mut decl = std::strbuf::StrBuf::from_str(blob.slice_to(n+1));
                decl = decl.append("}");
                if decl.as_slice().find_str(searchstr).is_some() {
                    debug!("PHIL decl {}",decl);
                    ast::parse_impl_name(decl).map(|name|{
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

pub fn do_external_search(path: &[&str], filepath: &Path, pos: uint, search_type: SearchType, outputfn: &mut |Match|) {
    if path.len() == 1 {
        let searchstr = path[0];
        search_next_scope(pos, searchstr, filepath, search_type, false, outputfn);

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
                    search_for_impls(m.point, m.matchstr, &m.filepath, m.local, &mut |m|{
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
