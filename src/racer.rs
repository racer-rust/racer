extern crate std;
extern crate log;
extern crate collections;
use std::io::File;
use std::io::BufferedReader;
use std::str;

pub mod scopes;
pub mod ast;
pub mod resolve;

pub enum MatchType {
    Struct,
    Module,
    Function,
    Crate,
    Let,
    StructField
}

pub struct Match {
    pub matchstr: ~str,
    pub filepath: Path,
    pub point: uint,
    pub linetxt: ~str,
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

fn locate_defn_in_module(filepath : &Path, s : &str, outputfn : &|Match|) {
    debug!("locate_defn_in_module {} {}",filepath.display(), s);
    let file = File::open(filepath);
    if file.is_err() { return; }
    let modsearchstr = "mod ";
    let fnsearchstr = "fn ";
    let structsearchstr = "struct ";
    let cratesearchstr = "extern crate ";
    let mut pt = 0;

    for line_r in BufferedReader::new(file).lines() {
        let line = line_r.unwrap();

        line.find_str(modsearchstr+s).map(|n|{
           let end = find_path_end(line, n+modsearchstr.len());
           let l = line.slice(n + modsearchstr.len(), end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pt + n + modsearchstr.len(), 
                           linetxt: line.to_owned(),
                           mtype: Module
            };
            (*outputfn)(m);            
        });

        for n in line.find_str(fnsearchstr+s).move_iter() {
            debug!("Found {}",fnsearchstr+s);
            let end = find_path_end(line, n+3);
            let l = line.slice(n + 3, end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pt + n + fnsearchstr.len(), 
                           linetxt: line.to_owned(),
                           mtype: Function};
            (*outputfn)(m);
        }
        for n in line.find_str(structsearchstr+s).move_iter() {
            let end = find_path_end(line, n+7);
            let l = line.slice(n+7, end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pt + n + structsearchstr.len(),
                           linetxt: line.to_owned(),
                           mtype: Struct};
            (*outputfn)(m);
        }

        for n in line.find_str(cratesearchstr+s).move_iter() {
            let end = find_path_end(line, n+ cratesearchstr.len());
            let cratename = line.slice(n + cratesearchstr.len(), end);
                let m = Match {matchstr: cratename+"::",
                               filepath: filepath.clone(), 
                               point: pt + n + cratesearchstr.len(), 
                               linetxt: line.to_owned(),
                               mtype: Crate};
                (*outputfn)(m);
        }

        if line.find_str(s).is_some() {
            for n in line.find_str("pub use ").iter() { 
                let end = find_path_end(line, n+8);
                let modname = line.slice(n+8, end);

                if modname.starts_with("self::") {
                    let mut l = modname.split_str("::");
                    let c : ~[&str] = l.collect();
                    if c.ends_with([""]) {
                        let mut c2 = c.slice_to(c.len()-1).to_owned();
                        c2.push(s);
                        locate_path_via_module(filepath, c2.slice_from(1), outputfn);
                        } else if c[c.len()-1].starts_with(s) {
                            locate_path_via_module(filepath, c.slice_from(1), outputfn);
                        }
                }
            }
        }
        pt += line.len();  // no need to add 1 for \n when iterating lines
    }
}

fn locate_path_via_module(filepath: &Path, p: &[&str], outputfn: &|Match|) {
    debug!("locate_path_via_module: {} {} ",filepath.as_str(),p);
    
    if p.len() == 0 {
        return locate_defn_in_module(filepath, "", outputfn);
    }

    if p.len() == 1 {
        return locate_defn_in_module(filepath, p[0], outputfn);
    }

    let file = File::open(filepath);
    if file.is_err() { return }

    let modsearchstr = "mod ";
    let mut pt = 0;
    for line_r in BufferedReader::new(file).lines() {
        let line = line_r.unwrap();
        for n in line.find_str(modsearchstr + p[0]).move_iter() {
            let end = find_path_end(line, n+modsearchstr.len());
            let l = line.slice(n + modsearchstr.len(), end);
            if p.len() == 1 {
                    (*outputfn)(Match {matchstr:l.to_owned(), 
                                       filepath:filepath.clone(), 
                                       point:pt+n+modsearchstr.len(),
                                       linetxt:line.to_owned(),
                                       mtype: Module
                    });
            } else {
                debug!("PHIL following: {}: {} ",l,line);
                let dir = filepath.dir_path();
                debug!("PHIL DIR {}", dir.as_str().unwrap());
                // try searching file.rs
                locate_path_via_module(&dir.join(l+".rs"), p.tail(), outputfn);
                // try searching dir/mod.rs
                locate_path_via_module(&dir.join_many([l, "mod.rs"]), p.tail(), outputfn)
            }
        }
        pt += line.len();  // +1 for /n
    }
}

// silently returns if path doesn't exist
fn search_lines(filepath: &Path, f:|~str| ) {
    let file = File::open(filepath);
    if file.is_err() { return }
    for line in BufferedReader::new(file).lines() {
        f(line.unwrap());
    }
}

pub fn locate_abs_path(p : &[&str], outputfn : &|Match|) {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let cratename = p[0];

    let v: Vec<&str> = srcpaths.split_str(":").collect();
    let v = v.append_one(".");
    for srcpath in v.move_iter() {
        println!("PHIL searching srcpath: {}",srcpath);
        {
            // try lib<cratename>/lib.rs, like in the rust source dir
            let cratelibname = "lib" + cratename;
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            locate_path_via_module(&filepath, p.slice_from(1), outputfn);
        }
        {            
            // try <cratename>/<cratename>.rs, like in the servo codebase
            let filepath = Path::new(srcpath).join_many([Path::new(cratename), 
                                                     Path::new(cratename + ".rs")]);
            locate_path_via_module(&filepath, p.slice_from(1), outputfn);
        }
        {            
            // try <cratename>/lib.rs
            let filepath = Path::new(srcpath).join_many([Path::new(cratename),
                                                     Path::new("lib.rs")]);
            locate_path_via_module(&filepath, p.slice_from(1), outputfn);
        }
        {            
            // try just <cratename>.rs
            let filepath = Path::new(srcpath).join_many([Path::new(cratename+".rs")]);
            //println!("PHIL search_crate path {}",path.as_str());
            locate_path_via_module(&filepath, p.slice_from(1), outputfn);
        }
    }    
}


pub fn to_refs<'a>(v: &'a Vec<~str>) -> Vec<&'a str> {
    let mut out = Vec::new();
    for item in v.iter() {
        out.push(item.as_slice()); 
    }
    return out;
}

fn search_use_imports(filepath : &Path, path : &[&str], outputfn : &|Match|) {
    search_lines(filepath, |line|{
        if line.find_str("use ").and(line.find_str(path[0])).is_some() {
            println!("PHIL found {}", line);
            for fqn_ in ast::parse_view_item(line).iter() {
                // HACK, convert from &[~str] to &[&str]
                let v = to_refs(fqn_);  
                let fqn = v.as_slice();

                // if searching for a symbol and the last bit matches the symbol
                // then find the fqn
                if path.len() == 1 && fqn[fqn.len()-1].starts_with(path[0]) {
                    locate_abs_path(fqn, outputfn);

                // if searching for a path and the last bit matches the first bit of 
                // the path then expand the path and find it
                //  (e.g. use foo;   searching for foo::bar)
                } else if path.len() > 1 && fqn[fqn.len()-1] == path[0].to_owned() {
                    let p2 = fqn + path.slice_from(1);
                    locate_abs_path(p2, outputfn);
                }
            }
        }
    });
}


fn search_crate_decls(filepath : &Path, path : &[&str], outputfn : &|Match|) {
    if path[0] == "std" {
        locate_abs_path(path, outputfn);
        return;
    }

    let file = File::open(filepath);
    if file.is_err() {return}

    for line_r in BufferedReader::new(file).lines() {
        let searchstr = "extern crate ";
        let line = line_r.unwrap();
        for n in line.find_str(searchstr+path[0]).iter() {
            let end = find_path_end(line, n+ searchstr.len());
            let cratename = line.slice(n + searchstr.len(), end);
            if path[0] == cratename {
                locate_abs_path(path, outputfn);
            }
        }
    }
}


fn search_for_let(mut point: uint, scopesrc:&str, searchstr:&str, filepath:&Path, 
                  outputfn : &|Match|) {
    
    for line in scopesrc.lines() {
        // search for let statements
        if line.find_str("let ").and(line.find_str(searchstr)).is_some() {
            let res = ast::parse_let(line.to_owned());
            res.map(|letresult| {
                if letresult.name.as_slice().starts_with(searchstr) {
                    (*outputfn)(Match { matchstr: letresult.name.to_owned(),
                                        filepath: filepath.clone(),
                                        point: point + letresult.point,
                                        linetxt: line.to_owned(),
                                        mtype: Let});
                }
            });
        }
        point += line.len() + 1;  // +1 for \n
    }
}

fn search_scope(searchstr:&str, filepath: &Path, msrc: &str, mut point:uint,
                          outputfn : &|Match|) {
    while point > 0 {
        let n = scopes::scope_start(msrc, point);
        let s = scopes::mask_sub_scopes(msrc.slice(n,point));
        search_for_let(n, s, searchstr, filepath, outputfn);
        if n == 0 { 
            break; 
        }
        point = n-1;
    }
}

fn search_file_text(searchstr:&str, filepath: &Path, point: uint,
                          outputfn: &|Match|) {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);
    let mut l = searchstr.split_str(".");
    let path: Vec<&str> = l.collect();

    search_file_text_(path.as_slice(), filepath, msrc, point, outputfn);
}

fn search_file_text_(path: &[&str], filepath: &Path, msrc: &str, point: uint, 
                          outputfn : &|Match|) {
    
    if path.len() == 1 {
        search_scope(path[0], filepath, msrc, point, outputfn); 
    } else {
        // field reference. 
        // (path.init = all but last elem of path)
        let def = first_match(|m| search_file_text_(path.init(), filepath, msrc, point, m));        
        def.map(|m| {
            let t = resolve::get_type_of(&m, filepath, msrc);
            t.map(|m| {
                match m.mtype {
                    Struct => {
                        println!("PHIL search str is {}",path[path.len()-1]);
                        for field in resolve::get_fields_of_struct(&m).iter() {
                            if field.starts_with(path[path.len()-1]) {
                                (*outputfn)(Match { matchstr: field.to_owned(),
                                                    filepath: m.filepath.clone(),
                                                    point: 0,
                                                    linetxt: ~"",
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

fn convert_output(m: &Match, outputfn: &|&str,uint,&Path,&str|) {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let (line, _) = scopes::point_to_coords(src, m.point);
    (*outputfn)(m.matchstr, line, &m.filepath, m.linetxt);
}

pub fn complete_from_file(src: &str, filepath: &Path, pos: uint, outputfn: &|Match|) {
    let s = expand_searchstr(src, pos);

    let mut l = s.split_str("::");
    let path : ~[&str] = l.collect(); 

    if path.len() == 1 {
       search_file_text(path[0], filepath, pos, outputfn); 
    }
    search_crate_decls(filepath, path, outputfn);
    search_use_imports(filepath, path, outputfn);
    locate_path_via_module(filepath, path, outputfn);
    
    if path.len() == 1 && "std".starts_with(path[0]) {
        let m = Match {matchstr: ~"std::",
                       filepath: filepath.clone(),
                       point: 1,
                       linetxt: ~"std::",
                       mtype: Crate};
        (*outputfn)(m);
    }
}

pub fn first_match(myfn: |outputfn : &|Match||) -> Option<Match> {
    let mut result: Option<Match> = None;
    {
        let output_fn = &|m: Match| {
            if result.is_none() {
                result = Some(m);
            }
        };

        myfn(output_fn);
    }
    return result;
}

pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return first_match(|m| find_definition_(src, filepath, pos, m));
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint, outputfn: &|Match|) {
    let (start, end) = expand_fqn(src, pos);
    let s = src.slice(start, end);    

    let mut l = s.split_str("::");
    let path : ~[&str] = l.collect(); 

    let find_definition_output_fn = &|m: Match| {
        if m.matchstr == path[path.len()-1].to_owned() {  // only if is an exact match
            (*outputfn)(m);
        }
    };
    do_search(path, filepath, pos, find_definition_output_fn);
}

pub fn do_search(path: &[&str], fpath: &Path, pos: uint, outputfn: &|Match|) {
    if path.len() == 1 {
       search_file_text(path[0], fpath, pos, outputfn); 
    }
    search_crate_decls(fpath, path, outputfn);
    search_use_imports(fpath, path, outputfn);
    locate_path_via_module(fpath, path, outputfn);
    
    if path.len() == 1 && "std".starts_with(path[0]) {
        let m = Match {matchstr: ~"std::",
                       filepath: fpath.clone(),
                       point: 1,
                       linetxt: ~"std::",
                       mtype: Crate};
        (*outputfn)(m);
    }
}
