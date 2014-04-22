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

fn locate_defn_in_module(pos: uint, msrc: &str, filepath: &Path, 
                         defnstr: &str, outputfn: &|Match|) {
    println!("locate_defn_in_module {} {}",filepath.display(), defnstr);

    let modsearchstr = "mod ";
    let fnsearchstr = "fn ";
    let structsearchstr = "struct ";
    let cratesearchstr = "extern crate ";
    let mut pt = 0;

    for line in msrc.lines() {

        line.find_str(modsearchstr+defnstr).map(|n|{
           let end = find_path_end(line, n+modsearchstr.len());
           let l = line.slice(n + modsearchstr.len(), end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pos + pt + n + modsearchstr.len(), 
                           linetxt: line.to_owned(),
                           mtype: Module
            };
            (*outputfn)(m);            
        });

        for n in line.find_str(fnsearchstr+defnstr).move_iter() {
            debug!("Found {}",fnsearchstr+defnstr);
            let end = find_path_end(line, n+3);
            let l = line.slice(n + 3, end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pos + pt + n + fnsearchstr.len(), 
                           linetxt: line.to_owned(),
                           mtype: Function};
            (*outputfn)(m);
        }
        for n in line.find_str(structsearchstr+defnstr).move_iter() {
            let end = find_path_end(line, n+7);
            let l = line.slice(n+7, end);
            let m = Match {matchstr: l.to_owned(), 
                           filepath: filepath.clone(), 
                           point: pos + pt + n + structsearchstr.len(),
                           linetxt: line.to_owned(),
                           mtype: Struct};
            (*outputfn)(m);
        }

        for n in line.find_str(cratesearchstr+defnstr).move_iter() {
            let end = find_path_end(line, n+ cratesearchstr.len());
            let cratename = line.slice(n + cratesearchstr.len(), end);
                let m = Match {matchstr: cratename+"::",
                               filepath: filepath.clone(), 
                               point: pos + pt + n + cratesearchstr.len(), 
                               linetxt: line.to_owned(),
                               mtype: Crate};
                (*outputfn)(m);
        }

        if line.find_str(defnstr).is_some() {
            for n in line.find_str("pub use ").iter() { 
                let end = find_path_end(line, n+8);
                let modname = line.slice(n+8, end);

                if modname.starts_with("self::") {
                    let mut l = modname.split_str("::");
                    let mut c : Vec<&str> = l.collect();
                    if c.as_slice().ends_with([""]) {
                        println!("PHIL ends1");
                        //let mut c2 = cslice(c.len()-1)
                        c.pop();
                        c.push(defnstr);
                        locate_path_in_external_module(filepath, c.slice_from(1), outputfn);
                    } else if c.as_slice()[c.len()-1].starts_with(defnstr) {
                        println!("PHIL ends2");
                        locate_path_in_external_module(filepath, c.slice_from(1), outputfn);
                    }
                }
            }
        }
        pt += line.len() + 1;  // +1 for \n
    }
}

fn locate_path_in_external_module(filepath: &Path, path: &[&str], outputfn: &|Match|) {
    println!("locate_path_in_external_module: {} {} ",filepath.as_str(),path);

    let filetxt_ = BufferedReader::new(File::open(filepath)).read_to_end();
    if filetxt_.is_err() {
        return;
    }
    let filetxt = filetxt_.unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();



    for (start,end) in codeiter::iter_stmts(src) {
        let blob = src.slice(start,end);
        if blob.starts_with("pub use ") && blob.find_str(path[0]).is_some() {
            for fqn_ in ast::parse_view_item(blob.to_owned()).iter() {
                // HACK, convert from &[~str] to &[&str]
                let v = to_refs(fqn_);  
                let mut fqn = v.as_slice();
                println!("PHIL fqn is {}",fqn);

                if fqn[0] == "self" {
                    fqn = fqn.slice_from(1);
                }

            }            
        }
        
    }
}

// fn locate_path_via_module_(filepath: &Path, path: &[&str], outputfn: &|Match|) {
//     println!("locate_path_via_module: {} {} ",filepath.as_str(),path);

//     let filetxt_ = BufferedReader::new(File::open(filepath)).read_to_end();
//     if filetxt_.is_err() {
//         return;
//     }
//     let filetxt = filetxt_.unwrap();
//     let src = str::from_utf8(filetxt.as_slice()).unwrap();
//     let msrc = scopes::mask_comments(src);
        
//     if path.len() == 0 {
//         return locate_defn_in_module(0, msrc, filepath, "", outputfn);
//     }

//     if path.len() == 1 {
//         return locate_defn_in_module(0, msrc, filepath, path[0], outputfn);
//     }

//     let file = File::open(filepath);
//     if file.is_err() { return }

//     let modsearchstr = "mod ";
//     let implsearchstr = "impl";
//     let mut pt = 0;
    
//     for line in msrc.lines() {
//         for n in line.find_str(modsearchstr + path[0]).move_iter() {
//             let end = find_path_end(line, n+modsearchstr.len());
//             let l = line.slice(n + modsearchstr.len(), end);
//             let dir = filepath.dir_path();
//             // try searching file.rs
//             locate_path_via_module(&dir.join(l+".rs"), path.tail(), outputfn);
//             // try searching dir/mod.rs
//             locate_path_via_module(&dir.join_many([l, "mod.rs"]), path.tail(), outputfn)
//         }

//         if line.find_str("pub use ").and(line.find_str(path[0])).is_some() {
//             println!("PHIL found pub use {}", line);
//             for fqn_ in ast::parse_view_item(line.to_owned()).iter() {
//                 // HACK, convert from &[~str] to &[&str]
//                 let v = to_refs(fqn_);  
//                 let mut fqn = v.as_slice();
//                 println!("PHIL fqn is {}",fqn);

//                 if fqn[0] == "self" {
//                     fqn = fqn.slice_from(1);
//                 }

//                 println!("PHIL fqn is now {}. path {}",fqn,path);

//                 // if searching for a symbol and the last bit matches the symbol
//                 // then find the fqn
//                 if path.len() == 1 && fqn[fqn.len()-1].starts_with(path[0]) {
//                     let dir = filepath.dir_path();
//                     println!("PHIL locate_abs_path {}",fqn);
//                     locate_abs_path(fqn, &dir, outputfn);
                
//                 // if searching for a path and the last bit matches the first bit of 
//                 // the path then expand the path and find it
//                 //  (e.g. use foo;   searching for foo::bar)
//                 } else if path.len() > 1 && fqn[fqn.len()-1] == path[0].to_owned() {
//                     let p2 = fqn + path.slice_from(1);
//                     let dir = filepath.dir_path();
//                     locate_abs_path(p2, &dir, outputfn);
//                 }

//                 // TODO:  use mod::Foo;  searching for Foo::new
//             }
//         }


//         // if line.find_str(path[0]).is_some() {
//         //     for n in line.find_str("pub use ").iter() { 
//         //         let end = find_path_end(line, n+8);
//         //         let modname = line.slice(n+8, end);
//         //         println!("PHIL line {}",line);
//         //         println!("PHIL path: {}",path);
//         //         println!("PHIL modname: {}",modname);
//         //         if modname.starts_with("self::") {
//         //             let mut l = modname.split_str("::");
//         //             let c : ~[&str] = l.collect();
//         //             println!("PHIL c {}",c);
//         //             if c.ends_with([""]) {
//         //                 // chop of the last bit
//         //                 let mut c2 = Vec::from_slice(c.slice_to(c.len()-1));
//         //                 println!("PHIL c2 1: {}",c2);
//         //                 // append the new stuff
//         //                 c2 = c2.append(path);
//         //                 println!("PHIL c2 2: {}",c2);
//         //                 locate_path_via_module(filepath, c2.slice_from(1), outputfn);
//         //             // TODO(PD): fix this
//         //             } else if c[c.len()-1].starts_with(path[0]) {
//         //                 println!("PHIL ERROR!!!! {}", c)
//         //                 locate_path_via_module(filepath, c.slice_from(1), outputfn);
//         //             }
//         //         }
//         //     }
//         // }


//         if line.find_str(implsearchstr).and(line.find_str(path[0])).is_some() {
//             // TODO: use parser to double check this impl
//             let implscope = scopes::end_of_next_scope(msrc.slice_from(pt));
//             println!("PHIL implscope {}",implscope);
//             locate_defn_in_module(pt, implscope, filepath, path[1], outputfn)
//         }

//         pt += line.len() + 1; // +1 for \n 
//     }
// }

// silently returns if path doesn't exist
fn search_lines(filepath: &Path, f:|~str| ) {
    let file = File::open(filepath);
    if file.is_err() { return }
    for line in BufferedReader::new(file).lines() {
        f(line.unwrap());
    }
}

pub fn locate_abs_path(path : &[&str], currentdir: &Path, outputfn : &|Match|) {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let rootname = path[0];

    let v: Vec<&str> = srcpaths.split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    let v = v.append_one(".");
    for srcpath in v.move_iter() {
        println!("PHIL searching srcpath: {} for {}",srcpath,path);
        {
            // maybe path is from crate. 
            // try lib<rootname>/lib.rs, like in the rust source dir
            let cratelibname = "lib" + rootname;
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            locate_path_in_external_module(&filepath, path.slice_from(1), outputfn);
        }
        {
            // try <rootname>/<rootname>.rs, like in the servo codebase
            let filepath = Path::new(srcpath).join_many([Path::new(rootname), 
                                                     Path::new(rootname + ".rs")]);
            locate_path_in_external_module(&filepath, path.slice_from(1), outputfn);
        }
        {
            // try <srcpath>/mod.rs
            let filepath = Path::new(srcpath).join_many([Path::new(rootname),
                                                     Path::new("mod.rs")]);
            locate_path_in_external_module(&filepath, path.slice_from(1), outputfn);
        }
        {
            // try <rootname>/lib.rs
            let filepath = Path::new(srcpath).join_many([Path::new(rootname),
                                                     Path::new("lib.rs")]);
            locate_path_in_external_module(&filepath, path.slice_from(1), outputfn);
        }
        {            
            // try just <rootname>.rs
            let filepath = Path::new(srcpath).join_many([Path::new(rootname+".rs")]);
            locate_path_in_external_module(&filepath, path.slice_from(1), outputfn);
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
                    let dir = filepath.dir_path();
                    locate_abs_path(fqn, &dir, outputfn);
                
                // if searching for a path and the last bit matches the first bit of 
                // the path then expand the path and find it
                //  (e.g. use foo;   searching for foo::bar)
                } else if path.len() > 1 && fqn[fqn.len()-1] == path[0].to_owned() {
                    let p2 = fqn + path.slice_from(1);
                    let dir = filepath.dir_path();
                    locate_abs_path(p2, &dir, outputfn);
                }
            }
        }
    });
}

fn search_crate_decls(filepath : &Path, path : &[&str], outputfn : &|Match|) {
    if path[0] == "std" {
        locate_abs_path(path, &filepath.dir_path(), outputfn);
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
                locate_abs_path(path, &filepath.dir_path(), outputfn);
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

pub fn complete_from_file(src: &str, filepath: &Path, pos: uint, outputfn: &|Match|) {
    let s = expand_searchstr(src, pos);

    let mut l = s.split_str("::");
    let path : ~[&str] = l.collect(); 

    do_search(path, filepath, pos, outputfn);
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
    //locate_path_via_module(fpath, path, outputfn);
    
    if path.len() == 1 && "std".starts_with(path[0]) {
        let m = Match {matchstr: ~"std::",
                       filepath: fpath.clone(),
                       point: 1,
                       linetxt: ~"std::",
                       mtype: Crate};
        (*outputfn)(m);
    }
}
