extern crate std;
extern crate log;
extern crate collections;
use std::io::File;
use std::io::BufferedReader;
use std::str;

mod scopes;
mod ast;

pub struct Match {
    matchstr: ~str,
    path: Path,
    point: uint,
    linetxt: ~str    
}

pub fn getline(path : &Path, linenum : uint) -> ~str {
    let mut i = 0;
    let mut file = BufferedReader::new(File::open(path));
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

pub fn expand_fqn(s : &str, pos : uint) -> (uint,uint) {
    let sb = s.slice_to(pos);
    let mut start = pos;

    // backtrack to find start of word
    for (i, c) in sb.char_indices_rev() {
        if !is_path_char(c) {
            break;
        }
        start = i;
    }
    return (start, find_end(s, pos));
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

pub fn find_end(s : &str, pos : uint) -> uint {
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

fn find_in_module(path : &Path, s : &str, outputfn : &|Match|) {
    debug!("find_in_module {} {}",path.display(), s);
    let file = File::open(path);
    if file.is_err() { return; }
    //let modsearchstr = "pub mod "+s;
    let modsearchstr = "mod ";
    let fnsearchstr = "fn ";
    let structsearchstr = "struct ";
    let cratesearchstr = "extern crate ";
    let mut pt = 0;

    for line_r in BufferedReader::new(file).lines() {
        let line = line_r.unwrap();

        for n in line.find_str(modsearchstr+s).move_iter() {
           let end = find_end(line, n+modsearchstr.len());
           let l = line.slice(n + modsearchstr.len(), end);
            let m = Match {matchstr: l.to_owned(), 
                           path: path.clone(), 
                           point: pt + n + modsearchstr.len(), 
                           linetxt: line.to_owned()};
            (*outputfn)(m);
        }
        for n in line.find_str(fnsearchstr+s).move_iter() {
            debug!("Found {}",fnsearchstr+s);
            let end = find_end(line, n+3);
            let l = line.slice(n + 3, end);
            let m = Match {matchstr: l.to_owned(), 
                           path: path.clone(), 
                           point: pt + n + fnsearchstr.len(), 
                           linetxt: line.to_owned()};
            (*outputfn)(m);
        }
        for n in line.find_str(structsearchstr+s).move_iter() {
            let end = find_end(line, n+7);
            let l = line.slice(n+7, end);
            let m = Match {matchstr: l.to_owned(), 
                           path: path.clone(), 
                           point: pt + n + structsearchstr.len(),
                           linetxt: line.to_owned()};
            (*outputfn)(m);
        }

        for n in line.find_str(cratesearchstr+s).move_iter() {
            let end = find_end(line, n+ cratesearchstr.len());
            let cratename = line.slice(n + cratesearchstr.len(), end);
                let m = Match {matchstr: cratename+"::",
                               path: path.clone(), 
                               point: pt + n + cratesearchstr.len(), 
                               linetxt: line.to_owned()};
                (*outputfn)(m);
        }

        if line.find_str(s).is_some() {
            for n in line.find_str("pub use ").iter() { 
                let end = find_end(line, n+8);
                let modname = line.slice(n+8, end);

                if modname.starts_with("self::") {
                    let mut l = modname.split_str("::");
                    let c : ~[&str] = l.collect();
                    if c.ends_with([""]) {
                        let mut c2 = c.slice_to(c.len()-1).to_owned();
                        c2.push(s);
                        search_f(path, c2.slice_from(1), outputfn);
                        } else if c[c.len()-1].starts_with(s) {
                            search_f(path, c.slice_from(1), outputfn);
                        }
                }
            }
        }
        pt += line.len();  // no need to add 1 for \n when iterating lines
    }
}

fn search_f(path: &Path, p: &[&str], outputfn: &|Match|) {
    debug!("search_f: {} {} ",path.as_str(),p);
    
    if p.len() == 0 {
        return find_in_module(path, "", outputfn);
    }

    if p.len() == 1 {
        return find_in_module(path, p[0], outputfn);
    }

    let mut file = File::open(path);
    if file.is_err() { return }

    let modsearchstr = "mod ";
    let mut i = 0;
    let mut pt = 0;
    for line_r in BufferedReader::new(file).lines() {
        let line = line_r.unwrap();
        i+=1;
        for n in line.find_str(modsearchstr + p[0]).move_iter() {
            let end = find_end(line, n+modsearchstr.len());
            let l = line.slice(n + modsearchstr.len(), end);
            if p.len() == 1 {
                    (*outputfn)(Match {matchstr:l.to_owned(), 
                                       path:path.clone(), 
                                       point:pt+n+modsearchstr.len(),
                                       linetxt:line.to_owned()
                    });
            } else {
                debug!("PHIL following: {}: {} ",l,line);
                let dir = path.dir_path();
                debug!("PHIL DIR {}", dir.as_str().unwrap());
                // try searching file.rs
                search_f(&dir.join(l+".rs"), p.tail(), outputfn);
                // try searching dir/mod.rs
                search_f(&dir.join_many([l, "mod.rs"]), p.tail(), outputfn)
            }
        }
        pt += line.len();  // +1 for /n
    }
}

// pub struct MyIter {
//     iter: ~Iterator<std::io::IoResult<~str>>
// }

// impl Iterator<~str> for MyIter {
//     fn next(&mut self) -> Option<~str> {
//         let l = self.iter.next();
//         match l {
//             Some(x) => x.unwrap(),
//             None => None
//         }
//     }
// }


// silently returns if path doesn't exist
fn search_lines(path: &Path, f:|~str| ) {
    let mut file = File::open(path);
    if file.is_err() { return }
    for line in BufferedReader::new(file).lines() {
        f(line.unwrap());
    }
}


fn search_use_imports(path : &Path, p : &[&str], outputfn : &|Match|) {
    search_lines(path, |line|{
        if line.find_str("use ").is_some() {
            let mut s = line.slice_from(4).trim();

            if s.find_str(p[0]).is_some() {
                let end = find_end(s, 0);
                s = s.slice(0, end);
                let pieces : ~[&str] = s.split_str("::").collect();
                if p.len() == 1 && pieces[pieces.len()-1].starts_with(p[0]) {
                    search_crate(pieces, outputfn);
                } else if p.len() > 1 && pieces[pieces.len()-1] == p[0] {
                    let p2 = pieces + p.slice_from(1);
                    search_crate(p2, outputfn);
                }
            }
        }
    });


    // let mut file = File::open(path);
    // if file.is_err() { return }
    // for line_r in BufferedReader::new(file).lines() {
    //     let line = line_r.unwrap();
    //     if line.find_str("use ").is_some() {
    //         let mut s = line.slice_from(4).trim();

    //         if s.find_str(p[0]).is_some() {
    //             let end = find_end(s, 0);
    //             s = s.slice(0, end);
    //             let pieces : ~[&str] = s.split_str("::").collect();
    //             if p.len() == 1 && pieces[pieces.len()-1].starts_with(p[0]) {
    //                 search_crate(pieces, outputfn);
    //             } else if p.len() > 1 && pieces[pieces.len()-1] == p[0] {
    //                 let p2 = pieces + p.slice_from(1);
    //                 search_crate(p2, outputfn);
    //             }
    //         }
    //     }
    // }
}

fn search_crates(path : &Path, p : &[&str], outputfn : &|Match|) {
    if p[0] == "std" {
        search_crate(p, outputfn);
        return;
    }

    let file = File::open(path);
    if file.is_err() {return}

    for line_r in BufferedReader::new(file).lines() {
        let searchstr = "extern crate ";
        let line = line_r.unwrap();
        for n in line.find_str(searchstr+p[0]).iter() {
            let end = find_end(line, n+ searchstr.len());
            let cratename = line.slice(n + searchstr.len(), end);
            if p[0] == cratename {
                search_crate(p, outputfn);
            }
        }
    }
}

pub fn search_crate(p : &[&str], outputfn : &|Match|) {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let cratename = p[0];

    for srcpath in srcpaths.split_str(":") {
        {
            // try lib<cratename>/lib.rs, like in the rust source dir
            let cratelibname = "lib" + cratename;
            let path = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            search_f(&path, p.slice_from(1), outputfn);
        }
        {            
            // try <cratename>/<cratename>.rs, like in the servo codebase
            let path = Path::new(srcpath).join_many([Path::new(cratename), 
                                                     Path::new("cratename.rs")]);
            search_f(&path, p.slice_from(1), outputfn);
        }
        {            
            // try <cratename>/lib.rs
            let path = Path::new(srcpath).join_many([Path::new("cratename"),
                                                     Path::new("lib.rs")]);
            search_f(&path, p.slice_from(1), outputfn);
        }
        {            
            // try just <cratename>.rs
            let path = Path::new(srcpath).join_many([Path::new("cratename.rs")]);
            search_f(&path, p.slice_from(1), outputfn);
        }
    }    
}

fn search_for_let(src:&str, searchstr:&str, path:&Path, 
                  outputfn : &|Match|) {
    for line in src.lines() {
        // search for let statements
        for n in line.find_str("let "+searchstr).iter() {
            let end = find_end(line, n+"let ".len());
            let l = line.slice(n+"let ".len(), end);
            // TODO - make linenum something correct
            let lineno = 1;
            (*outputfn)(Match { matchstr: l.to_owned(),
                                path: path.clone(),
                                point: 1,
                                linetxt: line.to_owned()});
        }
    }
}

fn search_scope(searchstr:&str, path: &Path, msrc: &str, mut point:uint,
                          outputfn : &|Match|) {
    while point > 0 {
        let n = scopes::scope_start(msrc, point);
        let s = scopes::mask_sub_scopes(msrc.slice(n,point));
        search_for_let(s, searchstr, path, outputfn);
        if n == 0 { 
            break; 
        }
        point = n-1;
    }
}

fn search_file_text(searchstr:&str, path: &Path, linenum: uint, charnum: uint, 
                          outputfn : &|Match|) {
    let filetxt = BufferedReader::new(File::open(path)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt).unwrap();
    let msrc = scopes::mask_comments(src);
    let point = scopes::coords_to_point(src, linenum, charnum);

    let mut l = searchstr.split_str(".");
    let bits : ~[&str] = l.collect();
    
    if bits.len() == 1 {
        search_scope(searchstr, path, msrc, point, outputfn); 
    } else {
        // field reference. 
        //get_type_of(bits);
    }
}

fn convert_output(m: &Match, outputfn: &|&str,uint,&Path,&str|) {
    let filetxt = BufferedReader::new(File::open(&m.path)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt).unwrap();
    let (line, _) = scopes::point_to_coords(src, m.point);
    (*outputfn)(m.matchstr, line, &m.path, m.linetxt);
}

pub fn complete_from_file(path: &Path, linenum: uint, charnum: uint, 
                          outputfn : &|Match|) {
    let line = getline(path, linenum);
    let s = expand_searchstr(line, charnum);

    let mut l = s.split_str("::");
    let bits : ~[&str] = l.collect(); 

    if bits.len() == 1 {
       search_file_text(bits[0], path, linenum, charnum, outputfn); 
    }
    search_crates(path, bits, outputfn);
    search_use_imports(path, bits, outputfn);
    search_f(path, bits, outputfn);
    
    if bits.len() == 1 && "std".starts_with(bits[0]) {
        let m = Match {matchstr: ~"std::",
                       path: path.clone(),
                       point: 1,
                       linetxt: ~"std::"};
        (*outputfn)(m);
    }
}

pub fn find_definition(path :Path, linenum: uint, charnum: uint, outputfn: &|Match|) {
    let line = getline(&path, linenum);
    let (start, end) = expand_fqn(line, charnum);
    let s = line.slice(start, end);

    let mut l = s.split_str("::");
    let bits : ~[&str] = l.collect(); 

    let find_definition_output_fn = &|m: Match| {
        debug!("PHIL outputfn match {:?}. comparing to {:?}",m.matchstr, bits[bits.len()-1].to_owned());
        if m.matchstr == bits[bits.len()-1].to_owned() {  // only if is an exact match
            (*outputfn)(m);
        }
    };

    if bits.len() == 1 {
       search_file_text(bits[0], &path, linenum, charnum, find_definition_output_fn); 
    }
    search_crates(&path, bits, find_definition_output_fn);
    search_use_imports(&path, bits, find_definition_output_fn);
    search_f(&path, bits, find_definition_output_fn);    
}
