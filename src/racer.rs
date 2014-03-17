extern crate std;
extern crate collections;
use std::io::File;
use std::io::BufferedReader;
use std::str;

mod scopes;

pub fn getline(fname : &str, linenum : uint) -> ~str {
    let path = Path::new(fname);
    let mut i = 0;
    let mut file = BufferedReader::new(File::open(&path));
    for line in file.lines() {
        //print!("{}", line);
        i += 1;
        if i == linenum {
            return line.to_owned();
        }
    }
    return ~"not found";
}

fn is_path_char(c : char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!') || (c == ':')
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

fn expand_searchstr(s : &str, pos : uint) -> ~str {
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

fn find_end(s : &str, pos : uint) -> uint {
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


fn find_in_module(path : &Path, s : &str, outputfn : &|&str,uint,&Path,&str|) {
    let mut file = BufferedReader::new(File::open(path));
    //let modsearchstr = "pub mod "+s;
    let modsearchstr = "mod ";
    let fnsearchstr = "fn ";
    let structsearchstr = "struct ";
    let cratesearchstr = "extern crate ";
    let mut i = 0;
    for line in file.lines() {
        i += 1;
        for n in line.find_str(modsearchstr+s).iter() {
           let end = find_end(line, n+modsearchstr.len());
           let l = line.slice(n + modsearchstr.len(), end);
           (*outputfn)(l, i, path, line);
        }
        for n in line.find_str(fnsearchstr+s).iter() {
            let end = find_end(line, n+3);
            let l = line.slice(n + 3, end);
            (*outputfn)(l, i, path, line);
        }
        for n in line.find_str(structsearchstr+s).iter() {
            let end = find_end(line, n+7);
            let l = line.slice(n+7, end);
            (*outputfn)(l, i, path, line);
        }

        for n in line.find_str(cratesearchstr+s).iter() {
            let end = find_end(line, n+ cratesearchstr.len());
            let cratename = line.slice(n + cratesearchstr.len(), end);
            (*outputfn)(cratename+"::", i, path, line)
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
    }
}

fn search_f(path: &Path, p: &[&str], outputfn: &|&str,uint,&Path,&str|) {
    debug!("search_f: {} {} ",path.as_str(),p);
    
    if p.len() == 0 {
        return find_in_module(path, "", outputfn);
    }


    if p.len() == 1 {
        return find_in_module(path, p[0], outputfn);
    }

    let mut file = BufferedReader::new(File::open(path));
    let modsearchstr = "mod ";
    let mut i = 0;
    for line in file.lines() {
        i+=1;
        for n in line.find_str(modsearchstr + p[0]).iter() {
            let end = find_end(line, n+modsearchstr.len());
            let l = line.slice(n + modsearchstr.len(), end);
            if p.len() == 1 {
                (*outputfn)(l, i, path,line);
            } else {
                debug!("PHIL NOT Found {} {} ",l,line);
                let dir = path.dir_path();
                debug!("PHIL DIR {}", dir.as_str().unwrap());
                // try searching file.rs
                search_f(&dir.join(l+".rs"), p.tail(), outputfn);
                // try searching dir/mod.rs
                search_f(&dir.join_many([l, "mod.rs"]), p.tail(), outputfn)
            }
        }
    }
}


fn search_use_imports(path : &Path, p : &[&str], f : &|&str,uint,&Path,&str|) {
    let mut file = BufferedReader::new(File::open(path));
    for line in file.lines() {
        if line.find_str("use ").is_some() {
            let mut s = line.slice_from(4).trim();

            if s.find_str(p[0]).is_some() {
                let end = find_end(s, 0);
                s = s.slice(0, end);
                let pieces : ~[&str] = s.split_str("::").collect();
                if p.len() == 1 && pieces[pieces.len()-1].starts_with(p[0]) {
                    search_crate(pieces, f);
                } else if p.len() > 1 && pieces[pieces.len()-1] == p[0] {
                    let p2 = pieces + p.slice_from(1);
                    search_crate(p2, f);
                }
            }
        }
    }
}

fn search_crates(path : &Path, p : &[&str], outputfn : &|&str,uint,&Path,&str|) {
    if p[0] == "std" {
        search_crate(p, outputfn);
        return;
    }

    let mut file = BufferedReader::new(File::open(path));
    for line in file.lines() {
        let searchstr = "extern crate ";
        for n in line.find_str(searchstr+p[0]).iter() {
            let end = find_end(line, n+ searchstr.len());
            let cratename = line.slice(n + searchstr.len(), end);
            if p[0] == cratename {
                search_crate(p, outputfn);
            }
        }
    }
}

pub fn search_crate(p : &[&str], outputfn : &|&str,uint,&Path,&str|) {
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
                  outputfn : &|&str,uint,&Path,&str|) {
    for line in src.lines() {
        println!("PHIL l {}",line);
        // search for let statements
        for n in line.find_str("let "+searchstr).iter() {
            let end = find_end(line, n+"let ".len());
            let l = line.slice(n+"let ".len(), end);
            // TODO - make linenum something correct
            let lineno = 1;
            println!("PHIL MATCH! {} :-> {}",l, line);
            (*outputfn)(l, lineno, path, line);
        }
    }
    println!("PHIL HERE155");
}

fn search_scope(searchstr:&str, path: &Path, linenum: uint, charnum: uint, 
                          outputfn : &|&str,uint,&Path,&str|) {
    let filetxt = BufferedReader::new(File::open(path)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt).unwrap();
    let msrc = scopes::mask_comments(src);
    let mut point = scopes::coords_to_point(src, linenum, charnum);

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

pub fn complete_from_file(fname : &str, linenum: uint, charnum: uint, 
                          outputfn : &|&str,uint,&Path,&str|) {
    let line = getline(fname, linenum);
    let s = expand_searchstr(line, charnum);

    let mut l = s.split_str("::");
    let c : ~[&str] = l.collect();

    if c.len() == 1 {
        search_scope(c[0], &Path::new(fname), linenum, charnum, outputfn);
    }
    search_crates(&Path::new(fname), c, outputfn);
    search_use_imports(&Path::new(fname), c, outputfn);
    search_f(&Path::new(fname), c, outputfn);
    
    if c.len() == 1 && "std".starts_with(c[0]) {
        (*outputfn)("std::",1,&Path::new(fname),"std::");
    }

}

