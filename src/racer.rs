extern crate std;
extern crate collections;
use std::io::File;
use std::io::BufferedReader;

fn getline(fname : &str, linenum : uint) -> ~str {
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

fn expand_ident(s : &str, pos : uint) -> (uint,uint) {
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


fn find_in_module(path : &Path, s : &str, f : &|&str,&str|) {
    let mut file = BufferedReader::new(File::open(path));
    //let modsearchstr = "pub mod "+s;
    let modsearchstr = "mod ";
    let fnsearchstr = "fn ";
    let structsearchstr = "struct ";
    let cratesearchstr = "extern crate ";
    debug!("PHIL modsearchstr {}",modsearchstr + s);
    debug!("PHIL fnsearchstr {}",fnsearchstr + s);
    debug!("PHIL structsearchstr {}",structsearchstr + s);
    for line in file.lines() {
        match line.find_str(modsearchstr+s) {
            Some(n) => {
                let end = find_end(line, n+modsearchstr.len());
                let l = line.slice(n + modsearchstr.len(), end);
                (*f)(l, line);
            }
            None => {}
        }
        match line.find_str(fnsearchstr+s) {
            Some(n) => {
                let end = find_end(line, n+3);
                let l = line.slice(n + 3, end);
                (*f)(l + "(", line);
            }
            None => {}
        }
        match line.find_str(structsearchstr+s) {
            Some(n) => {
                let end = find_end(line, n+7);
                let l = line.slice(n+7, end);
                (*f)(l, line);
            }
            None => {}
        }

        match line.find_str(cratesearchstr+s) {
            Some(n) => {
                let end = find_end(line, n+ cratesearchstr.len());
                let cratename = line.slice(n + cratesearchstr.len(), end);
                (*f)(cratename+"::", line)
            }
            None => {}
        }

        match line.find_str(s) {
            Some(_) => {
                match line.find_str("pub use ") { 
                    Some(n) => {
                        let end = find_end(line, n+8);
                        let modname = line.slice(n+8, end);
                        debug!("PHIL modname is {}",modname);

                        if modname.starts_with("self::") {
                            let mut l = modname.split_str("::");
                            let c : ~[&str] = l.collect();
                            debug!("PHIL c {}",c);
                            if c.ends_with([""]) {
                                let mut c2 = c.slice_to(c.len()-1).to_owned();
                                c2.push(s);
                                search_f(path, c2.slice_from(1), f);
                            } else if c[c.len()-1].starts_with(s) {
                                search_f(path, c.slice_from(1), f);
                            }
                        }
                    }
                    None => {}
                }
            }
            None => {}
        }
    }
}

fn search_f(path : &Path, p : &[&str], f : &|&str,&str|) {
    debug!("search_f: {} {} ",path.as_str(),p);
    
    if p.len() == 1 {
        return find_in_module(path, p[0], f);
    }

    let mut file = BufferedReader::new(File::open(path));
    let modsearchstr = "mod ";
    for line in file.lines() {
        match line.find_str(modsearchstr + p[0]) {
            Some(n) => {
                let end = find_end(line, n+modsearchstr.len());
                let l = line.slice(n + modsearchstr.len(), end);
                if p.len() == 1 {
                    (*f)(l, line);
                } else {
                    debug!("PHIL NOT Found {} {} ",l,line);
                    let dir = path.dir_path();
                    debug!("PHIL DIR {}", dir.as_str().unwrap());
                    // try searching file.rs
                    search_f(&dir.join(l+".rs"), p.tail(), f);
                    // try searching dir/mod.rs
                    search_f(&dir.join_many([l, "mod.rs"]), p.tail(), f)
                }
            }
            None => {}
        }
    }
}

fn search_in_module(path : &Path, p : &[&str], f : &|&str,&str|) {
    let mut file = BufferedReader::new(File::open(path));
    for line in file.lines() {
        match line.find_str("use ") {
            Some(_) => {
                let mut s = line.slice_from(4).trim();

                match s.find_str(p[0]) {
                    Some(_) => {
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
                    None => {}
                }
            }
            None => {}
        }
    }
}

fn search_crates(path : &Path, p : &[&str], f : &|&str,&str|) {
    if p[0] == "std" {
        search_crate(p, f);
        return;
    }

    let mut file = BufferedReader::new(File::open(path));
    for line in file.lines() {
        let searchstr = "extern crate ";
        match line.find_str(searchstr+p[0]) {
            Some(n) => {
                let end = find_end(line, n+ searchstr.len());
                let cratename = line.slice(n + searchstr.len(), end);
                if p[0] == cratename {
                    search_crate(p, f);
                }
            }
            None => {}
        }
    }
}

fn search_crate(p : &[&str], f : &|&str,&str|) {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let cratename = p[0];

    for srcpath in srcpaths.split_str(":") {
        {
            // try lib<cratename>/lib.rs, like in the rust source dir
            let cratelibname = "lib" + cratename;
            let path = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            let ss = path.dirname_str().unwrap();
            debug!("PHIL path2 {}", ss);
            debug!("PHIL p {:?}",p.slice_from(1));
            search_f(&path, p.slice_from(1), f);
        }
        {            
            // try <cratename>/<cratename>.rs, like in the servo codebase
            let path = Path::new(srcpath).join_many([Path::new(cratename), 
                                                     Path::new("cratename.rs")]);
            search_f(&path, p.slice_from(1), f);
        }
        {            
            // try <cratename>/lib.rs
            let path = Path::new(srcpath).join_many([Path::new("cratename"),
                                                     Path::new("lib.rs")]);
            search_f(&path, p.slice_from(1), f);
        }
        {            
            // try just <cratename>.rs
            let path = Path::new(srcpath).join_many([Path::new("cratename.rs")]);
            search_f(&path, p.slice_from(1), f);
        }
    }    
}

pub fn complete_from_file(fname : &str, linenum: uint, charnum: uint, 
                          f : &|&str,&str|) {
    let line = getline(fname, linenum);
    let s = expand_searchstr(line, charnum);

    let mut l = s.split_str("::");
    let c : ~[&str] = l.collect();

    search_crates(&Path::new(fname), c, f);
    search_in_module(&Path::new(fname), c, f);
    search_f(&Path::new(fname), c, f);
}

fn print_usage() {
    let program = std::os::args()[0].clone();
    println!("usage: {} complete linenum charnum fname", program);
    println!("or:    {} complete fullyqualifiedname   (e.g. std::io::)",program);
    println!("or:    {} prefix linenum charnum fname",program);
}

fn complete() {
   match std::uint::parse_bytes(std::os::args()[2].as_bytes(), 10) {
        Some(n) => { 
            // input: linenum, colnum, fname
            let linenum = n;
            let charnum = std::uint::parse_bytes(std::os::args()[3].as_bytes(), 10).unwrap();
            let fname = std::os::args()[4];

            // print the start-end of the identifier being matched
            let line = getline(fname, linenum);
            let (start, pos) = expand_ident(line, charnum);
            println!("PREFIX {},{},{}", start, pos, line.slice(start, pos));

            complete_from_file(fname, linenum, charnum, 
                 &|l, line| std::io::print("COMPLETE "+l + ","+line));
        }
        None => {
            // input: a command line string passed in
            let arg = std::os::args()[2];
            let mut it = arg.split_str("::");
            let p : ~[&str] = it.collect();
            search_crate(p, &|l, line| std::io::print("COMPLETE "+l + ","+line));
        }
    }
}

fn main() {
    match std::os::getenv("RUST_SRC_PATH") {
        Some(_) => {}
        None() => {
            println!("RUST_SRC_PATH environment variable must be set");
            return;
        }
    }

    let args = std::os::args().to_owned();

    if args.len() == 1 {
        print_usage();
        return;
    }

    let command = args[1];
    match  command {
        ~"prefix" => {
            let args = std::os::args().to_owned();
            let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
            let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
            let fname = args[4];

            // print the start, end, and the identifier prefix being matched
            let line = getline(fname, linenum);
            let (start, pos) = expand_ident(line, charnum);
            println!("PREFIX {},{},{}", start, pos, line.slice(start, pos));
        }
        ~"complete" => complete(),
        ~"help" => print_usage(),
        _ => { 
            println!("Sorry, I didn't understand command {}", command ); 
            print_usage(); 
            return;
        }
    }

 }

