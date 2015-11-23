#![cfg_attr(all(test, feature = "nightly"), feature(test))] // we only need test feature when testing

#[macro_use] extern crate log;

extern crate syntex_syntax;
extern crate toml;
extern crate env_logger;

extern crate racer;

#[cfg(not(test))]
use racer::core;
#[cfg(not(test))]
use racer::util;
#[cfg(not(test))]
use racer::core::Match;
#[cfg(not(test))]
use racer::util::{getline, path_exists};
#[cfg(not(test))]
use racer::nameres::{do_file_search, do_external_search, PATH_SEP};
#[cfg(not(test))]
use racer::scopes;
#[cfg(not(test))]
use std::path::Path;

#[cfg(not(test))]
fn match_with_snippet_fn(m: Match, session: core::SessionRef) {
    let (linenum, charnum) = scopes::point_to_coords_from_file(&m.filepath, m.point, session).unwrap();
    if m.matchstr == "" {
        panic!("MATCHSTR is empty - waddup?");
    }

    let snippet = racer::snippets::snippet_for_match(&m, session);
    println!("MATCH {};{};{};{};{};{:?};{}", m.matchstr,
                                    snippet,
                                    linenum.to_string(),
                                    charnum.to_string(),
                                    m.filepath.to_str().unwrap(),
                                    m.mtype,
                                    m.contextstr
             );
}

#[cfg(not(test))]
fn match_fn(m: Match, session: core::SessionRef) {
    if let Some((linenum, charnum)) = scopes::point_to_coords_from_file(&m.filepath,
                                                                        m.point,
                                                                        session) {
        println!("MATCH {},{},{},{},{:?},{}", m.matchstr,
                                    linenum.to_string(),
                                    charnum.to_string(),
                                    m.filepath.to_str().unwrap(),
                                    m.mtype,
                                    m.contextstr
                 );
    } else {
        error!("Could not resolve file coords for match {:?}", m);
    }
}

#[cfg(not(test))]
fn complete(args: Vec<String>, print_type: CompletePrinter) {
    if args.len() < 1 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    match args[0].parse::<usize>() {
        Ok(linenum) => {
            complete_by_line_coords(args, linenum, print_type);
        }
        Err(_) => {
            external_complete(&args);
        }
    }
}

#[cfg(not(test))]
fn complete_by_line_coords(args: Vec<String>, 
                           linenum: usize, 
                           print_type: CompletePrinter) {
    // input: linenum, colnum, fname
    let tb = std::thread::Builder::new().name("searcher".to_string());

    // PD: this probably sucks for performance, but lots of plugins
    // end up failing and leaving tmp files around if racer crashes, 
    // so catch the crash.
    let res = tb.spawn(move || {
        run_the_complete_fn(args, linenum, print_type);
    }).unwrap();
    match res.join() {
        Ok(_) => {},
        Err(e) => {
            error!("Search thread paniced: {:?}", e);
        }
    }

    println!("END");

}

#[cfg(not(test))]
enum CompletePrinter {
    Normal,
    WithSnippets
}

#[cfg(not(test))]
fn run_the_complete_fn(args: Vec<String>, linenum: usize, print_type: CompletePrinter) {
        if args.len() < 3 {
            println!("Provide more arguments!");
            print_usage();
            std::process::exit(1);
        }
        let charnum = args[1].parse::<usize>().unwrap();
        let fname = &args[2];
        let substitute_file = Path::new(match args.len() > 3 {
            true => &args[3],
            false => fname
        });

    let fpath = Path::new(fname);
    let session = core::Session::from_path(&fpath, &substitute_file);
    let src = session.load_file(&fpath);
    let line = &getline(&substitute_file, linenum, &session);
    let (start, pos) = util::expand_ident(line, charnum);
    println!("PREFIX {},{},{}", start, pos, &line[start..pos]);

    let point = scopes::coords_to_point(&src, linenum, charnum);

    for m in core::complete_from_file(&src, &fpath, point, &session) {
        match print_type {
            CompletePrinter::Normal => match_fn(m, &session),
            CompletePrinter::WithSnippets => match_with_snippet_fn(m, &session),
        };
    }
}


#[cfg(not(test))]
fn external_complete(args: &[String]) {
    // input: a command line string passed in
    let arg = &args[0];
    let it = arg.split("::");
    let p: Vec<&str> = it.collect();
    let session = core::Session::from_path(&Path::new("."), &Path::new("."));
    
    for m in do_file_search(p[0], &Path::new(".")) {
        if p.len() == 1 {
            match_fn(m, &session);
        } else {
            for m in do_external_search(&p[1..], &m.filepath, m.point,
                                        core::SearchType::StartsWith,
                                        core::Namespace::BothNamespaces, &session) {
                match_fn(m, &session);
            }
        }
    }
}

#[cfg(not(test))]
fn prefix(args: &[String]) {
    if args.len() < 3 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    let linenum = args[0].parse::<usize>().unwrap();
    let charnum = args[1].parse::<usize>().unwrap();
    let fname = &args[2];
    let substitute_file = Path::new(match args.len() > 3 {
        true => &args[3],
        false => fname
    });
    let fpath = Path::new(&fname);
    let session = core::Session::from_path(&fpath, &substitute_file);

    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    let line = &getline(&path, linenum, &session);
    let (start, pos) = util::expand_ident(line, charnum);
    println!("PREFIX {},{},{}", start, pos, &line[start..pos]);
}

#[cfg(not(test))]
fn find_definition(args: &[String]) {
    if args.len() < 3 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    let linenum = args[0].parse::<usize>().unwrap();
    let charnum = args[1].parse::<usize>().unwrap();
    let fname = &args[2];
    let substitute_file = Path::new(match args.len() > 3 {
        true => &args[3],
        false => fname
    });
    let fpath = Path::new(&fname);
    let session = core::Session::from_path(&fpath, &substitute_file);
    let src = session.load_file(&fpath);
    let pos = scopes::coords_to_point(&src, linenum, charnum);

    core::find_definition(&src, &fpath, pos, &session).map(|m| match_fn(m, &session));
    println!("END");
}

#[cfg(not(test))]
fn print_usage() {
    let program = std::env::args().next().unwrap().clone();
    println!("usage: {} complete linenum charnum fname [substitute_file]", program);
    println!("or:    {} find-definition linenum charnum fname [substitute_file]", program);
    println!("or:    {} complete fullyqualifiedname   (e.g. std::io::)", program);
    println!("or:    {} prefix linenum charnum fname", program);
    println!("or replace complete with complete-with-snippet for more detailed completions.");
    println!("or:    {} daemon     - to start a process that receives the above commands via stdin", program);
}

#[cfg(not(test))]
fn check_rust_src_env_var() {
    if let Ok(srcpaths) = std::env::var("RUST_SRC_PATH") {
        let v = srcpaths.split(PATH_SEP).collect::<Vec<_>>();
        if !v.is_empty() {
            let f = Path::new(v[0]);
            if !path_exists(f) {
                println!("racer can't find the directory pointed to by the RUST_SRC_PATH variable \"{}\". Try using an absolute fully qualified path and make sure it points to the src directory of a rust checkout - e.g. \"/home/foouser/src/rust/src\".", srcpaths);
                std::process::exit(1);
            } else if !path_exists(f.join("libstd")) {
                println!("Unable to find libstd under RUST_SRC_PATH. N.B. RUST_SRC_PATH variable needs to point to the *src* directory inside a rust checkout e.g. \"/home/foouser/src/rust/src\". Current value \"{}\"", srcpaths);
                std::process::exit(1);
            }
        }
    } else {
        println!("RUST_SRC_PATH environment variable must be set to point to the src directory of a rust checkout. E.g. \"/home/foouser/src/rust/src\"");
        std::process::exit(1);
    }
}

#[cfg(not(test))]
fn daemon() {
    use std::io;
    let mut input = String::new();
    while let Ok(n) = io::stdin().read_line(&mut input) {
        if n == 0 {
            break;
        }
        let args: Vec<String> = input.split(" ").map(|s| s.trim().to_string()).collect();
        run(args);
        
        input.clear();
    }
}


#[cfg(not(test))]
fn main() {
    // make sure we get a stack trace ifwe panic
    ::std::env::set_var("RUST_BACKTRACE","1");

    env_logger::init().unwrap();
    check_rust_src_env_var();

    let mut args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        print_usage();
        std::process::exit(1);
    }

    args.remove(0);
    run(args);
}

#[cfg(not(test))]
fn run(mut args: Vec<String>) {
    //let command  = &args[0];
    let command = args.remove(0);
    match &command[..] {
        "daemon" => daemon(),
        "prefix" => prefix(&args),
        "complete" => complete(args, CompletePrinter::Normal),
        "complete-with-snippet" => complete(args, CompletePrinter::WithSnippets),
        "find-definition" => find_definition(&args),
        "help" => print_usage(),
        cmd => {
            println!("Sorry, I didn't understand command {}", cmd);
            print_usage();
            std::process::exit(1);
        }
    }
}
