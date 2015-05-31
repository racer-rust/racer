#![cfg_attr(all(test, feature = "nightly"), feature(test))] // we only need test feature when testing

#[macro_use] extern crate log;

extern crate syntex_syntax;
extern crate toml;
extern crate env_logger;

#[cfg(not(test))]
use racer::Match;
#[cfg(not(test))]
use racer::util::{getline, path_exists};
#[cfg(not(test))]
use racer::nameres::{do_file_search, do_external_search, PATH_SEP};
#[cfg(not(test))]
use racer::scopes;
#[cfg(not(test))]
use std::path::Path;

pub mod racer;

#[cfg(not(test))]
fn match_with_snippet_fn(m: Match) {
    let (linenum, charnum) = scopes::point_to_coords_from_file(&m.filepath, m.point).unwrap();
    if m.matchstr == "" {
        panic!("MATCHSTR is empty - waddup?");
    }

    let snippet = racer::snippets::snippet_for_match(&m);
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
fn match_fn(m: Match) {
    let (linenum, charnum) = scopes::point_to_coords_from_file(&m.filepath, m.point).unwrap();
    if m.matchstr == "" {
        panic!("MATCHSTR is empty - waddup?");
    }
    println!("MATCH {},{},{},{},{:?},{}", m.matchstr,
                                    linenum.to_string(),
                                    charnum.to_string(),
                                    m.filepath.to_str().unwrap(),
                                    m.mtype,
                                    m.contextstr
             );
}

#[cfg(not(test))]
fn complete(match_found: &Fn(Match)) {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 3 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    match args[2].parse::<usize>() {
        Ok(linenum) => {
            // input: linenum, colnum, fname
            if args.len() < 5 {
                println!("Provide more arguments!");
                print_usage();
                std::process::exit(1);
            }
            let charnum = args[3].parse::<usize>().unwrap();
            let fname = &args[4];
            let fpath = Path::new(fname);
            let src = racer::load_file(&fpath);
            let line = &*getline(&fpath, linenum);
            let (start, pos) = racer::util::expand_ident(line, charnum);
            println!("PREFIX {},{},{}", start, pos, &line[start..pos]);

            let point = scopes::coords_to_point(&*src, linenum, charnum);
            for m in racer::complete_from_file(&*src, &fpath, point) {
                match_found(m);
            }
        }
        Err(_) => {
            // input: a command line string passed in
            let arg = &args[2];
            let it = arg.split("::");
            let p: Vec<&str> = it.collect();

            for m in do_file_search(p[0], &Path::new(".")) {
                if p.len() == 1 {
                    match_found(m);
                } else {
                    for m in do_external_search(&p[1..], &m.filepath, m.point, racer::SearchType::StartsWith, racer::Namespace::BothNamespaces) {
                        match_found(m);
                    }
                }
            }
        }
    }
}

#[cfg(not(test))]
fn prefix() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 5 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    let linenum = args[2].parse::<usize>().unwrap();
    let charnum = args[3].parse::<usize>().unwrap();
    let fname = &args[4];

    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    let line = &*getline(&path, linenum);
    let (start, pos) = racer::util::expand_ident(line, charnum);
    println!("PREFIX {},{},{}", start, pos, &line[start..pos]);
}

#[cfg(not(test))]
fn find_definition() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 5 {
        println!("Provide more arguments!");
        print_usage();
        std::process::exit(1);
    }
    let linenum = args[2].parse::<usize>().unwrap();
    let charnum = args[3].parse::<usize>().unwrap();
    let fname = &args[4];
    let fpath = Path::new(fname);
    let src = racer::load_file(&fpath);
    let pos = scopes::coords_to_point(&*src, linenum, charnum);

    racer::find_definition(&*src, &fpath, pos).map(match_fn);
}

#[cfg(not(test))]
fn print_usage() {
    let program = std::env::args().next().unwrap().clone();
    println!("usage: {} complete linenum charnum fname", program);
    println!("or:    {} find-definition linenum charnum fname", program);
    println!("or:    {} complete fullyqualifiedname   (e.g. std::io::)", program);
    println!("or:    {} prefix linenum charnum fname", program);
    println!("or replace complete with complete-with-snippet for more detailed completions.");
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
fn main() {
    env_logger::init().unwrap();
    check_rust_src_env_var();

    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        print_usage();
        std::process::exit(1);
    }

    let command = &args[1][..];
    match command {
        "prefix" => prefix(),
        "complete" => complete(&match_fn),
        "complete-with-snippet" => complete(&match_with_snippet_fn),
        "find-definition" => find_definition(),
        "help" => print_usage(),
        cmd => {
            println!("Sorry, I didn't understand command {}", cmd);
            print_usage();
            std::process::exit(1);
        }
    }
}
