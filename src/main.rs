#![feature(macro_rules)]
#![feature(phase)]
#![feature(associated_types)]
#[phase(plugin, link)] extern crate log;

extern crate syntax;
extern crate collections;
extern crate core;

#[cfg(not(test))]
use racer::Match;
#[cfg(not(test))]
use racer::util::getline;
#[cfg(not(test))]
use racer::nameres::{do_file_search, do_external_search};
#[cfg(not(test))]
use racer::scopes;

pub mod racer;

#[cfg(not(test))]
fn match_fn(m:Match) {
    let (linenum, charnum) = scopes::point_to_coords2(&m.filepath, m.point).unwrap();
    if m.matchstr.as_slice() == "" {
        panic!("MATCHSTR is empty - waddup?");
    }
    println!("MATCH {},{},{},{},{},{}", m.matchstr,
                                    linenum.to_string(),
                                    charnum.to_string(),
                                    m.filepath.as_str().unwrap(),
                                    m.mtype,
                                    m.contextstr
             );
}

#[cfg(not(test))]
fn complete() {
    let args = std::os::args();
    if args.len() < 3 {
        println!("Provide more arguments!");
        print_usage();
        std::os::set_exit_status(1);
        return;
    }
    match std::os::args().as_slice()[2].as_slice().parse::<uint>() {
        Some(linenum) => {
            // input: linenum, colnum, fname
            if args.len() < 5 {
                println!("Provide more arguments!");
                print_usage();
                std::os::set_exit_status(1);
                return;
            }
            let charnum = std::os::args().as_slice()[3].as_slice().parse::<uint>().unwrap();
            let fname = args.as_slice()[4].as_slice();
            let fpath = Path::new(fname);
            let src = racer::load_file(&fpath);
            let line = getline(&fpath, linenum);
            let (start, pos) = racer::util::expand_ident(line.as_slice(), charnum);
            println!("PREFIX {},{},{}", start, pos, line.as_slice().slice(start, pos));

            let point = scopes::coords_to_point(&*src, linenum, charnum);
            for m in racer::complete_from_file(&*src, &fpath, point) {
                match_fn(m);
            }
        }
        None => {
            // input: a command line string passed in
            let arg = args.as_slice()[2].as_slice();
            let it = arg.split_str("::");
            let p : Vec<&str> = it.collect();

            for m in do_file_search(p.as_slice()[0], &Path::new(".")) {
                if p.len() == 1 {
                    match_fn(m);
                } else {
                    for m in do_external_search(p.slice_from(1), &m.filepath, m.point, racer::SearchType::StartsWith, racer::Namespace::BothNamespaces) {
                        match_fn(m);
                    }
                }
            }
        }
    }
}

#[cfg(not(test))]
fn prefix() {
    let args_ = std::os::args();
    let args = args_.as_slice();
    if args.len() < 5 {
        println!("Provide more arguments!");
        print_usage();
        std::os::set_exit_status(1);
        return;
    }
    let linenum = args[2].as_slice().parse::<uint>().unwrap();
    let charnum = args[3].as_slice().parse::<uint>().unwrap();
    let fname = args[4].as_slice();

    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    let line = getline(&path, linenum);
    let (start, pos) = racer::util::expand_ident(line.as_slice(), charnum);
    println!("PREFIX {},{},{}", start, pos, line.as_slice().slice(start, pos));
}

#[cfg(not(test))]
fn find_definition() {
    let args_ = std::os::args();
    let args = args_.as_slice();
    if args.len() < 5 {
        println!("Provide more arguments!");
        print_usage();
        std::os::set_exit_status(1);
        return;
    }
    let linenum = args[2].as_slice().parse::<uint>().unwrap();
    let charnum = args[3].as_slice().parse::<uint>().unwrap();
    let fname = args[4].as_slice();
    let fpath = Path::new(fname);
    let src = racer::load_file(&fpath);
    let pos = scopes::coords_to_point(&*src, linenum, charnum);

    racer::find_definition(&*src, &fpath, pos).map(match_fn);
}

#[cfg(not(test))]
fn print_usage() {
    let program = std::os::args().as_slice()[0].clone();
    println!("usage: {} complete linenum charnum fname", program);
    println!("or:    {} find-definition linenum charnum fname", program);
    println!("or:    {} complete fullyqualifiedname   (e.g. std::io::)",program);
    println!("or:    {} prefix linenum charnum fname",program);
}


#[cfg(not(test))]
fn main() {
    if std::os::getenv("RUST_SRC_PATH").is_none() {
        println!("RUST_SRC_PATH environment variable must be set");
        std::os::set_exit_status(1);
        return;
    }

    let args_ = std::os::args();
    let args = args_.as_slice();

    if args.len() == 1 {
        print_usage();
        std::os::set_exit_status(1);
        return;
    }

    let command = args[1].as_slice();
    match command.as_slice() {
        "prefix" => prefix(),
        "complete" => complete(),
        "find-definition" => find_definition(),
        "help" => print_usage(),
        _ => {
            println!("Sorry, I didn't understand command {}", command );
            print_usage();
            std::os::set_exit_status(1);
            return;
        }
    }
}
