#![feature(phase)]
#[phase(plugin, link)] extern crate log;


extern crate syntax;
extern crate time;
extern crate debug;

#[cfg(not(test))]
use racer::{getline,Match,do_file_search, do_external_search};
#[cfg(not(test))]
use std::io::File;
#[cfg(not(test))]
use std::io::BufferedReader;
#[cfg(not(test))]
use std::str;
#[cfg(not(test))]
use racer::scopes;

pub mod racer;

#[cfg(not(test))]
fn match_fn(m:Match) {
    let (linenum, charnum) = scopes::point_to_coords2(&m.filepath, m.point).unwrap();
    if m.matchstr.as_slice() == "" {
        fail!("MATCHSTR is empty - waddup?");
    }
    println!("MATCH {},{},{},{}", m.matchstr,
                                    linenum.to_str(),
                                    charnum.to_str(),
                                    m.filepath.as_str().unwrap());
}

#[cfg(not(test))]
fn complete() {
    let args = std::os::args();
    match std::uint::parse_bytes(std::os::args().as_slice()[2].as_bytes(), 10) {
        Some(linenum) => { 
            // input: linenum, colnum, fname
            let charnum = std::uint::parse_bytes(std::os::args().as_slice()[3].as_bytes(), 10).unwrap();
            let fname = args.as_slice()[4].as_slice();
            let fpath = Path::new(fname);
            let filetxt = BufferedReader::new(File::open(&fpath)).read_to_end().unwrap();
            // print the start-end of the identifier being matched
            let src = str::from_utf8(filetxt.as_slice()).unwrap();
            let line = getline(&fpath, linenum);
            let (start, pos) = racer::expand_ident(line.as_slice(), charnum);
            println!("PREFIX {},{},{}", start, pos, line.as_slice().slice(start, pos));

            let point = scopes::coords_to_point(src, linenum, charnum);
            racer::complete_from_file(src, &fpath, point, &mut |m| match_fn(m));
        }
        None => {
            // input: a command line string passed in
            let arg = args.as_slice()[2].as_slice();
            let mut it = arg.split_str("::");
            let p : Vec<&str> = it.collect();

            do_file_search(p.as_slice()[0], &Path::new("."), &mut |m| {
                if p.len() == 1 {
                    match_fn(m);
                } else {
                    do_external_search(p.slice_from(1), &m.filepath, m.point, racer::StartsWith, &mut |m| match_fn(m));
                }
            });

        }
    }
}

#[cfg(not(test))]
fn prefix() {
    let args_ = std::os::args();
    let args = args_.as_slice();
    let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
    let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
    let fname = args[4].as_slice();

    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    let line = getline(&path, linenum);
    let (start, pos) = racer::expand_ident(line.as_slice(), charnum);
    println!("PREFIX {},{},{}", start, pos, line.as_slice().slice(start, pos));
}

#[cfg(not(test))]
fn find_definition() {
    let args_ = std::os::args();
    let args = args_.as_slice();
    let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
    let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
    let fname = args[4].as_slice();
    let fpath = Path::new(fname);
    let filetxt = BufferedReader::new(File::open(&fpath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let pos = scopes::coords_to_point(src, linenum, charnum);

    racer::find_definition(src, &fpath, pos).map(match_fn);
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
