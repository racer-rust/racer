#[feature(managed_boxes)];   // need this to use libsyntax
#[feature(phase)];
#[phase(syntax, link)] extern crate log;
extern crate syntax;

use racer::{getline,search_crate,Match};
mod racer;
mod scopes;

//std::str::is

fn match_fn(m:Match) {
    let (linenum, charnum) = scopes::point_to_coords2(&m.path, m.point).unwrap();
    std::io::println("MATCH "+m.matchstr + 
                     "," + linenum.to_str() + 
                     "," + charnum.to_str() + 
                     "," + m.path.as_str().unwrap() + 
                     "," +  m.linetxt);    
}

fn complete() {
   match std::uint::parse_bytes(std::os::args()[2].as_bytes(), 10) {
        Some(n) => { 
            // input: linenum, colnum, fname
            let linenum = n;
            let charnum = std::uint::parse_bytes(std::os::args()[3].as_bytes(), 10).unwrap();
            let fname = std::os::args()[4];

            // print the start-end of the identifier being matched
            let path = Path::new(fname);
            let line = getline(&path, linenum);
            let (start, pos) = racer::expand_ident(line, charnum);
            println!("PREFIX {},{},{}", start, pos, line.slice(start, pos));

            racer::complete_from_file(&path, linenum, charnum, &|m| match_fn(m));
        }
        None => {
            // input: a command line string passed in
            let arg = std::os::args()[2];
            let mut it = arg.split_str("::");
            let p : ~[&str] = it.collect();
            search_crate(p, &|m|  match_fn(m));
        }
    }
}

fn prefix() {
    let args = std::os::args().to_owned();
    let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
    let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
    let fname = args[4];

    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    let line = getline(&path, linenum);
    let (start, pos) = racer::expand_ident(line, charnum);
    println!("PREFIX {},{},{}", start, pos, line.slice(start, pos));
}

fn find_definition() {
    let args = std::os::args().to_owned();
    let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
    let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
    let fname = args[4];


    // print the start, end, and the identifier prefix being matched
    let path = Path::new(fname);
    racer::find_definition(path, linenum, charnum, &|m| match_fn(m));
}

fn print_usage() {
    let program = std::os::args()[0].clone();
    println!("usage: {} complete linenum charnum fname", program);
    println!("or:    {} find-definition linenum charnum fname", program);
    println!("or:    {} complete fullyqualifiedname   (e.g. std::io::)",program);
    println!("or:    {} prefix linenum charnum fname",program);
}


fn main() {
    if std::os::getenv("RUST_SRC_PATH").is_none() {
        println!("RUST_SRC_PATH environment variable must be set");
        return;
    }

    let args = std::os::args().to_owned();

    if args.len() == 1 {
        print_usage();
        return;
    }

    let command = args[1];
    match command.as_slice() {
        &"prefix" => prefix(),
        &"complete" => complete(),
        &"find-definition" => find_definition(),
        &"help" => print_usage(),
        _ => { 
            println!("Sorry, I didn't understand command {}", command ); 
            print_usage(); 
            return;
        }
    }
}
