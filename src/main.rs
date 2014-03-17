use racer::{getline,expand_ident,complete_from_file,search_crate};
mod racer;

fn match_fn(l: &str, lineno: uint, file:&Path,  linetxt: &str) {
    std::io::println("COMPLETE "+l + 
                     "," + lineno.to_str() + 
                     "," + file.as_str().unwrap() + 
                     "," +  linetxt);
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
                  &|l, match_linenum, file, line| {
                     // ignore matches from the same line in the same file
                     if file.as_str().unwrap() == fname && match_linenum == linenum {
                         return;
                     }
                     match_fn(l, match_linenum, file, line);
                  });
        }
        None => {
            // input: a command line string passed in
            let arg = std::os::args()[2];
            let mut it = arg.split_str("::");
            let p : ~[&str] = it.collect();
            search_crate(p, 
                  &|l, linenum, file, line|  match_fn(l, linenum, file, line));
        }
    }
}

fn prefix() {
    let args = std::os::args().to_owned();
    let linenum = std::uint::parse_bytes(args[2].as_bytes(), 10).unwrap();
    let charnum = std::uint::parse_bytes(args[3].as_bytes(), 10).unwrap();
    let fname = args[4];

    // print the start, end, and the identifier prefix being matched
    let line = getline(fname, linenum);
    let (start, pos) = expand_ident(line, charnum);
    println!("PREFIX {},{},{}", start, pos, line.slice(start, pos));
}

fn print_usage() {
    let program = std::os::args()[0].clone();
    println!("usage: {} complete linenum charnum fname", program);
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
    match  command {
        ~"prefix" => prefix(),
        ~"complete" => complete(),
        ~"help" => print_usage(),
        _ => { 
            println!("Sorry, I didn't understand command {}", command ); 
            print_usage(); 
            return;
        }
    }
 }

