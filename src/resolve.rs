use racer::Match;
use racer::{do_search,first_match,to_refs};
use racer::ast;
use racer;

use std::io::File;
use std::io::BufferedReader;
use std::str;

pub fn find_stmt_start(msrc: &str, point: uint) -> uint{
    // a crappy way to do it, but programming time is short
    let mut p = 0u;
    for line in msrc.lines() {
        if point > p && point < (p + line.len()) {
            return p;
        }
        p += line.len() + 1;
    }
    fail!();
}

pub fn get_type_of(m: &Match, fpath: &Path, msrc: &str) -> Option<Match> {
    // ASSUMPTION: this is being called on a let decl
    let mut result = None;

    let point = find_stmt_start(msrc, m.point);
    for line in msrc.slice_from(point).lines() {
        ast::parse_let(line.to_owned()).map(|letres|{
            // HACK, convert from &[~str] to &[&str]
            let v = to_refs(&letres.init);
            let fqn = v.as_slice();
            result = first_match(|m| do_search(fqn, fpath, point, m));
        });
        break;
    }
    return result;
}


pub fn get_fields_of_struct(m: &Match) -> Vec<~str> {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let point = find_stmt_start(src, m.point);

    return ast::parse_struct_fields(src.slice_from(point).to_owned());
}
