use racer::Match;
use racer::{do_local_search,first_match,to_refs};
use racer::ast;
use racer::codeiter;
use racer;

use std::io::File;
use std::io::BufferedReader;
use std::str;
use std::strbuf::StrBuf;

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
    debug!("PHIL get_type_of {:?}",m);
    debug!("PHIL get_type_of {}",m.matchstr);
    // ASSUMPTION: this is being called on a let decl
    let mut result = None;

    let point = find_stmt_start(msrc, m.point);

    let src = msrc.slice_from(point);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);

        ast::parse_let(blob.to_owned()).map(|letres|{
            debug!("PHIL parse let result {}", &letres.init);
            // HACK, convert from &[~str] to &[&str]
            let v = to_refs(&letres.init);
            let fqn = v.as_slice();
            result = first_match(|m| do_local_search(fqn, fpath, point+start, racer::ExactMatch, m));
        });
        break;
    }

    debug!("PHIL let search result is {:?}",result);

    // if it's a struct then cool
    // if it's a function then we need to resolve the return type

    if result.is_none(){ 
        return result
    }

    let res = result.unwrap();
    match res.mtype {
        Struct => {
            return Some(res);
        }
        Function => {
            let path = get_return_type_of_function(&res);
            if path.len() == 0 {
                return None;
            }

            debug!("PHIL return type is {}", path);
            let v = to_refs(&path);
            let fqn = v.as_slice();
            return first_match(|m| do_local_search(fqn, &res.filepath, res.point, racer::ExactMatch, m));
        }
        _ => { return None }
    };
}

pub fn get_fields_of_struct(m: &Match) -> Vec<~str> {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let point = find_stmt_start(src, m.point);
    let structsrc = racer::scopes::end_of_next_scope(src.slice_from(point));

    return ast::parse_struct_fields(structsrc.to_owned());
}


pub fn get_return_type_of_function(fnmatch: &Match) -> Vec<~str> {
    let filetxt = BufferedReader::new(File::open(&fnmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let point = find_stmt_start(src, fnmatch.point);

    debug!("get_return_type_of_function |{}|",src.slice_from(point));
        
    let outputpath = src.slice_from(point).find_str("{").map(|n|{

        // TODO: surround this with 'impl { ... }' so that methods get parsed also
        let ss = src.slice(point, point+n+1);
        let mut decl = StrBuf::from_str(ss);
        decl = decl.append("}");
        debug!("PHIL: passing in {}",decl);
        return ast::parse_fn_output(decl.into_owned());
    }).unwrap_or(Vec::new());
    return outputpath;
}
