use racer::{Match, Struct, Function, Let, FnArg};
use racer::{do_local_search,do_local_search_with_string,first_match,to_refs};
use racer::ast;
use racer::codeiter;
use racer::scopes;
use racer;

use std::io::File;
use std::io::BufferedReader;
use std::str;
use std::strbuf::StrBuf;

fn find_start_of_function_body(src: &str) -> uint {
    // TODO: this should ignore anything inside parens so as to skip the arg list
    return src.find_str("{").unwrap();
}

fn get_type_of_fnarg(m: &Match, fpath: &Path, msrc: &str) -> Option<Match> {
    debug!("PHIL get type of fn arg {:?}",m);
    let point = scopes::find_stmt_start(msrc, m.point).unwrap();
    for (start,end) in codeiter::iter_stmts(msrc.slice_from(point)) { 
        let blob = msrc.slice(point+start,point+end);

        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut s = StrBuf::new();
        s.push_str("impl blah {");
        let impl_header_len = s.len();
        s.push_str(blob.slice_to(find_start_of_function_body(blob)+1));
        s.push_str("}}");
        let fn_ = ast::parse_fn(s);
        let mut result = None;
        for (_/*name*/, pos, ty_) in fn_.args.move_iter() {
            let globalpos = pos - impl_header_len + start + point;
            if globalpos == m.point && ty_.len() != 0 {
                let v = to_refs(&ty_);
                let fqn = v.as_slice();
                result = first_match(|m| do_local_search_with_string(fqn, 
                                                                     fpath, 
                                                                     globalpos, 
                                                                     racer::ExactMatch, m));
            }
        }
        return result;
    }
    None
}

fn get_type_of_let_expr(m: &Match, fpath: &Path, msrc: &str) -> Option<Match> {
    // ASSUMPTION: this is being called on a let decl
    let mut result = None;

    let opoint = scopes::find_stmt_start(msrc, m.point);
    let point = opoint.unwrap();

    let src = msrc.slice_from(point);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);
        
        ast::parse_let(StrBuf::from_str(blob)).map(|letres|{
            debug!("PHIL parse let result {}", &letres.init);
            // HACK, convert from &[~str] to &[&str]
            let v = to_refs(&letres.init);
            let fqn = v.as_slice();
            if fqn.len() != 0 {
                result = first_match(|m| do_local_search(fqn, fpath, point+start, racer::ExactMatch, m));
            }
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
            return first_match(|m| do_local_search_with_string(fqn, &res.filepath, res.point, racer::ExactMatch, m));
        }
        _ => { return None }
    };
}


pub fn get_type_of_OLD(m: &Match, fpath: &Path, msrc: &str) -> Option<Match> {
    debug!("PHIL get_type_of OLD {:?}",m);
    debug!("PHIL get_type_of OLD {}",m.matchstr);

    match m.mtype {
        Let => get_type_of_let_expr(m, fpath, msrc),
        FnArg => get_type_of_fnarg(m, fpath, msrc),
        _ => { println!("Can't get type of {:?}",m.mtype); None }
    }
}

pub fn get_fields_of_struct(m: &Match) -> Vec<(StrBuf, uint)> {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, m.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    return ast::parse_struct_fields(StrBuf::from_str(structsrc));
}


pub fn get_return_type_of_function(fnmatch: &Match) -> Vec<StrBuf> {
    let filetxt = BufferedReader::new(File::open(&fnmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let point = scopes::find_stmt_start(src, fnmatch.point).unwrap();

    debug!("get_return_type_of_function |{}|",src.slice_from(point));
    
    let outputpath = src.slice_from(point).find_str("{").map(|n|{

        // TODO: surround this with 'impl { ... }' so that methods get parsed also
        let ss = src.slice(point, point+n+1);
        let mut decl = StrBuf::from_str(ss);
        decl = decl.append("}");
        debug!("PHIL: passing in {}",decl);
        return ast::parse_fn_output(decl);
    }).unwrap_or(Vec::new());

    return outputpath;
}
