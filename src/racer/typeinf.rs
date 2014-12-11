// Type inference

use racer::{Match};
use racer::nameres::{resolve_path_with_str};
use racer::{ast,codeiter,scopes};
use racer;

use std::io::File;
use std::io::BufferedReader;
use std::{str};

use racer::SearchType::ExactMatch;
use racer::Namespace::TypeNamespace;
use racer::util::txt_matches;

fn find_start_of_function_body(src: &str) -> uint {
    // TODO: this should ignore anything inside parens so as to skip the arg list
    return src.find_str("{").unwrap();
}

// Removes the body of the statement (anything in the braces {...}), leaving just
// the header 
// TODO: this should skip parens (e.g. function arguments)
pub fn generate_skeleton_for_parsing(src: &str) -> String {
    let mut s = String::new();
    let n = src.find_str("{").unwrap();
    s.push_str(src.slice_to(n+1));
    s.push_str("};");
    return s;
}

pub fn first_param_is_self(blob: &str) -> bool {
    return blob.find_str("(").map_or(false, |start| {
        let end = scopes::find_closing_paren(blob, start+1);
        debug!("searching fn args: |{}| {}",blob.slice(start+1,end), txt_matches(ExactMatch, "self", blob.slice(start+1,end)));
        return txt_matches(ExactMatch, "self", blob.slice(start+1,end));
    });
}


#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {};", out.as_slice());
}

fn get_type_of_self_arg(m: &Match, msrc: &str) -> Option<racer::Ty> {
    debug!("get_type_of_self_arg {}", m)
    return scopes::find_impl_start(msrc, m.point, 0).and_then(|start| {
        let decl = generate_skeleton_for_parsing(msrc.slice_from(start));
        debug!("get_type_of_self_arg impl skeleton |{}|", decl)
        
        if decl.as_slice().starts_with("impl") {
            let implres = ast::parse_impl(decl);
            debug!("get_type_of_self_arg implres |{}|", implres);
            return resolve_path_with_str(&implres.name_path.expect("failed parsing impl name"), 
                                         &m.filepath, start,
                                         ExactMatch, TypeNamespace).nth(0).map(|m| racer::Ty::TyMatch(m));
        } else {
            // // must be a trait
            return ast::parse_trait(decl).name.and_then(|name| {
                Some(racer::Ty::TyMatch(Match {matchstr: name,
                           filepath: m.filepath.clone(), 
                           point: start,
                           local: m.local,
                           mtype: racer::MatchType::Trait,
                           contextstr: racer::matchers::first_line(msrc.slice_from(start)),
                           generic_args: Vec::new(), generic_types: Vec::new()
                }))
            });
        }
    });
}

fn get_type_of_fnarg(m: &Match, msrc: &str) -> Option<racer::Ty> {
    debug!("get type of fn arg {}",m);

    if m.matchstr.as_slice() == "self" {
        return get_type_of_self_arg(m, msrc);
    }

    let point = scopes::find_stmt_start(msrc, m.point).unwrap();
    for (start,end) in codeiter::iter_stmts(msrc.slice_from(point)) { 
        let blob = msrc.slice(point+start,point+end);
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut s = String::new();
        s.push_str("impl blah {");
        let impl_header_len = s.len();
        s.push_str(blob.slice_to(find_start_of_function_body(blob)+1));
        s.push_str("}}");
        let fn_ = ast::parse_fn(s, racer::Scope::from_match(m));
        let mut result = None;
        for (_/*name*/, pos, ty_) in fn_.args.into_iter() {
            let globalpos = pos - impl_header_len + start + point;
            if globalpos == m.point && ty_.is_some() {
                result = resolve_path_with_str(&ty_.unwrap(), 
                                               &m.filepath, 
                                               globalpos, 
                                               racer::SearchType::ExactMatch,
                                               racer::Namespace::TypeNamespace,  // just the type namespace
                                               ).nth(0).map(|m| racer::Ty::TyMatch(m));
            }
        }
        return result;
    }
    None
}

fn get_type_of_let_expr(m: &Match, msrc: &str) -> Option<racer::Ty> {
    // ASSUMPTION: this is being called on a let decl
    let point = scopes::find_stmt_start(msrc, m.point).unwrap();

    let src = msrc.slice_from(point);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);
        debug!("get_type_of_let_expr calling parse_let");

        let pos = m.point - point - start;
        let scope = racer::Scope{ filepath: m.filepath.clone(), point: m.point };
        return ast::get_let_type(blob.to_string(), pos, scope);
    }
    return None;
}

pub fn get_struct_field_type(fieldname: &str, structmatch: &Match) -> Option<racer::Ty> {
    assert!(structmatch.mtype == racer::MatchType::Struct);

    let filetxt = BufferedReader::new(File::open(&structmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, structmatch.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc), 
                                          racer::Scope::from_match(structmatch));
    for (field, _, ty) in fields.into_iter() {

        if fieldname == field.as_slice() {
            return ty;
        }
    }
    return None;
}

pub fn get_tuplestruct_field_type(fieldnum: uint, structmatch: &Match) -> Option<racer::Ty> {
    assert!(structmatch.mtype == racer::MatchType::Struct);

    let filetxt = BufferedReader::new(File::open(&structmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, structmatch.point);
    let structsrc = get_first_stmt(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc), 
                                          racer::Scope::from_match(structmatch));
    let mut i = 0u;
    for (_, _, ty) in fields.into_iter() {
        if i == fieldnum {
            return ty;
        }
        i+=1;
    }
    return None;    
}

pub fn get_first_stmt(src: &str) -> &str {
    match codeiter::iter_stmts(src).next() {
        Some((from, to)) => src.slice(from,to),
        None => src
    }    
}

pub fn get_type_of_match(m: Match, msrc: &str) -> Option<racer::Ty> {
    debug!("get_type_of match {} ",m);

    return match m.mtype {
        racer::MatchType::Let => get_type_of_let_expr(&m, msrc),
        racer::MatchType::FnArg => get_type_of_fnarg(&m, msrc),
        racer::MatchType::Struct => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Enum => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Function => Some(racer::Ty::TyMatch(m)),
        racer::MatchType::Module => Some(racer::Ty::TyMatch(m)),
        _ => { debug!("!!! WARNING !!! Can't get type of {}",m.mtype); None }
    }
}

pub fn get_return_type_of_function(fnmatch: &Match) -> Option<racer::Ty> {
    let filetxt = BufferedReader::new(File::open(&fnmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let point = scopes::find_stmt_start(src, fnmatch.point).unwrap();

    //debug!("get_return_type_of_function |{}|",src.slice_from(point));
    
    return src.slice_from(point).find_str("{").and_then(|n|{
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(src.slice(point, point+n+1));
        decl.push_str("}}");
        debug!("get_return_type_of_function: passing in |{}|",decl);
        return ast::parse_fn_output(decl, racer::Scope::from_match(fnmatch));
    });
}
