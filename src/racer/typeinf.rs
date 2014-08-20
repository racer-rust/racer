// Type inference

use racer::{Match};
use racer::util::{to_refs};
use racer::nameres::{resolve_path_with_str};
use racer::{ast,codeiter,scopes};
use racer;

use racer::util;

use std::io::File;
use std::io::BufferedReader;
use std::{str};

use racer::{ExactMatch, TypeNamespace};
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
        debug!("PHIL searching fn args: |{}| {}",blob.slice(start+1,end), txt_matches(ExactMatch, "self", blob.slice(start+1,end)));
        return txt_matches(ExactMatch, "self", blob.slice(start+1,end));
    });
}


#[test]
fn generates_skeleton_for_mod() {
    let src = "mod foo { blah };";
    let out = generate_skeleton_for_parsing(src);
    assert_eq!("mod foo {};", out.as_slice());
}

fn get_type_of_self_arg(m: &Match, msrc: &str) -> Option<Match> {
    debug!("PHIL get_type_of_self_arg {}", m)
    return scopes::find_impl_start(msrc, m.point, 0).and_then(|start| {
        let decl = generate_skeleton_for_parsing(msrc.slice_from(start));
        debug!("PHIL get_type_of_self_arg impl skeleton |{}|", decl)
        
        if decl.as_slice().starts_with("impl") {
            let implres = ast::parse_impl(decl);
            debug!("PHIL get_type_of_self_arg implres |{:?}|", implres);
            return resolve_path_with_str(to_refs(&implres.name_path).as_slice(), &m.filepath, start, 
                                                   ExactMatch, TypeNamespace).nth(0);
        } else {
            // // must be a trait
            return ast::parse_trait(decl).name.and_then(|name| {
                Some(Match {matchstr: name,
                           filepath: m.filepath.clone(), 
                           point: start,
                           local: m.local,
                           mtype: racer::Trait,
                           contextstr: racer::matchers::first_line(msrc.slice_from(start)),
                           generic_args: Vec::new(), generic_types: Vec::new()
                })
            });
        }
    });
}

fn get_type_of_fnarg(m: &Match, msrc: &str) -> Option<Match> {
    debug!("PHIL get type of fn arg {}",m);

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
        let fn_ = ast::parse_fn(s);
        let mut result = None;
        for (_/*name*/, pos, ty_) in fn_.args.move_iter() {
            let globalpos = pos - impl_header_len + start + point;
            if globalpos == m.point && ty_.len() != 0 {
                let v = to_refs(&ty_);
                let fqn = v.as_slice();
                result = resolve_path_with_str(fqn, 
                                               &m.filepath, 
                                               globalpos, 
                                               racer::ExactMatch, 
                                               racer::TypeNamespace,  // just the type namespace
                                               ).nth(0);
            }
        }
        return result;
    }
    None
}

fn get_type_of_let_expr(m: &Match, msrc: &str) -> Option<Match> {
    // ASSUMPTION: this is being called on a let decl
    let opoint = scopes::find_stmt_start(msrc, m.point);
    let point = opoint.unwrap();

    let src = msrc.slice_from(point);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);
        debug!("PHIL get_type_of_let_expr calling parse_let");
        return ast::parse_let(String::from_str(blob), m.filepath.clone(), m.point, true).map_or(None, |letres|{

            let inittype = letres.inittype;
            debug!("PHIL parse let result {:?}", inittype);

            inittype.as_ref().map(|m|{
                debug!("PHIL parse let type is {}",m);
            });

            return inittype;
        });
    }
    return None;
}


pub fn get_struct_field_type(fieldname: &str, structmatch: &Match) -> Option<Match> {

    assert!(structmatch.mtype == racer::Struct);

    let filetxt = BufferedReader::new(File::open(&structmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, structmatch.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc));

    for (field, fpos, typepath) in fields.move_iter() {

        if fieldname == field.as_slice() {
            let typepath = util::to_refs(&typepath);
            let type_ = racer::nameres::resolve_path(typepath.as_slice(),
                                           &structmatch.filepath, 
                                           fpos + opoint.unwrap(),
                                           ExactMatch, TypeNamespace).nth(0);
            if type_.is_some() {
                return type_;
            }
        }
    }
    return None
}

pub fn get_type_of_match(m: Match, msrc: &str) -> Option<Match> {
    debug!("PHIL get_type_of match {} ",m);

    return match m.mtype {
        racer::Let => get_type_of_let_expr(&m, msrc),
        racer::FnArg => get_type_of_fnarg(&m, msrc),
        racer::Struct => Some(m),
        racer::Enum => Some(m),
        racer::Function => Some(m),
        racer::Module => Some(m),
        _ => { debug!("!!! WARNING !!! Can't get type of {:?}",m.mtype); None }
    }
}

pub fn get_return_type_of_function(fnmatch: &Match) -> Vec<String> {
    let filetxt = BufferedReader::new(File::open(&fnmatch.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let point = scopes::find_stmt_start(src, fnmatch.point).unwrap();

    //debug!("get_return_type_of_function |{}|",src.slice_from(point));
    
    let outputpath = src.slice_from(point).find_str("{").map(|n|{
        // wrap in "impl blah { }" so that methods get parsed correctly too
        let mut decl = String::new();
        decl.push_str("impl blah {");
        decl.push_str(src.slice(point, point+n+1));
        decl.push_str("}}");
        debug!("PHIL: passing in |{}|",decl);
        return ast::parse_fn_output(decl);
    }).unwrap_or(Vec::new());
    return outputpath;
}
