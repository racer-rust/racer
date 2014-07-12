// Name resolution
use racer;

use racer::{SearchType, StartsWith, ExactMatch, Match, Module, 
            Function, Struct, Enum, FnArg,
            StructField, Impl};

use racer::typeinf;
use racer::matchers;
use racer::codeiter;
use racer::ast;
use racer::util::{symbol_matches, txt_matches, find_ident_end, first_match};
use racer::scopes;
use std::io::{BufferedReader, File};
use std::str;
use std;
use time;

fn reverse_to_start_of_fn(point: uint, msrc: &str) -> Option<uint> {
    debug!("PHIL reverse to start of fn. {}", point);
    scopes::find_stmt_start(msrc, point).map_or(None, |n| {
        let block = msrc.slice_from(n);
        if block.starts_with("fn") || block.starts_with("pub fn") {
            return Some(n);
        } else {
            return None;
        }
    })
}

fn search_struct_fields(searchstr: &str, m: &Match,
                        search_type: SearchType, outputfn: &mut |Match|) {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, m.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc));
    for (field, fpos) in fields.move_iter() {

        if symbol_matches(search_type, searchstr, field.as_slice()) {
            (*outputfn)(Match { matchstr: field.to_string(),
                                filepath: m.filepath.clone(),
                                point: fpos + opoint.unwrap(),
                                local: m.local,
                                mtype: StructField});
        }
    }
}

pub fn search_for_impl_methods(implsearchstr: &str,
                           fieldsearchstr: &str, point: uint, 
                           fpath: &Path, local: bool,
                           search_type: SearchType,
                           outputfn: &mut |Match|) {
    
    debug!("PHIL searching for impl methods |{}| |{}| {}",implsearchstr, fieldsearchstr, fpath.as_str());
    search_for_impls(point, implsearchstr, fpath, local, &mut |m|{
        debug!("PHIL found impl!! |{}| looking for methods",m.matchstr);
        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
        // find the opening brace and skip to it. 
        src.slice_from(m.point).find_str("{").map(|n|{
            let point = m.point + n + 1;
            search_scope_for_methods(point, src, fieldsearchstr, &m.filepath, search_type, outputfn);
        });
    });
}

fn search_scope_for_methods(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, 
                      outputfn: &mut |Match|) {
    debug!("PHIL searching scope for methods {} {} {}",point, searchstr, filepath.as_str());
    
    let scopesrc = src.slice_from(point);

    for (blobstart,blobend) in codeiter::iter_stmts(scopesrc) { 
        let blob = scopesrc.slice(blobstart, blobend);

        if txt_matches(search_type, format!("fn {}", searchstr).as_slice(), blob) 
            && typeinf::first_param_is_self(blob) {
            debug!("PHIL found a method starting |{}| |{}|",searchstr,blob);
            // TODO: parse this properly
            let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: true,
                           mtype: Function
            };
            (*outputfn)(m);
        }
    }
}


fn search_for_impls(pos: uint, searchstr: &str, filepath: &Path, local: bool,
                    outputfn: &mut |Match|) {
    debug!("PHIL search_for_impls {}, {}, {}", pos, searchstr, filepath.as_str());
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let mut src = str::from_utf8(filetxt.as_slice()).unwrap();
    src = src.slice_from(pos);
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);

        if blob.starts_with("impl") {
            blob.find_str("{").map(|n|{
                let mut decl = String::from_str(blob.slice_to(n+1));
                decl = decl.append("}");
                if txt_matches(ExactMatch, searchstr, decl.as_slice()) {
                    debug!("PHIL impl decl {}",decl);
                    let t0 = time::precise_time_s();
                    let implres = ast::parse_impl_name(decl);
                    let t1 = time::precise_time_s();
                    implres.map(|name|{
                        debug!("PHIL parsed an impl {}",name);
                        
                        let m = Match {matchstr: name.to_string(),
                                       filepath: filepath.clone(), 
                                       point: pos + start + 5,
                                       local: local,
                                       mtype: Impl
                        };
                        (*outputfn)(m);
                    });
                    debug!("PHIL ast parse impl {}s",t1-t0);
                }
            });
        }
    }
}

fn search_fn_args(point: uint, msrc:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL search_fn_args for |{}| pt: {}",searchstr, point);
    // 'point' points to the opening brace
    reverse_to_start_of_fn(point-1, msrc).map(|n| {
        let mut fndecl = String::new();
        // wrap in 'impl blah {}' so that methods get parsed correctly too
        fndecl.push_str("impl blah {");
        let impl_header = fndecl.len();
        fndecl.push_str(msrc.slice(n,point+1));
        fndecl.push_str("}}");
        debug!("PHIL found start of fn!! '{}' {} |{}|",searchstr, n, fndecl);
        if txt_matches(search_type, searchstr, fndecl.as_slice()) {
            let fn_ = ast::parse_fn(fndecl);
            debug!("PHIL parsed fn got {:?}",fn_);
            for (s, pos, _) in fn_.args.move_iter() {
                if match search_type {
                    ExactMatch => s.as_slice() == searchstr,
                    StartsWith => s.as_slice().starts_with(searchstr)
                    } {
                    (*outputfn)(Match { matchstr: s.to_string(),
                                        filepath: filepath.clone(),
                                        point: n + pos - impl_header,
                                        local: local,
                                        mtype: FnArg});
                };
            }
        }
    });
}

pub fn do_file_search(searchstr: &str, currentdir: &Path, outputfn: &mut |Match|) {
    debug!("PHIL do_file_search {}",searchstr);
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    let v = v.append_one(currentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        match std::io::fs::readdir(&Path::new(srcpath)) {
            Ok(v) => {
                for fpath in v.iter() {
                    //debug!("PHIL fpath {}",fpath.as_str());
                    let fname = fpath.str_components().rev().next().unwrap().unwrap();
                    if fname.starts_with(format!("lib{}", searchstr).as_slice()) {
                        //debug!("PHIL Yeah found {}",fpath.as_str());
                        let filepath = Path::new(fpath).join_many([Path::new("lib.rs")]);
                        if File::open(&filepath).is_ok() {
                            let m = Match {matchstr: fname.slice_from(3).to_string(),
                                           filepath: filepath.clone(), 
                                           point: 0,
                                           local: false,
                                           mtype: Module};
                            (*outputfn)(m);
                        }
                    }

                    if fname.starts_with(searchstr) {
                        {
                            // try <name>/<name>.rs, like in the servo codebase
                            let filepath = Path::new(fpath).join_many([Path::new(format!("{}.rs", fname))]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/mod.rs
                            let filepath = Path::new(fpath).join_many([Path::new("mod.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {
                            // try <name>/lib.rs
                            let filepath = Path::new(srcpath).join_many([Path::new("lib.rs")]);
                            if File::open(&filepath).is_ok() {
                                let m = Match {matchstr: fname.to_string(),
                                               filepath: filepath.clone(), 
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);
                            }
                        }
                        {            
                            // try just <name>.rs
                            if fname.ends_with(".rs") {
                                let m = Match {matchstr: fname.slice_to(fname.len()-3).to_string(),
                                               filepath: fpath.clone(),
                                               point: 0,
                                               local: false,
                                               mtype: Module};
                                (*outputfn)(m);                
                            }

                        }

                    }

                }
            }
            Err(_) => ()
        }
    }
}

pub fn search_crate_root(searchstr: &str, modfpath: &Path, 
                         searchtype: SearchType, outputfn: &mut |Match|) {
    let crateroots = find_possible_crate_root_modules(&modfpath.dir_path());
    for crateroot in crateroots.iter() {
        if crateroot == modfpath {
            continue;
        }
        debug!("PHIL going to search for {} in crateroot {}",searchstr, crateroot.as_str());
        resolve_path([searchstr], crateroot, 0, searchtype, outputfn);
    }
}

pub fn find_possible_crate_root_modules(currentdir: &Path) -> Vec<Path> {
    let mut res = Vec::new();
    
    {
        let filepath = currentdir.join_many([Path::new("lib.rs")]);
        if File::open(&filepath).is_ok() {
            res.push(filepath);
            return res;   // for now stop at the first match
        }
    }
    {
        let filepath = currentdir.join_many([Path::new("main.rs")]);
        if File::open(&filepath).is_ok() {
            res.push(filepath);
            return res;   // for now stop at the first match
        }
    }
    {
        // recurse up the directory structure
        let parentdir = currentdir.dir_path();
        if parentdir != *currentdir {
            res.push_all(find_possible_crate_root_modules(&parentdir).as_slice());
            return res;   // for now stop at the first match
        }
    }

    return res;
}

fn search_next_scope(mut startpoint: uint, searchstr:&str, filepath:&Path, 
                     search_type: SearchType, local: bool, outputfn: &mut |Match|) {

    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let filesrc = str::from_utf8(filetxt.as_slice()).unwrap();
    if startpoint != 0 {
        // is a scope inside the file. Point should point to the definition 
        // (e.g. mod blah {...}), so the actual scope is past the first open brace.
        let src = filesrc.slice_from(startpoint);
        //debug!("PHIL search_next_scope src1 |{}|",src);
        // find the opening brace and skip to it. 
        src.find_str("{").map(|n|{
            startpoint = startpoint + n + 1;
        });
    }

    search_scope(startpoint, filesrc, searchstr, filepath, search_type, local, outputfn);
}

pub fn get_module_file(name: &str, parentdir: &Path) -> Option<Path> {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    let v = v.append_one(parentdir.as_str().unwrap());
    for srcpath in v.move_iter() {
        debug!("PHIL searching srcpath: {} for {}",srcpath, name);
        {
            // maybe path is from crate. 
            // try lib<name>/lib.rs, like in the rust source dir
            let cratelibname = format!("lib{}", name);
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/<name>.rs, like in the servo codebase
            let filepath = Path::new(srcpath).join_many([Path::new(name), 
                                                     Path::new(format!("{}.rs", name))]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/mod.rs
            let filepath = Path::new(srcpath).join_many([Path::new(name),
                                                     Path::new("mod.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {
            // try <name>/lib.rs
            let filepath = Path::new(srcpath).join_many([Path::new(name),
                                                     Path::new("lib.rs")]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
        {            
            // try just <name>.rs
            let filepath = Path::new(srcpath).join_many([Path::new(format!("{}.rs", name))]);
            if File::open(&filepath).is_ok() {
                return Some(filepath);
            }
        }
    }
    return None;
}

fn search_scope(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool,
                      outputfn: &mut |Match|) {
    debug!("PHIL searching scope {} {} {}",point, searchstr, filepath.as_str());
    
    let scopesrc = src.slice_from(point);

    let mut skip_next_block = false;

    for (blobstart,blobend) in codeiter::iter_stmts(scopesrc) { 

        // sometimes we need to skip blocks of code if the preceeding attribute disables it
        //  (e.g. #[cfg(test)])
        if skip_next_block {
            skip_next_block = false;
            continue;
        }

        let blob = scopesrc.slice(blobstart,blobend);

        // for now skip stuff that's meant for testing. Often the test
        // module hierarchy is incompatible with the non-test
        // hierarchy and we get into recursive loops
        if blob.starts_with("#[cfg(test)") {
            skip_next_block = true;
            continue;
        }

        //debug!("PHIL search_scope BLOB |{}|",blob);

        for m in matchers::match_types(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local) {
            (*outputfn)(m);
        }

        // matchers::match_let(src, point+blobstart, point+blobend, searchstr, filepath, exact_match, local).map(|m|{
        //     (*outputfn)(m);
        // });

        // matchers::match_extern_crate(src, point+blobstart, point+blobend, searchstr, filepath, search_type).map(|m|{
        //     (*outputfn)(m);
        // });

        // matchers::match_mod(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).map(|m|{
        //     (*outputfn)(m);
        // });

        // matchers::match_fn(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).map(|m|{
        //     (*outputfn)(m);
        // });

        // matchers::match_struct(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).map(|m|{
        //     (*outputfn)(m);
        // });

        // matchers::match_type(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).map(|m|{
        //     (*outputfn)(m);
        // });

        // for m in matchers::match_trait(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).move_iter() {
        //     (*outputfn)(m);
        // }

        // for m in matchers::match_enum(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local).move_iter() {
        //     (*outputfn)(m);
        // }

        // for m in matchers::match_enum_variants(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local) {
        //     (*outputfn)(m);
        // }

        // matchers::match_use(src, point+blobstart, point+blobend, searchstr, filepath, search_type, local, outputfn);
    }
}



fn search_local_scopes(searchstr: &str, filepath: &Path, msrc: &str, mut point:uint,
                       search_type: SearchType, outputfn: &mut |Match|) {
    debug!("PHIL searching local scopes for {}",searchstr);

    let is_local = true;
    if point == 0 {
        // search the whole file
        search_scope(0, msrc, searchstr, filepath, search_type, is_local, outputfn);
    } else {
        // search each parent scope in turn
        while point > 0 {
            let n = scopes::scope_start(msrc, point);
               search_scope(n, msrc, searchstr, filepath, search_type, is_local, outputfn);
            if n == 0 { 
                break; 
            }
            point = n-1;

            search_fn_args(point, msrc, searchstr, filepath, search_type, is_local, outputfn);

        }
    }
}

pub fn search_prelude_file(searchstr: &str, search_type: SearchType, outputfn: &mut |Match|) {
    // find the prelude file from the search path and scan it
    let srcpaths = match std::os::getenv("RUST_SRC_PATH") { 
        Some(paths) => paths,
        None => return
    };

    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    for srcpath in v.move_iter() {
        let filepath = Path::new(srcpath).join_many([Path::new("libstd/prelude.rs")]);
        if File::open(&filepath).is_ok() {
            let msrc = racer::load_file_and_mask_comments(&filepath);
            search_local_scopes(searchstr, &filepath, msrc.as_slice(), 0,
                                search_type, outputfn);
        }
    }
}

pub fn do_local_search_with_string(path: &[&str], filepath: &Path, pos: uint, 
                       search_type: SearchType,
                       outputfn: &mut |Match|) {
    debug!("PHIL: do_local_search_with_string {}", path);
    // HACK
    if path.len() == 1 && path[0] == "str" {
        debug!("PHIL {} == {}", path[0], "str");
        let str_match = first_match(|m| resolve_path(["Str"], filepath, pos, ExactMatch, m));
        debug!("PHIL: str_match {:?}", str_match);
        
        str_match.map(|str_match|{
            debug!("PHIL: found Str, converting to str");
            let m = Match {matchstr: "str".to_string(),
                           filepath: str_match.filepath.clone(), 
                           point: str_match.point,
                           local: false,
                           mtype: Struct
            };
            (*outputfn)(m);
        });
    } else {
        resolve_path(path, filepath, pos, search_type, outputfn);
    }
}

pub fn resolve_name(searchstr: &str, filepath: &Path, pos: uint, 
                    search_type: SearchType, outputfn: &mut |Match|) {

    // search the current file
    let msrc = racer::load_file_and_mask_comments(filepath);
    search_local_scopes(searchstr, filepath, msrc.as_slice(), pos,
                        search_type, outputfn);

    // search the prelude
    search_prelude_file(searchstr, search_type, outputfn);

    // don't need to match substrings here because substring matches are done
    // on the use stmts.
    search_crate_root(searchstr, filepath, search_type, outputfn);

    // don't need to match substrings here because substring matches are done
    // on the use stmts.
    get_module_file(searchstr, &filepath.dir_path()).map(|path|{
        let m = Match {matchstr: searchstr.to_string(),
                       filepath: path.clone(), 
                       point: 0,
                       local: false,
                       mtype: Module
        };
        (*outputfn)(m);
    });

    match search_type {
        StartsWith => do_file_search(searchstr, &filepath.dir_path(), outputfn),
        ExactMatch => ()
    };
}

pub fn resolve_path(path: &[&str], filepath: &Path, pos: uint, 
                       search_type: SearchType,
                       outputfn: &mut |Match|) {
    debug!("PHIL do_local_search path {} in {}",path, filepath.as_str());

    if path.len() == 1 {
        let searchstr = path[0];
        resolve_name(searchstr, filepath, pos, search_type, outputfn);
    } else {
        if path[0] == "" {
            // match global searches starting with :: - e.g. ::std::blah::...
            return do_external_search(path.slice_from(1), filepath, pos, search_type, outputfn);
            
        }

        let parent_path = path.slice_to(path.len()-1);
        debug!("PHIL doing nested search: {} -> {}", path, parent_path);
        let context = first_match(|m| resolve_path(parent_path, filepath, pos, ExactMatch, m));
        context.map(|m| {
            debug!("PHIL context match is : {:?} {}", m, m.matchstr);
            match m.mtype {
                Module => {
                    debug!("PHIL searching a module '{}' (whole path: {})",m.matchstr, path);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                }
                Struct => {
                    debug!("PHIL found a struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found impl!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];

                        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
                        // find the opening brace and skip to it. 
                        src.slice_from(m.point).find_str("{").map(|n|{
                            let point = m.point + n + 1;
                            search_scope(point, src, searchstr, &m.filepath, search_type, m.local, outputfn);
                        });
                        
                    });
                }
                _ => ()
            }
        });
    }
}

pub fn do_external_search(path: &[&str], filepath: &Path, pos: uint, search_type: SearchType, outputfn: &mut |Match|) {
    debug!("PHIL do_external_search path {} {}",path, filepath.as_str());
    if path.len() == 1 {
        let searchstr = path[0];
        search_next_scope(pos, searchstr, filepath, search_type, false, outputfn);

        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            let m = Match {matchstr: searchstr.to_string(),
                           filepath: path.clone(), 
                           point: 0,
                           local: false,
                           mtype: Module
            };
            (*outputfn)(m);
        });

    } else {
        let parent_path = path.slice_to(path.len()-1);
        let context = first_match(|m| do_external_search(parent_path, filepath, pos, ExactMatch, m));
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL found an external module {}",m.matchstr);
                    let searchstr = path[path.len()-1];
                    search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                }

                Struct => {
                    debug!("PHIL found a pub struct. Now need to look for impl");
                    search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, &mut |m|{
                        debug!("PHIL found  impl2!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];
                        debug!("PHIL about to search impl scope...");
                        search_next_scope(m.point, searchstr, &m.filepath, search_type, false, outputfn);
                    });
                }
                _ => ()
            }
        });
    }
}

pub fn search_for_field(context: Match, searchstr: &str, search_type: SearchType, outputfn: &mut |Match|) {
    let m = context;
    match m.mtype {
        Struct => {
            debug!("PHIL got a struct, looking for fields and impl methods!! {}",m.matchstr);
            search_struct_fields(searchstr, &m, search_type, outputfn);
            search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    outputfn);
        }

        Enum => {
            search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type,
                                    outputfn);
        }
        _ => ()
    };
}
