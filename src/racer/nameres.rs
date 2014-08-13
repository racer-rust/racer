// Name resolution
use racer;

use racer::{SearchType, StartsWith, ExactMatch, Match, Module, 
            Function, Struct, Enum, FnArg, Trait,
            StructField, Impl, Namespace, TypeNamespace, 
            ValueNamespace, BothNamespaces};

use racer::typeinf;
use racer::matchers;
use racer::codeiter;
use racer::ast;
use racer::util;
use racer::util::{symbol_matches, txt_matches, find_ident_end};
use racer::scopes;
use std::io::{BufferedReader, File};
use std::{str,vec,option};
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
                        search_type: SearchType) -> vec::MoveItems<Match> {
    let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();

    let opoint = scopes::find_stmt_start(src, m.point);
    let structsrc = scopes::end_of_next_scope(src.slice_from(opoint.unwrap()));

    let fields = ast::parse_struct_fields(String::from_str(structsrc));


    let mut out = Vec::new();
    
    for (field, fpos, _) in fields.move_iter() {

        if symbol_matches(search_type, searchstr, field.as_slice()) {
            out.push(Match { matchstr: field.to_string(),
                                filepath: m.filepath.clone(),
                                point: fpos + opoint.unwrap(),
                                local: m.local,
                                mtype: StructField,
                                contextstr: field.to_string()
            });
        }
    }
    return out.move_iter();
}

pub fn search_for_impl_methods(implsearchstr: &str,
                           fieldsearchstr: &str, point: uint, 
                           fpath: &Path, local: bool,
                           search_type: SearchType) -> vec::MoveItems<Match> {
    
    debug!("PHIL searching for impl methods |{}| |{}| {}",implsearchstr, fieldsearchstr, fpath.as_str());

    let mut out = Vec::new();

    for m in search_for_impls(point, implsearchstr, fpath, local, true) {
        debug!("PHIL found impl!! |{}| looking for methods",m);
        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
        // find the opening brace and skip to it. 
        src.slice_from(m.point).find_str("{").map(|n|{
            let point = m.point + n + 1;
            for m in search_scope_for_methods(point, src, fieldsearchstr, &m.filepath, search_type) {
                out.push(m);
            }
        });
    };
    return out.move_iter();
}

fn search_scope_for_methods(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType) -> vec::MoveItems<Match> {
    debug!("PHIL searching scope for methods {} |{}| {}",point, searchstr, filepath.as_str());
    
    let scopesrc = src.slice_from(point);
    let mut out = Vec::new();
    for (blobstart,blobend) in codeiter::iter_stmts(scopesrc) { 
        let blob = scopesrc.slice(blobstart, blobend);

        if txt_matches(search_type, format!("fn {}", searchstr).as_slice(), blob) 
            && typeinf::first_param_is_self(blob) {
            debug!("PHIL found a method starting |{}| |{}|",searchstr,blob);
            // TODO: parse this properly
            let start = blob.find_str(format!("fn {}", searchstr).as_slice()).unwrap() + 3;
            let end = find_ident_end(blob, start);
            let l = blob.slice(start, end);
            // TODO: make a better context string for functions
            blob.find_str("{").map(|n| { // only matches if is a method implementation
                let ctxt = blob.slice_to(n -1);
                let m = Match {matchstr: l.to_string(),
                           filepath: filepath.clone(), 
                           point: point + blobstart + start,
                           local: true,
                           mtype: Function,
                           contextstr: ctxt.to_string()
                };
                out.push(m);
            });
        }
    }
    return out.move_iter();
}


fn search_for_impls(pos: uint, searchstr: &str, filepath: &Path, local: bool, include_traits: bool) -> vec::MoveItems<Match> {
    debug!("PHIL search_for_impls {}, {}, {}", pos, searchstr, filepath.as_str());
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let mut src = str::from_utf8(filetxt.as_slice()).unwrap();
    src = src.slice_from(pos);

    let mut out = Vec::new();
    for (start,end) in codeiter::iter_stmts(src) { 
        let blob = src.slice(start,end);

        if blob.starts_with("impl") {
            blob.find_str("{").map(|n|{
                let mut decl = String::from_str(blob.slice_to(n+1));
                decl = decl.append("}");
                if txt_matches(ExactMatch, searchstr, decl.as_slice()) {
                    debug!("PHIL impl decl {}",decl);
                    let t0 = time::precise_time_s();
                    let implres = ast::parse_impl(decl);
                    let t1 = time::precise_time_s();

                    implres.name_path.last().map(|name| {
                        debug!("PHIL parsed an impl {}",name);
                        
                        let m = Match {matchstr: name.to_string(),
                                       filepath: filepath.clone(), 
                                       point: pos + start + 5,
                                       local: local,
                                       mtype: Impl,
                                       contextstr: name.to_string()
                        };
                        out.push(m);
                    });

                    // // find trait
                    if include_traits && implres.trait_path.len() > 0 {
                            let t0 = time::precise_time_s();
                        //println!("PHIL finding trait {} |{}|",
                        //         t0,
                        //         &implres.trait_path);
                        let m = resolve_path(util::to_refs(&implres.trait_path).as_slice(), filepath, pos + start, ExactMatch, TypeNamespace).nth(0);
                        println!("PHIL found trait {} |{}| {}",
                                 time::precise_time_s() - t0,
                                 &implres.trait_path, m);
                        m.map(|m| out.push(m));
                    }
                    debug!("PHIL ast parse impl {}s",t1-t0);
                }
            });
        }
    }
    return out.move_iter();
}

fn search_fn_args(point: uint, msrc:&str, searchstr:&str, filepath:&Path, 
                      search_type: SearchType, local: bool) -> vec::MoveItems<Match> {
    debug!("PHIL search_fn_args for |{}| pt: {}",searchstr, point);
    // 'point' points to the opening brace
    let mut out = Vec::new();

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
                    out.push(Match { matchstr: s.to_string(),
                                        filepath: filepath.clone(),
                                        point: n + pos - impl_header,
                                        local: local,
                                        mtype: FnArg,
                                     contextstr: s.to_string()
                    });
                };
            }
        }
    });
    return out.move_iter();
}

pub fn do_file_search(searchstr: &str, currentdir: &Path) -> vec::MoveItems<Match> {
    debug!("PHIL do_file_search {}",searchstr);
    let mut out = Vec::new();
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
                                           mtype: Module,
                                           contextstr: fname.slice_from(3).to_string()
                            };
                            out.push(m);
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
                                               mtype: Module,
                                               contextstr: filepath.as_str().unwrap().to_string()
                                };
                                out.push(m);
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
                                               mtype: Module,
                                               contextstr: filepath.as_str().unwrap().to_string()
                                };
                                out.push(m);
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
                                               mtype: Module,
                                               contextstr: filepath.as_str().unwrap().to_string()
                                };
                                out.push(m);
                            }
                        }
                        {            
                            // try just <name>.rs
                            if fname.ends_with(".rs") {
                                let m = Match {matchstr: fname.slice_to(fname.len()-3).to_string(),
                                               filepath: fpath.clone(),
                                               point: 0,
                                               local: false,
                                               mtype: Module,
                                               contextstr: fpath.as_str().unwrap().to_string()
                                };
                                out.push(m);
                            }

                        }

                    }

                }
            }
            Err(_) => ()
        }
    }
    return out.move_iter();
}

pub fn search_crate_root(searchstr: &str, modfpath: &Path, 
                         searchtype: SearchType, namespace: Namespace) -> vec::MoveItems<Match> {
    debug!("PHIL search_crate_root |{}| {}", searchstr, modfpath.as_str());

    let crateroots = find_possible_crate_root_modules(&modfpath.dir_path());
    let mut out = Vec::new();
    for crateroot in crateroots.iter() {
        if crateroot == modfpath {
            continue;
        }
        debug!("PHIL going to search for {} in crateroot {}",searchstr, crateroot.as_str());
        for m in resolve_name(searchstr, crateroot, 0, searchtype, namespace) {
            out.push(m);
        }
        break
    }
    return out.move_iter();
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
                     search_type: SearchType, local: bool, 
                     namespace: Namespace) -> vec::MoveItems<Match> {
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

    return search_scope(startpoint, filesrc, searchstr, filepath, search_type, local, namespace);
}

pub fn get_crate_file(name: &str) -> Option<Path> {
    let srcpaths = std::os::getenv("RUST_SRC_PATH").unwrap();
    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();
    for srcpath in v.move_iter() {
        {
            // try lib<name>/lib.rs, like in the rust source dir
            let cratelibname = format!("lib{}", name);
            let filepath = Path::new(srcpath).join_many([Path::new(cratelibname), 
                                                        Path::new("lib.rs")]);
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
    }
    return None;
}

pub fn get_module_file(name: &str, parentdir: &Path) -> Option<Path> {
    {            
        // try just <name>.rs
        let filepath = parentdir.join_many([Path::new(format!("{}.rs", name))]);
        if File::open(&filepath).is_ok() {
            return Some(filepath);
        }
    }
    {
        // try <name>/mod.rs
        let filepath = parentdir.join_many([Path::new(name),
                                            Path::new("mod.rs")]);
        if File::open(&filepath).is_ok() {
            return Some(filepath);
        }
    }

    return None;
}


fn search_scope(point: uint, src:&str, searchstr:&str, filepath:&Path, 
                search_type: SearchType, local: bool,
                namespace: Namespace) -> vec::MoveItems<Match> {
    debug!("PHIL searching scope {} {} {}",point, searchstr, filepath.as_str());
    
    let mut out = Vec::new();

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

        match namespace {
            TypeNamespace => 
                for m in matchers::match_types(src, point+blobstart, 
                                       point+blobend, searchstr, 
                                       filepath, search_type, local) {
                    out.push(m);
                },
            ValueNamespace => 
                for m in matchers::match_values(src, point+blobstart, 
                                       point+blobend, searchstr, 
                                       filepath, search_type, local) {
                    out.push(m);
                },
            BothNamespaces => {
                for m in matchers::match_types(src, point+blobstart, 
                                       point+blobend, searchstr, 
                                       filepath, search_type, local) {
                    out.push(m);
                }
                for m in matchers::match_values(src, point+blobstart, 
                                       point+blobend, searchstr, 
                                       filepath, search_type, local) {
                    out.push(m);
                }
            }
        }
    }
    return out.move_iter();
}

fn search_local_scopes(searchstr: &str, filepath: &Path, msrc: &str, mut point:uint,
                       search_type: SearchType, namespace: Namespace) -> vec::MoveItems<Match> {
    debug!("PHIL searching local scopes for {}",searchstr);

    let is_local = true;
    if point == 0 {
        // search the whole file
        return search_scope(0, msrc, searchstr, filepath, search_type, is_local, namespace);
    } else {

        let mut out = Vec::new();

        // search each parent scope in turn
        while point > 0 {
            let n = scopes::scope_start(msrc, point);
            for m in search_scope(n, msrc, searchstr, filepath, search_type, is_local, namespace) {
                out.push(m);
            }
            if n == 0 { 
                break; 
            }
            point = n-1;

            for m in search_fn_args(point, msrc, searchstr, filepath, search_type, is_local){
                out.push(m);
            };
        }
        return out.move_iter();
    }

}

pub fn search_prelude_file(searchstr: &str, search_type: SearchType, 
                           namespace: Namespace) -> vec::MoveItems<Match> {
    let mut out : Vec<Match> = Vec::new();

    // find the prelude file from the search path and scan it
    let srcpaths = match std::os::getenv("RUST_SRC_PATH") { 
        Some(paths) => paths,
        None => return out.move_iter()
    };

    let v: Vec<&str> = srcpaths.as_slice().split_str(":").collect();

    for srcpath in v.move_iter() {
        let filepath = Path::new(srcpath).join_many([Path::new("libstd"), 
                                                     Path::new("prelude.rs")]);
        if File::open(&filepath).is_ok() {
            let msrc = racer::load_file_and_mask_comments(&filepath);
            let is_local = true;
            for m in search_scope(0, msrc.as_slice(), searchstr, &filepath, search_type, is_local, namespace){
                out.push(m);
            }
        }
    }
    return out.move_iter();
}

pub fn resolve_path_with_str(path: &[&str], filepath: &Path, pos: uint, 
                                   search_type: SearchType, namespace: Namespace) -> vec::MoveItems<Match> {
    debug!("PHIL: do_local_search_with_string {}", path);
    
    let mut out = Vec::new();

    // HACK
    if path.len() == 1 && path[0] == "str" {
        debug!("PHIL {} == {}", path[0], "str");
        let str_match = resolve_name("Str", filepath, pos, ExactMatch, namespace).nth(0);
        debug!("PHIL: str_match {:?}", str_match);
        
        str_match.map(|str_match|{
            debug!("PHIL: found Str, converting to str");
            let m = Match {matchstr: "str".to_string(),
                           filepath: str_match.filepath.clone(), 
                           point: str_match.point,
                           local: false,
                           mtype: Struct,
                           contextstr: "str".to_string()
            };
            out.push(m);
        });
    } else {
        for m in resolve_path(path, filepath, pos, search_type, namespace) {
            out.push(m);
        }
    }
    return out.move_iter();
}


// HACK: Make box iterator support iterator trait
//
// I can't get the type signature to resolve_name to compile, so instead am boxing it into a trait object and then returning it as an iterator
pub struct BoxIter<T> {
    iter: Box<Iterator<T>>
}

impl<T> Iterator<T> for BoxIter<T> {
    #[inline]
    fn next(&mut self) -> Option<T> {
        return self.iter.next();
    }
}

pub fn wrap_boxed_iter<T>(boxediter: Box<Iterator<T>>) -> BoxIter<T> {
    return BoxIter{ iter: boxediter };
}

local_data_key!(pub searchstack: Vec<Search>)

#[deriving(PartialEq,Show)]
pub struct Search {
    path: Vec<String>,
    filepath: String,
    pos: uint
}

// impl fmt::Show for Search {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         //f.write("helloSearch!");
//         //try!(write!(f,"hellosearch"));

        
//         write!(f,"Search[ {} {} {} ] ",self.searchstr, self.filepath.as_str(), self.pos)
//     }
// }

pub fn is_a_repeat_search(new_search: &Search) -> bool {
    let o = searchstack.get();
    return match o {
        Some(v) => {
            for s in v.iter() {
                if s == new_search {
                    println!("PHIL is a repeat search {} Stack: {}", new_search, v);
                    return true;
                }
            }
            return false;
        }
        None => { 
            return false;
        }
    }
}

fn update_searchstack(new_search: Search) {
    let o = searchstack.replace(None);
    let mut v = o.unwrap_or(Vec::new());
    v.push(new_search);
    //println!("PHIL searchstack after  push {}",v);
    searchstack.replace(Some(v));
}

fn pop_searchstack() {
    let mut v = searchstack.replace(None).unwrap();
    v.pop();
    //println!("PHIL searchstack after  pop {}",v);
    searchstack.replace(Some(v));
}

pub fn resolve_name(searchstr: &str, filepath: &Path, pos: uint, 
                    search_type: SearchType, namespace: Namespace) -> BoxIter<Match> {
    
    let msrc = racer::load_file_and_mask_comments(filepath);


    let is_exact_match = match search_type { ExactMatch => true, StartsWith => false };

    if (is_exact_match && searchstr == "std") || (!is_exact_match && "std".starts_with(searchstr)) {
        let r = get_crate_file("std").map(|cratepath|{
            Match { matchstr: "std".to_string(),
                        filepath: cratepath.clone(), 
                        point: 0,
                        local: false,
                        mtype: Module,
                        contextstr: cratepath.as_str().unwrap().to_string()
            }
        });
        return BoxIter { iter: box r.move_iter() };
    }


    let s = String::from_str(searchstr);
    let p = filepath.clone();

    let it = util::lazyit(proc() {
        let searchstr = s.as_slice();
        let filepath = &p;

        let it = search_local_scopes(searchstr, filepath, msrc.as_slice(), pos,
                                          search_type, namespace);
        return it;
    });


    let s = String::from_str(searchstr);

    let it = it.chain(util::lazyit(proc() {
        let searchstr = s.as_slice();
        let it = search_prelude_file(searchstr, search_type, namespace);;
        return it;
    }));


    let s = String::from_str(searchstr);
    let p = filepath.clone();

    let it = it.chain(util::lazyit(proc() {
        let searchstr = s.as_slice();
        let filepath = &p;        
        let it = search_crate_root(searchstr, filepath, search_type, namespace);
        return it;
    }));

    // filesearch. Used to complete e.g. extern crate blah or mod foo
    let s = String::from_str(searchstr);
    let p = filepath.clone();

    let it = it.chain(match search_type {
        StartsWith => 
            Some(proc() { 
                let searchstr = s.as_slice();
                let filepath = p;
                let it = do_file_search(searchstr, &filepath.dir_path());
                it
            }),
        ExactMatch => 
            None
    }.move_iter().flat_map(|p| p()));

    let it = box it as Box<Iterator<Match>>;
    return BoxIter{ iter: it };

}

fn to_strvec(path: &[&str]) -> Vec<String> {
    let v : Vec<String> = path.iter().map(|s| s.to_string()).collect();
    return v;
}

pub fn resolve_path(path: &[&str], filepath: &Path, pos: uint, 
                  search_type: SearchType, namespace: Namespace) -> BoxIter<Match> {
    let search = Search { path: to_strvec(path), 
                                   filepath: filepath.as_str().to_string(),
                                   pos: pos };
    if is_a_repeat_search(&search) {
        let it : option::Item<Match> = None.move_iter();
        return BoxIter {iter: box it as Box<Iterator<Match>> }; 
    }

    update_searchstack(search);
    
    let it = resolve_path_(path, filepath, pos, search_type, namespace);
    pop_searchstack();
    return it;
}

pub fn resolve_path_(path: &[&str], filepath: &Path, pos: uint, 
                  search_type: SearchType, namespace: Namespace) -> BoxIter<Match> {
    let t0 = time::precise_time_s();
    debug!("PHIL resolve_path {} {} in {}",t0, path, filepath.as_str());


    if path.len() == 1 {
        let searchstr = path[0];
        return resolve_name(searchstr, filepath, pos, search_type, namespace);
    } else {
        let mut out = Vec::new();
        if path[0] == "" {
            // match global searches starting with :: - e.g. ::std::blah::...
            for m in do_external_search(path.slice_from(1), filepath, pos, search_type, namespace) {
                out.push(m);
            }
        }

        let parent_path = path.slice_to(path.len()-1);
        debug!("PHIL doing nested search: {} -> {}", path, parent_path);

        let mut out = Vec::new();

        let context = resolve_path(parent_path, filepath, pos, 
                                       ExactMatch, TypeNamespace).nth(0);

        context.map(|m| {
            debug!("PHIL context match is : {}", m);
            match m.mtype {
                Module => {
                    debug!("PHIL searching a module '{}' (whole path: {})",m.matchstr, path);
                    let searchstr = path[path.len()-1];
                    for m in search_next_scope(m.point, searchstr, &m.filepath, search_type, false, namespace) { 
                        out.push(m);
                    }
                }
                Struct => {
                    debug!("PHIL found a struct. Now need to look for impl");
                    for m in search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, false) {
                        debug!("PHIL found impl!! {}",m);
                        let searchstr = path[path.len()-1];

                        let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
                        let src = str::from_utf8(filetxt.as_slice()).unwrap();
                        
                        // find the opening brace and skip to it. 
                        src.slice_from(m.point).find_str("{").map(|n|{
                            let point = m.point + n + 1;
                            for m in search_scope(point, src, searchstr, &m.filepath, search_type, m.local, namespace) {
                                out.push(m);
                            }
                        });
                        
                    };
                }
                _ => ()
            }
        });

        return BoxIter{iter: box out.move_iter() as Box<Iterator<Match>>};
    }
}

pub fn do_external_search(path: &[&str], filepath: &Path, pos: uint, search_type: SearchType, namespace: Namespace) -> vec::MoveItems<Match> {
    debug!("PHIL do_external_search path {} {}",path, filepath.as_str());
    let mut out = Vec::new();
    if path.len() == 1 {
        let searchstr = path[0];
        for m in search_next_scope(pos, searchstr, filepath, search_type, false, namespace) { 
            out.push(m);
        }

        get_module_file(searchstr, &filepath.dir_path()).map(|path|{
            out.push(Match {matchstr: searchstr.to_string(),
                           filepath: path.clone(), 
                           point: 0,
                           local: false,
                           mtype: Module,
                           contextstr: path.as_str().unwrap().to_string()
                           });
        });

    } else {
        let parent_path = path.slice_to(path.len()-1);
        let context = do_external_search(parent_path, filepath, pos, ExactMatch, TypeNamespace).nth(0);
        context.map(|m| {
            match m.mtype {
                Module => {
                    debug!("PHIL found an external module {}",m.matchstr);
                    let searchstr = path[path.len()-1];
                    for m in search_next_scope(m.point, searchstr, &m.filepath, search_type, false, namespace) {
                        out.push(m);
                    }
                }

                Struct => {
                    debug!("PHIL found a pub struct. Now need to look for impl");
                    for m in search_for_impls(m.point, m.matchstr.as_slice(), &m.filepath, m.local, false) {
                        debug!("PHIL found  impl2!! {}",m.matchstr);
                        let searchstr = path[path.len()-1];
                        debug!("PHIL about to search impl scope...");
                        for m in search_next_scope(m.point, searchstr, &m.filepath, search_type, false, namespace) {
                            out.push(m);
                        }
                    };
                }
                _ => ()
            }
        });
    }
    return out.move_iter();
}

pub fn search_for_field_or_method(context: Match, searchstr: &str, search_type: SearchType) -> vec::MoveItems<Match> {
    let m = context;
    let mut out = Vec::new();
    match m.mtype {
        Struct => {
            debug!("PHIL got a struct, looking for fields and impl methods!! {}",m.matchstr);
            for m in search_struct_fields(searchstr, &m, search_type) {
                out.push(m);
            }
            for m in search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type) {
                out.push(m);
            }
        },
        Enum => {
            debug!("PHIL got an enum, looking for impl methods {}",m.matchstr);
            for m in search_for_impl_methods(m.matchstr.as_slice(),
                                    searchstr,
                                    m.point,
                                    &m.filepath,
                                    m.local,
                                    search_type) {
                out.push(m);
            }
        },
        Trait => {
            debug!("PHIL got a trait, looking for methods {}",m.matchstr);

            let filetxt = BufferedReader::new(File::open(&m.filepath)).read_to_end().unwrap();
            let mut src = str::from_utf8(filetxt.as_slice()).unwrap();
            src = src.slice_from(m.point);
            src.slice_from(m.point).find_str("{").map(|n|{
                let point = m.point + n + 1;
                for m in search_scope_for_methods(point, src, searchstr, &m.filepath, search_type) {
                    out.push(m);
                }
            });
        }
        _ => ()
    };
    return out.move_iter();
}
