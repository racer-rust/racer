use std::fs::File;
use std::io::{BufReader, Read};
use std::{str, vec, fmt};
use std::path;
use std::io;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;


use scopes;
use nameres;
use ast;

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum MatchType {
    Struct,
    Module,
    MatchArm,
    Function,
    Crate,
    Let,
    IfLet,
    StructField,
    Impl,
    Enum,
    EnumVariant,
    Type,
    FnArg,
    Trait,
    Const,
    Static
}

#[derive(Debug,Clone,Copy)]
pub enum SearchType {
    ExactMatch,
    StartsWith
}

#[derive(Debug,Clone,Copy)]
pub enum Namespace {
    TypeNamespace,
    ValueNamespace,
    BothNamespaces
}

#[derive(Debug,Clone,Copy)]
pub enum CompletionType {
    CompleteField,
    CompletePath
}

#[derive(Clone)]
pub struct Match {
    pub matchstr: String,
    pub filepath: path::PathBuf,
    pub point: usize,
    pub local: bool,
    pub mtype: MatchType,
    pub contextstr: String,
    pub generic_args: Vec<String>,
    pub generic_types: Vec<PathSearch>,  // generic types are evaluated lazily
    pub session: Rc<Session>,
}


impl Match {
    pub fn with_generic_types(&self, generic_types: Vec<PathSearch>) -> Match {
        Match {
            matchstr: self.matchstr.clone(),
            filepath: self.filepath.clone(),
            point: self.point,
            local: self.local,
            mtype: self.mtype,
            contextstr: self.contextstr.clone(),
            generic_args: self.generic_args.clone(),
            generic_types: generic_types,
            session: self.session.clone()
        }
    }
}

impl fmt::Debug for Match {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Match [{:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?} |{}|, {:?}]",
               self.matchstr,
               self.filepath.to_str(),
               self.point,
               self.local,
               self.mtype,
               self.generic_args,
               self.generic_types,
               self.contextstr,
               self.session)
    }
}

#[derive(Clone)]
pub struct Scope {
    pub filepath: path::PathBuf,
    pub point: usize,
    pub session: Rc<Session>
}

impl Scope {
    pub fn from_match(m: &Match) -> Scope {
        Scope{ filepath: m.filepath.clone(), point: m.point, session: m.session.clone() }
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope [{:?}, {:?}, {:?}]",
               self.filepath.to_str(),
               self.point,
               self.session)
    }
}

// Represents a type. Equivilent to rustc's ast::Ty but can be passed across threads
#[derive(Debug,Clone)]
pub enum Ty {
    TyMatch(Match),
    TyPathSearch(Path, Scope),   // A path + the scope to be able to resolve it
    TyTuple(Vec<Ty>),
    TyUnsupported
}

// The racer implementation of an ast::Path. Difference is that it is Send-able
#[derive(Clone)]
pub struct Path {
    pub global: bool,
    pub segments: Vec<PathSegment>
}

impl Path {
    pub fn generic_types(&self) -> ::std::slice::Iter<Path> {
        self.segments[self.segments.len()-1].types.iter()
    }

    pub fn from_vec(global: bool, v: Vec<&str>) -> Path {
        let segs = v
            .into_iter()
            .map(|x| PathSegment{ name: x.to_owned(), types: Vec::new() })
            .collect::<Vec<_>>();
        Path{ global: global, segments: segs }
    }

    pub fn from_svec(global: bool, v: Vec<String>) -> Path {
        let segs = v
            .into_iter()
            .map(|x| PathSegment{ name: x, types: Vec::new() })
            .collect::<Vec<_>>();
        Path{ global: global, segments: segs }
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "P["));
        let mut first = true;
        for seg in &self.segments {
            if first {
                try!(write!(f, "{}", seg.name));
                first = false;
            } else {
                try!(write!(f, "::{}", seg.name));
            }

            if !seg.types.is_empty() {
                try!(write!(f, "<"));
                let mut tfirst = true;
                for typath in &seg.types {
                    if tfirst {
                        try!(write!(f, "{:?}", typath));
                        tfirst = false;
                    } else {
                        try!(write!(f, ",{:?}", typath))
                    }
                }
                try!(write!(f, ">"));
            }
        }
        write!(f, "]")
    }
}

#[derive(Debug,Clone)]
pub struct PathSegment {
    pub name: String,
    pub types: Vec<Path>
}

#[derive(Clone)]
pub struct PathSearch {
    pub path: Path,
    pub filepath: path::PathBuf,
    pub point: usize,
    pub session: Rc<Session>
}

impl fmt::Debug for PathSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Search [{:?}, {:?}, {:?}, {:?}]",
               self.path,
               self.filepath.to_str(),
               self.point,
               self.session)
    }
}

#[derive(Debug)]
pub struct FileCache {
    raw_cache: RefCell<HashMap<path::PathBuf, Rc<String>>>,
    masked_cache: RefCell<HashMap<path::PathBuf, Rc<String>>>
}

impl FileCache {
    pub fn new() -> FileCache {
        FileCache {
            raw_cache: RefCell::new(HashMap::new()),
            masked_cache: RefCell::new(HashMap::new())
        }
    }
}

#[derive(Debug)]
pub struct Session {
    pub query_path: path::PathBuf,            // the input path of the query
    pub substitute_file: path::PathBuf,       // the temporary file
    file_cache: Rc<FileCache>                 // cache for file contents
}

impl Session {
    pub fn from_path(query_path: &path::Path, substitute_file: &path::Path) -> Rc<Session> {
        Rc::new(Session {
            query_path: query_path.to_path_buf(),
            substitute_file: substitute_file.to_path_buf(),
            file_cache: Rc::new(FileCache::new())
        })
    }

    pub fn derived(&self, path: &path::Path) -> Rc<Session> {
        Rc::new(Session {
            query_path: path.to_path_buf(),
            substitute_file: path.to_path_buf(),
            file_cache: self.file_cache.clone()
        })
    }

    pub fn open_file(&self, path: &path::Path) -> io::Result<File> {
        if path == self.query_path.as_path() {
            File::open(&self.substitute_file)
        } else {
            File::open(path)
        }
    }

    pub fn read_file(&self, path: &path::Path) -> Vec<u8> {
        let mut rawbytes = Vec::new();
        if let Ok(f) = self.open_file(path) {
            BufReader::new(f).read_to_end(&mut rawbytes).unwrap();
            rawbytes
        } else {
            error!("read_file couldn't open {:?}. Returning empty string", path);
            Vec::new()
        }
    }

    pub fn load_file(&self, filepath: &path::Path) -> Rc<String> {
        let mut cache = self.file_cache.raw_cache.borrow_mut();
        cache.entry(filepath.to_path_buf()).or_insert_with(|| {
            let rawbytes = self.read_file(filepath);
            // skip BOM bytes, if present
            if rawbytes.len() > 2 && rawbytes[0..3] == [0xEF, 0xBB, 0xBF] {
                let mut it = rawbytes.into_iter();
                it.next(); it.next(); it.next();
                Rc::new(String::from_utf8(it.collect::<Vec<_>>()).unwrap())
            } else {
                Rc::new(String::from_utf8(rawbytes).unwrap())
            }
        }).clone()
    }

    pub fn load_file_and_mask_comments(&self, filepath: &path::Path) -> Rc<String> {
        let mut cache = self.file_cache.masked_cache.borrow_mut();
        cache.entry(filepath.to_path_buf()).or_insert_with(|| {
            let rawbytes = self.read_file(filepath);
            let src = str::from_utf8(&rawbytes).unwrap();
            Rc::new(scopes::mask_comments(src))
        }).clone()
    }
}


pub fn complete_from_file(src: &str, filepath: &path::Path, pos: usize, session: &Rc<Session>) -> vec::IntoIter<Match> {
    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = &src[start..pos];

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("{:?}: contextstr is |{}|, searchstr is |{}|", completetype, contextstr, searchstr);

    let mut out = Vec::new();

    match completetype {
        CompletionType::CompletePath => {
            let mut v = expr.split("::").collect::<Vec<_>>();
            let mut global = false;
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::old_io::blah
                v.remove(0);
                global = true;
            }

            let path = Path::from_vec(global, v);
            for m in nameres::resolve_path(&path, filepath, pos,
                                         SearchType::StartsWith, Namespace::BothNamespaces,
                                         session) {
                out.push(m);
            }
        },
        CompletionType::CompleteField => {
            let context = ast::get_type_of(contextstr.to_owned(), filepath, pos, session);
            debug!("complete_from_file context is {:?}", context);
            context.map(|ty| {
                if let Ty::TyMatch(m) = ty {
                    for m in nameres::search_for_field_or_method(m, searchstr, SearchType::StartsWith) {
                        out.push(m)
                    }
                }
            });
        }
    }
    out.into_iter()
}

pub fn find_definition(src: &str, filepath: &path::Path, pos: usize, session: &Rc<Session>) -> Option<Match> {
    find_definition_(src, filepath, pos, session)
}

pub fn find_definition_(src: &str, filepath: &path::Path, pos: usize, session: &Rc<Session>) -> Option<Match> {
    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = &src[start..end];

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("find_definition_ for |{:?}| |{:?}| {:?}", contextstr, searchstr, completetype);

    match completetype {
        CompletionType::CompletePath => {
            let mut v = expr.split("::").collect::<Vec<_>>();
            let mut global = false;
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::old_io::blah
                v.remove(0);
                global = true;
            }

            let segs = v
                .into_iter()
                .map(|x| PathSegment{ name: x.to_owned(), types: Vec::new() })
                .collect::<Vec<_>>();
            let path = Path{ global: global, segments: segs };

            nameres::resolve_path(&path, filepath, pos,
                                  SearchType::ExactMatch, Namespace::BothNamespaces,
                                  session).nth(0)
        },
        CompletionType::CompleteField => {
            let context = ast::get_type_of(contextstr.to_owned(), filepath, pos, session);
            debug!("context is {:?}", context);

            context.and_then(|ty| {
                // for now, just handle matches
                match ty {
                    Ty::TyMatch(m) => {
                        nameres::search_for_field_or_method(m, searchstr, SearchType::ExactMatch).nth(0)
                    }
                    _ => None
                }
            })
        }
    }
}
