use std::fs::File;
use std::io::Read;
use std::{vec, fmt};
use std::path;
use std::io;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::slice;
use std::cmp::{min, max};
use std::iter::{Fuse, Iterator};
use codeiter::StmtIndicesIter;

use typed_arena::Arena;

use scopes;
use nameres;
use ast;
use codecleaner;

#[derive(Debug,Clone,Copy,PartialEq)]
pub enum MatchType {
    Struct,
    Module,
    MatchArm,
    Function,
    Crate,
    Let,
    IfLet,
    WhileLet,
    For,
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
        }
    }
}

impl fmt::Debug for Match {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Match [{:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?} |{}|]",
               self.matchstr,
               self.filepath.to_str(),
               self.point,
               self.local,
               self.mtype,
               self.generic_args,
               self.generic_types,
               self.contextstr)
    }
}

#[derive(Clone)]
pub struct Scope {
    pub filepath: path::PathBuf,
    pub point: usize
}

impl Scope {
    pub fn from_match(m: &Match) -> Scope {
        Scope{ filepath: m.filepath.clone(), point: m.point }
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope [{:?}, {:?}]",
               self.filepath.to_str(),
               self.point)
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
    pub point: usize
}

impl fmt::Debug for PathSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Search [{:?}, {:?}, {:?}]",
               self.path,
               self.filepath.to_str(),
               self.point)
    }
}

pub struct IndexedSource {
    pub code: String,
    pub idx: Vec<(usize, usize)>
}

#[derive(Clone,Copy)]
pub struct Src<'c> {
    pub src: &'c IndexedSource,
    pub from: usize,
    pub to: usize
}

impl IndexedSource {
    pub fn new(src: String) -> IndexedSource {
        let indices = codecleaner::code_chunks(&src).collect();
        IndexedSource {
            code: src,
            idx: indices
        }
    }

    pub fn with_src(&self, new_src: String) -> IndexedSource {
        IndexedSource {
            code: new_src,
            idx: self.idx.clone()
        }
    }

    pub fn as_ref(&self) -> Src {
        Src {
            src: self,
            from: 0,
            to: self.len()
        }
    }
}

impl<'c> Src<'c> {
    pub fn iter_stmts(&self) -> Fuse<StmtIndicesIter<CodeChunkIter>> {
        StmtIndicesIter::from_parts(&self[..], self.chunk_indices())
    }

    pub fn from(&self, from: usize) -> Src<'c> {
        Src {
            src: self.src,
            from: self.from + from,
            to: self.to
        }
    }

    pub fn to(&self, to: usize) -> Src<'c> {
        Src {
            src: self.src,
            from: self.from,
            to: self.from + to
        }
    }

    pub fn from_to(&self, from: usize, to: usize) -> Src<'c> {
        Src {
            src: self.src,
            from: self.from + from,
            to: self.from + to
        }
    }

    pub fn chunk_indices(&self) -> CodeChunkIter<'c> {
        CodeChunkIter { src: *self, iter: self.src.idx.iter() }
    }
}

// iterates cached code chunks. 
// N.b. src can be a substr, so iteration skips chunks that aren't part of the substr
pub struct CodeChunkIter<'c> {
    src: Src<'c>,
    iter: slice::Iter<'c, (usize, usize)>
}

impl<'c> Iterator for CodeChunkIter<'c> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<(usize, usize)> {
        loop {
            match self.iter.next() {
                None => return None,
                Some(&(start, end)) => {
                    if end < self.src.from {
                        continue;
                    }
                    if start > self.src.to {
                        return None;
                    } else {
                        return Some((
                            max(start, self.src.from) - self.src.from,
                            min(end, self.src.to) - self.src.from));
                    }
                }
            }
        }
    }
}

impl Deref for IndexedSource {
    type Target = str;
    fn deref(&self) -> &str {
        &self.code
    }
}

impl<'c> Deref for Src<'c> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.src.code[self.from..self.to]
    }
}

pub fn new_source(src: String) -> IndexedSource {
    IndexedSource::new(src)
}

pub struct FileCache<'s> {
    arena: Arena<IndexedSource>,
    raw_map: RefCell<HashMap<path::PathBuf, &'s IndexedSource>>,
    masked_map: RefCell<HashMap<path::PathBuf, &'s IndexedSource>>,
}

impl<'s> FileCache<'s> {
    pub fn new<'a>() -> FileCache<'a> {
        FileCache {
            arena: Arena::new(),
            raw_map: RefCell::new(HashMap::new()),
            masked_map: RefCell::new(HashMap::new()),
        }
    }
}

pub struct Session<'s> {
    query_path: path::PathBuf,            // the input path of the query
    substitute_file: path::PathBuf,       // the temporary file
    cache: FileCache<'s>                  // cache for file contents
}

pub type SessionRef<'s> = &'s Session<'s>;

impl<'s> fmt::Debug for Session<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Session({:?}, {:?})", self.query_path, self.substitute_file)
    }
}

impl<'s> Session<'s> {
    pub fn from_path<'a>(query_path: &path::Path, substitute_file: &path::Path) -> Session<'a> {
        Session {
            query_path: query_path.to_path_buf(),
            substitute_file: substitute_file.to_path_buf(),
            cache: FileCache::new()
        }
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
        if let Ok(mut f) = self.open_file(path) {
            f.read_to_end(&mut rawbytes).unwrap();
            // skip BOM bytes, if present
            if rawbytes.len() > 2 && rawbytes[0..3] == [0xEF, 0xBB, 0xBF] {
                let mut it = rawbytes.into_iter();
                it.next(); it.next(); it.next();
                it.collect()
            } else {
                rawbytes
            }
        } else {
            error!("read_file couldn't open {:?}. Returning empty string", path);
            Vec::new()
        }
    }

    pub fn load_file(&'s self, filepath: &path::Path) -> Src<'s> {
        let mut cache = self.cache.raw_map.borrow_mut();
        cache.entry(filepath.to_path_buf()).or_insert_with(|| {
            let rawbytes = self.read_file(filepath);
            let res = String::from_utf8(rawbytes).unwrap();
            self.cache.arena.alloc(IndexedSource::new(res))
        }).as_ref()
    }

    pub fn load_file_and_mask_comments(&'s self, filepath: &path::Path) -> Src<'s> {
        let mut cache = self.cache.masked_map.borrow_mut();
        cache.entry(filepath.to_path_buf()).or_insert_with(|| {
            let src = self.load_file(filepath);
            // create a new IndexedSource with new source, but same indices
            self.cache.arena.alloc(src.src.with_src(scopes::mask_comments(src)))
        }).as_ref()
    }
}


pub fn complete_from_file(src: &str, filepath: &path::Path, pos: usize, session: SessionRef) -> vec::IntoIter<Match> {
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
                    for m in nameres::search_for_field_or_method(m, searchstr, SearchType::StartsWith, session) {
                        out.push(m)
                    }
                }
            });
        }
    }
    out.into_iter()
}

pub fn find_definition(src: &str, filepath: &path::Path, pos: usize, session: SessionRef) -> Option<Match> {
    find_definition_(src, filepath, pos, session)
}

pub fn find_definition_(src: &str, filepath: &path::Path, pos: usize, session: SessionRef) -> Option<Match> {
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
                        nameres::search_for_field_or_method(m, searchstr, SearchType::ExactMatch, session).nth(0)
                    }
                    _ => None
                }
            })
        }
    }
}
