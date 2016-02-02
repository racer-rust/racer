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
    Static,
    Macro,
    Builtin,
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

pub struct FileCache {
    /// raw source for cached files
    raw_map: HashMap<path::PathBuf, IndexedSource>,
    /// masked source for cached files
    masked_map: HashMap<path::PathBuf, IndexedSource>,
}

/// Caches file contents for re-use between sessions.
///
/// There are two versions of each file that can be cached indepdendently:
/// "raw" and "masked", where "masked" is a version with comments and
/// strings replaced by spaces, so that they aren't found when scanning
/// the source for signatures.
impl FileCache {
    pub fn new() -> FileCache {
        FileCache {
            raw_map: HashMap::new(),
            masked_map: HashMap::new(),
        }
    }

    /// Add/Replace a file in both versions.
    pub fn cache_file_contents<T>(&mut self, filepath: &path::Path, buf: T)
        where T: Into<String>
    {
        let src = IndexedSource::new(buf.into());
        let masked_src = IndexedSource::new(scopes::mask_comments(src.as_ref()));
        self.raw_map.insert(filepath.to_path_buf(), src);
        self.masked_map.insert(filepath.to_path_buf(), masked_src);
    }
}

/// Version of FileCache that is used while a Session is active.
///
/// The need for separate types is due to several requirements:
///
/// - we have to add files to the cache during completion, so the
///   cache has to be at least internally mutable (RefCells)
///
/// - that the racer core methods use references to source strings
///   all over the place, so we can't use references tied to RefCell
///   borrow lifetimes
///
/// These two requirements make the use of a scheme like the Arena in
/// this type necessary: it provides a lifetime that is long enough
/// (lifetime of the session), and the hash-map RefCells only need to
/// store references to its contents.
///
/// However, there is one more requirement:
///
/// - the file cache should be long-lived (a Session is one completion
///   run, but if racer runs as a daemon the cache will remain)
///
/// This makes the Arena-RefCell cache unsuitable, because there is no
/// way to free now-unused but still allocated source strings.
///
/// The two-layer allows append-only allocation during Session run, but
/// when the session is finished (with `destroy`) the long lived cache
/// is updated with all new entries from the short lived cache.
///
/// This should also make it possible to share the long lived cache
/// among threads, if that would be desired at one point.
pub struct SessionFileCache<'c> {
    base: FileCache,
    arena: Arena<(bool, path::PathBuf, IndexedSource)>,
    raw_map: RefCell<HashMap<&'c path::Path, &'c IndexedSource>>,
    masked_map: RefCell<HashMap<&'c path::Path, &'c IndexedSource>>,
}

impl<'c> SessionFileCache<'c> {
    pub fn new<'a>(base: FileCache) -> SessionFileCache<'a> {
        SessionFileCache {
            base: base,
            arena: Arena::new(),
            raw_map: RefCell::new(HashMap::new()),
            masked_map: RefCell::new(HashMap::new()),
        }
    }

    fn open_file(&self, path: &path::Path) -> io::Result<File> {
        File::open(path)
    }

    fn read_file(&self, path: &path::Path) -> Vec<u8> {
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

    pub fn load_file<'s: 'c>(&'s self, filepath: &path::Path) -> Src<'c> {
        if let Some(src) = self.raw_map.borrow().get(filepath) {
            return src.as_ref();
        }
        if let Some(src) = self.base.raw_map.get(filepath) {
            return src.as_ref();
        }
        // nothing found, insert into cache
        let rawbytes = self.read_file(filepath);
        let res = String::from_utf8(rawbytes).unwrap();
        let alloc = self.arena.alloc((false, filepath.to_path_buf(),
                                      IndexedSource::new(res)));
        self.raw_map.borrow_mut().insert(&alloc.1, &alloc.2);
        alloc.2.as_ref()
    }

    pub fn load_file_and_mask_comments<'s: 'c>(&'s self, filepath: &path::Path) -> Src<'c> {
        if let Some(src) = self.masked_map.borrow().get(filepath) {
            return src.as_ref();
        }
        if let Some(src) = self.base.masked_map.get(filepath) {
            return src.as_ref();
        }
        // nothing found, insert into cache
        let src = self.load_file(filepath);
        let alloc = self.arena.alloc((true, filepath.to_path_buf(),
                                      src.src.with_src(scopes::mask_comments(src))));
        self.masked_map.borrow_mut().insert(&alloc.1, &alloc.2);
        alloc.2.as_ref()
    }
}

pub struct Session<'c> {
    query_path: path::PathBuf,            // the input path of the query
    substitute_file: path::PathBuf,       // the temporary file
    cache: SessionFileCache<'c>,          // cache for file contents
}

pub type SessionRef<'c> = &'c Session<'c>;


impl<'c> fmt::Debug for Session<'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Session({:?}, {:?})", self.query_path, self.substitute_file)
    }
}

impl<'c> Session<'c> {
    pub fn from_path(cache: FileCache,
                     query_path: &path::Path,
                     substitute_file: &path::Path) -> Session<'c> {
        Session {
            query_path: query_path.to_path_buf(),
            substitute_file: substitute_file.to_path_buf(),
            cache: SessionFileCache::new(cache),
        }
    }

    /// Transfer allocated cache entries to the long lived cache and return it.
    pub fn destroy(mut self) -> FileCache {
        for (masked, filepath, src) in self.cache.arena.into_vec() {
            if masked {
                self.cache.base.masked_map.insert(filepath, src);
            } else {
                self.cache.base.raw_map.insert(filepath, src);
            }
        }
        self.cache.base
    }

    /// Resolve appropriate path for current query
    ///
    /// If path is the query path, returns the substitute file
    fn resolve_path<'a>(&'a self, path: &'a path::Path) -> &path::Path {
        if path == self.query_path.as_path() {
            &self.substitute_file
        } else {
            path
        }
    }

    pub fn load_file(&'c self, filepath: &path::Path) -> Src<'c> {
        self.cache.load_file(self.resolve_path(filepath))
    }

    pub fn load_file_and_mask_comments(&'c self, filepath: &path::Path) -> Src<'c> {
        self.cache.load_file_and_mask_comments(self.resolve_path(filepath))
    }
}


pub fn complete_from_file(src: &str, filepath: &path::Path,
                          pos: usize, session: SessionRef) -> vec::IntoIter<Match> {
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
