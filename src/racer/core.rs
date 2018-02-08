use std::fs::File;
use std::io::Read;
use std::{vec, fmt};
use std::{str, path};
use std::io;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::slice;
use std::cmp::{min, max};
use std::iter::{Fuse, Iterator};
use std::rc::Rc;
use codeiter::StmtIndicesIter;
use matchers::PendingImports;

use scopes;
use nameres;
use ast;
use codecleaner;
use util;

/// Within a [`Match`], specifies what was matched
///
/// [`Match`]: struct.Match.html
#[derive(Debug, Clone, PartialEq)]
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
    TraitImpl,
    Enum,
    /// EnumVariant needs to have Enum type to complete methods
    EnumVariant(Option<Box<Match>>),
    Type,
    FnArg,
    Trait,
    Const,
    Static,
    Macro,
    Builtin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SearchType {
    ExactMatch,
    StartsWith
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Namespace {
    Type,
    Value,
    Both
}

#[derive(Debug, Clone, Copy)]
pub enum CompletionType {
    Field,
    Path
}

/// A byte offset in a file.
pub type Point = usize;

/// A range of text between two positions.
pub type SourceByteRange = (Point, Point);

/// Line and Column position in a file
#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub struct Coordinate {
    /// Line number, 1 based
    pub line: usize,

    /// Column number - 1 based
    pub column: usize,
}

/// Context, source, and etc. for detected completion or definition
#[derive(Clone, PartialEq)]
pub struct Match {
    pub matchstr: String,
    pub filepath: path::PathBuf,
    pub point: Point,
    pub coords: Option<Coordinate>,
    pub local: bool,
    pub mtype: MatchType,
    pub contextstr: String,
    pub generic_args: Vec<String>,
    pub generic_types: Vec<PathSearch>,  // generic types are evaluated lazily
    pub docs: String,
}

impl Match {
    /// Checks if two matches can be considered the same for deduplication purposes.
    ///
    /// This could be the basis for a `PartialEq` implementation in the future,
    /// but in the interest of minimizing the crate's public API surface it's exposed
    /// as a private method for now.
    fn is_same_as(&self, other: &Match) -> bool {
        self.point == other.point 
        && self.matchstr == other.matchstr
        && self.filepath == other.filepath
    }
}

/// The cursor position used by public search methods
#[derive(Debug, Clone, Copy)]
pub enum Location {
    /// A byte offset in the file
    Point(Point),

    /// 1-based line and column indices.
    Coords(Coordinate),
}

impl From<Point> for Location {
    fn from(val: Point) -> Location {
        Location::Point(val)
    }
}

impl From<Coordinate> for Location {
    fn from(val: Coordinate) -> Location {
        Location::Coords(val)
    }
}

/// Internal cursor methods
pub trait LocationExt {
    fn to_point(&self, src: &IndexedSource) -> Option<Point>;
    fn to_coords(&self, src: &IndexedSource) -> Option<Coordinate>;
}

impl LocationExt for Location {
    fn to_point(&self, src: &IndexedSource) -> Option<Point> {
        match *self {
            Location::Point(val) => Some(val),
            Location::Coords(ref coords) => {
                src.coords_to_point(coords)
            }
        }
    }

    fn to_coords(&self, src: &IndexedSource) -> Option<Coordinate> {
        match *self {
            Location::Coords(val) => Some(val),
            Location::Point(point) => src.point_to_coords(point)
        }
    }
}


impl fmt::Debug for Match {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Match [{:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?} |{}|]",
               self.matchstr,
               self.filepath.display(),
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
    pub point: Point
}

impl Scope {
    pub fn from_match(m: &Match) -> Scope {
        Scope{ filepath: m.filepath.clone(), point: m.point }
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope [{:?}, {:?}]",
               self.filepath.display(),
               self.point)
    }
}

// Represents a type. Equivilent to rustc's ast::Ty but can be passed across threads
#[derive(Debug,Clone)]
pub enum Ty {
    Match(Match),
    PathSearch(Path, Scope),   // A path + the scope to be able to resolve it
    Tuple(Vec<Ty>),
    FixedLengthVec(Box<Ty>, String), // ty, length expr as string
    RefPtr(Box<Ty>),
    Vec(Box<Ty>),
    Unsupported
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ty::Match(ref m) => {
                write!(f, "{}", m.matchstr)
            }
            Ty::PathSearch(ref p, _) => {
                write!(f, "{}", p)
            }
            Ty::Tuple(ref vec) => {
                let mut first = true;
                try!(write!(f, "("));
                for field in vec.iter() {
                    if first {
                        try!(write!(f, "{}", field));
                            first = false;
                    } else {
                        try!(write!(f, ", {}", field));
                    }
                }
                write!(f, ")")
            }
            Ty::FixedLengthVec(ref ty, ref expr) => {
                try!(write!(f, "["));
                try!(write!(f, "{}", ty));
                try!(write!(f, "; "));
                try!(write!(f, "{}", expr));
                write!(f, "]")
            }
            Ty::Vec(ref ty) => {
                try!(write!(f, "["));
                try!(write!(f, "{}", ty));
                write!(f, "]")
            }
            Ty::RefPtr(ref ty) => {
                write!(f, "&{}", ty)
            }
            Ty::Unsupported => {
                write!(f, "_")
            }
        }
    }
}

// The racer implementation of an ast::Path. Difference is that it is Send-able
#[derive(Clone, PartialEq)]
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
            .map(PathSegment::from)
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
                let mut t_first = true;
                for typath in &seg.types {
                    if t_first {
                        try!(write!(f, "{:?}", typath));
                        t_first = false;
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

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
                let mut t_first = true;
                for typath in &seg.types {
                    if t_first {
                        try!(write!(f, "{}", typath));
                        t_first = false;
                    } else {
                        try!(write!(f, ", {}", typath))
                    }
                }
                try!(write!(f, ">"));
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub name: String,
    pub types: Vec<Path>
}

impl From<String> for PathSegment {
    fn from(name: String) -> Self {
        PathSegment {
            name: name,
            types: Vec::new(),
        }
    }
}

/// Information about generic types in a match
#[derive(Clone, PartialEq)]
pub struct PathSearch {
    pub path: Path,
    pub filepath: path::PathBuf,
    pub point: Point
}

impl fmt::Debug for PathSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Search [{:?}, {:?}, {:?}]",
               self.path,
               self.filepath.display(),
               self.point)
    }
}

pub struct IndexedSource {
    pub code: String,
    pub idx: Vec<SourceByteRange>,
    pub lines: RefCell<Vec<SourceByteRange>>
}

#[derive(Clone,Copy)]
pub struct Src<'c> {
    pub src: &'c IndexedSource,
    pub from: Point,
    pub to: Point
}

impl IndexedSource {
    pub fn new(src: String) -> IndexedSource {
        let indices = codecleaner::code_chunks(&src).collect();
        IndexedSource {
            code: src,
            idx: indices,
            lines: RefCell::new(Vec::new())
        }
    }

    pub fn with_src(&self, new_src: String) -> IndexedSource {
        IndexedSource {
            code: new_src,
            idx: self.idx.clone(),
            lines: self.lines.clone()
        }
    }

    pub fn as_src(&self) -> Src {
        self.from(0)
    }

    pub fn from(&self, from: Point) -> Src {
        Src {
            src: self,
            from: from,
            to: self.len()
        }
    }

    fn cache_lineoffsets(&self) {
        if self.lines.borrow().len() == 0 {
            let mut result = Vec::new();
            let mut point = 0;
            for line in self.code.split('\n') {
                result.push((point, line.len() + 1));
                point += line.len() + 1;
            }
            *self.lines.borrow_mut() = result;
        }
    }

    pub fn coords_to_point(&self, coords: &Coordinate) -> Option<Point> {
        self.cache_lineoffsets();
        self.lines
            .borrow()
            .get(coords.line - 1)
            .and_then(|&(i, l)| {
                if coords.column <= l {
                    Some(i + coords.column)
                } else {
                    None
                }
            })
    }

    pub fn point_to_coords(&self, point: Point) -> Option<Coordinate> {
        self.cache_lineoffsets();
        for (n, &(i, l)) in self.lines.borrow().iter().enumerate() {
            if i <= point && (point - i) <= l {
                return Some(Coordinate { line: n + 1, column: point - i });
            }
        }
        None
    }
}

pub struct MatchIter<'c> {
    session: &'c Session<'c>,
    matches: vec::IntoIter<Match>,
}

impl<'c> Iterator for MatchIter<'c> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        self.matches
            .next()
            .map(|mut m| {
                if m.coords.is_none() {
                    let point = m.point;
                    let src = self.session.load_file(m.filepath.as_path());
                    m.coords = src.point_to_coords(point);
                }

                m
            })

    }
}

#[test]
fn coords_to_point_works() {
    let src = "
fn myfn() {
    let a = 3;
    print(a);
}";
    let src = new_source(src.into());
    assert_eq!(src.coords_to_point(&Coordinate { line: 3, column: 5}), Some(18));
}

#[test]
fn test_point_to_coords() {
    let src = "
fn myfn(b:usize) {
   let a = 3;
   if b == 12 {
       let a = 24;
       do_something_with(a);
   }
   do_something_with(a);
}
";
    fn round_trip_point_and_coords(src: &str, lineno: usize, charno: usize) {
        let src = new_source(src.into());
        let point = src.coords_to_point(&Coordinate {
            line: lineno,
            column: charno
        }).unwrap();
        let coords = src.point_to_coords(point).unwrap();
        assert_eq!(coords, Coordinate { line: lineno, column: charno });
    }
    round_trip_point_and_coords(src, 4, 5);
}


impl<'c> Src<'c> {
    pub fn iter_stmts(&self) -> Fuse<StmtIndicesIter<CodeChunkIter>> {
        StmtIndicesIter::from_parts(self, self.chunk_indices())
    }

    pub fn from(&self, from: Point) -> Src<'c> {
        Src {
            src: self.src,
            from: self.from + from,
            to: self.to
        }
    }

    pub fn to(&self, to: Point) -> Src<'c> {
        Src {
            src: self.src,
            from: self.from,
            to: self.from + to
        }
    }

    pub fn from_to(&self, from: Point, to: Point) -> Src<'c> {
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
    iter: slice::Iter<'c, SourceByteRange>
}

impl<'c> Iterator for CodeChunkIter<'c> {
    type Item = SourceByteRange;

    fn next(&mut self) -> Option<SourceByteRange> {
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

/// Caches file contents for re-use between sessions.
///
/// The file cache is an opaque blob outside of racer which contains maps of loaded and masked
/// files.
pub struct FileCache {
    /// raw source for cached files
    raw_map: RefCell<HashMap<path::PathBuf, Rc<IndexedSource>>>,

    /// masked source for cached files
    ///
    /// a version with comments and strings replaced by spaces, so that they
    /// aren't found when scanning the source for signatures.
    masked_map: RefCell<HashMap<path::PathBuf, Rc<IndexedSource>>>,

    /// The file loader
    loader: Box<FileLoader>,
}

/// Used by the FileCache for loading files
///
/// Implement one of these and pass it to `FileCache::new()` to override Racer's
/// file loading behavior.
pub trait FileLoader {
    /// Load a single file
    fn load_file(&self, path: &path::Path) -> io::Result<String>;
}

/// Provide a blanket impl for Arc<T> since Rls uses that
impl<T: FileLoader> FileLoader for ::std::sync::Arc<T> {
    fn load_file(&self, path: &path::Path) -> io::Result<String> {
        (&self as &T).load_file(path)
    }
}

/// The default file loader
///
/// Private since this shouldn't be needed outside of racer
struct DefaultFileLoader;

impl FileLoader for DefaultFileLoader {
    fn load_file(&self, path: &path::Path) -> io::Result<String> {
        let mut rawbytes = Vec::new();
        let mut f = try!(File::open(path));
        try!(f.read_to_end(&mut rawbytes));

        // skip BOM bytes, if present
        if rawbytes.len() > 2 && rawbytes[0..3] == [0xEF, 0xBB, 0xBF] {
            str::from_utf8(&rawbytes[3..])
                .map(|s| s.to_owned())
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
        } else {
            String::from_utf8(rawbytes)
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
        }
    }
}

impl Default for FileCache {
    fn default() -> FileCache {
        FileCache::new(DefaultFileLoader)
    }
}

impl FileCache {
    /// Create a new file cache
    ///
    /// In order to load files into the cache, please see
    /// [`Session::cache_file_contents()`]
    ///
    /// [`Session::cache_file_contents()`]: struct.Session.html#method.cache_file_contents
    pub fn new<L: FileLoader + 'static>(loader: L) -> FileCache {
        FileCache {
            raw_map: RefCell::new(HashMap::new()),
            masked_map: RefCell::new(HashMap::new()),
            loader: Box::new(loader),
        }
    }

    /// Remove specific files from the cache
    ///
    /// Returns true if a file was removed
    pub fn remove_file<P: AsRef<path::Path>>(&self, path: &P) -> bool {
        let path = path.as_ref();
        let mut raw = self.raw_map.borrow_mut();
        let mut masked = self.masked_map.borrow_mut();
        raw.remove(path).is_some() || masked.remove(path).is_some()
    }

    /// Add/Replace a file in both versions.
    fn cache_file_contents<P, T>(&self, filepath: P, buf: T)
        where T: Into<String>,
              P: Into<path::PathBuf>
    {
        let pathbuf = filepath.into();
        let src = IndexedSource::new(buf.into());
        let masked_src = IndexedSource::new(scopes::mask_comments(src.as_src()));
        self.raw_map.borrow_mut().insert(pathbuf.clone(), Rc::new(src));
        self.masked_map.borrow_mut().insert(pathbuf, Rc::new(masked_src));
    }


    fn load_file(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        if let Some(src) = self.raw_map.borrow().get(filepath) {
            return src.clone();
        }

        // nothing found, insert into cache
        // Ugh, really need handle results on all these methods :(
        let res = self.loader.load_file(filepath).expect("load file successfully");
        let src = Rc::new(IndexedSource::new(res));
        self.raw_map.borrow_mut().insert(filepath.to_path_buf(), src.clone());
        src
    }

    fn load_file_and_mask_comments(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        if let Some(src) = self.masked_map.borrow().get(filepath) {
            return src.clone();
        }
        // nothing found, insert into cache
        let src = self.load_file(filepath);
        let msrc = Rc::new(src.with_src(scopes::mask_comments(src.as_src())));
        self.masked_map.borrow_mut().insert(filepath.to_path_buf(),
                                            msrc.clone());
        msrc
    }
}

/// Private methods for the Session type
pub trait SessionExt {
    /// Request that a file is loaded into the cache
    ///
    /// This API is unstable and should not be used outside of Racer
    fn load_file(&self, &path::Path) -> Rc<IndexedSource>;

    /// Request that a file is loaded into the cache with comments masked
    ///
    /// This API is unstable and should not be used outside of Racer
    fn load_file_and_mask_comments(&self, &path::Path) -> Rc<IndexedSource>;
}

/// Context for a Racer operation
pub struct Session<'c> {
    /// Cache for files
    ///
    /// The file cache is used within a session to prevent multiple reads. It is
    /// borrowed here in order to support reuse across Racer operations.
    cache: &'c FileCache,
}

impl<'c> fmt::Debug for Session<'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Session {{ .. }}")
    }
}

impl<'c> Session<'c> {
    /// Create a Session for use in Racer operations
    ///
    /// * `cache` is a reference to a `FileCache`. It's take by reference for
    ///   use across racer operations.
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate racer;
    ///
    /// let cache = racer::FileCache::default();
    /// let session = racer::Session::new(&cache);
    /// ```
    ///
    /// [`FileCache`]: struct.FileCache.html
    pub fn new(cache: &'c FileCache) -> Session<'c> {
        Session {
            cache: cache
        }
    }

    /// Specify the contents of a file to be used in completion operations
    ///
    /// The path to the file and the file's contents must both be specified.
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate racer;
    ///
    /// let cache = racer::FileCache::default();
    /// let session = racer::Session::new(&cache);
    ///
    /// session.cache_file_contents("foo.rs", "pub struct Foo;\\n");
    /// ```
    pub fn cache_file_contents<T, P>(&self, filepath: P, buf: T)
        where T: Into<String>,
              P: Into<path::PathBuf>
    {
        self.cache.cache_file_contents(filepath, buf);
    }

    pub fn contains_file<P: AsRef<path::Path>>(&self, path: P) -> bool {
        let path = path.as_ref();
        let raw = self.cache.raw_map.borrow();
        let masked = self.cache.masked_map.borrow();
        raw.contains_key(path) && masked.contains_key(path)
    }

}

impl<'c> SessionExt for Session<'c> {
    fn load_file(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        self.cache.load_file(filepath)
    }

    fn load_file_and_mask_comments(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        self.cache.load_file_and_mask_comments(filepath)
    }
}

/// Get the racer point of a line/character number pair for a file.
pub fn to_point<'c, P>(
    coords: Coordinate, 
    path: P, 
    session: &'c Session
) -> Option<Point> 
    where 
        P: AsRef<path::Path> {
    Location::from(coords).to_point(&session.load_file(path.as_ref()))
}

/// Get the racer point of a line/character number pair for a file.
pub fn to_coords<'c, P>(
    point: Point, 
    path: P, 
    session: &'c Session
) -> Option<Coordinate> 
    where 
        P: AsRef<path::Path> {
    Location::from(point).to_coords(&session.load_file(path.as_ref()))
} 

/// Find completions for a fully qualified name like `std::io::`
///
/// Searchs are started relative to `path`.
///
/// * `query` - is the fqn to search for
/// * `path` - the directory to start searching in
/// * `session` - reference to a racer::Session
///
/// ```no_run
/// extern crate racer;
///
/// let path = std::path::Path::new(".");
/// let cache = racer::FileCache::default();
/// let session = racer::Session::new(&cache);
///
/// let m = racer::complete_fully_qualified_name(
///     "std::fs::canon",
///     &path,
///     &session
/// ).next().unwrap();
///
/// assert_eq!(&m.matchstr[..], "canonicalize");
/// assert_eq!(m.mtype, racer::MatchType::Function);
/// ```
#[inline]
pub fn complete_fully_qualified_name<'c, S, P>(
    query: S,
    path: P,
    session: &'c Session
) -> MatchIter<'c>
    where S: AsRef<str>,
          P: AsRef<path::Path>,
{
    let mut matches = complete_fully_qualified_name_(query.as_ref(), path.as_ref(), session);
    matches.dedup_by(|a, b| a.is_same_as(b));
    
    MatchIter {
        matches: matches.into_iter(),
        session: session
    }
}

/// Actual implementation without generic bounds
fn complete_fully_qualified_name_(
    query: &str,
    path: &path::Path,
    session: &Session
) -> Vec<Match> {
    let p: Vec<&str> = query.split("::").collect();

    let mut matches = Vec::new();

    for m in nameres::do_file_search(p[0], path, session) {
        if p.len() == 1 {
            matches.push(m);
        } else {
            let external_search_matches = nameres::do_external_search(
                &p[1..],
                &m.filepath,
                m.point,
                SearchType::StartsWith,
                Namespace::Both,
                &session
            );

            for m in external_search_matches {
                matches.push(m);
            }
        }
    }

    matches
}


/// Search for completion at position in a file
///
/// * `src` - the file contents to search in
/// * `filepath` - path to file containing `src`
/// * `pos` - byte offset in file with path/expr to complete
/// * `session` - a racer::Session
///
/// # Examples
///
/// ```
/// extern crate racer;
///
/// # fn main() {
/// let src = "
/// fn apple() {
/// }
///
/// fn main() {
///     let b = ap
/// }";
///
/// println!("{:?}", src);
///
/// let cache = racer::FileCache::default();
/// let session = racer::Session::new(&cache);
///
/// session.cache_file_contents("lib.rs", src);
///
/// let got = racer::complete_from_file("lib.rs", racer::Location::Point(42), &session)
///     .nth(0).unwrap();
/// assert_eq!("apple", got.matchstr);
/// assert_eq!(got.mtype, racer::MatchType::Function);
///
/// # }
/// ```
pub fn complete_from_file<'c, P, C>(
    filepath: P,
    cursor: C,
    session: &'c Session
) -> MatchIter<'c>
    where P: AsRef<path::Path>,
          C: Into<Location>
{
    let mut matches = complete_from_file_(filepath.as_ref(), cursor.into(), session);
    matches.dedup_by(|a, b| a.is_same_as(b));

    MatchIter {
        matches: matches.into_iter(),
        session: session,
    }
}

fn complete_from_file_(
    filepath: &path::Path,
    cursor: Location,
    session: &Session
) -> Vec<Match> {
    let src = session.load_file_and_mask_comments(filepath);
    let src_text = &src.as_src()[..];

    // TODO return result
    let pos = match cursor.to_point(&session.load_file(filepath)) {
        Some(pos) => pos,
        None => {
            debug!("Failed to convert cursor to point");
            return Vec::new();
        }
    };

    let start = scopes::get_start_of_search_expr(src_text, pos);
    let expr = &src_text[start..pos];

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("{:?}: contextstr is |{}|, searchstr is |{}|", completetype, contextstr, searchstr);

    let mut out = Vec::new();

    match completetype {
        CompletionType::Path => {
            // reparse the string, searchstr is not corrected parsed with split_into_context_and_completion
            // it will stop by character like '{', and ' ', which occurs in the following case
            // 1. The line is use contextstr::{A, B, C, searchstr
            // 2. The line started with contextstr or ::
            // 3. FIXME(may not correct): Neither above case, then expr parsed above is corrected
            let linestart = scopes::find_stmt_start(src.as_src(), pos).unwrap_or_else(|| scopes::get_line(src_text, pos));

            // step 1, get full line, take the rightmost part split by semicolon
            //   prevent the case that someone write multiple line in one line
            let line = src_text[linestart..pos].trim().rsplit(';').nth(0).unwrap();
            debug!("Complete path with line: {:?}", line);

            // Test if the **path expression** starts with `::`, in which case the path
            // should be checked against the global namespace rather than the items currently
            // in scope.
            let is_global = expr.starts_with("::");
            let is_use = line.starts_with("use ");

            // when in the function ident position, only look for methods
            // from a trait to complete.
            if util::in_fn_name(line) {
                trace!("Path is in fn declaration: `{}`", expr);

                return nameres::resolve_method(
                    pos, 
                    src.as_src(), 
                    expr, 
                    filepath, 
                    SearchType::StartsWith,
                    session,
                    &PendingImports::empty());
            }

            let v = (if is_use {
                // trim the `use ` statement
                &line[4..]
            } else if is_global {
                // trim the leading semi-colon
                &expr[2..]
            } else {
                expr
            }).split("::").collect::<Vec<_>>();

            let path = Path::from_vec(is_global, v);
            for m in nameres::resolve_path(&path, filepath, pos,
                                           SearchType::StartsWith, Namespace::Both,
                                           session, &PendingImports::empty()) {
                out.push(m);
            }
        },
        CompletionType::Field => {
            let context = ast::get_type_of(contextstr.to_owned(), filepath, pos, session);
            debug!("complete_from_file context is {:?}", context);
            context.map(|ty| {
                complete_field_for_ty(ty, searchstr, SearchType::StartsWith, session, &mut out);
            });
        }
    }
    
    out
}

fn complete_field_for_ty(ty: Ty, searchstr: &str, stype: SearchType, session: &Session, out: &mut Vec<Match>) {
    // TODO would be nice if this and other methods could operate on a ref instead of requiring
    // ownership
    match ty {
        Ty::Match(m) => {
            for m in nameres::search_for_field_or_method(m, searchstr, stype, session) {
                out.push(m)
            }
        },
        Ty::RefPtr(m) => {
            complete_field_for_ty(*m, searchstr, stype, session, out)
        }
        _ => {}
    }
}

/// Find the definition for item at given a file, source, and cursor index
///
/// # Examples
///
/// ```
/// extern crate racer;
/// extern crate env_logger;
///
/// use std::path::Path;
///
/// # fn main() {
/// let _ = env_logger::init();
/// let cache = racer::FileCache::default();
/// let session = racer::Session::new(&cache);
///
/// // This is the file where we request completion from
/// let src = stringify! {
///    mod sub;
///    use sub::foo;
///    fn main() {
///        foo();
///    };
/// };
///
/// // This is the submodule where the definition is found
/// let sub = stringify! {
///     pub fn foo() {}
/// };
///
/// // Load files into cache to prevent trying to read from disk
/// session.cache_file_contents("sub.rs", sub);
/// session.cache_file_contents("lib.rs", src);
///
/// // Search for the definition. 45 is the byte offset
/// // in `src` after stringify! runs. Specifically, this asks
/// // for the definition of `foo()`.
/// let m = racer::find_definition("lib.rs", racer::Location::Point(45), &session)
///               .expect("find definition returns a match");
///
/// // Should have found definition in the "sub.rs" file
/// assert_eq!(m.filepath, Path::new("sub.rs"));
/// // The definition should be for foo
/// assert_eq!(&m.matchstr[..], "foo");
/// // The definition should be a function
/// assert_eq!(m.mtype, racer::MatchType::Function);
/// # }
/// ```
pub fn find_definition<P, C>(
    filepath: P,
    cursor: C,
    session: &Session
) -> Option<Match>
    where P: AsRef<path::Path>,
          C: Into<Location>
{
    find_definition_(filepath.as_ref(), cursor.into(), session)
        .map(|mut m| {
            if m.coords.is_none() {
                let point = m.point;
                let src = session.load_file(m.filepath.as_path());
                m.coords = src.point_to_coords(point);
            }

            m
        })
}

pub fn find_definition_(filepath: &path::Path, cursor: Location, session: &Session) -> Option<Match> {
    let src = session.load_file_and_mask_comments(filepath);
    let src = &src.as_src()[..];

    // TODO return result
    let pos = match cursor.to_point(&session.load_file(filepath)) {
        Some(pos) => pos,
        None => {
            debug!("Failed to convert cursor to point");
            return None;
        }
    };

    // Make sure `src` is in the cache
    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = &src[start..end];
    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("find_definition_ for |{:?}| |{:?}| {:?}", contextstr, searchstr, completetype);

    match completetype {
        CompletionType::Path => {
            let mut v = expr.split("::").collect::<Vec<_>>();
            let global = v[0] == "";
            if global {      // i.e. starts with '::' e.g. ::std::old_io::blah
                v.remove(0);
            }

            let segs = v
                .into_iter()
                .map(|x| PathSegment{ name: x.to_owned(), types: Vec::new() })
                .collect::<Vec<_>>();
            let path = Path{ global: global, segments: segs };

            nameres::resolve_path(&path, filepath, pos,
                                  SearchType::ExactMatch, Namespace::Both,
                                  session, &PendingImports::empty()).nth(0)
        },
        CompletionType::Field => {
            let context = ast::get_type_of(contextstr.to_owned(), filepath, pos, session);
            debug!("context is {:?}", context);

            let match_type:MatchType = if src[end..].starts_with('(') { MatchType::Function } else { MatchType::StructField };
            context.and_then(|ty| {
                // for now, just handle matches
                if let Ty::Match(m) = ty {
                    nameres::search_for_field_or_method(m, searchstr, SearchType::ExactMatch, session).filter(|m| m.mtype == match_type).nth(0)
                } else {
                    None
                }
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use super::FileCache;
    use super::{Session, SessionExt};

    #[test]
    fn overwriting_cached_files() {
        let src1 = "src1";
        let src2 = "src2";
        let src3 = "src3";
        let src4 = "src4";

        // Need session and path to cache files
        let path = Path::new("not_on_disk");
        let cache = FileCache::default();

        // Cache contents for a file and assert that load_file and load_file_and_mask_comments return
        // the newly cached contents.
        macro_rules! cache_and_assert {
            ($src:ident) => {{
                let session = Session::new(&cache);
                session.cache_file_contents(path, $src);
                assert_eq!($src, &session.load_file(path).code[..]);
                assert_eq!($src, &session.load_file_and_mask_comments(path).code[..]);
            }}
        }

        // Check for all srcN
        cache_and_assert!(src1);
        cache_and_assert!(src2);
        cache_and_assert!(src3);
        cache_and_assert!(src4);
    }
}
