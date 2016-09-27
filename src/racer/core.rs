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
use std::rc::Rc;
use codeiter::StmtIndicesIter;

use scopes;
use nameres;
use ast;
use codecleaner;

#[derive(Debug, Clone, Copy, PartialEq)]
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
    EnumVariant,
    Type,
    FnArg,
    Trait,
    Const,
    Static,
    Macro,
    Builtin,
}

#[derive(Debug, Clone, Copy)]
pub enum SearchType {
    ExactMatch,
    StartsWith
}

#[derive(Debug, Clone, Copy)]
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
    pub docs: String,
}


impl Match {
    pub fn with_generic_types(self, generic_types: Vec<PathSearch>) -> Match {
        Match {
            matchstr: self.matchstr,
            filepath: self.filepath,
            point: self.point,
            local: self.local,
            mtype: self.mtype,
            contextstr: self.contextstr,
            generic_args: self.generic_args,
            generic_types: generic_types,
            docs: self.docs,
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
               self.filepath.display(),
               self.point)
    }
}

pub struct IndexedSource {
    pub code: String,
    pub idx: Vec<(usize, usize)>,
    pub lines: RefCell<Vec<(usize, usize)>>
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

    pub fn from(&self, from: usize) -> Src {
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

    pub fn get_line(&self, linenum: usize) -> Option<&str> {
        self.cache_lineoffsets();
        self.lines.borrow().get(linenum - 1).map(|&(i, l)| &self.code[i..i+l])
    }

    pub fn coords_to_point(&self, linenum: usize, col: usize) -> Option<usize> {
        self.cache_lineoffsets();
        self.lines.borrow().get(linenum - 1).and_then(|&(i, l)| {
            if col <= l { Some(i + col) } else { None }
        })
    }

    pub fn point_to_coords(&self, point: usize) -> Option<(usize, usize)> {
        self.cache_lineoffsets();
        for (n, &(i, l)) in self.lines.borrow().iter().enumerate() {
            if i <= point && (point - i) <= l {
                return Some((n + 1, point - i));
            }
        }
        None
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
    assert_eq!(src.coords_to_point(3, 5), Some(18));
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
        let (a,b) = src.point_to_coords(src.coords_to_point(lineno, charno).unwrap()).unwrap();
        assert_eq!((a,b), (lineno,charno));
    }
    round_trip_point_and_coords(src, 4, 5);
}


impl<'c> Src<'c> {
    pub fn iter_stmts(&self) -> Fuse<StmtIndicesIter<CodeChunkIter>> {
        StmtIndicesIter::from_parts(self, self.chunk_indices())
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
    raw_map: RefCell<HashMap<path::PathBuf, Rc<IndexedSource>>>,
    /// masked source for cached files
    masked_map: RefCell<HashMap<path::PathBuf, Rc<IndexedSource>>>,
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
            raw_map: RefCell::new(HashMap::new()),
            masked_map: RefCell::new(HashMap::new()),
        }
    }

    /// Add/Replace a file in both versions.
    pub fn cache_file_contents<T>(&self, filepath: &path::Path, buf: T)
        where T: Into<String>
    {
        let src = IndexedSource::new(buf.into());
        let masked_src = IndexedSource::new(scopes::mask_comments(src.as_src()));
        self.raw_map.borrow_mut().insert(filepath.to_path_buf(), Rc::new(src));
        self.masked_map.borrow_mut().insert(filepath.to_path_buf(), Rc::new(masked_src));
    }

    fn open_file(&self, path: &path::Path) -> io::Result<File> {
        File::open(path)
    }

    fn read_file(&self, path: &path::Path) -> Vec<u8> {
        let mut rawbytes = Vec::new();
        let mut f = self.open_file(path).unwrap();
        f.read_to_end(&mut rawbytes).unwrap();
        // skip BOM bytes, if present
        if rawbytes.len() > 2 && rawbytes[0..3] == [0xEF, 0xBB, 0xBF] {
            let mut it = rawbytes.into_iter();
            it.next(); it.next(); it.next();
            it.collect()
        } else {
            rawbytes
        }
    }

    pub fn load_file(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        if let Some(src) = self.raw_map.borrow().get(filepath) {
            return src.clone();
        }
        // nothing found, insert into cache
        let rawbytes = self.read_file(filepath);
        let res = String::from_utf8(rawbytes).unwrap();
        let src = Rc::new(IndexedSource::new(res));
        self.raw_map.borrow_mut().insert(filepath.to_path_buf(), src.clone());
        src
    }

    pub fn load_file_and_mask_comments(&self, filepath: &path::Path) -> Rc<IndexedSource> {
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

pub struct Session<'c> {
    query_path: path::PathBuf,            // the input path of the query
    substitute_file: path::PathBuf,       // the temporary file
    cache: &'c FileCache,                 // cache for file contents
}


impl<'c> fmt::Debug for Session<'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Session({:?}, {:?})", self.query_path, self.substitute_file)
    }
}

impl<'c> Session<'c> {
    pub fn from_path(cache: &'c FileCache,
                     query_path: &path::Path,
                     substitute_file: &path::Path) -> Session<'c> {
        Session {
            query_path: query_path.to_path_buf(),
            substitute_file: substitute_file.to_path_buf(),
            cache: cache
        }
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

    pub fn cache_file_contents<T>(&self, filepath: &path::Path, buf: T)
        where T: Into<String>
    {
        self.cache.cache_file_contents(filepath, buf);
    }

    pub fn load_file(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        self.cache.load_file(self.resolve_path(filepath))
    }

    pub fn load_file_and_mask_comments(&self, filepath: &path::Path) -> Rc<IndexedSource> {
        self.cache.load_file_and_mask_comments(self.resolve_path(filepath))
    }
}


pub fn complete_from_file(src: &str, filepath: &path::Path,
                          pos: usize, session: &Session) -> vec::IntoIter<Match> {
    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = &src[start..pos];

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
            let linestart = scopes::get_line(src, pos);

            // step 1, get full line, take the rightmost part split by semicolon
            //   prevent the case that someone write multiple line in one line
            let mut line = src[linestart..pos].trim().rsplit(';').nth(0).unwrap();
            debug!("Complete path with line: {:?}", line);

            let is_global = line.starts_with("::");
            let is_use = line.starts_with("use ");

            let v = if is_use || is_global {
                if is_use { line = &line[4..]; }
                if is_global { line = &line[2..]; }

                line.split("::").collect::<Vec<_>>()
            } else {
                expr.split("::").collect::<Vec<_>>()
            };

            let path = Path::from_vec(is_global, v);
            for m in nameres::resolve_path(&path, filepath, pos,
                                         SearchType::StartsWith, Namespace::Both,
                                         session) {
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
    out.into_iter()
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

pub fn find_definition(src: &str, filepath: &path::Path, pos: usize, session: &Session) -> Option<Match> {
    find_definition_(src, filepath, pos, session)
}

pub fn find_definition_(src: &str, filepath: &path::Path, pos: usize, session: &Session) -> Option<Match> {
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
                                  session).nth(0)
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
