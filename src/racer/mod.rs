use std::io::File;
use std::io::BufferedReader;
use std::{str,vec,fmt,path};

pub mod scopes;
pub mod ast;
pub mod typeinf;
pub mod nameres;
pub mod codeiter;
pub mod codecleaner;
pub mod testutils;
pub mod util;
pub mod matchers;

#[cfg(test)] pub mod test;

#[deriving(Show,Clone,PartialEq)]
pub enum MatchType {
    Struct,
    Module,
    Function,
    Crate,
    Let,
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

impl Copy for MatchType {}

#[deriving(Show)]
pub enum SearchType {
    ExactMatch,
    StartsWith
}

impl Copy for SearchType {}

#[deriving(Show)]
pub enum Namespace {
    TypeNamespace,
    ValueNamespace,
    BothNamespaces
}

impl Copy for Namespace {}

#[deriving(Show)]
pub enum CompletionType {
    CompleteField,
    CompletePath
}

impl Copy for CompletionType {}

#[deriving(Clone)]
pub struct Match {
    pub matchstr: String,
    pub filepath: path::Path,
    pub point: uint,
    pub local: bool,
    pub mtype: MatchType,
    pub contextstr: String,
    pub generic_args: Vec<String>,
    pub generic_types: Vec<PathSearch>  // generic types are evaluated lazily
}


impl Match {
    fn with_generic_types(&self, generic_types: Vec<PathSearch>) -> Match {
        Match {
            matchstr: self.matchstr.clone(),
            filepath: self.filepath.clone(),
            point: self.point,
            local: self.local,
            mtype: self.mtype,
            contextstr: self.contextstr.clone(),
            generic_args: self.generic_args.clone(),
            generic_types: generic_types
        }
    }
}

impl fmt::Show for Match {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Match [{}, {}, {}, {}, {}, {}, {} |{}|]", 
               self.matchstr, 
               self.filepath.as_str(), 
               self.point, 
               self.local, 
               self.mtype, 
               self.generic_args,
               self.generic_types,
               self.contextstr)
    }
}

#[deriving(Clone)]
pub struct Scope {
    pub filepath: path::Path,
    pub point: uint
}

impl Scope {
    pub fn from_match(m: &Match) -> Scope {
        Scope{filepath: m.filepath.clone(), point: m.point}
    }
}

impl fmt::Show for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope [{}, {}]", 
               self.filepath.as_str(), 
               self.point)
    }
}

// Represents a type. Equivilent to rustc's ast::Ty but can be passed across threads
#[deriving(Show,Clone)]
pub enum Ty {
    TyMatch(Match),
    TyPathSearch(Path, Scope),   // A path + the scope to be able to resolve it
    TyTuple(Vec<Ty>),
    TyUnsupported
}

// The racer implementation of an ast::Path. Difference is that it is Send-able
#[deriving(Clone)]
pub struct Path {
    global: bool,
    segments: Vec<PathSegment>
}

impl Path {
    pub fn generic_types(&self) -> ::std::slice::Items<Path> {
        return self.segments[self.segments.len()-1].types.iter();
    }
}

impl fmt::Show for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "P["));
        let mut first = true;
        for seg in self.segments.iter() {
            if first {
                try!(write!(f, "{}", seg.name));
                first = false;
            } else {
                try!(write!(f, "::{}", seg.name));
            }

            if !seg.types.is_empty() {
                try!(write!(f, "<"));
                let mut tfirst = true;
                for typath in seg.types.iter() {
                    if tfirst {
                        try!(write!(f, "{}", typath));
                        tfirst = false;
                    } else {
                        try!(write!(f, ",{}", typath))
                    }
                }
                try!(write!(f, ">"));
            }
        }
        return write!(f, "]");
    }
}

#[deriving(Show,Clone)]
pub struct PathSegment {
    pub name: String,
    pub types: Vec<Path>
}

#[deriving(Clone)]
pub struct PathSearch {
    path: Path,
    filepath: path::Path,
    point: uint
}

impl fmt::Show for PathSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Search [{}, {}, {}]", 
               self.path, 
               self.filepath.as_str(), 
               self.point)
    }
}

pub fn load_file_and_mask_comments(filepath: &path::Path) -> String {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);
    return msrc;
}

pub fn complete_from_file(src: &str, filepath: &path::Path, pos: uint) -> vec::MoveItems<Match> {

    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = src.slice(start,pos);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("{}: contextstr is |{}|, searchstr is |{}|",
           completetype, contextstr, searchstr);

    let mut out = Vec::new();

    match completetype {
        CompletionType::CompletePath => {
            let mut v = expr.split_str("::").collect::<Vec<_>>();
            let mut global = false;
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::io::blah
                v.remove(0);
                global = true;
            }

            let segs = v
                .iter()
                .map(|x| PathSegment{name:x.to_string(), types: Vec::new()})
                .collect::<Vec<_>>();
            let path = Path{ global: global, segments: segs };

            for m in nameres::resolve_path(&path, filepath, pos, 
                                         SearchType::StartsWith, Namespace::BothNamespaces) {
                out.push(m);
            }
        },
        CompletionType::CompleteField => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            debug!("complete_from_file context is {}", context);
            context.map(|ty| {
                match ty {
                    Ty::TyMatch(m) => {
                        for m in nameres::search_for_field_or_method(m, searchstr, SearchType::StartsWith) {
                            out.push(m)
                        }
                    }
                    _ => {}
                }
            });
        }
    }
    return out.into_iter();
}


pub fn find_definition(src: &str, filepath: &path::Path, pos: uint) -> Option<Match> {
    return find_definition_(src, filepath, pos);
}

pub fn find_definition_(src: &str, filepath: &path::Path, pos: uint) -> Option<Match> {
    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = src.slice(start,end);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("find_definition_ for |{}| |{}| {}",contextstr, searchstr, completetype);

    return match completetype {
        CompletionType::CompletePath => {
            let mut v = expr.split_str("::").collect::<Vec<_>>();
            let mut global = false;
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::io::blah
                v.remove(0);
                global = true;
            }

            let segs = v
                .iter()
                .map(|x| PathSegment{ name: x.to_string(), types: Vec::new() })
                .collect::<Vec<_>>();
            let path = Path{ global: global, segments: segs };

            return nameres::resolve_path(&path, filepath, pos, 
                                         SearchType::ExactMatch, Namespace::BothNamespaces).nth(0);
        },
        CompletionType::CompleteField => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            debug!("context is {}",context);

            return context.and_then(|ty| {
                // for now, just handle matches
                match ty {
                    Ty::TyMatch(m) => {
                        return nameres::search_for_field_or_method(m, searchstr, SearchType::ExactMatch).nth(0);
                    }
                    _ => None
                }
            });
        }
    }
}
