use std::io::File;
use std::io::BufferedReader;
use std::{str,vec,fmt};

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
    Trait
}

#[deriving(Show)]
pub enum SearchType {
    ExactMatch,
    StartsWith
}

#[deriving(Show)]
pub enum Namespace {
    TypeNamespace,
    ValueNamespace,
    BothNamespaces
}

#[deriving(Show)]
pub enum CompletionType {
    Field,
    Path
}

#[deriving(Clone)]
pub struct Match {
    pub matchstr: String,
    pub filepath: Path,
    pub point: uint,
    pub local: bool,
    pub mtype: MatchType,
    pub contextstr: String,
    pub generic_args: Vec<String>,
    pub generic_types: Vec<Match>
}

impl fmt::Show for Match {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Match [{}, {}, {}, {}, {}, |{}|]", self.matchstr, self.filepath.as_str(), 
               self.point, self.local, self.mtype, self.contextstr)
    }
}


pub fn load_file_and_mask_comments(filepath: &Path) -> String {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);
    return msrc;
}

pub fn complete_from_file(src: &str, filepath: &Path, pos: uint) -> vec::MoveItems<Match> {

    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = src.slice(start,pos);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL {}: contextstr is |{}|, searchstr is |{}|",
           completetype, contextstr, searchstr);

    let mut out = Vec::new();

    match completetype {
        Path => {
            let mut v : Vec<&str> = expr.split_str("::").collect();
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::io::blah
                v.remove(0);
            }
            for m in nameres::resolve_path(v.as_slice(), filepath, pos, 
                                         StartsWith, BothNamespaces) {
                out.push(m);
            }
        },
        Field => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            debug!("PHIL complete_from_file context is {}", context);
            context.map(|m| {
                for m in nameres::search_for_field_or_method(m, searchstr, StartsWith) {
                    out.push(m)
                }
            });
        }
    }
    return out.move_iter();
}


pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return find_definition_(src, filepath, pos);
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint) -> Option<Match> {

    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = src.slice(start,end);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL searching for |{}| |{}| {:?}",contextstr, searchstr, completetype);

    return match completetype {
        Path => {
            let mut v : Vec<&str> = expr.split_str("::").collect();
            if v[0] == "" {      // i.e. starts with '::' e.g. ::std::io::blah
                v.remove(0);
            }
            return nameres::resolve_path(v.as_slice(), filepath, pos, ExactMatch, BothNamespaces).nth(0);
        },
        Field => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            debug!("PHIL context is {}",context);

            return context.and_then(|m| {
                return nameres::search_for_field_or_method(m, searchstr, ExactMatch).nth(0);
            });
        }
    }
}
