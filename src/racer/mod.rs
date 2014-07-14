use std::io::File;
use std::io::BufferedReader;
use std::str;

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

pub enum MatchType {
    Struct,
    Module,
    Function,
    Crate,
    Let,
    StructField,
    Impl,
    Enum,
    Type,
    FnArg,
    Trait
}

pub enum SearchType {
    ExactMatch,
    StartsWith
}

pub enum Namespace {
    TypeNamespace,
    ValueNamespace,
    BothNamespaces
}

pub enum CompletionType {
    Field,
    Path
}

pub struct Match {
    pub matchstr: String,
    pub filepath: Path,
    pub point: uint,
    pub local: bool,
    pub mtype: MatchType
}

pub fn load_file_and_mask_comments(filepath: &Path) -> String {
    let filetxt = BufferedReader::new(File::open(filepath)).read_to_end().unwrap();
    let src = str::from_utf8(filetxt.as_slice()).unwrap();
    let msrc = scopes::mask_comments(src);
    return msrc;
}

pub fn complete_from_file(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {

    let start = scopes::get_start_of_search_expr(src, pos);
    let expr = src.slice(start,pos);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL contextstr is |{}|, searchstr is |{}|",contextstr, searchstr);


    match completetype {
        Path => {
            let v : Vec<&str> = expr.split_str("::").collect();
            nameres::resolve_path(v.as_slice(), filepath, pos, StartsWith, BothNamespaces, outputfn);
        },
        Field => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            context.map(|m| {
                nameres::search_for_field(m, searchstr, StartsWith, outputfn);
            });
        }
    }
}

pub fn find_definition(src: &str, filepath: &Path, pos: uint) -> Option<Match> {
    return util::first_match(|m| find_definition_(src, filepath, pos, m));
}

pub fn find_definition_(src: &str, filepath: &Path, pos: uint, outputfn: &mut |Match|) {

    let (start, end) = scopes::expand_search_expr(src, pos);
    let expr = src.slice(start,end);

    let (contextstr, searchstr, completetype) = scopes::split_into_context_and_completion(expr);

    debug!("PHIL searching for |{}| |{}| {:?}",contextstr, searchstr, completetype);

    let find_definition_output_fn = &mut |m: Match| {
        if m.matchstr == searchstr.to_string() {  // only if is an exact match
            (*outputfn)(m);
        }
    };

    match completetype {
        Path => {
            let v : Vec<&str> = expr.split_str("::").collect();
            nameres::resolve_path(v.as_slice(), filepath, pos, ExactMatch, BothNamespaces, find_definition_output_fn);
        },
        Field => {
            let context = ast::get_type_of(contextstr.to_string(), filepath, pos);
            context.map(|m| {
                nameres::search_for_field(m, searchstr, ExactMatch, find_definition_output_fn);
            });
        }
    }
}
