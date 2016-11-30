use ast::with_error_checking_parse;
use core::{Match, MatchType, Session};
use typeinf::get_function_declaration;

use syntex_syntax::ast::ImplItemKind;

/// Returns completion snippets usable by some editors
///
/// Generates a snippet string given a `Match`. The provided snippet contains
/// substrings like "${1:name}" which some editors can use to quickly fill in
/// arguments.
///
/// # Examples
///
/// ```no_run
/// extern crate racer;
///
/// use std::path::Path;
///
/// let path = Path::new(".");
/// let cache = racer::FileCache::default();
/// let session = racer::Session::new(&cache);
///
/// let m = racer::complete_fully_qualified_name(
///     "std::fs::canonicalize",
///     &path,
///     &session
/// ).next().unwrap();
///
/// let snip = racer::snippet_for_match(&m, &session);
/// assert_eq!(snip, "canonicalize(${1:path})");
/// ```
pub fn snippet_for_match(m: &Match, session: &Session) -> String {
    if m.mtype == MatchType::Function {
        let method = get_function_declaration(m, session);
        if let Some(m) = MethodInfo::from_source_str(&method) {
            m.snippet()
        } else {
            "".into()
        }
    } else {
        m.matchstr.clone()
    }
}

struct MethodInfo {
    name: String,
    args: Vec<String>
}

impl MethodInfo {
    ///Parses method declaration as string and returns relevant data
    fn from_source_str(source: &str) -> Option<MethodInfo> {
        let trim: &[_] = &['\n', '\r', '{', ' '];
        let decorated = format!("{} {{}}()", source.trim_right_matches(trim));

        with_error_checking_parse(decorated, |p| {
            if let Ok(method) = p.parse_impl_item() {
                if let ImplItemKind::Method(ref msig, _) = method.node {
                        let decl = &msig.decl;
                        return Some(MethodInfo {
                            // ident.as_str calls Ident.name.as_str
                            name: method.ident.name.to_string(),
                            args: decl.inputs
                                      .iter()
                                      .map(|arg| {
                                          let codemap = &p.sess.codemap();
                                          match codemap.span_to_snippet(arg.pat.span) {
                                              Ok(name) => name,
                                              _ => "".into()
                                          }
                                      })
                                      .collect()
                        })
                }
            }
            debug!("Unable to parse method declaration. |{}|", source);
            None
        })
    }

    ///Returns completion snippets usable by some editors
    fn snippet(&self) -> String {
        format!("{}({})",
                self.name,
                &self.args
                     .iter()
                     .filter(|&s| !s.ends_with("self"))
                     .enumerate()
                     .fold(String::new(), |cur, (i, ref s)| {
                         let arg = format!("${{{}:{}}}", i + 1, s);
                         let delim = if i > 0 { ", " } else { "" };
                         cur + delim + &arg
                     }))
    }
}


#[test]
fn method_info_test() {
    let info = MethodInfo::from_source_str("pub fn new() -> Vec<T>").unwrap();
    assert_eq!(info.name, "new");
    assert_eq!(info.args.len(), 0);
    assert_eq!(info.snippet(), "new()");

    let info = MethodInfo::from_source_str("pub fn reserve(&mut self, additional: uint)").unwrap();
    assert_eq!(info.name, "reserve");
    assert_eq!(info.args.len(), 2);
    assert_eq!(info.args[0], "&mut self");
    assert_eq!(info.snippet(), "reserve(${1:additional})");
}
