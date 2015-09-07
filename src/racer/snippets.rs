use ast::with_error_checking_parse;
use core::{Match, MatchType, SessionRef};
use typeinf::get_function_declaration;

use syntex_syntax::ast::ImplItem_;

pub fn snippet_for_match(m: &Match, session: SessionRef) -> String {
    match m.mtype {
        MatchType::Function => {
            let method = get_function_declaration(&m, session);
            if let Some(m) = MethodInfo::from_source_str(&method) {
                m.snippet()
            } else {
                "".into()
            }
        }
        _ => m.matchstr.clone()
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
            use std::result::Result::{Ok, Err};
            use syntex_syntax::diagnostic::FatalError;
            match p.parse_impl_item() {
                Ok(method) => {
                    match method.node {
                        ImplItem_::MethodImplItem(ref msig, _) => {
                            let ref decl = msig.decl;
                            Some(MethodInfo {
                                // ident.as_str calls Ident.name.as_str
                                name: method.ident.name.as_str().to_string(),
                                args: decl.inputs.iter().map(|arg| {
                                    let ref codemap = p.sess.span_diagnostic.cm;
                                    match codemap.span_to_snippet(arg.pat.span) {
                                        Ok(name) => name,
                                        _ => "".into()
                                    }
                                }).collect(),
                            })
                        },
                        _ => {
                            debug!("Unable to parse method declaration. |{}|", source);
                            None
                        }
                    }
                },
                Err(FatalError) => {
                    debug!("Unable to parse method declaration. |{}|",source);
                    None
                }
            }
        })
    }

    ///Returns completion snippets usable by some editors
    fn snippet(&self) -> String {
        format!("{}({})", self.name, &self.args.iter()
            .filter(|&s| *s != "self").enumerate()
            .fold(String::new(), |cur, (i, ref s)| {
                let arg = format!("${{{}:{}}}", i+1, s);
                let delim = if i > 0 {", "} else {""};
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
    assert_eq!(info.args[0], "self");
    assert_eq!(info.snippet(), "reserve(${1:additional})");
}
