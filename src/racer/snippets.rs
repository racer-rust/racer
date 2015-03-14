use syntax::ast::{ImplItem_};
use syntax::ext::quote::rt::ToSource;
use racer::ast::with_error_checking_parse;
use racer::{Match, MatchType};
use racer::typeinf::get_function_declaration;

pub fn snippet_for_match(m : &Match) -> String {
    match m.mtype {
        MatchType::Function => {
            let method= get_function_declaration(&m);
            MethodInfo::from_source_str(&method).snippet()
        }
        _ => m.matchstr.clone()
    }
}

struct MethodInfo {
    name : String,
    args : Vec<String>
}

impl MethodInfo {

    ///Parses method declaration as string and returns relevant data
    fn from_source_str(source : &str) -> MethodInfo {

        let trim: &[_] = &['\n', '\r', '{', ' '];
        let decorated = format!("{} {{}}()", source.trim_right_matches(trim));

        with_error_checking_parse(decorated, |p| {
            let ref method = p.parse_impl_item_with_outer_attributes();

            match method.node {
                ImplItem_::MethodImplItem(ref msig, _) => {
                    let ref decl = msig.decl;
                    MethodInfo {
                        name: method.ident.to_source(),
                        args: decl.inputs.iter().map(|a| (*a).to_source()).collect(),
                    }
                },
                _ => { panic!("Unable to parse method declaration.") }
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
    let info = MethodInfo::from_source_str("pub fn new() -> Vec<T>");
    assert_eq!(info.name, "new".as_slice());
    assert_eq!(info.args.len(), 0);
    assert_eq!(info.snippet(), "new()");

    let info = MethodInfo::from_source_str("pub fn reserve(&mut self, additional: uint)");
    assert_eq!(info.name, "reserve".as_slice());
    assert_eq!(info.args.len(), 2);
    assert_eq!(info.args[0].as_slice(), "self");
    assert_eq!(info.snippet(), "reserve(${1:additional: uint})");
}
