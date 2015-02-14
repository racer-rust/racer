use syntax::ast::{FunctionRetTy, Method_};
use syntax::ext::quote::rt::ToSource;
use racer::ast::with_error_checking_parse;
use racer::{Match, MatchType};

pub fn snippet_for_match(m : &Match) -> String {
    match m.mtype {
        MatchType::Function => MethodInfo::from_source_str(&m.contextstr).snippet(),
        _ => m.matchstr.clone()
    }
}

struct MethodInfo {
    name : String,
    output : Option<String>,
    args : Vec<String>
}

impl MethodInfo {

    ///Parses method declaration as string and returns relevant data
    fn from_source_str(source : &str) -> MethodInfo {

        let trim: &[_] = &['\n', '\r', '{', ' '];
        let decorated = format!("{} {{}}()", source.trim_right_matches(trim));

        with_error_checking_parse(decorated, |p| {

            let ref method = p.parse_method_with_outer_attributes();

            match method.node {
                Method_::MethDecl(ident, _, _, _, _,ref decl, _, _) => 
                    MethodInfo { 
                        name: ident.to_source(), 
                        args: decl.inputs.iter().map(|a| (*a).to_source()).collect(),
                        output: match decl.output {
                            FunctionRetTy::Return(ref tp) => Some(tp.to_source()),
                            _ => None
                        } 
                    },
                _ => panic!("Unable to parse method declaration.")
            }
        })
    }

    ///Returns completion snippets usable by some editors
    fn snippet(&self) -> String {
        let args: String = 
        if self.args.len() > 0 && 
         !(self.args.len() == 1 && self.args[0] == "self")  {
            self.args.iter()
                    .filter(|s| &s[] != "self")
                    .enumerate()
                    .fold(String::new(), |cur, (i, ref s)|
                      cur + &format!(", ${{{}:{}}}", i+1, s)[])[2..].to_string()
        } else {
            // TODO: Figure out why this happens. It appears this happens when 
            // constants are handled like a method
            // produces matches like this one
            // MATCH consts;consts;504;8;/Users/byron/Documents/dev/ext/rust-lang/rust/src/libstd/env.rs;Module;pub mod consts
            "".to_string()
        };
        format!("{}({})", self.name, args)
    }
}


#[test]
fn method_info_test() {
    let info = MethodInfo::from_source_str("pub fn new() -> Vec<T>");
    assert_eq!(info.name, "new".as_slice());
    assert_eq!(info.args.len(), 0);
    assert_eq!(info.output, Some("Vec<T>".to_string()));

    let info = MethodInfo::from_source_str("pub fn reserve(&mut self, additional: uint)");
    assert_eq!(info.name, "reserve".as_slice());
    assert_eq!(info.args.len(), 2);
    assert_eq!(info.args[0].as_slice(), "self");
}
