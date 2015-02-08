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

            if let Method_::MethDecl(ident, _, _, _, _,ref decl, _, _) = method.node {
                let mut args = Vec::new();
                let mut out = None;

                if let FunctionRetTy::Return(ref tp) = decl.output {
                    out = Some(tp.to_source());
                }

                for arg in decl.inputs.iter() {
                    args.push(arg.to_source());
                }

                return MethodInfo{ name:ident.to_source(), args:args, output:out};
            }

            panic!("Unable to parse method declaration.");
        })
    }

    ///Returns completion snippets usable by some editors
    fn snippet(&self) -> String {
        let mut args = String::new();
        let mut counter = 1;
        for arg in self.args.iter() {
            if arg.as_slice() == "self" {
                continue;
            }

            if counter > 1 {
                args.push_str(", ");
            }

            args.push_str(format!("${{{}:{}}}", counter, arg).as_slice());
            counter = counter + 1;
        }

        return format!("{}({})", self.name, args);
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