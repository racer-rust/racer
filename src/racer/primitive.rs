use ast_types::PathSegment;
use core::{BytePos, Match, MatchType, Namespace, SearchType, Session};
use matchers::ImportInfo;
use nameres::{self, RUST_SRC_PATH};

const PRIM_DOC: &str = "libstd/primitive_docs.rs";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimKind {
    Bool,
    Never,
    Char,
    Unit,
    Pointer,
    Array,
    Slice,
    Str,
    Tuple,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    Isize,
    Usize,
    Ref,
    Fn,
}

// TODO: use trie
const PRIM_KINDS: [PrimKind; 25] = [
    PrimKind::Bool,
    PrimKind::Never,
    PrimKind::Char,
    PrimKind::Unit,
    PrimKind::Pointer,
    PrimKind::Array,
    PrimKind::Slice,
    PrimKind::Str,
    PrimKind::Tuple,
    PrimKind::F32,
    PrimKind::F64,
    PrimKind::I8,
    PrimKind::I16,
    PrimKind::I32,
    PrimKind::I64,
    PrimKind::I128,
    PrimKind::U8,
    PrimKind::U16,
    PrimKind::U32,
    PrimKind::U64,
    PrimKind::U128,
    PrimKind::Isize,
    PrimKind::Usize,
    PrimKind::Ref,
    PrimKind::Fn,
];

impl PrimKind {
    fn match_name(self) -> &'static str {
        match self {
            PrimKind::Bool => "bool",
            PrimKind::Never => "never",
            PrimKind::Char => "char",
            PrimKind::Unit => "unit",
            PrimKind::Pointer => "pointer",
            PrimKind::Array => "array",
            PrimKind::Slice => "slice",
            PrimKind::Str => "str",
            PrimKind::Tuple => "tuple",
            PrimKind::F32 => "f32",
            PrimKind::F64 => "f64",
            PrimKind::I8 => "i8",
            PrimKind::I16 => "i16",
            PrimKind::I32 => "i32",
            PrimKind::I64 => "i64",
            PrimKind::I128 => "i128",
            PrimKind::U8 => "u8",
            PrimKind::U16 => "u16",
            PrimKind::U32 => "u32",
            PrimKind::U64 => "u64",
            PrimKind::U128 => "u128",
            PrimKind::Isize => "isize",
            PrimKind::Usize => "usize",
            PrimKind::Ref => "ref",
            PrimKind::Fn => "fn",
        }
    }
    pub fn to_doc_match(self, session: &Session) -> Option<Match> {
        let seg: PathSegment = format!("prim_{}", self.match_name()).into();
        let src_path = RUST_SRC_PATH.as_ref()?;
        let prim_path = src_path.join(PRIM_DOC);
        let mut m = nameres::resolve_name(
            &seg,
            &prim_path,
            BytePos::ZERO,
            SearchType::ExactMatch,
            Namespace::Mod,
            session,
            &ImportInfo::default(),
        ).next()?;
        m.mtype = MatchType::Builtin;
        Some(m)
    }
}

pub fn get_primitives(searchstr: &str, stype: SearchType, session: &Session, out: &mut Vec<Match>) {
    for prim in PRIM_KINDS.iter() {
        let prim_str = prim.match_name();
        if (stype == SearchType::StartsWith && prim_str.starts_with(searchstr))
            || (stype == SearchType::ExactMatch && prim_str == searchstr)
        {
            if let Some(m) = prim.to_doc_match(session) {
                out.push(m);
                if stype == SearchType::ExactMatch {
                    return;
                }
            }
        }
    }
}
