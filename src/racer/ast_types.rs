//! type conversion between racer types and libsyntax types
use core::{self, BytePos, Match, MatchType, Scope, Session};
use matchers::ImportInfo;
use nameres;
use std::fmt;
use std::path::{Path as FilePath, PathBuf};
use syntax::ast::{
    self, GenericArg, GenericArgs, GenericBound, GenericBounds, GenericParamKind, TraitRef, TyKind,
    WherePredicate,
};
use syntax::print::pprust;
use syntax::source_map;

/// The leaf of a `use` statement.
#[derive(Clone, Debug)]
pub struct PathAlias {
    /// the leaf of Use Tree
    /// it can be one of one of 3 types, e.g.
    /// use std::collections::{self, hashmap::*, HashMap};
    pub kind: PathAliasKind,
    /// The path.
    pub path: Path,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathAliasKind {
    Ident(String),
    Self_(String),
    Glob,
}

impl AsRef<Path> for PathAlias {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

// Represents a type. Equivilent to rustc's ast::Ty but can be passed across threads
#[derive(Debug, Clone)]
pub enum Ty {
    Match(Match),
    PathSearch(Path, Scope), // A path + the scope to be able to resolve it
    Tuple(Vec<Ty>),
    FixedLengthVec(Box<Ty>, String), // ty, length expr as string
    RefPtr(Box<Ty>),
    Vec(Box<Ty>),
    Unsupported,
}

impl Ty {
    pub(crate) fn from_ast(ty: &ast::Ty, scope: &Scope) -> Option<Ty> {
        match ty.node {
            TyKind::Tup(ref items) => {
                let mut res = Vec::new();
                for t in items {
                    res.push(match Ty::from_ast(t, scope) {
                        Some(t) => t,
                        None => return None,
                    });
                }
                Some(Ty::Tuple(res))
            }
            TyKind::Rptr(ref _lifetime, ref ty) => {
                Ty::from_ast(&ty.ty, scope).map(|ref_ty| Ty::RefPtr(Box::new(ref_ty)))
            }
            TyKind::Path(_, ref path) => Some(Ty::PathSearch(Path::from_ast(path), scope.clone())),
            TyKind::Array(ref ty, ref expr) => Ty::from_ast(ty, scope).map(|racer_ty| {
                Ty::FixedLengthVec(Box::new(racer_ty), pprust::expr_to_string(&expr.value))
            }),
            TyKind::Slice(ref ty) => {
                Ty::from_ast(ty, scope).map(|ref_ty| Ty::Vec(Box::new(ref_ty)))
            }
            TyKind::Never => None,
            _ => {
                trace!("unhandled Ty node: {:?}", ty.node);
                None
            }
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ty::Match(ref m) => write!(f, "{}", m.matchstr),
            Ty::PathSearch(ref p, _) => write!(f, "{}", p),
            Ty::Tuple(ref vec) => {
                let mut first = true;
                write!(f, "(")?;
                for field in vec.iter() {
                    if first {
                        write!(f, "{}", field)?;
                        first = false;
                    } else {
                        write!(f, ", {}", field)?;
                    }
                }
                write!(f, ")")
            }
            Ty::FixedLengthVec(ref ty, ref expr) => {
                write!(f, "[")?;
                write!(f, "{}", ty)?;
                write!(f, "; ")?;
                write!(f, "{}", expr)?;
                write!(f, "]")
            }
            Ty::Vec(ref ty) => {
                write!(f, "[")?;
                write!(f, "{}", ty)?;
                write!(f, "]")
            }
            Ty::RefPtr(ref ty) => write!(f, "&{}", ty),
            Ty::Unsupported => write!(f, "_"),
        }
    }
}

/// Prefix of path.
/// e.g. for path `::std` => Global
///      for path `self::abc` => Self_
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PathPrefix {
    Crate,
    Super,
    Self_,
    Global,
}

impl PathPrefix {
    pub(crate) fn from_str(s: &str) -> Option<PathPrefix> {
        match s {
            "crate" => Some(PathPrefix::Crate),
            "super" => Some(PathPrefix::Super),
            "self" => Some(PathPrefix::Self_),
            "{{root}}" => Some(PathPrefix::Global),
            _ => None,
        }
    }
}

// The racer implementation of an ast::Path. Difference is that it is Send-able
#[derive(Clone, PartialEq)]
pub struct Path {
    pub prefix: Option<PathPrefix>,
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn from_ast(path: &ast::Path) -> Path {
        let mut segments = Vec::new();
        for seg in path.segments.iter() {
            let name = seg.ident.name.to_string();
            let mut types = Vec::new();
            // TODO: support GenericArgs::Parenthesized (A path like `Foo(A,B) -> C`)
            if let Some(ref params) = seg.args {
                if let GenericArgs::AngleBracketed(ref angle_args) = **params {
                    angle_args.args.iter().for_each(|arg| {
                        if let GenericArg::Type(ty) = arg {
                            if let TyKind::Path(_, ref path) = ty.node {
                                types.push(Path::from_ast(path));
                            }
                        }
                    })
                }
            }
            segments.push(PathSegment::new(name, types));
        }
        Path {
            prefix: None,
            segments,
        }
    }

    pub fn generic_types(&self) -> ::std::slice::Iter<Path> {
        self.segments[self.segments.len() - 1].types.iter()
    }

    pub fn single(seg: PathSegment) -> Path {
        Path {
            prefix: None,
            segments: vec![seg],
        }
    }

    pub fn set_prefix(&mut self) {
        if self.prefix.is_some() {
            return;
        }
        self.prefix = self
            .segments
            .first()
            .and_then(|seg| PathPrefix::from_str(&seg.name));
        if self.prefix.is_some() {
            self.segments.remove(0);
        }
    }

    pub fn from_vec(global: bool, v: Vec<&str>) -> Path {
        Self::from_iter(global, v.into_iter().map(|s| s.to_owned()))
    }

    pub fn from_svec(global: bool, v: Vec<String>) -> Path {
        Self::from_iter(global, v.into_iter())
    }

    pub fn from_iter(global: bool, iter: impl Iterator<Item = String>) -> Path {
        let mut prefix = if global {
            Some(PathPrefix::Global)
        } else {
            None
        };
        let segments: Vec<_> = iter
            .enumerate()
            .filter_map(|(i, s)| {
                if i == 0 && prefix.is_none() {
                    if let Some(pre) = PathPrefix::from_str(&s) {
                        prefix = Some(pre);
                        return None;
                    }
                }
                Some(PathSegment::from(s))
            }).collect();
        Path { prefix, segments }
    }

    pub fn extend(&mut self, path: Path) -> &mut Self {
        self.segments.extend(path.segments);
        self
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn name(&self) -> Option<&str> {
        self.segments.last().map(|seg| &*seg.name)
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "P[")?;
        let mut first = true;
        for seg in &self.segments {
            if first {
                write!(f, "{}", seg.name)?;
                first = false;
            } else {
                write!(f, "::{}", seg.name)?;
            }

            if !seg.types.is_empty() {
                write!(f, "<")?;
                let mut t_first = true;
                for typath in &seg.types {
                    if t_first {
                        write!(f, "{:?}", typath)?;
                        t_first = false;
                    } else {
                        write!(f, ",{:?}", typath)?
                    }
                }
                write!(f, ">")?;
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
                write!(f, "{}", seg.name)?;
                first = false;
            } else {
                write!(f, "::{}", seg.name)?;
            }

            if !seg.types.is_empty() {
                write!(f, "<")?;
                let mut t_first = true;
                for typath in &seg.types {
                    if t_first {
                        write!(f, "{}", typath)?;
                        t_first = false;
                    } else {
                        write!(f, ", {}", typath)?
                    }
                }
                write!(f, ">")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathSegment {
    pub name: String,
    pub types: Vec<Path>,
}

impl PathSegment {
    pub fn new(name: String, types: Vec<Path>) -> Self {
        PathSegment { name, types }
    }
}

impl From<String> for PathSegment {
    fn from(name: String) -> Self {
        PathSegment {
            name,
            types: Vec::new(),
        }
    }
}

/// Information about generic types in a match
#[derive(Clone, PartialEq)]
pub struct PathSearch {
    pub path: Path,
    pub filepath: PathBuf,
    pub point: BytePos,
}

impl fmt::Debug for PathSearch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Search [{:?}, {:?}, {:?}]",
            self.path,
            self.filepath.display(),
            self.point
        )
    }
}

/// Wrapper struct for representing trait bounds.
/// Its usages are
/// - for generic types like T: Debug + Clone
/// - for trait inheritance like trait A: Debug + Clone
/// - for impl_trait like fn f(a: impl Debug + Clone)
/// - for dynamic traits(dyn_trait) like Box<Debug + Clone> or Box<dyn Debug + Clone>
#[derive(Clone, Debug, PartialEq)]
pub struct TraitBounds(Vec<PathSearch>);

impl TraitBounds {
    /// checks if it contains a trait, whick its name is 'name'
    pub fn find_by_name(&self, name: &str) -> Option<&PathSearch> {
        self.0.iter().find(|path_search| {
            let seg = &path_search.path.segments;
            if seg.len() != 1 {
                return false;
            }
            &seg[0].name == name
        })
    }
    /// Search traits included in bounds and return Matches
    pub fn get_traits(&self, session: &Session) -> Vec<Match> {
        self.0
            .iter()
            .filter_map(|ps| {
                nameres::resolve_path_with_str(
                    &ps.path,
                    &ps.filepath,
                    ps.point,
                    core::SearchType::ExactMatch,
                    core::Namespace::Type,
                    session,
                ).nth(0)
            }).collect()
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub(crate) fn from_generic_bounds<P: AsRef<FilePath>>(
        bounds: &GenericBounds,
        filepath: P,
        offset: i32,
    ) -> TraitBounds {
        let vec = bounds
            .iter()
            .filter_map(|bound| {
                if let GenericBound::Trait(ref ptrait_ref, _) = *bound {
                    let ast_path = &ptrait_ref.trait_ref.path;
                    let source_map::BytePos(point) = ast_path.span.lo();
                    let path = Path::from_ast(&ast_path);
                    let path_search = PathSearch {
                        path: path,
                        filepath: filepath.as_ref().to_path_buf(),
                        point: BytePos::from((point as i32 + offset) as u32),
                    };
                    Some(path_search)
                } else {
                    None
                }
            }).collect();
        TraitBounds(vec)
    }
    fn extend(&mut self, other: Self) {
        self.0.extend(other.0)
    }
}

/// Argument of generics like T: From<String>
/// It's intended to use this type only for declaration of type parameter.
// TODO: impl trait's name
#[derive(Clone, Debug, PartialEq)]
pub struct TypeParameter {
    /// the name of type parameter declared in generics, like 'T'
    pub name: String,
    /// The point 'T' appears
    pub point: BytePos,
    /// bounds
    pub bounds: TraitBounds,
}

impl TypeParameter {
    pub fn name(&self) -> &str {
        &(*self.name)
    }
    pub(crate) fn into_match<P: AsRef<FilePath>>(self, filepath: &P) -> Option<Match> {
        // TODO: contextstr, local
        Some(Match {
            matchstr: self.name,
            filepath: filepath.as_ref().to_path_buf(),
            point: self.point,
            coords: None,
            local: false,
            mtype: MatchType::TypeParameter(Box::new(self.bounds)),
            contextstr: String::new(),
            generic_args: Vec::new(),
            generic_types: Vec::new(),
            docs: String::new(),
        })
    }
}

/// List of Args in generics, e.g. <T: Clone, U, P>
/// Now it's intended to use only for type parameters
// TODO: should we extend this type enable to handle both type parameters and true types?
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GenericsArgs(pub Vec<TypeParameter>);

impl GenericsArgs {
    pub(crate) fn find_type_param(&self, name: &str) -> Option<&TypeParameter> {
        self.0.iter().find(|v| &v.name == name)
    }
    pub(crate) fn extend(&mut self, other: GenericsArgs) {
        self.0.extend(other.0);
    }
    pub(crate) fn from_generics<'a, P: AsRef<FilePath>>(
        generics: &'a ast::Generics,
        filepath: P,
        offset: i32,
    ) -> Self {
        let mut args = Vec::new();
        for param in generics.params.iter() {
            match param.kind {
                // TODO: lifetime support
                GenericParamKind::Lifetime => {}
                // TODO: should we handle default type here?
                GenericParamKind::Type { default: _ } => {
                    let param_name = param.ident.name.to_string();
                    let source_map::BytePos(point) = param.ident.span.lo();
                    let bounds = TraitBounds::from_generic_bounds(&param.bounds, &filepath, offset);
                    args.push(TypeParameter {
                        name: param_name,
                        point: BytePos::from((point as i32 + offset) as u32),
                        bounds,
                    })
                }
            }
        }
        for pred in generics.where_clause.predicates.iter() {
            match pred {
                WherePredicate::BoundPredicate(bound) => match bound.bounded_ty.node {
                    TyKind::Path(ref _qself, ref path) => {
                        if let Some(seg) = path.segments.get(0) {
                            let name = seg.ident.name.as_str();
                            if let Some(mut tp) = args.iter_mut().find(|tp| tp.name == name) {
                                tp.bounds.extend(TraitBounds::from_generic_bounds(
                                    &bound.bounds,
                                    &filepath,
                                    offset,
                                ));
                            }
                        }
                    }
                    // TODO 'self' support
                    TyKind::ImplicitSelf => {}
                    _ => {}
                },
                // TODO: lifetime support
                WherePredicate::RegionPredicate(_) => {}
                _ => {}
            }
        }
        GenericsArgs(args)
    }
    pub fn get_idents(&self) -> Vec<String> {
        self.0.iter().map(|g| g.name.clone()).collect()
    }
    pub fn params(&self) -> impl Iterator<Item = &TypeParameter> {
        self.0.iter()
    }
}

/// `Impl` information
#[derive(Clone, Debug, PartialEq)]
pub struct ImplHeader {
    self_path: Path,
    trait_path: Option<Path>,
    generics: GenericsArgs,
    filepath: PathBuf,
    // TODO: should be removed
    local: bool,
    impl_start: BytePos,
}

impl ImplHeader {
    pub(crate) fn new(
        generics: &ast::Generics,
        path: &FilePath,
        otrait: &Option<TraitRef>,
        self_type: &ast::Ty,
        offset: BytePos,
        local: bool,
        impl_start: BytePos,
    ) -> Option<Self> {
        let generics = GenericsArgs::from_generics(generics, path, offset.0 as i32);
        let self_path = destruct_ref_ptr(&self_type.node).map(Path::from_ast)?;
        let trait_path = otrait.as_ref().map(|tref| Path::from_ast(&tref.path));
        Some(ImplHeader {
            self_path,
            trait_path,
            generics,
            filepath: path.to_owned(),
            local,
            impl_start,
        })
    }
    pub(crate) fn self_path(&self) -> &Path {
        &self.self_path
    }
    pub(crate) fn trait_path(&self) -> Option<&Path> {
        self.trait_path.as_ref()
    }
    pub(crate) fn file_path(&self) -> &FilePath {
        self.filepath.as_ref()
    }
    pub(crate) fn generics(&self) -> &GenericsArgs {
        &self.generics
    }
    pub(crate) fn impl_start(&self) -> BytePos {
        self.impl_start
    }
    // TODO: should be removed
    pub(crate) fn is_local(&self) -> bool {
        self.local || self.trait_path.is_some()
    }
    pub(crate) fn is_trait(&self) -> bool {
        self.trait_path.is_some()
    }
    pub(crate) fn resolve_trait(
        &self,
        session: &Session,
        import_info: &ImportInfo,
    ) -> Option<Match> {
        nameres::resolve_path(
            self.trait_path()?,
            self.file_path(),
            self.impl_start,
            core::SearchType::ExactMatch,
            core::Namespace::Type,
            session,
            import_info,
        ).nth(0)
    }
}

fn destruct_ref_ptr(ty: &TyKind) -> Option<&ast::Path> {
    match ty {
        TyKind::Rptr(_, ref ty) => destruct_ref_ptr(&ty.ty.node),
        TyKind::Path(_, ref path) => Some(path),
        _ => None,
    }
}
