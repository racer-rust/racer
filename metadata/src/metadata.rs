//! Data structures for metadata
use racer_interner::InternedString;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Metadata {
    pub packages: Vec<Package>,
    pub workspace_members: Vec<PackageId>,
    pub resolve: Option<Resolve>,
    #[serde(default)]
    pub workspace_root: PathBuf,
    pub target_directory: PathBuf,
    version: usize,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Package {
    #[serde(skip)]
    name: (),
    #[serde(skip)]
    version: (),
    #[serde(skip)]
    authors: (),
    pub id: PackageId,
    #[serde(skip)]
    source: (),
    #[serde(skip)]
    dependencies: (),
    #[serde(skip)]
    pub targets: Vec<Target>,
    #[serde(skip)]
    features: (),
    #[serde(skip)]
    pub manifest_path: PathBuf,
    #[serde(default = "edition_default")]
    pub edition: InternedString,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Resolve {
    pub nodes: Vec<ResolveNode>,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ResolveNode {
    pub id: PackageId,
    pub dependencies: Vec<PackageId>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dependency {
    /// Name as given in the `Cargo.toml`
    pub name: InternedString,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Target {
    pub name: InternedString,
    // todo: enum
    pub kind: Vec<InternedString>,
    #[serde(skip)]
    crate_types: (),
    pub src_path: PathBuf,
    #[serde(default = "edition_default")]
    pub edition: InternedString,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct PackageId(InternedString);

impl PackageId {
    pub fn name(&self) -> &str {
        let idx = self.0.find(' ').expect("Whitespace not found");
        &self.0[..idx]
    }
}

#[inline(always)]
fn edition_default() -> InternedString {
    InternedString::new("2015")
}
