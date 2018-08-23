extern crate serde;
extern crate serde_json;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::{self, Utf8Error};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Metadata {
    pub packages: HashSet<Package>,
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
    manifest_path: (),
    #[serde(default = "edition_default")]
    pub edition: String,
    #[serde(skip)]
    __guard: (),
}

impl Hash for Package {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.id, state)
    }
}

impl PartialEq for Package {
    fn eq(&self, other: &Package) -> bool {
        self.id == other.id
    }
}

impl Eq for Package {}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Resolve {
    pub nodes: HashSet<ResolveNode>,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ResolveNode {
    pub id: PackageId,
    pub dependencies: HashSet<PackageId>,
}

impl Hash for ResolveNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.id, state)
    }
}

impl PartialEq for ResolveNode {
    fn eq(&self, other: &ResolveNode) -> bool {
        self.id == other.id
    }
}

impl Eq for ResolveNode {}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Dependency {
    /// Name as given in the `Cargo.toml`
    pub name: String,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Target {
    pub name: String,
    pub kind: Vec<String>,
    #[serde(skip)]
    crate_types: (),
    pub src_path: PathBuf,
    #[serde(default = "edition_default")]
    pub edition: String,
    #[serde(skip)]
    __guard: (),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct PackageId(String);

impl PackageId {
    pub fn name(&self) -> &str {
        let idx = self.0.find(' ').expect("Whitespace not found");
        &self.0[..idx]
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Encode(Utf8Error),
    Json(serde_json::Error),
    Io(io::Error),
    Subprocess(String),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::Encode(e) => fmt::Display::fmt(e, f),
            ErrorKind::Json(e) => fmt::Display::fmt(e, f),
            ErrorKind::Io(e) => fmt::Display::fmt(e, f),
            ErrorKind::Subprocess(s) => write!(f, "stderr: {}", s),
        }
    }
}

impl Error for ErrorKind {
    fn description(&self) -> &str {
        match self {
            ErrorKind::Encode(e) => e.description(),
            ErrorKind::Json(e) => e.description(),
            ErrorKind::Io(e) => e.description(),
            ErrorKind::Subprocess(s) => &s,
        }
    }
}

impl From<Utf8Error> for ErrorKind {
    fn from(e: Utf8Error) -> ErrorKind {
        ErrorKind::Encode(e)
    }
}

impl From<serde_json::Error> for ErrorKind {
    fn from(e: serde_json::Error) -> ErrorKind {
        ErrorKind::Json(e)
    }
}

impl From<io::Error> for ErrorKind {
    fn from(e: io::Error) -> ErrorKind {
        ErrorKind::Io(e)
    }
}

#[inline(always)]
fn edition_default() -> String {
    "2015".to_owned()
}

pub fn find_manifest(mut current: &Path) -> Option<PathBuf> {
    let file = "Cargo.toml";
    if current.is_dir() {
        let manifest = current.join(file);
        if manifest.exists() {
            return Some(manifest.to_owned());
        }
    }
    while let Some(parent) = current.parent() {
        let manifest = current.join(file);
        if manifest.exists() {
            return Some(manifest.to_owned());
        }
        current = parent;
    }
    None
}

pub fn run(manifest_path: &Path, no_deps: bool) -> Result<Metadata, ErrorKind> {
    let cargo = env::var("CARGO").unwrap_or_else(|_| "cargo".to_owned());
    let mut cmd = Command::new(cargo);
    cmd.arg("metadata");
    cmd.arg("--all-features");
    cmd.args(&["--format-version", "1"]);
    cmd.arg("--frozen");
    cmd.args(&["--color", "never"]);
    cmd.arg("--manifest-path");
    if no_deps {
        cmd.arg("--no-deps");
    }
    cmd.arg(manifest_path.as_os_str());
    let op = cmd.output()?;
    if !op.status.success() {
        let stderr = String::from_utf8(op.stderr).map_err(|e| e.utf8_error())?;
        return Err(ErrorKind::Subprocess(stderr));
    }
    serde_json::from_slice(&op.stdout).map_err(From::from)
}
