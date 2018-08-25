use metadata::{Metadata, Package, PackageId, Resolve, ResolveNode, Target};
use racer_interner::InternedString;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Cached dependencies for racer
#[derive(Clone, Debug)]
pub struct PackageMap {
    manifest_to_id: HashMap<PathBuf, PackageId>,
    id_to_edition: HashMap<PackageId, InternedString>,
    id_to_deps: HashMap<PackageId, HashMap<InternedString, PathBuf>>,
    id_to_lib: HashMap<PackageId, Target>,
}

impl PackageMap {
    pub fn from_metadata(meta: Metadata) -> Self {
        let Metadata {
            packages, resolve, ..
        } = meta;
        PackageMap::new(packages, resolve)
    }
    pub fn new(packages: Vec<Package>, resolve: Option<Resolve>) -> Self {
        let mut manifest_to_id = HashMap::new();
        let mut id_to_lib = HashMap::new();
        let mut id_to_edition = HashMap::new();
        for package in packages {
            let Package {
                id,
                targets,
                manifest_path,
                edition,
                ..
            } = package;
            manifest_to_id.insert(manifest_path, id);
            id_to_edition.insert(id, edition);
            if let Some(t) = targets.into_iter().find(|t| t.is_lib()) {
                id_to_lib.insert(package.id, t.to_owned());
            }
        }
        let id_to_deps = resolve.map_or_else(
            || HashMap::new(),
            |res| construct_deps(res.nodes, &id_to_lib),
        );
        PackageMap {
            manifest_to_id,
            id_to_edition,
            id_to_deps,
            id_to_lib,
        }
    }
    pub fn get_id(&self, path: &Path) -> Option<PackageId> {
        self.manifest_to_id.get(path).map(|&id| id)
    }
    pub fn get_edition(&self, id: PackageId) -> Option<&str> {
        self.id_to_edition.get(&id).map(|s| s.as_str())
    }
    pub fn get_lib(&self, id: PackageId) -> Option<&Target> {
        self.id_to_lib.get(&id)
    }
    pub fn get_lib_src_path(&self, id: PackageId) -> Option<&Path> {
        self.get_lib(id).map(|t| t.src_path.as_ref())
    }
    pub fn ids(&self) -> impl Iterator<Item = &PackageId> {
        self.id_to_edition.keys()
    }
    pub fn get_dependencies(&self, id: PackageId) -> Option<&HashMap<InternedString, PathBuf>> {
        self.id_to_deps.get(&id)
    }
    pub fn get_src_path_from_libname(&self, id: PackageId, s: &str) -> Option<&Path> {
        let deps = self.get_dependencies(id)?;
        let query_str = InternedString::new_if_exists(s)?;
        deps.get(&query_str).map(AsRef::as_ref)
    }
}

fn construct_deps(
    nodes: Vec<ResolveNode>,
    targets: &HashMap<PackageId, Target>,
) -> HashMap<PackageId, HashMap<InternedString, PathBuf>> {
    nodes
        .into_iter()
        .map(|node| {
            let deps: HashMap<_, _> = node
                .dependencies
                .into_iter()
                .filter_map(|id| targets.get(&id).map(|t| (t.name, t.src_path.clone())))
                .collect();
            (node.id, deps)
        }).collect()
}
