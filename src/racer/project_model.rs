use std::path::{Path, PathBuf};

pub trait ProjectModelProvider {
    fn discover_project_manifest(&self, path: &Path) -> Option<PathBuf>;
    fn resolve_dependency(&self, manifest: &Path, dep_name: &str) -> Option<PathBuf>;
}
