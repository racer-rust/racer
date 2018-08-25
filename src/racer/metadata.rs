extern crate lazycell;
extern crate racer_cargo_metadata as metadata;
use self::lazycell::LazyCell;
use self::metadata::mapping::PackageMap;
use project_model::ProjectModelProvider;
use std::path::{Path, PathBuf};

struct MetadataCache {
    pkg_map: LazyCell<PackageMap>,
}

impl MetadataCache {
    fn new() -> Self {
        MetadataCache {
            pkg_map: LazyCell::new(),
        }
    }
    fn fill(&self, manifest: &Path) -> Result<(), ()> {
        let meta = metadata::run(manifest, false).map_err(|e| {
            warn!("Error in cargo metadata: {}", e);
        })?;
        let pkg_map = PackageMap::from_metadata(meta);
        self.pkg_map.fill(pkg_map).map_err(|_| {
            warn!("Error in initialize lazy cell");
        })
    }
}

impl ProjectModelProvider for MetadataCache {
    fn discover_project_manifest(&self, path: &Path) -> Option<PathBuf> {
        metadata::find_manifest(path)
    }
    fn resolve_dependency(&self, manifest: &Path, libname: &str) -> Option<PathBuf> {
        println!(
            "MetadataCache::resolve_dependency manifest: {:?} libname: {}",
            manifest, libname
        );
        if !self.pkg_map.filled() {
            self.fill(manifest).ok()?;
        }
        let pkg_map = self.pkg_map.borrow().unwrap();
        let id = pkg_map.get_id(manifest)?;

        pkg_map
            .get_src_path_from_libname(id, libname)
            .or_else(|| {
                let hyphnated = libname.replace('_', "-");
                pkg_map.get_src_path_from_libname(id, &hyphnated)
            }).or_else(|| pkg_map.get_lib_src_path(id))
            .map(|p| p.to_owned())
    }
}

pub fn project_model() -> Box<ProjectModelProvider> {
    Box::new(MetadataCache::new())
}
