use std::path::PathBuf;
use std::env;

use util::check_rust_src_env_var;


#[cfg(unix)]
pub const PATH_SEP: char = ':';
#[cfg(windows)]
pub const PATH_SEP: char = ';';

lazy_static! {
    pub static ref RUST_SRC_PATHS: Vec<PathBuf> = {
    		check_rust_src_env_var()
    			.expect("Missing the Rust stdlib source path! Set env var \
    			RUST_SRC_PATH manually to point to the source or install the \
    			`rust-src` component using rustup: \
    			`rustup component add rust-src`");
    		let srcpaths = env::var("RUST_SRC_PATH")
    			.expect("Invariant: RUST_SRC_PATH is already set");
			srcpaths.split(PATH_SEP).map(|p| p.into()).collect()
	};
}
