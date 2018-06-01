// Small functions of utility
use std::{cmp, error, fmt, path};
use std::rc::Rc;

use core::{IndexedSource, Session, SessionExt, Location, LocationExt, Point};
use core::SearchType::{self, ExactMatch, StartsWith};

#[cfg(unix)]
pub const PATH_SEP: char = ':';
#[cfg(windows)]
pub const PATH_SEP: char = ';';

pub fn is_pattern_char(c: char) -> bool {
    c.is_alphanumeric() || c.is_whitespace() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_search_expr_char(c: char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == ':') || (c == '.')
}

pub fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || (c == '_') || (c == '!')
}

/// Searches for `needle` as a standalone identifier in `haystack`. To be considered a match,
/// the `needle` must occur either at the beginning of `haystack` or after a non-identifier
/// character.
pub fn txt_matches(stype: SearchType, needle: &str, haystack: &str) -> bool {
    match stype {
        ExactMatch => {
            let n_len = needle.len();
            let h_len = haystack.len();

            if n_len == 0 {
                return true;
            }

            for (n, _) in haystack.match_indices(needle) {
                if (n == 0 || !is_ident_char(char_before(haystack, n))) &&
                    (n+n_len == h_len || !is_ident_char(char_at(haystack, n+n_len))) {
                    return true;
                }
            }
            false
        },
        StartsWith => {
            if needle.is_empty() {
                return true;
            }

            for (n, _) in haystack.match_indices(needle) {
                if n == 0 || !is_ident_char(char_before(haystack, n)) {
                    return true;
                }
            }
            false
        }
    }
}

pub fn symbol_matches(stype: SearchType, searchstr: &str, candidate: &str) -> bool {
   match stype {
        ExactMatch => searchstr == candidate,
        StartsWith => candidate.starts_with(searchstr)
    }
}

/// Try to valid if the given scope contains a valid closure arg scope.
pub fn closure_valid_arg_scope(scope_src: &str) -> Option<(usize, usize, &str)> {
    // Try to find the left and right pipe, if one or both are not present, this is not a valid
    // closure definition
    let left_pipe = scope_src.find('|')?;
    let candidate = &scope_src[left_pipe..];
    let mut brace_level = 0;
    for (i, c) in candidate.chars().skip(1).enumerate() {
        match c {
            '{' => brace_level += 1,
            '}' => brace_level -= 1,
            '|' => {
                let right_pipe = left_pipe + 1 + i;
                // now we find right |
                if brace_level == 0  {
                    return Some((left_pipe, right_pipe, &scope_src[left_pipe..=right_pipe]));
                }
                break;
            }
            ';' => break,
            _ => {}
        }
        if brace_level < 0 {
            break;
        }
    }
    None
}

#[test]
fn test_closure_valid_arg_scope() {
    let valid = r#"
    let a = |int, int| int * int;
"#;
    assert_eq!(closure_valid_arg_scope(valid), Some((13, 22, "|int, int|")));

    let confusing = r#"
    match a {
        EnumA::A => match b {
            EnumB::A(u) | EnumB::B(u) => println!("u: {}", u),
        },
        EnumA::B => match b {
            EnumB::A(u) | EnumB::B(u) => println!("u: {}", u),
        },
    }
"#;
    assert_eq!(closure_valid_arg_scope(confusing), None);
}

#[test]
fn txt_matches_matches_stuff() {
    assert_eq!(true, txt_matches(ExactMatch, "Vec", "Vec"));
    assert_eq!(true, txt_matches(ExactMatch, "Vec", "use Vec"));
    assert_eq!(false, txt_matches(ExactMatch, "Vec", "use Vecä"));

    assert_eq!(true, txt_matches(StartsWith, "Vec", "Vector"));
    assert_eq!(true, txt_matches(StartsWith, "Vec", "use Vector"));
    assert_eq!(true, txt_matches(StartsWith, "Vec", "use Vec"));
    assert_eq!(false, txt_matches(StartsWith, "Vec", "use äVector"));
}

#[test]
fn txt_matches_matches_methods() {
    assert_eq!(true, txt_matches(StartsWith, "do_st", "fn do_stuff"));
    assert_eq!(true, txt_matches(StartsWith, "do_st", "pub fn do_stuff"));
    assert_eq!(true, txt_matches(StartsWith, "do_st", "pub(crate) fn do_stuff"));
    assert_eq!(true, txt_matches(StartsWith, "do_st", "pub(in codegen) fn do_stuff"));
}


/// Given a string and index, return span of identifier
///
/// `pos` is coerced to be within `s`. Note that `expand_ident` only backtracks.
/// If the provided `pos` is in the middle of an identifier, the returned
/// `(start, end)` will have `end` = `pos`.
///
/// # Examples
///
/// ```
/// extern crate racer;
///
/// let src = "let x = this_is_an_identifier;";
/// let pos = racer::Location::Point(29);
/// let path = "lib.rs";
///
/// let cache = racer::FileCache::default();
/// let session = racer::Session::new(&cache);
///
/// session.cache_file_contents(path, src);
///
/// let expanded = racer::expand_ident(path, pos, &session).unwrap();
/// assert_eq!("this_is_an_identifier", expanded.ident());
/// ```
pub fn expand_ident<P, C>(
    filepath: P,
    cursor: C,
    session: &Session
) -> Option<ExpandedIdent>
    where P: AsRef<path::Path>,
          C: Into<Location>
{
    let cursor = cursor.into();
    let indexed_source = session.load_file(filepath.as_ref());
    let (start, pos) = {
        let s = &indexed_source.code[..];
        let pos = match cursor.to_point(&indexed_source) {
            Some(pos) => pos,
            None => {
                debug!("Failed to convert cursor to point");
                return None;
            }
        };

        // TODO: Would this better be an assertion ? Why are out-of-bound values getting here ?
        // They are coming from the command-line, question is, if they should be handled beforehand
        // clamp pos into allowed range
        let pos = cmp::min(s.len(), pos);
        let sb = &s[..pos];
        let mut start = pos;

        // backtrack to find start of word
        for (i, c) in sb.char_indices().rev() {
            if !is_ident_char(c) {
                break;
            }
            start = i;
        }

        (start, pos)
    };

    Some(ExpandedIdent {
        src: indexed_source,
        start,
        pos,
    })
}

pub struct ExpandedIdent {
    src: Rc<IndexedSource>,
    start: Point,
    pos: Point,
}

impl ExpandedIdent {
    pub fn ident(&self) -> &str {
        &self.src.code[self.start..self.pos]
    }

    pub fn start(&self) -> Point {
        self.start
    }

    pub fn pos(&self) -> Point {
        self.pos
    }
}

pub fn find_ident_end(s: &str, pos: Point) -> Point {
    // find end of word
    let sa = &s[pos..];
    for (i, c) in sa.char_indices() {
        if !is_ident_char(c) {
            return pos + i;
        }
    }
    s.len()
}

#[test]
fn find_ident_end_ascii() {
    assert_eq!(5, find_ident_end("ident", 0));
    assert_eq!(6, find_ident_end("(ident)", 1));
    assert_eq!(17, find_ident_end("let an_identifier = 100;", 4));
}

#[test]
fn find_ident_end_unicode() {
    assert_eq!(7, find_ident_end("num_µs", 0));
    assert_eq!(10, find_ident_end("ends_in_µ", 0));
}

fn char_before(src: &str, i: usize) -> char {
    let mut prev = '\0';
    for (ii, ch) in src.char_indices() {
        if ii >= i {
            return prev;
        }
        prev = ch;
    }
    prev
}

#[test]
fn test_char_before() {
    assert_eq!('ä', char_before("täst", 3));
    assert_eq!('ä', char_before("täst", 2));
    assert_eq!('s', char_before("täst", 4));
    assert_eq!('t', char_before("täst", 100));
}

pub fn char_at(src: &str, i: usize) -> char {
    src[i..].chars().next().unwrap()
}

/// Error type returned from validate_rust_src_path()
#[derive(Debug, PartialEq)]
pub enum RustSrcPathError {
    Missing,
    DoesNotExist(path::PathBuf),
    NotRustSourceTree(path::PathBuf),
}

impl error::Error for RustSrcPathError {
    fn cause(&self) -> Option<&error::Error> {
        None
    }

    fn description(&self) -> &str {
        match *self {
            RustSrcPathError::Missing => "RUSTC_SRC_PATH not set or not found in sysroot",
            RustSrcPathError::DoesNotExist(_) => "RUSTC_SRC_PATH does not exist on file system",
            RustSrcPathError::NotRustSourceTree(_) => "RUSTC_SRC_PATH isn't a rustc source tree",
        }
    }
}

impl fmt::Display for RustSrcPathError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RustSrcPathError::Missing => {
                write!(f, "RUST_SRC_PATH environment variable must be set to \
                           point to the src directory of a rust checkout. \
                           E.g. \"/home/foouser/src/rust/src\"")
            },
            RustSrcPathError::DoesNotExist(ref path) => {
                write!(f, "racer can't find the directory pointed to by the \
                           RUST_SRC_PATH variable \"{:?}\". Try using an \
                           absolute fully qualified path and make sure it \
                           points to the src directory of a rust checkout - \
                           e.g. \"/home/foouser/src/rust/src\".", path)
            },
            RustSrcPathError::NotRustSourceTree(ref path) => {
                write!(f, "Unable to find libstd under RUST_SRC_PATH. N.B. \
                           RUST_SRC_PATH variable needs to point to the *src* \
                           directory inside a rust checkout e.g. \
                           \"/home/foouser/src/rust/src\". \
                           Current value \"{:?}\"", path)
            },
        }
    }
}

fn check_rust_sysroot() -> Option<path::PathBuf> {
    use std::process::Command;
    let mut cmd = Command::new("rustc");
    cmd.arg("--print").arg("sysroot");

    if let Ok(output) = cmd.output() {
        if let Ok(s) = String::from_utf8(output.stdout) {
            let sysroot = path::Path::new(s.trim());
            let srcpath = sysroot.join("lib/rustlib/src/rust/src");
            if srcpath.exists() {
                return Some(srcpath);
            }
        }
    }
    None
}

/// Get the path for Rust standard library source code.
/// Checks first the paths in the `RUST_SRC_PATH` environment variable.
///
/// If the environment variable is _not_ set, it checks the rust sys
/// root for the `rust-src` component.
///
/// If that isn't available, checks `/usr/local/src/rust/src` and
/// `/usr/src/rust/src` as default values.
///
/// If the Rust standard library source code cannot be found, returns
/// `Err(racer::RustSrcPathError::Missing)`.
///
/// If the path in `RUST_SRC_PATH` or the path in rust sys root is invalid,
/// returns a corresponding error. If a valid path is found, returns that path.
///
/// # Examples
///
/// ```
/// extern crate racer;
///
/// match racer::get_rust_src_path() {
///     Ok(_path) => {
///         // RUST_SRC_PATH is valid
///     },
///     Err(racer::RustSrcPathError::Missing) => {
///         // path is not set
///     },
///     Err(racer::RustSrcPathError::DoesNotExist(_path)) => {
///         // provided path doesnt point to valid file
///     },
///     Err(racer::RustSrcPathError::NotRustSourceTree(_path)) => {
///         // provided path doesn't have rustc src
///     }
/// }
/// ```
pub fn get_rust_src_path() -> Result<path::PathBuf, RustSrcPathError> {
    use std::env;

    debug!("Getting rust source path. Trying env var RUST_SRC_PATH.");

    if let Ok(ref srcpaths) = env::var("RUST_SRC_PATH") {
         if !srcpaths.is_empty() {
            if let Some(path) = srcpaths.split(PATH_SEP).next() {
                return validate_rust_src_path(path::PathBuf::from(path));
            }
        }
    };

    debug!("Nope. Trying rustc --print sysroot and appending lib/rustlib/src/rust/src to that.");

    if let Some(path) = check_rust_sysroot() {
        return validate_rust_src_path(path);
    };

    debug!("Nope. Trying default paths: /usr/local/src/rust/src and /usr/src/rust/src");

    let default_paths = [
        "/usr/local/src/rust/src",
        "/usr/src/rust/src",
    ];

    for path in &default_paths {
        if let Ok(path) = validate_rust_src_path(path::PathBuf::from(path)) {
            return Ok(path);
        }
    }

    warn!("Rust stdlib source path not found!");

    Err(RustSrcPathError::Missing)
}

fn validate_rust_src_path(path: path::PathBuf) -> Result<path::PathBuf, RustSrcPathError> {
    if !path.exists() {
        Err(RustSrcPathError::DoesNotExist(path.to_path_buf()))
    } else if !path.join("libstd").exists() {
        Err(RustSrcPathError::NotRustSourceTree(path.join("libstd")))
    } else {
        Ok(path)
    }
}

#[cfg(test)]
lazy_static! {
    static ref TEST_SEMAPHORE: ::std::sync::Mutex<()> = Default::default();
}

#[test]
fn test_get_rust_src_path_env_ok() {
    use std::env;

    let _guard = TEST_SEMAPHORE.lock().unwrap();

    let original = env::var_os("RUST_SRC_PATH");
    if env::var_os("RUST_SRC_PATH").is_none() {
        env::set_var("RUST_SRC_PATH", check_rust_sysroot().unwrap());
    }
    let result = get_rust_src_path();

    match original {
        Some(path) => env::set_var("RUST_SRC_PATH", path),
        None => env::remove_var("RUST_SRC_PATH"),
    }
    assert!(result.is_ok());
}

#[test]
fn test_get_rust_src_path_does_not_exist() {
    use std::env;

    let _guard = TEST_SEMAPHORE.lock().unwrap();

    let original = env::var_os("RUST_SRC_PATH");
    env::set_var("RUST_SRC_PATH", "test_path");
    let result = get_rust_src_path();

    match original {
        Some(path) => env::set_var("RUST_SRC_PATH", path),
        None => env::remove_var("RUST_SRC_PATH"),
    }

    assert_eq!(Err(RustSrcPathError::DoesNotExist(path::PathBuf::from("test_path"))),
               result);
}

#[test]
fn test_get_rust_src_path_not_rust_source_tree() {
    use std::env;

    let _guard = TEST_SEMAPHORE.lock().unwrap();

    let original = env::var_os("RUST_SRC_PATH");

    env::set_var("RUST_SRC_PATH", "/");

    let result = get_rust_src_path();

    match original {
        Some(path) => env::set_var("RUST_SRC_PATH", path),
        None => env::remove_var("RUST_SRC_PATH"),
    }

    assert_eq!(Err(RustSrcPathError::NotRustSourceTree(path::PathBuf::from("/libstd"))),
               result);

}

#[test]
fn test_get_rust_src_path_missing() {
    use std::env;

    let _guard = TEST_SEMAPHORE.lock().unwrap();

    let path = env::var_os("PATH").unwrap();
    let original = env::var_os("RUST_SRC_PATH");

    env::remove_var("RUST_SRC_PATH");
    env::remove_var("PATH");

    let result = get_rust_src_path();

    env::set_var("PATH", path);
    match original {
        Some(path) => env::set_var("RUST_SRC_PATH", path),
        None => env::remove_var("RUST_SRC_PATH"),
    }

    assert_eq!(Err(RustSrcPathError::Missing),
        result);
}

#[test]
fn test_get_rust_src_path_rustup_ok() {
    use std::env;

    let _guard = TEST_SEMAPHORE.lock().unwrap();
    let original = env::var_os("RUST_SRC_PATH");
    env::remove_var("RUST_SRC_PATH");

    let result = get_rust_src_path();

    match original {
        Some(path) => env::set_var("RUST_SRC_PATH", path),
        None => env::remove_var("RUST_SRC_PATH"),
    }

    match result {
        Ok(_) => (),
        Err(_) => panic!("Couldn't get the path via rustup! \
            Rustup and the component rust-src needs to be installed for this test to pass!"),
    }
}


/// An immutable stack implemented as a linked list backed by a thread's stack.
pub struct StackLinkedListNode<'stack, T>(Option<StackLinkedListNodeData<'stack, T>>)
    where T: 'stack;

struct StackLinkedListNodeData<'stack, T>
    where T: 'stack
{
    item: T,
    previous: &'stack StackLinkedListNode<'stack, T>,
}

impl<'stack, T> StackLinkedListNode<'stack, T>
    where T: 'stack
{
    /// Returns an empty node.
    pub fn empty() -> Self {
        StackLinkedListNode(None)
    }

    /// Pushes a new node on the stack. Returns the new node.
    pub fn push(&'stack self, item: T) -> Self {
        StackLinkedListNode(Some(StackLinkedListNodeData {
            item,
            previous: self,
        }))
    }
}

impl<'stack, T> StackLinkedListNode<'stack, T>
    where T: 'stack + PartialEq
{
    /// Check if the stack contains the specified item.
    /// Returns `true` if the item is found, or `false` if it's not found.
    pub fn contains(&self, item: &T) -> bool {
        let mut current = self;
        while let StackLinkedListNode(Some(StackLinkedListNodeData { item: ref current_item, previous })) = *current {
            if current_item == item {
                return true;
            }

            current = previous;
        }

        false
    }
}

/// Removes `pub(...)` from the start of a blob so that other code
/// can assess the struct/trait/fn without worrying about restricted
/// visibility.
pub fn trim_visibility(blob: &str) -> &str {
    if !blob.trim_left().starts_with("pub") {
        return blob
    }

    let mut level = 0;
    let mut skip_restricted = 0;
    for (i, c) in blob[3..].char_indices() {
        match c {
            '(' => level += 1,
            ')' => level -= 1,
            _ if level >= 1 => (),
            // stop on the first thing that isn't whitespace
            _ if is_ident_char(c) => {
                skip_restricted = i + 3;
                break;
            },
            _ => continue,
        }
    }

    &blob[skip_restricted..]
}

#[test]
fn test_trim_visibility() {
    assert_eq!(trim_visibility("pub fn"), "fn");
    assert_eq!(trim_visibility("pub(crate)   struct"), "struct");
    assert_eq!(trim_visibility("pub (in super)  const fn"), "const fn");
}

/// Checks if the completion point is in a function declaration by looking
/// to see if the second-to-last word is `fn`.
pub fn in_fn_name(line_before_point: &str) -> bool {
    // Determine if the cursor is sitting in the whitespace after typing `fn ` before
    // typing a name.
    let has_started_name = !line_before_point.ends_with(|c: char| c.is_whitespace());

    let mut words = line_before_point.split_whitespace().rev();

    // Make sure we haven't finished the name and started generics or arguments
    if has_started_name {
        if let Some(ident) = words.next() {
            if ident.chars().any(|c| !is_ident_char(c)) {
                return false;
            }
        }
    }
    
    words
        .next()
        .map(|word| word == "fn")
        .unwrap_or_default()
}

#[test]
fn test_in_fn_name() {
    assert!(in_fn_name("fn foo"));
    assert!(in_fn_name(" fn  foo"));
    assert!(in_fn_name("fn "));
    assert!(!in_fn_name("fn foo(b"));
    assert!(!in_fn_name("fn"));
}
