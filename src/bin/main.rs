#![cfg_attr(all(test, feature = "nightly"), feature(test))] // we only need test feature when testing

#[macro_use] extern crate log;

extern crate syntex_syntax;
extern crate toml;
extern crate env_logger;
#[macro_use] extern crate clap;

extern crate racer;

#[cfg(not(test))]
use racer::core;
#[cfg(not(test))]
use racer::util;
#[cfg(not(test))]
use racer::core::Match;
#[cfg(not(test))]
use racer::util::getline;
#[cfg(not(test))]
use racer::nameres::{do_file_search, do_external_search, PATH_SEP};
#[cfg(not(test))]
use racer::scopes;
#[cfg(not(test))]
use std::path::{Path, PathBuf};
#[cfg(not(test))]
use std::io::{self, BufRead};
#[cfg(not(test))]
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};

#[cfg(not(test))]
fn match_with_snippet_fn(m: Match, session: &core::Session, interface: Interface) {
    let (linenum, charnum) = scopes::point_to_coords_from_file(&m.filepath, m.point, session).unwrap();
    if m.matchstr == "" {
        panic!("MATCHSTR is empty - waddup?");
    }

    let snippet = racer::snippets::snippet_for_match(&m, session);
    match interface {
        Interface::Text =>
            println!("MATCH {};{};{};{};{};{:?};{};{}",
                        m.matchstr,
                        snippet,
                        linenum.to_string(),
                        charnum.to_string(),
                        m.filepath.to_str().unwrap(),
                        m.mtype,
                        m.contextstr.replace(";", "\\;").split_whitespace().collect::<Vec<&str>>().join(" "),
                        format!("{:?}", m.docs).replace(";", "\\;")),
        Interface::TabText =>
            println!("MATCH\t{}\t{}\t{}\t{}\t{}\t{:?}\t{}\t{:?}",
                        m.matchstr,
                        snippet,
                        linenum.to_string(),
                        charnum.to_string(),
                        m.filepath.to_str().unwrap(),
                        m.mtype,
                        m.contextstr.replace("\t", "\\t").split_whitespace().collect::<Vec<&str>>().join(" "),
                        m.docs),
    }
}

#[cfg(not(test))]
fn match_fn(m: Match, session: &core::Session, interface: Interface) {
    if let Some((linenum, charnum)) = scopes::point_to_coords_from_file(&m.filepath,
                                                                        m.point,
                                                                        session) {
        match interface {
            Interface::Text =>
                println!("MATCH {},{},{},{},{:?},{}",
                            m.matchstr,
                            linenum.to_string(),
                            charnum.to_string(),
                            m.filepath.to_str().unwrap(),
                            m.mtype,
                            m.contextstr.split_whitespace().collect::<Vec<&str>>().join(" ")),
            Interface::TabText =>
                println!("MATCH\t{}\t{}\t{}\t{}\t{:?}\t{}",
                            m.matchstr,
                            linenum.to_string(),
                            charnum.to_string(),
                            m.filepath.to_str().unwrap(),
                            m.mtype,
                            m.contextstr.split_whitespace().collect::<Vec<&str>>().join(" ")),
        }
    } else {
        error!("Could not resolve file coords for match {:?}", m);
    }
}

#[cfg(not(test))]
fn complete(cfg: Config, print_type: CompletePrinter) {
    if cfg.fqn.is_some() {
        return external_complete(cfg, print_type);
    }
    complete_by_line_coords(cfg, print_type);
}

#[cfg(not(test))]
fn complete_by_line_coords(cfg: Config,
                           print_type: CompletePrinter) {
    // input: linenum, colnum, fname
    let tb = std::thread::Builder::new().name("searcher".to_owned());

    // PD: this probably sucks for performance, but lots of plugins
    // end up failing and leaving tmp files around if racer crashes,
    // so catch the crash.
    let res = tb.spawn(move || {
        run_the_complete_fn(&cfg, print_type);
    }).unwrap();
    if let Err(e) = res.join() {
        error!("Search thread paniced: {:?}", e);
    }

    println!("END");
}

#[cfg(not(test))]
#[derive(Debug)]
enum CompletePrinter {
    Normal,
    WithSnippets
}

#[cfg(not(test))]
fn cache_file_contents_from_stdin(file: &PathBuf, cache: &core::FileCache) {
    let stdin = io::stdin();

    let mut rawbytes = Vec::new();
    stdin.lock().read_until(0x04, &mut rawbytes).unwrap();

    let buf = String::from_utf8(rawbytes).unwrap();
    cache.cache_file_contents(file, buf);
}

#[cfg(not(test))]
fn run_the_complete_fn(cfg: &Config, print_type: CompletePrinter) {
    let fn_path = &*cfg.fn_name.as_ref().unwrap();
    let substitute_file = cfg.substitute_file.as_ref().unwrap_or(fn_path);

    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, fn_path, substitute_file);

    if substitute_file.to_str() == Some("-") {
        cache_file_contents_from_stdin(substitute_file, &cache);
    }

    let src = session.load_file(fn_path);
    let line = &getline(substitute_file, cfg.linenum, &session);
    let (start, pos) = util::expand_ident(line, cfg.charnum);
    match cfg.interface {
        Interface::Text =>
            println!("PREFIX {},{},{}", start, pos, &line[start..pos]),
        Interface::TabText =>
            println!("PREFIX\t{}\t{}\t{}", start, pos, &line[start..pos]),
    }

    let point = scopes::coords_to_point(&src, cfg.linenum, cfg.charnum);

    for m in core::complete_from_file(&src, fn_path, point, &session) {
        match print_type {
            CompletePrinter::Normal => match_fn(m, &session, cfg.interface),
            CompletePrinter::WithSnippets => match_with_snippet_fn(m, &session, cfg.interface),
        };
    }
}


#[cfg(not(test))]
fn external_complete(cfg: Config, print_type: CompletePrinter) {
    // input: a command line string passed in
    let p: Vec<&str> = cfg.fqn.as_ref().unwrap().split("::").collect();
    let cwd = Path::new(".");
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, cwd, cwd);

    for m in do_file_search(p[0], cwd) {
        if p.len() == 1 {
            match print_type {
                CompletePrinter::Normal => match_fn(m, &session, cfg.interface),
                CompletePrinter::WithSnippets => match_with_snippet_fn(m, &session, cfg.interface),
            }
        } else {
            for m in do_external_search(&p[1..], &m.filepath, m.point,
                                        core::SearchType::StartsWith,
                                        core::Namespace::BothNamespaces, &session) {
                match print_type {
                    CompletePrinter::Normal => match_fn(m, &session, cfg.interface),
                    CompletePrinter::WithSnippets => match_with_snippet_fn(m, &session, cfg.interface),
                }
            }
        }
    }
}

#[cfg(not(test))]
fn prefix(cfg: Config) {
    let fn_path = &*cfg.fn_name.as_ref().unwrap();
    let substitute_file = cfg.substitute_file.as_ref().unwrap_or(fn_path);
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, fn_path, substitute_file);

    if substitute_file.to_str() == Some("-") {
        cache_file_contents_from_stdin(substitute_file, &cache);
    }

    // print the start, end, and the identifier prefix being matched
    let line = &getline(fn_path, cfg.linenum, &session);
    let (start, pos) = util::expand_ident(line, cfg.charnum);
    match cfg.interface {
        Interface::Text =>
            println!("PREFIX {},{},{}", start, pos, &line[start..pos]),
        Interface::TabText =>
            println!("PREFIX\t{}\t{}\t{}", start, pos, &line[start..pos]),
    }
}

#[cfg(not(test))]
fn find_definition(cfg: Config) {
    let fn_path = &*cfg.fn_name.as_ref().unwrap();
    let substitute_file = cfg.substitute_file.as_ref().unwrap_or(fn_path);
    let cache = core::FileCache::new();
    let session = core::Session::from_path(&cache, fn_path, substitute_file);

    if substitute_file.to_str() == Some("-") {
        cache_file_contents_from_stdin(substitute_file, &cache);
    }

    let src = session.load_file(fn_path);
    let pos = scopes::coords_to_point(&src, cfg.linenum, cfg.charnum);

    core::find_definition(&src, fn_path, pos, &session).map(|m| match_fn(m, &session, cfg.interface));
    println!("END");
}

#[cfg(not(test))]
fn check_rust_src_env_var() {
    if let Ok(srcpaths) = std::env::var("RUST_SRC_PATH") {
        let v = srcpaths.split(PATH_SEP).collect::<Vec<_>>();
        if !v.is_empty() {
            let f = Path::new(v[0]);
            if !f.exists() {
                println!("racer can't find the directory pointed to by the RUST_SRC_PATH variable \"{}\". Try using an absolute fully qualified path and make sure it points to the src directory of a rust checkout - e.g. \"/home/foouser/src/rust/src\".", srcpaths);
                std::process::exit(1);
            } else if !f.join("libstd").exists() {
                println!("Unable to find libstd under RUST_SRC_PATH. N.B. RUST_SRC_PATH variable needs to point to the *src* directory inside a rust checkout e.g. \"/home/foouser/src/rust/src\". Current value \"{}\"", srcpaths);
                std::process::exit(1);
            }
        }
    } else {
        let default_paths = [
            "/usr/local/src/rust/src",
            "/usr/src/rust/src",
        ];
        for &path in &default_paths {
            let f = Path::new(path);
            if f.exists() {
                std::env::set_var("RUST_SRC_PATH", path);
                return;
            }
        }

        println!("RUST_SRC_PATH environment variable must be set to point to the src directory of a rust checkout. E.g. \"/home/foouser/src/rust/src\"");
        std::process::exit(1);
    }
}

#[cfg(not(test))]
fn daemon(cfg: Config) {
    let mut input = String::new();
    while let Ok(n) = io::stdin().read_line(&mut input) {
        // '\n' == 1
        if n == 1 {
            break;
        }
        // We add the setting NoBinaryName because in daemon mode we won't be passed the preceeding
        // binary name
        let cli = build_cli().setting(AppSettings::NoBinaryName);
        let matches = match cfg.interface {
            Interface::Text => cli.get_matches_from(input.trim_right().split_whitespace()),
            Interface::TabText => cli.get_matches_from(input.trim_right().split('\t'))
        };
        run(matches, cfg.interface);

        input.clear();
    }
}

#[cfg(not(test))]
#[derive(Copy, Clone)]
enum Interface {
    Text,    // The original human-readable format.
    TabText, // Machine-readable format.  This is basically the same as Text, except that all field
             // separators are replaced with tabs.
             // In `deamon` mode tabs are also used to delimit command arguments.
}

#[cfg(not(test))]
impl Default for Interface {
    fn default() -> Self { Interface::Text }
}

#[cfg(not(test))]
#[derive(Default)]
struct Config {
    fqn: Option<String>,
    linenum: usize,
    charnum: usize,
    fn_name: Option<PathBuf>,
    substitute_file: Option<PathBuf>,
    interface: Interface,
}

#[cfg(not(test))]
impl<'a> From<&'a ArgMatches<'a>> for Config {
    fn from(m: &'a ArgMatches) -> Self {
        // We check for charnum because it's the second argument, which means more than just
        // an FQN was used (i.e. racer complete <linenum> <charnum> <fn_name> [substitute_file])
        if m.is_present("charnum") {
             let cfg = Config {
                charnum: value_t_or_exit!(m.value_of("charnum"), usize),
                fn_name: m.value_of("path").map(PathBuf::from),
                substitute_file: m.value_of("substitute_file").map(PathBuf::from),
                ..Default::default()
             };
             if !m.is_present("linenum") {
            // Becasue of the hack to allow fqn and linenum to share a single arg we set FQN
            // to None and set the charnum correctly using the FQN arg so there's no
            // hackery later
                return Config {linenum: value_t_or_exit!(m.value_of("fqn"), usize), .. cfg };
            }
            return Config {linenum: value_t_or_exit!(m.value_of("linenum"), usize), .. cfg };
        }
        Config {fqn: m.value_of("fqn").map(ToOwned::to_owned), ..Default::default() }
    }
}

#[cfg(not(test))]
fn build_cli<'a, 'b>() -> App<'a, 'b> {
    // we use the more verbose "Builder Pattern" to create the CLI because it's a littel faster
    // than the less verbose "Usage String" method...faster, meaning runtime speed since that's
    // extremely important here
    App::new("racer")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Phil Dawes")
        .about("A Rust code completion utility")
        .settings(&[AppSettings::GlobalVersion,
		    AppSettings::SubcommandRequiredElseHelp])
        .arg(Arg::with_name("interface")
            .long("interface")
            .short("i")
            .takes_value(true)
            .possible_value("text")
            .possible_value("tab-text")
            .value_name("mode")
            .help("Interface mode"))
        .subcommand(SubCommand::with_name("complete")
            .about("performs completion and returns matches")
            // We set an explicit usage string here, instead of letting `clap` write one due to
            // using a single arg for multiple purposes
            .usage("racer complete <fqn>\n\t\
                    racer complete <linenum> <charnum> <path> [substitute_file]")
            // Next we make it an error to run without any args
            .setting(AppSettings::ArgRequiredElseHelp)
            // Because we want a single arg to play two roles and be compatible with previous
            // racer releases, we have to be a little hacky here...
            //
            // We start by making 'fqn' the first positional arg, which will hold this dual value
            // of either an FQN as it says, or secretly a line-number
            .arg(Arg::with_name("fqn")
                .help("complete with a fully-qualified-name (e.g. std::io::)"))
            .arg(Arg::with_name("charnum")
                .help("The char number to search for matches")
                .requires("path"))
            .arg(Arg::with_name("path")
                .help("The path to search for name to match"))
            .arg(Arg::with_name("substitute_file")
                .help("An optional substitute file"))
            // 'linenum' **MUST** be last (or have the highest index so that it's never actually
            // used by the user, but still appears in the help text)
            .arg(Arg::with_name("linenum")
                .help("The line number at which to find the match")))
        .subcommand(SubCommand::with_name("daemon")
            .about("start a process that receives the above commands via stdin"))
        .subcommand(SubCommand::with_name("find-definition")
            .about("finds the definition of a function")
            .arg(Arg::with_name("linenum")
                .help("The line number at which to find the match")
                .required(true))
            .arg(Arg::with_name("charnum")
                .help("The char number at which to find the match")
                .required(true))
            .arg(Arg::with_name("path")
                .help("The path to search for name to match")
                .required(true))
            .arg(Arg::with_name("substitute_file")
                .help("An optional substitute file")))
        .subcommand(SubCommand::with_name("prefix")
            .arg(Arg::with_name("linenum")
                .help("The line number at which to find the match")
                .required(true))
            .arg(Arg::with_name("charnum")
                .help("The char number at which to find the match")
                .required(true))
            .arg(Arg::with_name("path")
                .help("The path to search for the match to prefix")
                .required(true)))
        .subcommand(SubCommand::with_name("complete-with-snippet")
            .about("performs completion and returns more detailed matches")
            .usage("racer complete-with-snippet <fqn>\n\t\
                    racer complete-with-snippet <linenum> <charnum> <path> [substitute_file]")
            .setting(AppSettings::ArgRequiredElseHelp)
            .arg(Arg::with_name("fqn")
                .help("complete with a fully-qualified-name (e.g. std::io::)"))
            .arg(Arg::with_name("charnum")
                .help("The char number to search for matches")
                .requires("path"))
            .arg(Arg::with_name("path")
                .help("The path to search for name to match"))
            .arg(Arg::with_name("substitute_file")
                .help("An optional substitute file"))
            .arg(Arg::with_name("linenum")
                .help("The line number at which to find the match")))
        .after_help("For more information about a specific command try 'racer <command> --help'")
}

#[cfg(not(test))]
fn main() {
    env_logger::init().unwrap();
    check_rust_src_env_var();

    let matches = build_cli().get_matches();
    let interface = match matches.value_of("interface") {
            Some("tab-text") => Interface::TabText,
            Some("text") | _ => Interface::Text
        };
    run(matches, interface);
}

#[cfg(not(test))]
fn run(m: ArgMatches, interface: Interface) {
    use CompletePrinter::{Normal, WithSnippets};
    // match raw subcommand, and get it's sub-matches "m"
    if let (name, Some(sub_m)) = m.subcommand() {
        let mut cfg = Config::from(sub_m);
        cfg.interface = interface;
        match name {
            "daemon"                => daemon(cfg),
            "prefix"                => prefix(cfg),
            "complete"              => complete(cfg, Normal),
            "complete-with-snippet" => complete(cfg, WithSnippets),
            "find-definition"       => find_definition(cfg),
            _                       => unreachable!()
        }
    }
}
