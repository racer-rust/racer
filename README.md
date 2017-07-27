# *Racer* - code completion for [Rust](http://www.rust-lang.org/)

[![Build Status](https://travis-ci.org/racer-rust/racer.svg?branch=master)](https://travis-ci.org/racer-rust/racer)
[![Build status](https://ci.appveyor.com/api/projects/status/hq51xvr5wpfcfqgt/branch/master?svg=true)](https://ci.appveyor.com/project/TedDriggs/racer-xr5g5/branch/master)


![racer completion screenshot](images/racer_completion.png)

![racer eldoc screenshot](images/racer_eldoc.png)

*RACER* = *R*ust *A*uto-*C*omplete-*er*. A utility intended to provide Rust code completion for editors and IDEs. Maybe one day the 'er' bit will be exploring + refactoring or something.

## Installation

### With `cargo install`

This method requires Rust 1.5. Simply run:

```cargo install racer```

As mentioned in the command output, don't forget to add the installation directory to your `PATH`.

### From sources

1. Clone the repository: ```git clone https://github.com/racer-rust/racer.git```

2. ```cd racer; cargo build --release```.  The binary will now be in ```./target/release/racer```

3. Add the binary to your `PATH`. This can be done by moving it to a directory already in your `PATH` (i.e. `/usr/local/bin`) or by adding the `./target/release/` directory to your `PATH`

## Configuration

1. Fetch the Rust sourcecode

    1. automatically via [rustup](https://www.rustup.rs/) and run `rustup component add rust-src` in order to install the source to `$(rustc --print sysroot)/lib/rustlib/src/rust/src`. Rustup will keep the sources in sync with the toolchain if you run `rustup update`.

    2. manually from git, or download from https://www.rust-lang.org/install.html (the 'rustc' source download behind the 'source' link is the right one).

2. Set the ```RUST_SRC_PATH``` environment variable to point to the 'src' dir in the Rust source installation

   (e.g. ```% export RUST_SRC_PATH=/usr/local/src/rust/src``` or ```% export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"``` )
   
3. Test on the command line:

   ```racer complete std::io::B ```  (should show some completions)

## Editors/IDEs Supported

### Eclipse integration

Racer can be used with Eclipse through the use of [RustDT](https://github.com/RustDT/RustDT). (User guide is [linked](http://rustdt.github.io/) in repo description)

### Emacs integration

Emacs integration has been moved to a separate project: [emacs-racer](https://github.com/racer-rust/emacs-racer).

### Gedit integration

Gedit integration can be found [here](https://github.com/isamert/gracer).

### Kate integration

The Kate community maintains a [plugin](https://cgit.kde.org/kate.git/tree/addons/rustcompletion). It is bundled with recent releases of Kate (tested with 16.08 - read more [here](https://blogs.kde.org/2015/05/22/updates-kates-rust-plugin-syntax-highlighting-and-rust-source-mime-type)).

1. Enable 'Rust code completion' in the plugin list in the Kate config dialog;

2. On the new 'Rust code completion' dialog page, make sure 'Racer command' and 'Rust source tree location' are set correctly.

### Sublime Text integration

The Sublime Text community maintains some packages that integrates Racer
* [RustAutoComplete](https://github.com/defuz/RustAutoComplete) that offers auto completion and goto definition.
* [AnacondaRUST](https://github.com/DamnWidget/anaconda_rust) from the [anaconda](http://github.com/DamnWidget/anaconda) plugins family that offers auto completion, goto definition and show documentation

### Vim integration

Vim integration has been moved to a separate project: [vim-racer](https://github.com/racer-rust/vim-racer).

### Visual Studio Code extension

Racer recommends the [`vscode-rust` extension](https://github.com/editor-rs/vscode-rust). This is an actively-maintained fork of the now-deprecated [`RustyCode` extension](https://github.com/saviorisdead/RustyCode).

### Atom integration 

You can find the racer package for Atom [here](https://atom.io/packages/autocomplete-racer)

### Kakoune integration 

[Kakoune](https://github.com/mawww/kakoune) comes with a builtin integration for racer auto completion.
