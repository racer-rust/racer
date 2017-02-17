Change Log
==========

All notable changes to this project will be documented in this file. This
project adheres to [Semantic Versioning](http://semver.org/).

## 2.0.6

- resolve Self (e.g. in-impl function calls like Self::myfunction())
- Fix stack overflow issue on unresolvable imports :tada: #698

## 2.0.5

- Chained completions on separate lines now work #686

## 2.0.4

- Fix for find-doc not always returning full doc string #675

## 2.0.3

- Fix for recursion in certain `use foo::{self, ..}` cases #669

## 2.0.2

- Internal fixes so we can publish on crates.io

## 2.0.1

- Syntex 0.52 #643

- Fix `racer --help` bug from 2.0 refactor #662

- Support short revision identifiers for git checkout paths #664

- Handle self resolution when using `use mymod::{self, Thing}` #665

- Fix type alias resolution #666

## 2.0

- Rework public API to hide many implementation details and allow the project to
  move forward without breaking changes.

- Many fixes that didn't make it into the changelog, but we're going to work on
  that in the future!

## 1.2

- Added basic 'daemon' mode, racer process can be kept running between
  invocations

- now uses clap to parse command line options

- Adds caching of file source and code indices

- Adds an alternative 'tabbed' mode where inputs and outputs can be tab
  separated for easier parsing

- emacs and vim support split out into their own git projects [emacs-racer] and
  [vim-racer], respectively.

- Fix issue resolving some `std::*` modules in latest rust source: (rust std lib
  implicitly imports core with `#![no_std]`)

- Searches multirust overrides when locating cargo src directories

## 1.0.0 2015-07-29

- First release

[vim-racer]: https://github.com/racer-rust/vim-racer
[emacs-racer]: https://github.com/racer-rust/emacs-racer
