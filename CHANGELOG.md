# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]
### Changed
 
 - Added basic 'daemon' mode, racer process can be kept running between invocations

 - now uses clap to parse command line options

 - Adds caching of file source and code indices

 - Adds an alternative 'tabbed' mode where inputs and outputs can be tab separated for easier parsing

 - emacs and vim support split out into their own git projects at https://github.com/racer-rust/emacs-racer https://github.com/racer-rust/vim-racer

 - Fix issue resolving some std::* modules in latest rust source: (rust std lib implicitly imports core with #![no_std])

 - Searches multirust overrides when locating cargo src directories

## [1.0.0] 2015-07-29

- First release
