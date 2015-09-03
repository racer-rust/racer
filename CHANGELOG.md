# Change Log
All notable changes to this project will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]
### Changed
 - Add some caching of file source and code indices

 - Fix issue resolving some 	std::* modules in latest rust source: (rust std lib implicitly imports core with #![no_std])

 - Update emacs racer.el to write temporary files to tmpdir rather than the same directory as the source file

 - Search multirust overrides when locating cargo src directories

 - fix emacs block indenting (#296)

## [1.0.0] 2015-07-29
- First release
