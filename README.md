# *Racer* - code completion for [Rust](http://www.rust-lang.org/)

![alt text](https://github.com/phildawes/racer/raw/master/images/racer1.png "Racer emacs session")

*RACER* = *R*ust *A*uto-*C*omplete-*er*. A utility intended to provide rust code completion for editors and IDEs. Maybe one day the 'er' bit will be exploring + refactoring or something.

## Important - Racer does not work with Rust Beta!

Racer uses unstable features from the standard library, and so currently requires a rust nightly install 

## Installation

1. ```cd racer; cargo build --release```

2. Set the ```RUST_SRC_PATH``` env variable to point to the 'src' dir in your rust source installation

   (e.g. ```% export RUST_SRC_PATH=/usr/local/src/rust/src``` )

3. Test on the command line: 

   ```./target/release/racer complete std::io::B ```  (should show some completions)


## Emacs integration

1. Install emacs 24

2. Install [rust-mode](https://github.com/rust-lang/rust-mode). E.g. add the following to .emacs:

   ```
   (add-to-list 'load-path "<path-to-rust-mode-srcdir>/")
   (autoload 'rust-mode "rust-mode" nil t)
   (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
   ```

3. Install company mode. (e.g. via ELPA: ```M-x list-packages```, select ```'company'```)

4. Set some variables and install racer. E.g. add this to .emacs:

   ```
   (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
   (setq racer-cmd "<path-to-racer>/target/release/racer")
   (add-to-list 'load-path "<path-to-racer>/editors")
   (eval-after-load "rust-mode" '(require 'racer))
   ```

(N.B. racer.el currenly relies on 'company'. The reason for all the 'eval-after-load' stuff is to ensure rust-mode and racer evaluate after company has been loaded)

5. Open a rust file and try typing ```use std::io::B``` and press \<tab\>

6. Place your cursor over a symbol and hit M-. to jump to the definition



## Vim integration

1. Install using Pathogen, Vundle or NeoBundle. Or, copy racer/plugin/racer.vim into your .vim/plugin directory.

  Vundle users:
  ```
  Vundle 'phildawes/racer'
  ```

  NeoBundle users:
  ```
  NeoBundle 'phildawes/racer', {
  \   'build' : {
  \     'mac': 'cargo build --release',
  \     'unix': 'cargo build --release',
  \   }
  \ }
  ```

2. Add g:racer_cmd and $RUST_SRC_PATH variables to your .vimrc. Also it's worth turning on 'hidden' mode for buffers otherwise you need to save the current buffer every time you do a goto-definition. E.g.:

     ```
     set hidden
     let g:racer_cmd = "<path-to-racer>/target/release/racer"
     let $RUST_SRC_PATH="<path-to-rust-srcdir>/src/"
     ```

3. In insert mode use C-x-C-o to search for completions

4. In normal mode type 'gd' to go to a definition
