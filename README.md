# *Racer* - code completion for [Rust](http://www.rust-lang.org/)

![alt text](https://github.com/phildawes/racer/raw/master/images/racer1.png "Racer emacs session")

*RACER* = *R*ust *A*uto-*C*omplete-*er*. A utility intended to provide rust code completion for editors and IDEs. Maybe one day the 'er' bit will be exploring + refactoring or something.

## Status

- Does an ok job of static completions: functions, structs, enums etc.. 

- Not so hot on the dynamic stuff: methods, fields etc.. (yet)

- Also provides 'find-definition' support for jumping around source code

## Installation

1. ```cd racer; make```

2. Set the ```RUST_SRC_PATH``` env variable to point to the 'src' dir in your rust source installation

   (e.g. ```% export RUST_SRC_PATH=/usr/local/src/rust/src``` )

3. Test on the command line: 

   ```./bin/racer complete std::io::B ```  (should show some completions)


## Emacs integration

1. Install emacs 24

2. Install rust-mode. (e.g. add the following to .emacs:

   ```(add-to-list 'load-path "<rust-srcdir>/src/etc/emacs/")
     (require 'rust-mode)```)

3. Install company mode. (e.g. via ELPA: ```M-x list-packages```, select ```'company'```)

4. Edit the first couple of lines of editors/racer.el to reflect your environment

5. Load it. E.g. eval the buffer or add this to .emacs:

     ```(add-to-list 'load-path "<path-to-racer>/editors")
     (require 'racer)'''

6. Open a rust file and try typing ```use std::io::B``` and press \<tab\>

7. Place your cursor over a symbol and hit M-. to jump to the definition

N.B. So far I've only tested this on ubuntu linux + emacs 24

## Vim integration

1. Copy racer/editors/racer.vim into your .vim/plugin directory

2. Add g:racer_cmd and $RUST_SRC_PATH variables to your .vimrc. Also it's worth turning on 'hidden' mode for buffers otherwise you need to save the current buffer every time you do a goto-definition. E.g.:

     ```
     set hidden
     let g:racer_cmd = "/home/pld/src/rust/racer/bin/racer"
     let $RUST_SRC_PATH="/usr/local/src/rust/src"
     ```

3. In insert mode use C-x-C-o to search for completions

4. In normal mode type 'gd' to go to a definition
