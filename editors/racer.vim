" Vim plugin for Racer
" (by Phil Dawes)
"
" 1. Edit the variables below (or override in .vimrc)
" 2. copy this file into .vim/plugin/
" 3. - now in insert mode do 'C-x C-o' to autocomplete the thing at the cursor
"    - in normal mode do 'gd' to go to definition
"
" (This plugin is best used with the 'hidden' option enabled so that switching buffers doesn't force you to save) 


if !exists('g:racer_cmd')
    let g:racer_cmd = "/home/pld/src/rust/racer/bin/racer"
endif

if !exists('$RUST_SRC_PATH')
    let $RUST_SRC_PATH="/usr/local/src/rust/src"
endif

function! racer#GetPrefixCol()
    :w! %.racertmp
    let col = col(".")-1
    let b:racer_col = col
    let fname = expand("%:p")
    let tmpfname=fname.".racertmp"
    let cmd = g:racer_cmd." prefix ".line(".")." ".col." ".tmpfname
    let res = system(cmd)
    let prefixline = split(res, "\\n")[0]
    let startcol = split(prefixline[7:], ",")[0]
    return startcol
endfunction

function! racer#GetCompletions()
    let col = b:racer_col      " use the column from the previous racer#GetPrefixCol() call, since vim ammends it afterwards
    let fname = expand("%:p")
    let tmpfname=fname.".racertmp"
    let cmd = g:racer_cmd." complete ".line(".")." ".col." ".tmpfname
    let res = system(cmd)
    let lines = split(res, "\\n")
    let out = []
    for line in lines
       if line =~ "^MATCH"
           let completion = split(line[6:], ",")[0]
           let out = add(out, completion)
       endif
    endfor
    call delete(tmpfname)
    return out
endfunction

function! racer#GoToDefinition()
    :w! %.racertmp
    let col = col(".")-1
    let b:racer_col = col
    let fname = expand("%:p")
    let tmpfname=fname.".racertmp"
    let cmd = g:racer_cmd." find-definition ".line(".")." ".col." ".tmpfname
    let res = system(cmd)
    let lines = split(res, "\\n")
    for line in lines
        if line =~ "^MATCH"
             let linenum = split(line[6:], ",")[1]
             let colnum = split(line[6:], ",")[2]
             let fname = split(line[6:], ",")[3]
             call racer#JumpToLocation(fname, linenum, colnum)
             break
        endif
    endfor
    call delete(tmpfname)
endfunction

function! racer#JumpToLocation(filename, linenum, colnum)
    if(a:filename != '')
        if a:filename != bufname('%')
            exec 'e ' . fnameescape(a:filename)
        endif
        call cursor(a:linenum, a:colnum+1)
    endif
endfunction

"   Vec::

function! racer#Complete(findstart, base)
    if a:findstart
        return racer#GetPrefixCol()
    else
        return racer#GetCompletions()
    endif
endfunction

autocmd FileType rust setlocal omnifunc=racer#Complete
autocmd FileType rust nnoremap gd :call racer#GoToDefinition()<cr>

