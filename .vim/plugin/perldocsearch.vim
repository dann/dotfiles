" perldocsearch.vim
"
" Author: Takatoshi Kitano
" Version: 0.01 for Vim 7.1
" Last Change:  2009/10/31
" Licence: MIT Licence
"
" Description:
"   How to Use
"     :PerldocSearch coerce -G Moose

if exists("g:loaded_perldocsearch")
    finish
endif
let g:loaded_perldocsearch = 1

if !exists('PerldocOpenQuickfixWindow')
    let g:PerldocOpenQuickfixWindow = 1
end

function! s:PerldocSearch(...)
    let args = [ 'podsearch' ]
    let args += a:000
    let cmd_output =  system(join(args, ' '))

    let tmpfile = tempname()
    exe "redir! > " . tmpfile
    silent echon cmd_output
    redir END
    let old_efm = &efm

    setlocal efm=%f:%m
    if exists(":cgetfile")
        execute "silent! cgetfile " . tmpfile
    else
        execute "silent! cfile " . tmpfile
    endif

    let &efm = old_efm

    if g:PerldocOpenQuickfixWindow == 1
        botright copen
    endif

    call delete(tmpfile)

endfunction

command! -nargs=* -complete=file PerldocSearch :call s:PerldocSearch(<f-args>)

" vim: ft=vim
