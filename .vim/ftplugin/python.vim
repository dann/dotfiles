"" Only do this when not done yet for this buffer
if (exists("b:did_ftplugin"))
    finish
endif
let b:did_ftplugin = 1

"--------------------------------------------------
" Code style
"--------------------------------------------------
setlocal autoindent
setlocal smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
setlocal textwidth=80
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal smarttab

" tags
setlocal tags+=~/.vim/tags/python/python.tags

"--------------------------------------------------
" Execute Pyflakes 
"--------------------------------------------------
command Pyflakes :call Pyflakes()
function! Pyflakes()
    let tmpfile = tempname()
    execute "w" tmpfile
    execute "set makeprg=(pyflakes\\ " . tmpfile . "\\\\\\|sed\\ s@" . tmpfile ."@%@)"
    make
    cw
endfunction

" autocmd BufWrite *.{py} :call Pyflakes()

"--------------------------------------------------
" Syntax Check
"--------------------------------------------------
"syntax/python.vim
let python_highlight_all=1

"compiler/python.vim
compiler python



"--------------------------------------------------
" Execute Pylint 
"--------------------------------------------------
command Pylint :call Pylint()
function! Pylint()
    setlocal makeprg=(echo\ '[%]';\ pylint\ %)
    setlocal efm=%+P[%f],%t:\ %#%l:%m
    silent make
    cwindow
endfunction

" Turn on completion:
setlocal omnifunc=pythoncomplete#Complete
