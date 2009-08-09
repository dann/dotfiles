"--------------------------------------------------
" Code style
"--------------------------------------------------
setlocal tabstop=2 softtabstop=2 shiftwidth=2

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


"--------------------------------------------------
" Syntax Check
"--------------------------------------------------
setlocal makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
setlocal efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m


" autocmd BufWrite *.{py} :call Pyflakes()

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

"--------------------------------------------------
" Execute file being edited with <Shift> + e:
"--------------------------------------------------
map <buffer> <S-e> :w<CR>:!/usr/bin/env python % <CR>

"--------------------------------------------------
" Completion
"--------------------------------------------------
setlocal complete+=k~/.vim/dict/pydiction isk+=.,( 
" Turn on completion:
setlocal omnifunc=pythoncomplete#Complete
" Map it to <Ctrl> + Space:
inoremap <Nul> <C-x><C-o>
