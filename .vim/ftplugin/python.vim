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

"--------------------------------------------------
"syntax highlight
"--------------------------------------------------
let python_highlight_all=1

"--------------------------------------------------
" Completion
"--------------------------------------------------
" Turn on completion:
setlocal omnifunc=pythoncomplete#Complete

" tags
setlocal tags+=~/.vim/tags/python/python.tags

"--------------------------------------------------
" Format 
"--------------------------------------------------
noremap <buffer> ,pp :call Pep8()<CR>
"noremap <buffer> ,pp <Esc>:!pep8 %<CR>
noremap <buffer> ,pt <Esc>:call PythonTidy()<CR>
noremap <buffer> ,pf :call Pyflakes()<CR>

"--------------------------------------------------
" Syntax Check
"--------------------------------------------------
"autocmd BufWrite *.{py} :call Pyflakes()

"--------------------------------------------------
" Pydoc
"--------------------------------------------------
" remap the K (or 'help') key
nnoremap <silent> <buffer> K :call ShowPyDoc(expand("<cword>"), 1)<CR>


function! PythonTidy()
    let oldpos=getpos('.')
    %!pythontidy    
    call setpos('.',oldpos)
endfunction
