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
" Format 
"--------------------------------------------------
noremap <buffer> ,pp :call Autopep8()<CR>
"noremap <buffer> ,pp <Esc>:!pep8 %<CR>
noremap <buffer> ,pt <Esc>:call PythonTidy()<CR>
noremap <buffer> ,pf :call Pyflakes()<CR>

"--------------------------------------------------
" Autopep8
"--------------------------------------------------
function! Preserve(command)
    " Save the last search.
    let search = @/
    " Save the current cursor position.
    let cursor_position = getpos('.')
    " Save the current window position.
    normal! H
    let window_position = getpos('.')
    call setpos('.', cursor_position)
    " Execute the command.
    execute a:command
    " Restore the last search.
    let @/ = search
    " Restore the previous window position.
    call setpos('.', window_position)
    normal! zt
    " Restore the previous cursor position.
    call setpos('.', cursor_position)
endfunction

function! Autopep8()
    call Preserve(':silent %!autopep8 -')
endfunction

