" Only do this when not done yet for this buffer
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

"--------------------------------------------------
" Code style
"--------------------------------------------------
setlocal autoindent
setlocal textwidth=80
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

"--------------------------------------------------
" auto format
"--------------------------------------------------
au BufWritePre <buffer> Fmt 

"--------------------------------------------------
" compleion
"--------------------------------------------------
au FileType go set omnifunc=gocomplete#Complete
au FileType go let g:SuperTabDefaultCompletionType = "context"

"--------------------------------------------------
" Ref
"--------------------------------------------------
noremap K :Godoc<CR>

"--------------------------------------------------
" Utility
"--------------------------------------------------
function! s:GoVet()
    cexpr system("go vet " . shellescape(expand('%')))
    copen
endfunction
command! GoVet :call s:GoVet()

function! s:GoLint()
    cexpr system("golint " . shellescape(expand('%')))
    copen
endfunction
command! GoLint :call s:GoLint()
