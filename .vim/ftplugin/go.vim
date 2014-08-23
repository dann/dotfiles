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


"--------------------------------------------------
" Highlight
"--------------------------------------------------
autocmd FileType go :highlight goErr cterm=bold ctermfg=214
autocmd FileType go :match goErr /\<err\>/

"--------------------------------------------------
" vim-go
"--------------------------------------------------
" let g:go_bin_path = expand("~/.go/bin")
let g:go_play_open_browser = 0
let g:go_fmt_fail_silently = 1
let g:go_fmt_autosave = 0
let g:go_fmt_command = "gofmt"
let g:go_disable_autoinstall = 1

au FileType go nmap <Leader>i <Plug>(go-info)
au FileType go nmap <Leader>gd <Plug>(go-doc)
au FileType go nmap <Leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <leader>gb <Plug>(go-build)
au FileType go nmap <leader>gt <Plug>(go-test)
au FileType go nmap gd <Plug>(go-def)
au FileType go nmap <Leader>ds <Plug>(go-def-split)
au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
au FileType go nmap <Leader>dt <Plug>(go-def-tab)
au FileType go nmap <Leader>gl :GoLint<CR>
