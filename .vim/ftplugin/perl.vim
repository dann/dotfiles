" Only do this when not done yet for this buffer
if exists("b:did_PERL_ftplugin")
  finish
endif
let b:did_PERL_ftplugin = 1
" ---------- tabulator / shiftwidth ------------------------------------------
"  Set tabulator and shift width to 4 conforming to the Perl Style Guide.
"  Uncomment the next two lines to force these settings for all files with
"  filetype 'perl' . 
setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
setlocal iskeyword+=:

" auto syntax check with Perl support
au BufWritePost <buffer> call Perl_SyntaxCheck() | call Perl_SyntaxCheckMsg() | redraw!

" flymake settings
"setlocal makeprg=$HOME/devbin/vimparse.pl\ -c\ %\ $*
"setlocal errorformat=%f:%l:%m
"setlocal shellpipe=2>&1\ >
"au BufWritePost <buffer> silent make
"au CursorMoved <buffer> silent make

" prove
noremap <buffer> ,t <Esc>:!prv -lv t/%<CR>
noremap <buffer> ,T <Esc>:!prv -lv t/% \| less <CR>

" perltidy
noremap <buffer> ,pt <Esc>:%! perltidy -pbp<CR>
noremap <buffer> ,ptv <Esc>:'<,'>! perltidy -pbp<CR>

" prove
noremap <buffer> ,c <Esc>:!ctagsp<CR>

" dictionary
setlocal dictionary=~/.vim/dict/perl.dict

setlocal tags+=~/.vim/tags/perl/cpan.tags
