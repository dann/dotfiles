
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
au BufWritePost * call Perl_SyntaxCheck() | call Perl_SyntaxCheckMsg() | redraw!

" perldoc
"noremap    <buffer>  <silent>  <F1>             :call Perl_perldoc()<CR><CR>
"imap   <buffer>  <silent>  <F1>        <Esc>:call Perl_perldoc()<CR><CR>

" test
"map    <buffer>  <silent>  <F2>             :call RunLastT()<CR>
"imap   <buffer>  <silent>  <F2>        <Esc>:call RunLastT()<CR>

" prove
noremap ,t <Esc>:!prv -lv t/%<CR>
noremap ,T <Esc>:!prv -lv t/% \| less <CR>

" perltidy
noremap ,pt <Esc>:%! perltidy -pbp<CR>
noremap ,ptv <Esc>:'<,'>! perltidy -pbp<CR>

" prove
noremap ,c <Esc>:!ctagsp<CR>

" dictionary
setlocal dictionary=~/.vim/dict/perl.dict


setlocal tags+=~/.vim/tags/perl/cpan.tags
"set tags+=~/.vim/tags/perl/cpan.tags
