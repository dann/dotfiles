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

" Perldoc
command! -nargs=+ -complete=file Perldoc call <SID>Perldoc(<q-args>)

function! s:Perldoc(args)
    tabnew
    if bufexists('perldoc ' . a:args)
        execute 'buffer' bufnr('perldoc ' . a:args)
    else
        silent execute '0read!LANG=C perldoc -otext -T' a:args
        if v:shell_error
            echoerr getline(1)
            quit!
        else
            file `='perldoc ' . a:args`
            setlocal buftype=nofile noswapfile
            if a:args =~ '\v(<|^)-m>'
                setfiletype perl
            else
                setfiletype man
            endif
            0
        endif
    endif
endfunction


" dictionary
setlocal dictionary=~/.vim/dict/perl.dict

setlocal tags+=~/.vim/tags/perl/cpan.tags

" Test::Class
if exists("g:perl_test_class_test")
  finish
endif
let g:perl_test_class_test = 1


function! s:PerlTestClassMethodRun()
  let re = '\vsub\s+(\w+)\s*:\s*Test'
  let line = search(re, 'bn')
  if line
    let res = matchlist(getline(line), re)
    execute '!TEST_METHOD="' . res[1] . '" /usr/bin/env perl -MP -w %'
  endif
endfunction

nmap ,tc :call <SID>PerlTestClassMethodRun()<CR>

noremap K :Perldoc<CR>

