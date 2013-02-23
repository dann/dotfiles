if exists("did_load_filetypes")
    finish
endif
augroup filetypedetect
    au! BufNewFile,BufRead svk-commit*.tmp      setf svk
    au! BufNewFile,BufRead svn-commit*.tmp      setf svn
    au! BufNewFile,BufRead mason/*.html         setf mason
    au BufRead,BufNewFile *.mas                 setf =mason
    au BufRead,BufNewFile *.mxml                setf =mxml
    au BufRead,BufNewFile *.as                  setf actionscript
    au! BufNewFile,BufRead *.t                  setf perltest
    au! BufNewFile,BufRead *.hwd                setf hwd
    au! BufNewFile,BufRead *.wiki               setf wiki
    au BufNewFile,BufRead *.haml setf haml
    au BufNewFile,BufRead *.sass setf sass
    au! BufNewFile,BufRead *.html
        \ if ( getline(1) . getline(2) . getline(3) =~ '\[%' ) |
        \   setf tt2html |
        \ else |
        \   setf html |
        \ endif
    au BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
      \ if &ft =~# '^\%(conf\|modula2\)$' |
      \   set ft=markdown |
      \ else |
      \   setf markdown |
      \ endif
    " Ruby
    au BufNewFile,BufRead *.rb,*.rbw,*.gem,*.gemspec	set filetype=ruby
    " Ruby on Rails
    au BufNewFile,BufRead *.builder,*.rxml,*.rjs		set filetype=ruby
    " Rakefile
    au BufNewFile,BufRead [rR]akefile,*.rake		set filetype=ruby
    " Rantfile
    au BufNewFile,BufRead [rR]antfile,*.rant		set filetype=ruby
    " eRuby
    au BufNewFile,BufRead *.erb,*.rhtml			set filetype=eruby
    " psgi
    au BufNewFile,BufRead *.psgi setf perl
"    au! BufNewFile,BufRead *.coffe              setf coffe

    au BufRead,BufNewFile *.scala set filetype=scala

    au BufRead,BufNewFile *.scss setf scss
    au BufNewFile,BufRead *.tt2
            \ if ( getline(1) . getline(2) . getline(3) =~ '<\chtml'
            \           && getline(1) . getline(2) . getline(3) !~ '<[%?]' )
            \   || getline(1) =~ '<!DOCTYPE HTML' |
            \   setf tt2html |
            \ else |
            \   setf tt2 |
            \ endif
    au BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
        \ if &ft =~# '^\%(conf\|modula2\)$' |
        \   set ft=markdown |
        \ else |
        \   setf markdown |
        \ endif
augroup END
