au! BufNewFile,BufRead *.html
        \ if ( getline(1) . getline(2) . getline(3) =~ '\[%' ) |
        \   setf tt2html |
        \ else |
        \   setf html |
        \ endif


