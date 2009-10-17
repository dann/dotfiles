if exists("did_load_filetypes")
    finish
endif
augroup filetypedetect
    au! BufNewFile,BufRead svk-commit*.tmp      setf svk
    au! BufNewFile,BufRead svn-commit*.tmp      setf svn
    au! BufNewFile,BufRead mason/*.html         setf mason
    au BufRead,BufNewFile *.mas set ft=mason
    au BufRead,BufNewFile *.mxml set ft=mxml
    au BufRead,BufNewFile *.as set ft=actionscript
    au! BufNewFile,BufRead *.t                  setf perltest
    au! BufNewFile,BufRead *.hwd                setf hwd
    au! BufNewFile,BufRead *.wiki               setf wiki
augroup END
