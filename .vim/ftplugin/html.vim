if exists("b:did_HTML_ftplugin")
  finish
endif
let b:did_HTML_ftplugin = 1

runtime! ftplugin/xml.vim

setlocal filetype=xhtml
setlocal omnifunc=htmlcomplete#CompleteTags

" double quotations for % ("%") are needed for Windows
if has('win32')
    setlocal makeprg=tidy\ -raw\ -quiet\ -errors\ --gnu-emacs\ yes\ \"%\"
    setlocal errorformat=%f:%l:%c:\ %t%*[^:]:\ %m
else
    setlocal makeprg=tidy\ -raw\ -quiet\ -errors\ --gnu-emacs\ yes\ %
    setlocal errorformat=%f:%l:%c:\ %t%*[^:]:\ %m
endif

" Completion
set dictionary=~/.vim/dict/html.dict
set dictionary+=~/.vim/dict/css.dict
set iskeyword+=-,:

" Tab settings
setlocal tabstop=2 softtabstop=2 shiftwidth=2

" HTML tidy
noremap ,ht <ESC>:%! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>
noremap ,htv <ESC>:'<,'>! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>

" html close
inoremap ,/ </<C-X><C-O>

" autocmd BufWritePost * silent make %
