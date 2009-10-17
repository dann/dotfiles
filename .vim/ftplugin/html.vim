if exists("b:did_HTML_ftplugin")
  finish
endif
let b:did_HTML_ftplugin = 1

runtime! ftplugin/xml.vim

" Compiler
compiler tidy

" Completion
set dictionary=~/.vim/dict/html.dict
set dictionary+=~/.vim/dict/css.dict
set iskeyword+=-,:

" Tab settings
setlocal tabstop=4 softtabstop=4 shiftwidth=4

" HTML tidy
noremap ,ht <ESC>:%! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>
noremap ,htv <ESC>:'<,'>! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>
