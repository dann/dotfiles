runtime! ftplugin/xml.vim
compiler tidy
set dictionary=~/.vim/dict/html.dict
set dictionary+=~/.vim/dict/css.dict
set iskeyword+=-,:

setlocal tabstop=4 softtabstop=4 shiftwidth=4

" HTML tidy
noremap ,ht <ESC>:%! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>
noremap ,htv <ESC>:'<,'>! tidy -config ~/.tidyrc -m -q <CR>:retab<CR>
