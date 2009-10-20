" Tab
setlocal tabstop=4 softtabstop=4 shiftwidth=4

compiler css

" Completion
setlocal dictionary=~/.vim/dict/css.dict
setlocal iskeyword+=-
setlocal omnifunc=csscomplete#CompleteCSS

" csstidy
noremap ,ctv <Esc>:'<,'>! csstidy - --silent=true --template=low<CR>:retab<CR>
noremap ,ct <Esc>:%! csstidy - --silent=true --template=low<CR>:retab<CR>
