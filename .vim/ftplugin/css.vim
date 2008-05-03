compiler css
setlocal dictionary=~/.vim/dict/css.dict
setlocal iskeyword+=-

setlocal tabstop=4 softtabstop=4 shiftwidth=4

" csstidy
noremap ,ctv <Esc>:'<,'>! csstidy - --silent=true --template=low<CR>:retab<CR>
noremap ,ct <Esc>:%! csstidy - --silent=true --template=low<CR>:retab<CR>
