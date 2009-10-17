" Only do this when not done yet for this buffer
if exists("b:did_CPP_ftplugin")
  finish
endif
let b:did_CPP_ftplugin = 1

" ---------- tabulator / shiftwidth ------------------------------------------
setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal dictionary=~/.vim/dict/cpp-libstdc++.dict,~/.vim/dict/c-eglibc.dict,~/.vim/dict/cpp-boost.dict


