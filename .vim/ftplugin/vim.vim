" Only do this when not done yet for this buffer
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" ---------- tabulator / shiftwidth ------------------------------------------
setlocal tabstop=2
setlocal shiftwidth=2
setlocal expandtab

" ---------- matchit ------------------------------------------
let b:match_words = &matchpairs . ',\<if\>:\<en\%[dif]\>'
let b:match_words += ',\<fun\%[ction]!\=\>:\<endfun\%[ction]\>'
let b:match_words += ',\<wh\%[ile]\>:\<endwh\%[ile]\>'
let b:match_words += ',\<for\>:\<endfor\=\>'

" ---------- dict --------------------------------------------
setlocal dictionary=~/.vim/dict/vim.dict

