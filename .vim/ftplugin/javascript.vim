" Only do this when not done yet for this buffer
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

compiler javascriptlint

setlocal dictionary=~/.vim/dict/javascript.dict
