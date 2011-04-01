" Only do this when not done yet for this buffer
if exists("b:did_ACTIONSCRIPT_ftplugin")
  finish
endif
let b:did_ACTIONSCRIPT_ftplugin = 1

setlocal tabstop=4
setlocal shiftwidth=4
setlocal expandtab
"setlocal iskeyword+=.

setlocal dictionary=~/.vim/dict/actionscript.dict,~/.vim/dict/as3libs.dict

setlocal tags+=~/.vim/tags/actionscript/as3libs.tags

