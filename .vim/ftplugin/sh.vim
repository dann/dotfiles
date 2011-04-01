" Only do this when not done yet for this buffer
if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal tabstop=4
setlocal shiftwidth=4

let b:match_words = &matchpairs . ',\<if\>:\<fi\>'
let b:match_words += ',\<do\>:\<done\>'
 
