if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

compiler scss
autocmd BufWritePost <buffer> :silent make
