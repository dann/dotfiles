if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

" Compile SCCS to CSS if file name does not start with '_'
function! s:CompileSCCS(...)
  if expand('%:r') !~ '^_'
    execute 'silent make'
  else
    execute 'silent make -c'
  endif
endfunction

compiler scss

"autocmd BufWritePost <buffer> :call <SID>CompileSCCS()
