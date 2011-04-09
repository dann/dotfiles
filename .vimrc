if !exists('s:loaded_my_vimrc')
  " Don't reset twice on reloading - 'compatible' has SO many side effects.
  set nocompatible " to use many extensions of Vim.
endif

" ============================================
" persoal env config
" ============================================
if filereadable( $HOME . "/.vimrc-before" )
  source ~/.vimrc-before
endif

" add runtimepathe .vim/bundle/* 
call pathogen#runtime_append_all_bundles()

" ============================================
" minimum config 
" ============================================
if filereadable( $HOME . "/.vim/.vimrc-minimum" )
  source ~/.vim/.vimrc-minimum
endif

" ============================================
" functions 
" ============================================
if filereadable( $HOME . "/.vim/.vimrc-functions" )
  source ~/.vim/.vimrc-functions
endif

" ============================================
" plugin config 
" ============================================
if filereadable( $HOME . "/.vim/.vimrc-plugins" )
  source ~/.vim/.vimrc-plugins
endif

" ============================================
" mac
" ============================================
if has("mac") 
  if filereadable( $HOME . "/.vim/.vimrc-mac" )
    source ~/.vim/.vimrc-mac
  endif
endif

" ============================================
" personal env config
" ============================================
if filereadable( $HOME . "/.vimrc-after" )
  source ~/.vimrc-after
endif


if !exists('s:loaded_my_vimrc')
  let s:loaded_my_vimrc = 1
endif
 
set secure " must be written at the last. see :help 'secure'.
