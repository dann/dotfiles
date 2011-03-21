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

" ============================================
" minimum config 
" ============================================
if filereadable( $HOME . "/.vimrc-minimum" )
  source ~/.vimrc-minimum
endif

" backup
set backupdir=~/.vim/backup
let &directory = &backupdir

scriptencoding utf-8

" add runtimepathe .vim/bundle/* 
call pathogen#runtime_append_all_bundles()

" ============================================
" plugin config 
" ============================================
if filereadable( $HOME . "/.vimrc-plugins" )
  source ~/.vimrc-plugins
endif

" ============================================
" mac
" ============================================
if has("mac") 
  if filereadable( $HOME . "/.vimrc-mac" )
    source ~/.vimrc-mac
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
