" ============================================
" Persoal config
" ============================================
if filereadable( $HOME . "/.config/nvim/.nvimrc-before" )
  runtime ./.nvimrc-before
endif

" ============================================
" Plugins
" ============================================
runtime ./plug.vim
runtime ./plug_config.vim

" ============================================
" Basic configs 
" ============================================
runtime ./core.vim

" ============================================
" Personal config
" ============================================
if filereadable( $HOME . "/.config/nvim/.nvimrc-after" )
  runtime ./.nvimrc-after
endif



