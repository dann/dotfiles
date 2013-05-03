" =========================
" set runtime path like linux
" =========================
" set runtimepath+=$HOME/.vim
" set runtimepath+=$HOME/.vim/after
" set runtimepath+=$HOME/.vim/plugin

" ==========================
" Menu bar
" ==========================
" ツールバーを消す
set guioptions-=T
" メニューバーも消す
"set guioptions-=m

" ==========================
" Window
" ==========================
" window size
set columns=88
" window height
set lines=55

" ==========================
" colorscheme
" ==========================
colorscheme wombat
if g:colors_name ==? 'wombat'
  hi Visual gui=none guifg=khaki guibg=olivedrab

endif

" IME
set noimdisableactivate

" ============================================
" load common config
" ============================================
if filereadable( $HOME . "/.vimrc" )
  source ~/.vimrc
endif

if has('win32')
  source ~/.vim/_gvimrc-windows
endif

if has('gui_macvim')
  source ~/.vim/_gvimrc-mac
endif
