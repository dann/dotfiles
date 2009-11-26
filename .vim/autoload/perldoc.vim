" &&&& Perldoc Window &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
"=VERSION 1.1.
"
let s:perldoc_window = { 'width':50 , 'height': 82 }
let s:perldoc_window.version = 1.1

fun! s:perldoc_window.open(name,param)
  " XXX: save size for each window
  " save window size
  let self.previous_height = winheight('%')
  let self.previous_width  = winwidth('%')

  vnew
  setlocal modifiable noswapfile nobuflisted
  setlocal buftype=nofile bufhidden=hide fdc=0
  setlocal nowrap cursorline nonumber

  setfiletype perldoc
  silent file Perldoc

  exec 'r !perldoc -tT ' . a:param . ' ' . a:name

  syn match HEADER +^\w.*$+
  syn match STRING +".\{-}"+
  syn match STRING2 +'.\{-}'+
  hi link HEADER Identifier
  hi link STRING Comment
  hi link STRING2 Comment

  setlocal nomodifiable
  call cursor(1,1)
  exec 'resize ' . self.width
  exec 'vertical resize ' . self.height
  autocmd BufWinLeave <buffer> call g:perldoc.close()
  nmap <buffer> <ESC> <C-W>q
endf

fun! s:perldoc_window.close()
  bw
  exec 'vertical resize ' . self.previous_width
  exec 'resize ' . self.previous_height
  redraw
endf

fun! perldoc#load()
  if ! exists('g:perldoc')
    let g:perldoc = copy(s:perldoc_window)
  endif
endf
