"=============================================================================
" FILE: histdel.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 07 Jul 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:command = {
      \ 'name' : 'histdel',
      \ 'kind' : 'internal',
      \ 'description' : 'histdel {history-number}',
      \}
function! s:command.execute(command, args, fd, context)"{{{
  " Delete from history.

  if !empty(a:args)
    let l:del_hist = {}
    for d in a:args
      let l:del_hist[d] = 1
    endfor

    let l:new_hist = []
    let l:cnt = 0
    for h in g:vimshell#hist_buffer
      if !has_key(l:del_hist, l:cnt)
        call add(l:new_hist, h)
      endif
      let l:cnt += 1
    endfor
    let g:vimshell#hist_buffer = l:new_hist
  else
    call vimshell#error_line(a:fd, 'histdel: Arguments required.')
  endif
endfunction"}}}

function! vimshell#commands#histdel#define()
  return s:command
endfunction
