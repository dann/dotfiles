"=============================================================================
" FILE: view.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 16 Sep 2010
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
      \ 'name' : 'view',
      \ 'kind' : 'internal',
      \ 'description' : 'view [{filename}]',
      \}
function! s:command.execute(program, args, fd, context)"{{{
  " View file.

  if empty(a:args)
    if a:fd.stdin == ''
      vimshell#error_line(a:fd, 'view: Filename required.')
      return
    endif

    " Read from stdin.
    let l:filename = a:fd.stdin
  else
    let l:filename = a:args[0]
  endif

  if !isdirectory(l:filename)
    let l:lines = readfile(l:filename)
    if len(l:lines) < winheight(0)
      " Print lines if one screen.
      for l:line in l:lines
        call vimshell#print_line(a:fd, l:line)
      endfor

      return
    endif
  endif

  " Save current directiory.
  let l:cwd = getcwd()

  let l:save_winnr = winnr()

  " Split nicely.
  call vimshell#split_nicely()

  try
    if len(a:args) > 1
      execute 'edit' '+'.a:args[1] l:filename
    else
      edit `=l:filename`
    endif
  catch
    echohl Error | echomsg v:errmsg | echohl None
  endtry

  " Call explorer.
  doautocmd BufEnter

  call vimshell#cd(l:cwd)
  setlocal nomodifiable

  let l:last_winnr = winnr()
  execute l:save_winnr.'wincmd w'

  if has_key(a:context, 'is_single_command') && a:context.is_single_command
    call vimshell#print_prompt(a:context)
    execute l:last_winnr.'wincmd w'
    stopinsert
  endif
endfunction"}}}

function! vimshell#commands#view#define()
  return s:command
endfunction
