"=============================================================================
" FILE: vimproc.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com> (Modified)
"          Yukihiro Nakadaira <yukihiro.nakadaira at gmail.com> (Original)
" Last Modified: 08 Nov 2010
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
" Version: 5.0, for Vim 7.0
"=============================================================================

" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}

function! vimproc#version()
  return str2nr(printf('%2d%02d', 5, 0))
endfunction

let s:is_win = has('win32') || has('win64')
let s:last_status = 0

" Global options definition."{{{
if !exists('g:vimproc_dll_path')
  let g:vimproc_dll_path = expand("<sfile>:p:h") . (has('win32') || has('win64') || has('win32unix') ? '/proc.dll' : '/proc.so')
endif
"}}}

if has('iconv')
  " Dll path should be encoded with default encoding.  Vim does not convert
  " it from &enc to default encoding.
  let g:vimproc_dll_path = iconv(g:vimproc_dll_path, &encoding, "default")
endif

if !filereadable(g:vimproc_dll_path)
  echoerr printf('vimproc''s DLL: "%s" is not found. Please read :help vimproc and make it.', g:vimproc_dll_path)
  finish
endif

"-----------------------------------------------------------
" API

function! vimproc#open(filename)"{{{
  let l:filename = a:filename
  if &termencoding != '' && &encoding != &termencoding
    " Convert encoding.
    let l:filename = iconv(l:filename, &encoding, &termencoding)
  endif

  " Detect desktop environment.
  if s:is_win
    " For URI only.
    "execute '!start rundll32 url.dll,FileProtocolHandler' l:filename
    
    call s:libcall('vp_open', [fnamemodify(a:filename, ':p')])
  elseif has('win32unix')
    " Cygwin.
    call vimproc#system(['cygstart', l:filename])
  elseif executable('xdg-open')
    " Linux.
    call vimproc#system_bg(['xdg-open', l:filename])
  elseif exists('$KDE_FULL_SESSION') && $KDE_FULL_SESSION ==# 'true'
    " KDE.
    call vimproc#system_bg(['kioclient', 'exec', l:filename])
  elseif exists('$GNOME_DESKTOP_SESSION_ID')
    " GNOME.
    call vimproc#system_bg(['gnome-open', l:filename])
  elseif executable('exo-open')
    " Xfce.
    call vimproc#system_bg(['exo-open', l:filename])
  elseif (has('macunix') || system('uname') =~? '^darwin') && executable('open')
    " Mac OS.
    call vimproc#system_bg(['open', l:filename])
  else
    " Give up.
    throw 'vimproc#open: Not supported.'
  endif
endfunction"}}}

function! vimproc#get_command_name(command, ...)"{{{
  if a:0 > 3
    throw 'vimproc#get_command_name: Invalid argument.'
  endif

  if a:0 >= 1
    let l:path = a:1
  else
    let l:path = $PATH
  endif

  " Expand path.
  let l:path = substitute(l:path, (s:is_win ? ';' : ':'), ',', 'g')
  let l:path = join(split(l:path, ','), ',')
  let l:path = substitute(l:path, '\s', '\\\\ ', 'g')

  let l:count = a:0 < 2 ? 1 : a:2

  let l:command = expand(a:command)

  let l:pattern = printf('[/~]\?\f\+[%s]\f*$', s:is_win ? '/\\' : '/')
  if l:command =~ l:pattern
    if !executable(l:command)
      let l:command = resolve(l:command)
    endif

    if !filereadable(l:command)
      throw printf('vimproc#get_command_name: File "%s" is not found.', l:command)
    elseif !s:is_win && !executable(l:command)
      throw printf('vimproc#get_command_name: File "%s" is not executable.', l:command)
    endif

    return l:count < 0 ? [ l:command ] : l:command
  endif

  " Command search.
  let l:suffixesadd_save = &l:suffixesadd
  if s:is_win
    " On Windows, findfile() search a file which don't have file extension
    " also. When there are 'perldoc', 'perldoc.bat' in your $PATH,
    " executable('perldoc')  return 1 cause by you have 'perldoc.bat'.
    " But findfile('perldoc', $PATH, 1) return whether file exist there.
    if fnamemodify(l:command, ':e') == ''
      let &l:suffixesadd = ''
      for l:ext in split($PATHEXT.';.LNK', ';')
        let l:file = findfile(l:command . l:ext, l:path, l:count)
        if (l:count >= 0 && l:file != '') || (l:count < 0 && empty(l:file))
          break
        endif
      endfor
    else
      let &l:suffixesadd = substitute($PATHEXT.';.LNK', ';', ',', 'g')
      let l:file = findfile(l:command, l:path, l:count)
    endif
  else
    let &l:suffixesadd = ''
    let l:file = findfile(l:command, l:path, l:count)
  endif
  let &l:suffixesadd = l:suffixesadd_save

  if l:count < 0
    return map(filter(l:file, 'executable(v:val)'), 'fnamemodify(v:val, ":p")')
  else
    if l:file != ''
      let l:file = fnamemodify(l:file, ':p')
    endif

    if !executable(l:command)
      let l:file = resolve(l:file)
    endif

    if l:file == ''
      throw printf('vimproc#get_command_name: File "%s" is not found.', l:command)
    elseif !s:is_win && !executable(l:file)
      throw printf('vimproc#get_command_name: File "%s" is not executable.', l:file)
    endif
  endif

  return l:file
endfunction"}}}

function! vimproc#system(cmdline, ...)"{{{
  if type(a:cmdline) == type('')
    if a:cmdline =~ '&\s*$'
      return vimproc#system_bg(a:cmdline)
    elseif (!has('unix') || a:cmdline !~ '^\s*man ')
      if a:0 == 0
        return vimproc#parser#system(a:cmdline)
      elseif a:0 == 1
        return vimproc#parser#system(a:cmdline, a:1)
      else
        return vimproc#parser#system(a:cmdline, a:1, a:2)
      endif
    endif
  endif

  if empty(a:cmdline)
    let s:last_status = 0
    let s:last_errmsg = ''
    return ''
  endif
  
  let l:timeout = a:0 >= 2 ? a:2 : 0
  
  " Open pipe.
  let l:subproc = (type(a:cmdline[0]) == type('')) ? 
        \ vimproc#popen3(a:cmdline) : vimproc#pgroup_open(a:cmdline)

  if !empty(a:000)
    " Write input.
    call l:subproc.stdin.write(a:1)
  endif
  call l:subproc.stdin.close()
  
  if l:timeout > 0 && has('reltime') && v:version >= 702
    let l:start = reltime()
  else
    let l:timeout = 0
  endif
  
  let l:output = ''
  let s:last_errmsg = ''
  while !l:subproc.stdout.eof || !l:subproc.stderr.eof
    if l:timeout > 0
      " Check timeout.
      let l:end = split(reltimestr(reltime(l:start)))[0] * 1000
      if l:end > l:timeout && !l:subproc.stdout.eof
        " Kill process.
        " 15 == SIGTERM
        try
          call l:subproc.kill(15)
        catch
          " Ignore error.
        endtry
        
        return ''
      endif
    endif
    
    if !l:subproc.stdout.eof
      let l:output .= l:subproc.stdout.read(-1, 40)
    endif
    
    if !l:subproc.stderr.eof
      let s:last_errmsg .= l:subproc.stderr.read(-1, 40)
    endif
  endwhile
  
  call l:subproc.stdout.close()
  call l:subproc.stderr.close()

  let [l:cond, s:last_status] = l:subproc.waitpid()

  " Newline convert.
  if has('mac')
    let l:output = substitute(l:output, '\r', '\n', 'g')
  elseif has('win32') || has('win64')
    let l:output = substitute(l:output, '\r\n', '\n', 'g')
  endif

  return l:output
endfunction"}}}
function! vimproc#system_bg(cmdline)"{{{
  if type(a:cmdline) == type('')
    if s:is_win
      let l:cmdline = (a:cmdline =~ '&\s*$')? a:cmdline[: match(a:cmdline, '&\s*$') - 1] : a:cmdline
      silent execute '!start' l:cmdline
      return ''
    else
      return vimproc#parser#system_bg(a:cmdline)
    endif
  endif
  
  if s:is_win
    silent execute '!start' join(map(a:cmdline, '"\"".v:val."\""'))
  else
    if !exists('s:bg_processes')"{{{
      let s:bg_processes = {}

      augroup vimproc
        autocmd CursorHold * call s:garbage_collect()
      augroup END
    endif"}}}
    
    " Open pipe.
    let l:subproc = vimproc#popen3(a:cmdline)
    let s:bg_processes[l:subproc.pid] = l:subproc
  endif
  
  return ''
endfunction"}}}

function! vimproc#get_last_status()"{{{
  return s:last_status
endfunction"}}}
function! vimproc#get_last_errmsg()"{{{
  return s:last_errmsg
endfunction"}}}

function! vimproc#fopen(path, flags, ...)"{{{
  let l:mode = get(a:000, 0, 0)
  let l:fd = s:vp_file_open(a:path, a:flags, l:mode)
  return s:fdopen(l:fd, 'vp_file_close', 'vp_file_read', 'vp_file_write')
endfunction"}}}

function! vimproc#popen2(args)"{{{
  if type(a:args) == type('')
    return vimproc#parser#popen2(a:args)
  endif
  
  return s:popen(3, a:args)
endfunction"}}}
function! vimproc#popen3(args)"{{{
  if type(a:args) == type('')
    return vimproc#parser#popen3(a:args)
  endif

  return s:popen(3, a:args)
endfunction"}}}
function! s:popen(npipe, args)"{{{
  let l:pipe = s:vp_pipe_open(a:npipe, s:convert_args(a:args))
  if a:npipe == 3
    let [l:pid, l:fd_stdin, l:fd_stdout, l:fd_stderr] = l:pipe
  else
    let [l:pid, l:fd_stdin, l:fd_stdout] = l:pipe
  endif
  
  let l:proc = {}
  let l:proc.pid = l:pid
  let l:proc.stdin = s:fdopen(l:fd_stdin, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write')
  let l:proc.stdout = s:fdopen(l:fd_stdout, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write')
  if a:npipe == 3
    let l:proc.stderr = s:fdopen(l:fd_stderr, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write')
  endif
  let l:proc.kill = s:funcref('vp_kill')
  let l:proc.waitpid = s:funcref('vp_waitpid')
  let l:proc.is_valid = 1

  return proc
endfunction"}}}

function! vimproc#plineopen2(commands)"{{{
  if type(a:commands) == type('')
    return vimproc#parser#plineopen2(a:commands)
  endif

  return s:plineopen(2, a:commands)
endfunction"}}}
function! vimproc#plineopen3(commands)"{{{
  if type(a:commands) == type('')
    return vimproc#parser#plineopen3(a:commands)
  endif

  return s:plineopen(3, a:commands)
endfunction"}}}
function! s:plineopen(npipe, commands)"{{{
  let l:pid_list = []
  let l:stdin_list = []
  let l:stdout_list = []
  let l:stderr_list = []
  for l:command in a:commands
    let l:pipe = s:vp_pipe_open(a:npipe, s:convert_args(l:command.args))
    if a:npipe == 3
      let [l:pid, l:fd_stdin, l:fd_stdout, l:fd_stderr] = l:pipe
    else
      let [l:pid, l:fd_stdin, l:fd_stdout] = l:pipe
    endif
    
    call add(l:pid_list, l:pid)
    call add(l:stdin_list, s:fdopen(l:fd_stdin, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write'))
    call add(l:stdout_list, s:fdopen(l:fd_stdout, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write'))
    if a:npipe == 3
      call add(l:stderr_list, s:fdopen(l:fd_stderr, 'vp_pipe_close', 'vp_pipe_read', 'vp_pipe_write'))
    endif
  endfor

  " Set pipe redirection.
  let i = 0
  let max = len(l:pid_list) - 1
  while i < max
    let l:stdin_list[i].redirect_fd = []
    let l:stdout_list[i].redirect_fd = [ l:stdin_list[i+1] ]
    if a:npipe == 3
      let l:stderr_list[i].redirect_fd = []
    endif

    let i += 1
  endwhile
  let l:stdin_list[i].redirect_fd = []
  let l:stdout_list[i].redirect_fd = []
  if a:npipe == 3
    let l:stderr_list[i].redirect_fd = []
  endif

  let l:proc = {}
  let l:proc.pid_list = l:pid_list
  let l:proc.pid = l:pid_list[-1]
  let l:proc.stdin = s:fdopen_pipes(l:stdin_list, 'vp_pipes_front_close', 'read_pipes', 'write_pipes')
  let l:proc.stdout = s:fdopen_pipes(l:stdout_list, 'vp_pipes_back_close', 'read_pipes', 'write_pipes')
  if a:npipe == 3
    let l:proc.stderr = s:fdopen_pipes(l:stderr_list, 'vp_pipes_back_close', 'read_pipes', 'write_pipes')
  endif
  let l:proc.kill = s:funcref('vp_pipes_kill')
  let l:proc.waitpid = s:funcref('vp_waitpid')
  let l:proc.is_valid = 1

  return proc
endfunction"}}}

function! vimproc#pgroup_open(statements)"{{{
  if type(a:statements) == type('')
    return vimproc#parser#pgroup_open(a:statements)
  endif

  let l:proc = {}
  let l:proc.current_proc = vimproc#plineopen3(a:statements[0].statement)
  
  let l:proc.pid = l:proc.current_proc.pid
  let l:proc.condition = a:statements[0].condition
  let l:proc.statements = a:statements[1:]
  let l:proc.stdin = s:fdopen_pgroup(l:proc, l:proc.current_proc.stdin, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
  let l:proc.stdout = s:fdopen_pgroup(l:proc, l:proc.current_proc.stdout, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
  let l:proc.stderr = s:fdopen_pgroup(l:proc, l:proc.current_proc.stderr, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
  let l:proc.kill = s:funcref('vp_pgroup_kill')
  let l:proc.waitpid = s:funcref('vp_pgroup_waitpid')
  let l:proc.is_valid = 1

  return proc
endfunction"}}}

function! vimproc#ptyopen(args)"{{{
  if type(a:args) == type('')
    return vimproc#parser#ptyopen(a:args)
  endif
  
  if s:is_win
    let [l:pid, l:fd_stdin, l:fd_stdout] = s:vp_pipe_open(2, s:convert_args(a:args))
    let l:ttyname = ''

    let l:proc = s:fdopen_pty(l:fd_stdin, l:fd_stdout, 'vp_pty_close', 'vp_pty_read', 'vp_pty_write')
  else
    let [l:pid, l:fd, l:ttyname] = s:vp_pty_open(winwidth(0)-5, winheight(0), s:convert_args(a:args))

    let l:proc = s:fdopen(l:fd, 'vp_pty_close', 'vp_pty_read', 'vp_pty_write')
  endif

  let l:proc.pid = l:pid
  let l:proc.ttyname = l:ttyname
  let l:proc.get_winsize = s:funcref('vp_pty_get_winsize')
  let l:proc.set_winsize = s:funcref('vp_pty_set_winsize')
  let l:proc.kill = s:funcref('vp_kill')
  let l:proc.waitpid = s:funcref('vp_waitpid')
  let l:proc.is_valid = 1

  return l:proc
endfunction"}}}

function! vimproc#socket_open(host, port)"{{{
  let l:fd = s:vp_socket_open(a:host, a:port)
  return s:fdopen(l:fd, 'vp_socket_close', 'vp_socket_read', 'vp_socket_write')
endfunction"}}}

function! vimproc#kill(pid, sig)"{{{
  call s:libcall('vp_kill', [a:pid, a:sig])
endfunction"}}}

function! s:close() dict"{{{
  if self.is_valid
    call self.f_close()
  endif
  
  let self.is_valid = 0
  let self.eof = 1
  let self.fd = -1
endfunction"}}}
function! s:read(...) dict"{{{
  let l:number = get(a:000, 0, -1)
  let l:timeout = get(a:000, 1, s:read_timeout)
  let [l:hd, l:eof] = self.f_read(l:number, l:timeout)
  let self.eof = l:eof
  return s:hd2str(l:hd)
endfunction"}}}
function! s:write(str, ...) dict"{{{
  let l:timeout = get(a:000, 0, s:write_timeout)
  let l:hd = s:str2hd(a:str)
  return self.f_write(l:hd, l:timeout)
endfunction"}}}

function! s:fdopen(fd, f_close, f_read, f_write)"{{{
  return {
        \'fd' : a:fd, 'eof' : 0, 'is_valid' : 1,  
        \'f_close' : s:funcref(a:f_close), 'f_read' : s:funcref(a:f_read), 'f_write' : s:funcref(a:f_write), 
        \'close' : s:funcref('close'), 'read' : s:funcref('read'), 'write' : s:funcref('write')
        \}
endfunction"}}}
function! s:fdopen_pty(fd_stdin, fd_stdout, f_close, f_read, f_write)"{{{
  return {
        \'fd_stdin' : a:fd_stdin, 'fd_stdout' : a:fd_stdout, 'eof' : 0, 'is_valid' : 1, 
        \'f_close' : s:funcref(a:f_close), 'f_read' : s:funcref(a:f_read), 'f_write' : s:funcref(a:f_write), 
        \'close' : s:funcref('close'), 'read' : s:funcref('read'), 'write' : s:funcref('write')
        \}
endfunction"}}}
function! s:fdopen_pipes(fd, f_close, f_read, f_write)"{{{
  return {
        \'fd' : a:fd, 'eof' : 0, 'is_valid' : 1, 
        \'f_close' : s:funcref(a:f_close),
        \'close' : s:funcref('close'), 'read' : s:funcref(a:f_read), 'write' : s:funcref(a:f_write)
        \}
endfunction"}}}
function! s:fdopen_pgroup(proc, fd, f_close, f_read, f_write)"{{{
  return {
        \'proc' : a:proc, 'fd' : a:fd, 'eof' : 0, 'is_valid' : 1, 
        \'f_close' : s:funcref(a:f_close),
        \'close' : s:funcref('close'), 'read' : s:funcref(a:f_read), 'write' : s:funcref(a:f_write)
        \}
endfunction"}}}

function! s:garbage_collect()"{{{
  for l:proc in values(s:bg_processes)
    " Check processes.
    if !l:proc.stdout.eof
      call l:proc.stdout.read(-1, 0)
      continue
    endif
    
    try
      let [l:cond, s:last_status] = l:proc.waitpid()
      if l:cond != 'exit'
        " Kill process.
        " 15 == SIGTERM
        call l:proc.kill(15)
      endif
    catch
      " Ignore error.
    endtry

    call remove(s:bg_processes, l:proc.pid)
    if empty(s:bg_processes)
      unlet s:bg_processes

      augroup vimproc
        autocmd!
      augroup END
    endif
  endfor
endfunction"}}}

"-----------------------------------------------------------
" UTILS

function! s:str2hd(str)
  return join(map(range(len(a:str)), 'printf("%02X", char2nr(a:str[v:val]))'), '')
endfunction

function! s:hd2str(hd)
  " Since Vim can not handle \x00 byte, remove it.
  " do not use nr2char()
  " nr2char(255) => "\xc3\xbf" (utf8)
  return join(map(split(a:hd, '..\zs'), 'v:val == "00" ? "" : eval(''"\x'' . v:val . ''"'')'), '')
endfunction

function! s:str2list(str)
  return map(range(len(a:str)), 'char2nr(a:str[v:val])')
endfunction

function! s:list2str(lis)
  return s:hd2str(s:list2hd(a:lis))
endfunction

function! s:hd2list(hd)
  return map(split(a:hd, '..\zs'), 'str2nr(v:val, 16)')
endfunction

function! s:list2hd(lis)
  return join(map(a:lis, 'printf("%02X", v:val)'), '')
endfunction

function! s:convert_args(args)"{{{
  if empty(a:args)
    return []
  endif

  return s:analyze_shebang(vimproc#get_command_name(a:args[0])) + a:args[1:]
endfunction"}}}

function! s:analyze_shebang(filename)"{{{
  if !s:is_win && (has('macunix') || system('uname') =~? '^darwin')
    " Mac OS X's shebang support is imcomplete. :-(
    if getfsize(a:filename) > 100000

      " Maybe binary file.
      return [a:filename]
    endif
  elseif !s:is_win || '.'.fnamemodify(a:filename, ':e') !~? 
        \ '^' . substitute($PATHEXT, ';', '$\\|^', 'g') . '$'
    return [a:filename]
  endif
  
  let l:lines = readfile(a:filename, '', 1)
  if empty(l:lines) || l:lines[0] !~ '^#!.\+'
    " Not found shebang.
    return [a:filename]
  endif

  " Get shebang line.
  let l:shebang = split(matchstr(l:lines[0], '^#!\zs.\+'))

  " Convert command name.
  if s:is_win && l:shebang[0] =~ '^/'
    let l:shebang[0] = vimproc#get_command_name(fnamemodify(l:shebang[0], ':t'))
  endif

  return l:shebang + [a:filename]
endfunction"}}}

"-----------------------------------------------------------
" LOW LEVEL API

augroup vimproc
  autocmd!
  autocmd VimLeave * call s:finalize()
augroup END

" Initialize.
let s:lasterr = []
let s:read_timeout = 100
let s:write_timeout = 100

function! s:libcall(func, args)"{{{
  " End Of Value
  let l:EOV = "\xFF"
  let l:args = empty(a:args) ? '' : (join(reverse(copy(a:args)), l:EOV) . l:EOV)
  let l:stack_buf = libcall(g:vimproc_dll_path, a:func, l:args)
  let l:result = split(l:stack_buf, '[\xFF]', 1)
  if !empty(l:result) && l:result[-1] != ''
    let s:lasterr = l:result
    let l:msg = string(l:result)
    if has('iconv') && &termencoding != '' && &termencoding != &encoding
      " Kernel error message is encoded with system codepage.
      let l:msg = iconv(l:msg, &termencoding, &encoding)
    endif
    
    throw printf('proc: %s: %s', a:func, l:msg)
  endif
  return l:result[:-2]
endfunction"}}}

function! s:SID_PREFIX()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID_PREFIX$')
endfunction

function! s:print_error(string)
  echohl Error | echomsg a:string | echohl None
endfunction

" Get funcref.
function! s:funcref(funcname)
  return function(s:SID_PREFIX().a:funcname)
endfunction

function! s:finalize()
  call s:vp_dlclose(s:dll_handle)
endfunction

function! s:vp_dlopen(path)
  let [handle] = s:libcall('vp_dlopen', [a:path])
  return handle
endfunction

function! s:vp_dlclose(handle)
  call s:libcall('vp_dlclose', [a:handle])
endfunction

function! s:vp_file_open(path, flags, mode)
  let [l:fd] = s:libcall('vp_file_open', [a:path, a:flags, a:mode])
  return l:fd
endfunction

function! s:vp_file_close() dict
  if self.fd != 0
    call s:libcall('vp_file_close', [self.fd])
    let self.fd = 0
  endif
endfunction

function! s:vp_file_read(number, timeout) dict
  let [l:hd, l:eof] = s:libcall('vp_file_read', [self.fd, a:number, a:timeout])
  return [l:hd, l:eof]
endfunction

function! s:vp_file_write(hd, timeout) dict
  let [l:nleft] = s:libcall('vp_file_write', [self.fd, a:hd, a:timeout])
  return l:nleft
endfunction

function! s:vp_pipe_open(npipe, argv)"{{{
  if s:is_win
    let l:cmdline = ''
    for arg in a:argv
      let l:cmdline .= '"' . substitute(arg, '"', '\\"', 'g') . '" '
    endfor
    let [l:pid; l:fdlist] = s:libcall('vp_pipe_open', [a:npipe, l:cmdline])
  else
    let [l:pid; l:fdlist] = s:libcall('vp_pipe_open',
          \ [a:npipe, len(a:argv)] + a:argv)
  endif

  if a:npipe != len(l:fdlist)
    echoerr 'Bug behavior is detected!'
    echoerr printf('a:npipe = %d, a:argv = %s', a:npipe, string(a:argv))
    echoerr printf('l:fdlist = %s', string(l:fdlist))
  endif

  return [l:pid] + l:fdlist
endfunction"}}}

function! s:vp_pipe_close() dict
  if self.fd != 0
    call s:libcall('vp_pipe_close', [self.fd])
    let self.fd = 0
  endif
endfunction

function! s:vp_pipes_front_close() dict
  call self.fd[0].close()
endfunction

function! s:vp_pipes_back_close() dict
  call self.fd[-1].close()
endfunction

function! s:vp_pgroup_close() dict
  call self.fd.close()
endfunction

function! s:vp_pipe_read(number, timeout) dict
  let [l:hd, l:eof] = s:libcall('vp_pipe_read', [self.fd, a:number, a:timeout])
  return [l:hd, l:eof]
endfunction

function! s:vp_pipe_write(hd, timeout) dict
  let [l:nleft] = s:libcall('vp_pipe_write', [self.fd, a:hd, a:timeout])
  return l:nleft
endfunction

function! s:read_pipes(...) dict"{{{
  let l:number = get(a:000, 0, -1)
  let l:timeout = get(a:000, 1, s:read_timeout)
  
  let l:output = ''
  let l:eof = 0
  for l:fd in self.fd
    if !l:fd.eof
      let l:read = l:fd.read(l:number, l:timeout)
      while l:read != ''
        if empty(l:fd.redirect_fd)
          " Append output.
          let l:output .= l:read
        else
          " Write pipe.
          for l:redirect_fd in l:fd.redirect_fd
            call l:redirect_fd.write(l:read)
          endfor
        endif

        let l:read = l:fd.read(l:number, l:timeout)
      endwhile
    else
      " Close pipe.
      for l:redirect_fd in l:fd.redirect_fd
        if l:redirect_fd.fd >= 0
          call l:redirect_fd.close()
        endif
      endfor
    endif
  endfor
  
  let self.eof = self.fd[-1].eof

  return l:output
endfunction"}}}

function! s:write_pipes(str, ...) dict"{{{
  let l:timeout = get(a:000, 0, s:write_timeout)
  
  " Write data.
  let l:nleft = self.fd[0].write(a:str, l:timeout)

  for l:fd in self.fd[: -2]
    if !l:fd.eof
      let l:read = l:fd.read([-1, l:timeout])
      while l:read != ''
        if empty(l:fd.redirect_fd)
          " Append output.
          let l:output .= l:read
        else
          " Write pipe.
          for l:redirect_fd in l:fd.redirect_fd
            call l:redirect_fd.write(l:read)
          endfor
        endif

        let l:read = l:fd.read(-1, l:timeout)
      endwhile
    else
      " Close pipe.
      for l:redirect_fd in l:fd.redirect_fd
        if l:redirect_fd.fd >= 0
          call l:redirect_fd.close()
        endif
      endfor
    endif
  endfor

  return l:nleft
endfunction"}}}

function! s:read_pgroup(...) dict"{{{
  let l:number = get(a:000, 0, -1)
  let l:timeout = get(a:000, 1, s:read_timeout)
  
  let l:output = ''
  let l:eof = 0
  
  if !self.fd.eof
    let l:output = self.fd.read(l:number, l:timeout)
  endif

  if self.proc.current_proc.stdout.eof && self.proc.current_proc.stderr.eof
    " Get status.
    let [l:cond, l:status] = self.proc.current_proc.waitpid()

    if empty(self.proc.statements)
          \ || (self.proc.condition ==# 'true' && l:status)
          \ || (self.proc.condition ==# 'false' && !l:status)
      let self.proc.statements = []
      
      " Exit.
      let self.proc.stdout.eof = 1
      let self.proc.stderr.eof = 1

      " Caching status.
      let self.proc.cond = l:cond
      let self.proc.status = l:status
    else
      " Initialize next statement.
      let l:proc = vimproc#plineopen3(self.proc.statements[0].statement)
      let self.proc.current_proc = l:proc
      let self.proc.condition = self.proc.statements[0].condition
      let self.proc.statements = self.proc.statements[1:]

      let self.proc.stdin = s:fdopen_pgroup(self.proc, l:proc.stdin, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
      let self.proc.stdout = s:fdopen_pgroup(self.proc, l:proc.stdout, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
      let self.proc.stderr = s:fdopen_pgroup(self.proc, l:proc.stderr, 'vp_pgroup_close', 'read_pgroup', 'write_pgroup')
    endif
  endif

  return l:output
endfunction"}}}

function! s:write_pgroup(str, ...) dict"{{{
  let l:timeout = get(a:000, 0, s:write_timeout)
  
  let l:nleft = 0
  if !self.fd.eof
    " Write data.
    let l:nleft = self.fd.write(a:str, l:timeout)
  endif

  return l:nleft
endfunction"}}}

if s:is_win
  " For Windows.
  function! s:vp_pty_open(width, height, argv)
    let l:cmdline = ''
    for arg in a:argv
      let l:cmdline .= '"' . substitute(arg, '"', '\\"', 'g') . '" '
    endfor
    let [l:pid, l:fd_stdin, l:fd_stdout, l:ttyname] = s:libcall('vp_pty_open',
          \ [a:width, a:height, l:cmdline])
    return [l:pid, l:fd_stdin, l:fd_stdout, l:ttyname]
  endfunction

  function! s:vp_pty_close() dict
    if self.fd_stdin != 0
      call s:libcall('vp_pipe_close', [self.fd_stdin])
      call s:libcall('vp_pipe_close', [self.fd_stdout])
      let self.fd_stdin = 0
      let self.fd_stdout = 0
    endif
  endfunction

  function! s:vp_pty_read(number, timeout) dict
    let [l:hd, l:eof] = s:libcall('vp_pipe_read', [self.fd_stdout, a:number, a:timeout])
    return [l:hd, l:eof]
  endfunction

  function! s:vp_pty_write(hd, timeout) dict
    let [l:nleft] = s:libcall('vp_pipe_write', [self.fd_stdin, a:hd, a:timeout])
    return l:nleft
  endfunction

  function! s:vp_pty_get_winsize() dict
    " Not implemented.
    "let [width, height] = s:libcall('vp_pty_get_winsize', [self.fd_stdout])
    let [width, height] = [winwidth(0)-5, winheight(0)]
    return [width, height]
  endfunction

  function! s:vp_pty_set_winsize(width, height) dict
    " Not implemented.
    "call s:libcall('vp_pty_set_winsize', [self.fd_stdout, a:width, a:height])
  endfunction
else
  function! s:vp_pty_open(width, height, argv)
    let [l:pid, l:fd, l:ttyname] = s:libcall('vp_pty_open',
          \ [a:width, a:height, len(a:argv)] + a:argv)
    return [l:pid, l:fd, l:ttyname]
  endfunction

  function! s:vp_pty_close() dict
    call s:libcall('vp_pty_close', [self.fd])
  endfunction

  function! s:vp_pty_read(number, timeout) dict
    let [l:hd, l:eof] = s:libcall('vp_pty_read', [self.fd, a:number, a:timeout])
    return [l:hd, l:eof]
  endfunction

  function! s:vp_pty_write(hd, timeout) dict
    let [l:nleft] = s:libcall('vp_pty_write', [self.fd, a:hd, a:timeout])
    return l:nleft
  endfunction

  function! s:vp_pty_get_winsize() dict
    let [width, height] = s:libcall('vp_pty_get_winsize', [self.fd])
    return [width, height]
  endfunction

  function! s:vp_pty_set_winsize(width, height) dict
    call s:libcall('vp_pty_set_winsize', [self.fd, a:width-5, a:height])
    
    " Send SIGWINCH = 28 signal.
    call vimproc#kill(self.pid, 28)
  endfunction
endif

function! s:vp_kill(sig) dict
  if has_key(self, 'stdin')
    call self.stdin.close()
  endif
  if has_key(self, 'stdout')
    call self.stdout.close()
  endif
  if has_key(self, 'stderr')
    call self.stdout.close()
  endif
  if has_key(self, 'ttyname')
    call self.close()
  endif
  
  call s:libcall('vp_kill', [self.pid, a:sig])
  let self.is_valid = 0
endfunction

function! s:vp_pipes_kill(sig) dict
  if has_key(self, 'stdin')
    call self.stdin.close()
  endif
  if has_key(self, 'stdout')
    call self.stdout.close()
  endif
  if has_key(self, 'stderr')
    call self.stdout.close()
  endif
  if has_key(self, 'ttyname')
    call self.close()
  endif
  
  for l:pid in self.pid_list
    call s:libcall('vp_kill', [l:pid, a:sig])
  endfor
  let self.is_valid = 0
endfunction

function! s:vp_pgroup_kill(sig) dict
  if has_key(self, 'stdin')
    call self.stdin.close()
  endif
  if has_key(self, 'stdout')
    call self.stdout.close()
  endif
  if has_key(self, 'stderr')
    call self.stdout.close()
  endif
  if has_key(self, 'ttyname')
    call self.close()
  endif
  
  call self.proc.current_proc.kill(a:sig)
  
  let self.is_valid = 0
endfunction

function! s:vp_waitpid() dict
  if has_key(self, 'stdin')
    call self.stdin.close()
  endif
  if has_key(self, 'stdout')
    call self.stdout.close()
  endif
  if has_key(self, 'stderr')
    call self.stdout.close()
  endif
  if has_key(self, 'ttyname')
    call self.close()
  endif
  
  let [l:cond, l:status] = s:libcall('vp_waitpid', [self.pid])
  let self.is_valid = 0
  return [l:cond, str2nr(l:status)]
endfunction

function! s:vp_pgroup_waitpid() dict
  let [l:cond, l:status] = 
        \ has_key(self, 'cond') && has_key(self, 'status') ?
        \ [self.cond, self.status] : self.current_proc.waitpid()
  
  let self.is_valid = 0
  return [l:cond, str2nr(l:status)]
endfunction

function! s:vp_socket_open(host, port)
  let [socket] = s:libcall('vp_socket_open', [a:host, a:port])
  return socket
endfunction

function! s:vp_socket_close() dict
  call s:libcall('vp_socket_close', [self.fd])
  let self.is_valid = 0
endfunction

function! s:vp_socket_read(number, timeout) dict
  let [l:hd, l:eof] = s:libcall('vp_socket_read', [self.fd, a:number, a:timeout])
  return [l:hd, l:eof]
endfunction

function! s:vp_socket_write(hd, timeout) dict
  let [l:nleft] = s:libcall('vp_socket_write', [self.fd, a:hd, a:timeout])
  return l:nleft
endfunction

" Initialize.
if !exists('s:dlhandle')
  let s:dll_handle = s:vp_dlopen(g:vimproc_dll_path)
endif

" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}
" vim:foldmethod=marker:fen:sw=2:sts=2
