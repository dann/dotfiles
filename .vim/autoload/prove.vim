" prove.vim - prove for vim plugin.
"
" Author:  Kazuhito Hokamura <http://webtech-walker.com/>
" Version: 0.0.1
" License: MIT License <http://www.opensource.org/licenses/mit-license.php>


function! prove#run_cmd(arg)
  if !executable('prove')
    call s:error('required prove command')
    return
  endif

  let test_path = s:get_test_path(a:arg)
  if test_path ==# ''
    call s:error('no such file or directory')
    return
  endif

  let root_dir  = s:get_root_dir(test_path)
  let lib_dirs  = s:get_lib_dirs(root_dir)
  let command   = s:get_command(test_path, root_dir, lib_dirs)
  call s:open_window('[prove] prove', 'prove', command, 'rightbelow')
endfunction 

function! s:gsub(str, pat, rep)
  return substitute(a:str, '\v'.a:pat, a:rep, 'g')
endfunction

function! s:error(str)
  echohl ErrorMsg
  echomsg a:str
  echohl None
endfunction

function! s:get_test_path(arg)
  if a:arg == ''
    let test_path = expand('%:p')
    if !filereadable(test_path)
      let test_path = tempname() . expand('%:e')
      let original_bufname = bufname('')
      let original_modified = &l:modified
        silent keepalt write `=test_path`
        if original_bufname == ''
          silent 0 file
        endif
      let &l:modified = original_modified
    endif
  else
    let test_path = fnamemodify(a:arg, ':p')
    if !filereadable(test_path) && !isdirectory(test_path)
      return ''
    endif
  endif
  return test_path
endfunction

function! s:get_root_dir(path)
  let t_dir = finddir('t', a:path.';')
  let root_dir = ''
  if t_dir != ''
    let root_dir = substitute(fnamemodify(t_dir, ':p'), '\/t\/$', '', '')
  endif
  return root_dir
endfunction

function! s:get_lib_dirs(root_dir)
  let lib_dirs = []

  if exists('b:prove_lib_dirs')
    lib_dirs += b:prove_lib_dirs
  endif

  if exists('g:prove_lib_dirs')
    lib_dirs += g:prove_lib_dirs
  endif

  let locallib_dir = s:gsub(g:prove_local_lib_dir,
  \ '\%prove_root\%', a:root_dir)

  if !isdirectory(locallib_dir)
    return lib_dirs
  endif

  let perl5lib = system('perl -e ''
  \   eval { require local::lib };
  \   exit if $@;
  \   %env=local::lib->build_environment_vars_for("'.locallib_dir.'", 1);
  \   print $env{PERL5LIB}
  \''')

  let lib_dirs += split(perl5lib, ':')
  
  return lib_dirs
endfunction

function! s:get_command(test_path, root_dir, lib_dirs)
  let lib_dirs = ''
  for lib_dir in a:lib_dirs
    let lib_dirs .= ' -I' . lib_dir
  endfor

  let command = ''
  if a:root_dir != ''
    let command = printf('cd %s;', a:root_dir)
  endif
  let command .= printf('prove -lvr %s %s', lib_dirs, a:test_path)

  return command
endfunction

function! s:open_window(bufname, filetype, command, win_pos)
  if !bufexists(a:bufname)
    execute a:win_pos . ' new'
    setlocal bufhidden=unload
    setlocal nobuflisted
    setlocal buftype=nofile
    setlocal noswapfile
    execute 'setlocal filetype=' . a:filetype
    silent file `=a:bufname`
    nnoremap <buffer> <silent> q <C-w>c
  else
    let bufnr = bufnr(a:bufname)
    let winnr = bufwinnr(bufnr)
    if winnr == -1
      execute a:win_pos . ' split'
      execute bufnr 'buffer'
      execute 'setlocal filetype=' . a:filetype
    else
      execute winnr 'wincmd w'
    endif
  endif

  silent % delete _
  call append(0, 'now loading...')
  redraw
  silent % delete _
  execute 'silent! read !' a:command
  1
endfunction

" __END__
" vim:tw=78:sts=2:sw=2:ts=2:fdm=marker:
