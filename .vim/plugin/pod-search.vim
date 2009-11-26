" 
" Author: Cornelius ( cornelius.howl@delete-me.gmail.com )
"         Takatoshi Kitano
"
" Last Change:  2009/10/31
" Licence: MIT Licence
"
"=VERSION 0.1
"
"=DESCRIPTION 
"
" This is Perldoc search integration of vim plugin.
" Type <C-c><C-p> to search keyword in pods
"
" Recommended script: Quickfix Toggle
"
"=cut

if exists("g:loaded_pod_search") || v:version < 700
    "finish
endif

cal perldoc#load()

" configuration

" g:podsearch_window : to use quickfix window or use search-window.vim
let g:podsearch_search_window = 1
let g:loaded_pod_search = 1

" make sure we have podsearch script
if ! executable('podsearch')
    " echomsg 'podsearch is not found in PATH. plugin is not loaded.'
    " Skip loading the plugin
    " finish
endif


fun! s:echo(msg)
  redraw
  echomsg a:msg
endf

" pod search window &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

if swindow#class.version < 0.4
  echo "Please upgrade search-window.vim to version 0.4"
  echo "Current version: v" . string(swindow#class.version)
  echo "http://github.com/c9s/search-window.vim"
  finish
endif

let s:podsrh = copy( swindow#class  )
let s:podsrh.predefined_index = [ ]

fun! s:podsrh.index()
  return self.predefined_index
endf

fun! s:podsrh.init_buffer()
  setfiletype cpanwindow
  autocmd CursorMovedI <buffer>       call s:podsrh.update()
  autocmd BufWinLeave  <buffer>       call s:podsrh.close()
  cal self.buffer_name()
endf

fun! s:podsrh.buffer_reload_init()
  cal self.buffer_name()
  startinsert
  call cursor( 1 , col('$')  )
endf

fun! s:podsrh.init_mapping()
  nnoremap <silent> <buffer> $       :PodSrhPerldocOpen<CR>
  nnoremap <silent> <buffer> <Enter> :call libperl#open_module()<CR>
  nnoremap <silent> <buffer> t       :call libperl#tab_open_module_file_in_paths( getline('.') )<CR>
endf

fun! s:podsrh.buffer_name()
  exec 'silent file ' . s:last_pattern
endf

" only render the first column
fun! s:podsrh.filter_render(lines)
  cal map( a:lines , 'v:val[0]' )
endf

fun! s:podsrh.filter_result(ptn,list)
  " searching for name
  " XXX: provide mode for switching this
  return filter( copy( a:list ) , 'v:val[0] =~ "' . a:ptn . '"' )
endf

" pod search window &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

fun! s:open_perldoc()
  let line = getline('.')
  let mod = matchstr( line , '^\S\+' )
  cal  g:perldoc.open(mod,'')
endf
com! PodSrhPerldocOpen  :call s:open_perldoc()

fun! s:search_prompt()
  let pattern = input("Pod Search Pattern:")
  let path = input("Path:","","file")
  return { 'pattern' : pattern , 'path' : path }
endf

fun! s:pod_search(...)
  if exists('a:1')
    let ret = { 'pattern': a:1 , 'path': a:2  }
  else
    let ret = s:search_prompt()
    if strlen(ret.pattern) == 0 | redraw | return | endif
    if strlen(ret.path) == 0 | redraw | return | endif
  endif

  cal s:echo("Searching for '" . ret.pattern . "' in ". ret.path ."..." )
  let s:last_pattern = ret.pattern

  let result = s:search_from_pod([ ret.pattern , ret.path ])
  cal map( result , 'split(v:val," | ")')
  let s:podsrh.predefined_index = result
  cal s:podsrh.open('topleft', 'split',10)
endf

fun! s:search_from_pod(args)
    let command = extend(["perl","bin/podsearch"],a:args)
    return split(system( join(command," ") ),"\n")
endf

fun! s:emerge(result)
  if g:podsearch_search_window == 1
    " open search window
    cal s:podsrh_window.open('topleft','split',10)
  else
    let s:qflist = []
    for item in split(result,"\n") 
      let [path,name] = split(item," | ")
      call add(s:qflist, { 'filename':path, 'text':name , 'pattern': ret.pattern })
    endfor
    call setqflist( s:qflist )
    copen
  endif
endf

"command! -nargs=* -complete=file PerldocSearch :call s:PerldocSearch(<f-args>)
" call s:pod_search()
com! PodSearch            :cal s:pod_search()
com! OpenPodSearchWindow  :cal s:podsrh.open('topleft', 'split',10)

" nmap <C-c><C-p> :PodSearch<CR>

" test code
" cal s:pod_search( 'DBI', '/Users/c9s/svn_working/jifty-dbi/lib/Jifty/DBI' )
