" code-snippet.vim: Simple input assistance of code snippets or idioms
"
" Maintainer:  MIYAKAWA Taku <ripjohn@s28.xrea.com>
" Last Change: 2007-12-04.
" Require:     Vim 7.0 or later

" Initialization: {{{

if exists('g:CodeSnippet_disabled') && g:CodeSnippet_disabled
  finish
endif

if !exists('g:CodeSnippet_no_default_mappings') || !g:CodeSnippet_no_default_mappings
  imap <silent> <S-CR> <Plug>CodeSnippet_forward
  map <silent> <S-CR> <Plug>CodeSnippet_forward
  imap <silent> <C-CR> <Plug>CodeSnippet_backward
  map <silent> <C-CR> <Plug>CodeSnippet_backward
  vmap <silent> <CR> <Plug>CodeSnippet_fillin
endif

" }}}
" Public: :StringAbbrLocal & :StringAbbrGlobal {{{
" :StringAbbrLocal {lhs} {rhs-string}
"   - Escape {rhs-string} and execute "iabbrev <buffer> {lhs} {rhs}"
" :StringAbbrGlobal {lhs} {rhs-string}
"   - Escape {rhs-string} and execute "iabbrev {lhs} {rhs}"

command! -nargs=1 StringAbbrLocal call s:StringAbbr(<f-args>, 1)
command! -nargs=1 StringAbbrGlobal call s:StringAbbr(<f-args>, 0)

function! s:StringAbbr(arg, local)
  let list = matchlist(a:arg, '\v^(\S+)\s+(.+)')
  if list == []
    throw 'StringAbbr: invalid parameter'
  endif
  let lhs = list[1]
  let rhs = s:EscapeSnippet(eval(list[2]))
  execute 'iabbrev ' . (a:local ? '<buffer> ' : '') . lhs . ' ' . rhs
endfunction

function! s:EscapeSnippet(text)
  let esc = a:text
  let esc = substitute(esc, '<', '<lt>', 'g')
  let esc = substitute(esc, '\t', '<Tab>', 'g')
  let esc = substitute(esc, ' ', '<Space>', 'g')
  let esc = substitute(esc, '\\', '<Bslash>', 'g')
  let esc = substitute(esc, '|', '<Bar>', 'g')
  let esc = substitute(esc, '\r\n\|\n\|\r', '<CR>', 'g')
  return esc
endfunction

" }}}
" Public: <S-CR>: <Plug>CodeSnippet_forward & <C-CR>: <Plug>CodeSnippet_backward {{{
" [nvi] <Plug>CodeSnippet_forward/backward
"   - [i]: Try to expand a snippet in the same way as pressing <Esc>.
"   - [i] forward: Jump to the first placeholder from the start
"     position of the insert.
"   - other: Jump to the next/previous placeholder.

lmap <silent> <Plug>CodeSnippet_forward <Nop>
lmap <silent> <Plug>CodeSnippet_backward <Nop>
map <silent> <Plug>CodeSnippet_forward <SID>jumpfor<SID>cutempty
imap <silent> <Plug>CodeSnippet_forward <SID>jumpfor<SID>cutempty
map <silent> <Plug>CodeSnippet_backward <SID>jumpback<SID>cutempty
imap <silent> <Plug>CodeSnippet_backward <SID>jumpback<SID>cutempty

nmap <silent> <SID>jumpfor :call <SID>Jump('n', 0)<CR>
vmap <silent> <SID>jumpfor <C-\><C-N>g`>:call <SID>Jump('v', 0)<CR>
imap <silent> <SID>jumpfor <Esc>:call <SID>Jump('i', 0)<CR>
nmap <silent> <SID>jumpback :call <SID>Jump('n', 1)<CR>
vmap <silent> <SID>jumpback <C-\><C-N>g`<lt>:call <SID>Jump('v', 1)<CR>
imap <silent> <SID>jumpback <Esc>:call <SID>Jump('i', 1)<CR>

map <silent> <SID>cutempty <Nop>
lmap <silent> <SID>cutempty <Nop>
vmap <silent> <SID>cutempty <C-\><C-N>g`<lt>i<C-R>=<SID>CutEmpty()<CR>

" s:Jump(mode, back)
"   Jump to the next/previous placeholder and select it.
"   This function must be called in the normal mode.
function! s:Jump(mode, back)
  let [opener, closer, defind] = s:GetFormat()
  let saved_pos = getpos('.')
  if a:mode == 'i' && !a:back && exists('b:CodeSnippet_insert_pos')
    " Search in the region of the last inserting.
    call setpos('.', b:CodeSnippet_insert_pos)
  endif
  " Do search.
  let found = search('\v'.opener.'.{-}'.closer, (a:back ? 'bsw' : 'csw'))
  if found == 0
    " No placeholders.
    call setpos('.', saved_pos)
    return
  endif
  " Select it.
  let saved_me = getpos("'e")
  let [endline, endcol] = searchpos('\v('.opener.'.{-}'.closer.')@<=', 'n')
  call setpos("'e", [0, endline, endcol, 0])
  silent! foldopen!
  normal vg`eh
  call setpos("'e", saved_me)
endfunction

" s:CutEmpty()
"   If the latest selected region is an empty placeholder, cut it.
"   This function must be called in the insert mode.
function! s:CutEmpty()
  let [opener, closer, defind] = s:GetFormat()
  let region = s:LastSel()
  if match(region, '\v^'.opener.'\s*'.closer.'$') >= 0
    return "\<Esc>g`<vg`>\"_c"
  else
    return "\<Esc>g`<vg`>" . (s:UseSelectMode() ? "\<C-G>" : '')
  endif
endfunction

" }}}
" Public: <CR>: <Plug>CodeSnippet_fillin {{{
" [v] <Plug>CodeSnippet_fillin
"   - Cut the selected placeholder and switch to the insert mode.
"   - If the placeholder contains "defind", the following string is
"     inserted as the default.

map <silent> <Plug>CodeSnippet_fillin <Nop>
lmap <silent> <Plug>CodeSnippet_fillin <Nop>
vmap <silent> <Plug>CodeSnippet_fillin <C-\><C-N>g`<lt>i<C-R>=<SID>Fillin()<CR>

function! s:Fillin()
  if visualmode() != 'v'
    return "\<Esc>g`<"
  endif
  let [opener, closer, defind] = s:GetFormat()
  let region = s:LastSel()
  let list = matchlist(region, '\v^'.opener.'.{-}('.defind.'(.*)'.')?'.closer.'$')
  if len(list) != 0
    silent! foldopen!
    return "\<Esc>g`<vg`>\"_c" . list[2]
  else
    return "\<Esc>g`<"
  endif
endfunction

" }}}
" Internal: Autocommand to record where the insert started {{{

autocmd InsertEnter,InsertChange * call s:SavePos()
function! s:SavePos()
  if v:insertmode == 'i'
    let b:CodeSnippet_insert_pos = getpos('.')
  endif
endfunction

" }}}
" Internal: s:GetFormat() {{{
" s:GetFormat()
"   - Return the format of placeholders.

function! s:GetFormat()
  let opener = exists('b:CodeSnippet_opener') ? b:CodeSnippet_opener :
        \ exists('g:CodeSnippet_opener') ? g:CodeSnippet_opener : '[~'
  let closer = exists('b:CodeSnippet_closer') ? b:CodeSnippet_closer :
        \ exists('g:CodeSnippet_closer') ? g:CodeSnippet_closer : '~]'
  let defind = exists('b:CodeSnippet_defind') ? b:CodeSnippet_defind :
        \ exists('g:CodeSnippet_defind') ? g:CodeSnippet_defind : ':'
  return [s:EscapeFormat(opener), s:EscapeFormat(closer), s:EscapeFormat(defind)]
endfunction

function! s:EscapeFormat(text)
  if a:text == ''
    throw "code-snippet.vim: one of the format variables is empty."
          \ . "  see :help code-snippet-format."
  elseif match(a:text, '\v[ \t\r\n]') >= 0
    throw "code-snippet.vim: any format variables must not contain space characters."
          \ . "  see :help code-snippet-format."
  else
    return substitute(a:text, '\v[^a-zA-Z0-9_]', '\\\0', 'g')
  endif
endfunction

" }}}
" Internal: s:UseSelectMode() {{{
" s:UseSelectMode()
"   - Return whether to use the select mode instead of the visual mode.

function! s:UseSelectMode()
  return exists('b:CodeSnippet_use_selectmode') ? b:CodeSnippet_use_selectmode :
        \ exists('g:CodeSnippet_use_selectmode') ? g:CodeSnippet_use_selectmode : 0
endfunction

" }}}
" Internal: s:LastSel() {{{
" s:LastSel()
"   - Return the region lastly selected.

function! s:LastSel()
  let saved_reg_noname = @"
  let saved_reg_0 = @0
  normal g`<vg`>""y
  let region = @"
  let @" = saved_reg_noname
  let @0 = saved_reg_0
  return region
endfunction
" }}}
" vim:set sw=2 ts=2 et tw=0 fdm=marker:
