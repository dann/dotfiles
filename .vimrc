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
" Japanese setting
" ============================================
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8

" ============================================
" Default setting
" ============================================
set ambiwidth=double
set autoindent
set backspace=indent,eol,start

" backup
set backupdir=~/.vim/backup
let &directory = &backupdir

" highlight
"set hlsearch
"nmap <silent> gh :let @/=''<CR>
set nohlsearch

" highlight Zenkaku word
highlight ZenkakuSpace ctermbg=6
"match ZenkakuSpace /\s\+$\|　/
set listchars=tab:>.
set list

set ignorecase
set laststatus=2
set ruler

" tab setting
set expandtab
set smarttab
set softtabstop=4
set shiftwidth=4
set tabstop=4

" search setting
set incsearch
set wrapscan

set nobackup
set smartcase
set nofoldenable
set showcmd
set number

" syntax
syntax on

" filetype setting
filetype on
filetype indent on
filetype plugin on

" auto change directory
if has("autochdir")
    set autochdir 
endif

" ============================================
" mapping
" ============================================
nnoremap j gj
nnoremap k gk

" nerdtree toggle
map <leader>d :execute 'NERDTreeToggle ' . getcwd()<CR>

" disable highlight with <esc><esc>
nnoremap <Esc><Esc> :<C-u>set nohlsearch<Return>

" ============================================
" open perl module with gf
" ============================================
autocmd FileType perl set isfname-=-

" ============================================
" statusline
" ============================================
set statusline=%y%{GetFileEncoding()}%F%m%r%=%c:%l%5(\ %)%3p%%

function! GetFileEncoding()
    let str = &fileformat . ']'
    if has('multi_byte') && &fileencoding != ''
        let str = &fileencoding . ':' . str
    endif
    let str = '[' . str
    return str
endfunction


" ============================================
" surround.vim
" ============================================
" gettextnize texts
let g:surround_103 = "loc('\r')"  " 103 = g
let g:surround_71 = "loc(\"\r\")" " 71 = G
nmap g' cs'g
nmap g" cs"G


" ============================================
" fuzzyfinder
" ============================================
nnoremap <silent> ,f :FuzzyFinderFile<CR>
nnoremap <silent> ,b :FuzzyFinderBuffer<CR>
nnoremap <silent> ,d :FuzzyFinderDir<CR>
nnoremap <silent> ,mf :FuzzyFinderMruFile<CR>
nnoremap <silent> ,mc :FuzzyFinderMruCmd<CR>

" tag with fuzzyfinder
nnoremap <silent> <C-]> :FuzzyFinderTag <C-r>=expand('<cword>')<CR><CR>

" ============================================
" Ctags
" ============================================
noremap  t  <Nop>
noremap  T  <Nop>

noremap <c-[>  :pop<CR>

" search tags under current dir
set tags+=./tags;/

" use :tjump
nnoremap <C-]> g<C-]>
nnoremap g<C-]> <C-]>

" (Ctrl + [) for tag jump back
nmap <c-[>  :pop<CR>

"QuickFix list
nnoremap <C-q><C-n> :cnext<CR>
nnoremap <C-q>n :cnext<CR>
nnoremap <C-q><C-p> :cprevious<CR>
nnoremap <C-q>p :cprevious<CR>
" tag stack
" nnoremap <C-t><C-t> <C-t>
" nnoremap <C-t>t <C-t>
" nnoremap <C-t><C-n> :tnext<CR>
" nnoremap <C-t>n :tnext<CR>
" nnoremap <C-t><C-p> :tprevious<CR>
" nnoremap <C-t>p :tprevious<CR>

" ============================================
" completion color
" ============================================
set t_Co=16
set t_Sf=[3%dm
set t_Sb=[4%dm

hi Pmenu ctermbg=8
hi PmenuSel ctermbg=12
hi PmenuSbar ctermbg=0

" ============================================
" neocomplcache
" ============================================

set completeopt=menuone,preview
set complete=.,w,b,u,t,k
let g:NeoComplCache_EnableAtStartup = 1
let g:NeoComplCache_KeywordCompletionStartLength = 1
let g:NeoComplCache_PluginCompletionLength = {
  \ 'snipMate_complete' : 1,
  \ 'buffer_complete' : 1,
  \ 'include_complete' : 2,
  \ 'syntax_complete' : 2,
  \ 'filename_complete' : 2,
  \ 'keyword_complete' : 2,
  \ 'omni_complete' : 1
  \ }
let g:NeoComplCache_MinKeywordLength = 2
let g:NeoComplCache_MinSyntaxLength = 2
let g:NeoComplCache_SmartCase = 1
let g:NeoComplCache_PartialCompletionStartLength = 2
let g:NeoComplCache_PreviousKeywordCompletion = 1
let g:NeoComplCache_EnableCamelCaseCompletion = 1
let g:NeoComplCache_EnableUnderbarCompletion = 1
let g:NeoComplCache_EnableSkipCompletion = 0
let g:NeoComplCache_SnippetsDir = $HOME . '/.vim/snippets'
let g:NeoComplCache_DictionaryFileTypeLists = {
  \ 'default' : '',
  \ 'objc' : $HOME . '/.vim/dict/objectivec.dict',
  \ 'javascript' : $HOME . '/.vim/dict/javascript.dict',
  \ 'ruby' : $HOME . '/.vim/dict/ruby.dict',
  \ 'perl' : $HOME . '/.vim/dict/perl.dict',
  \ }
let g:NeoComplCache_SameFileTypeLists = {
  \ 'perl' : 'man',
  \ 'erlang' : 'man',
  \ }

" neocon keybindings
"------------------
" <TAB> completion.
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"

" snippets expand key
imap <silent> <C-e> <Plug>(neocomplcache_snippets_expand)
smap <silent> <C-e> <Plug>(neocomplcache_snippets_expand)


" ============================================
" Escape sequence mapping 
" ============================================
imap jj <Esc>

" ==
set wildmode=list:longest

" ============================================
" smartword.vim 
" ============================================
map w  <Plug>(smartword-w)
map b  <Plug>(smartword-b)
map e  <Plug>(smartword-e)
map ge <Plug>(smartword-ge)

" ============================================
" tt2
" =============================================
au BufNewFile,BufRead *.tt2 setf tt2

" ============================================
" Grep.vim
" ============================================
let Grep_Skip_Dirs = '.svn'
let Grep_Skip_Files = '*.bak *~'

" ============================================
" Perl
" ============================================
" Perldoc command
command! -nargs=+ -complete=file Perldoc tabnew | if bufexists('perldoc <args>') | execute 'buffer' bufnr('perldoc <args>') | else | silent execute '0read!perldoc -T' <q-args> | if v:shell_error | echoerr getline(1) | quit! | else | silent! %s/.^H//g | file `="perldoc <args>"` | set buftype=nofile | if <q-args> =~ '-m' | setf perl | else | setf man | endif | 0 | endif | endif

",e でそのコマンドを実行
function! ShebangExecute()
  let m = matchlist(getline(1), '#!\(.*\)')
  if(len(m) > 2)
    execute '!'. m[1] . ' %'
  else
    execute '!' &ft ' %'
  endif
endfunction

if has('win32')
  nmap ,e :execute '!' &ft ' %'<CR>
else
  noremap ,e :call ShebangExecute()<CR>
end

" ============================================
" manpage view
"=============================================
let g:manpageview_winopen="vsplit"

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

" ============================================
" Utility
" ============================================
function! GitGrep(arg)
  let gtmp = &grepprg
  let &grepprg = 'git-grep -n'
  silent execute ':grep '.a:arg
  let &grepprg = gtmp
  silent cwin
endfunction
command! -nargs=1 -complete=tag GitGrep call GitGrep(<q-args>)

au BufNewFile,BufRead *.txt set iminsert=2
inoremap <ESC> <ESC>:set iminsert=0<CR>

" Rename
command! -nargs=1 -complete=file Rename f <args>|call delete(expand('#'))

if !exists('s:loaded_my_vimrc')
  let s:loaded_my_vimrc = 1
endif
 
set secure " must be written at the last. see :help 'secure'.
