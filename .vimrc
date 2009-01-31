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

" highlight
"set hlsearch
"nmap <silent> gh :let @/=''<CR>
set nohlsearch

" highlight Zenkaku word
highlight ZenkakuSpace ctermbg=6
match ZenkakuSpace /\s\+$\|　/
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
nnoremap <silent> efv :FuzzyFinderFavFile<CR>
nnoremap <silent> efm :FuzzyFinderMruFile<CR>
nnoremap <silent> efc :FuzzyFinderMruCmd<CR>
nnoremap <silent> efd :FuzzyFinderDir<CR>
nnoremap <silent> efa :FuzzyFinderAddFavFile<CR>

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
" Escape sequence mapping 
" ============================================
imap jj <Esc>

" ==
set wildmode=list:longest

" ============================================
" autocomplpop.vim 
" ============================================
" complete option
"set complete=.,w,b,u,t,i,k
set complete=.,w,b,u,t,k
"let g:AutoComplPop_CompleteOption = '.,w,b,k'
let g:AutoComplPop_CompleteOption = '.,w,b,u,t,k'
let g:AutoComplPop_IgnoreCaseOption = 1


" ============================================
" smartword.vim 
" ============================================
"map w  <Plug>(smartword-w)
"map b  <Plug>(smartword-b)
"map e  <Plug>(smartword-e)
"map ge <Plug>(smartword-ge)
"noremap W  w
"noremap B  b
"noremap E  e
"noremap gE ge

" ============================================
" tt2
" =============================================
au BufNewFile,BufRead *.tt2 setf tt2

" ============================================
" omni completion
" ============================================
autocmd FileType html :set filetype=xhtml
autocmd FileType html :set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css :set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml :set omnifunc=xmlcomplete#CompleteTags

" ============================================
" Grep.vim
" ============================================
let Grep_Skip_Dirs = '.svn'
let Grep_Skip_Files = '*.bak *~'

" ============================================
" git 
" ============================================
let g:git_diff_spawn_mode=1 

" ============================================
" Tab completion
" ============================================
" {{{ Autocompletion using the TAB key
" This function determines, wether we are on the start of the line text (then tab indents) or
" if we want to try autocompletion
function! InsertTabWrapper()
        let col = col('.') - 1
        if !col || getline('.')[col - 1] !~ '\k'
                return "\<TAB>"
        else
                if pumvisible()
                        return "\<C-N>"
                else
                        return "\<C-N>\<C-P>"
                end
        endif
endfunction
" Remap the tab key to select action with InsertTabWrapper
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
" }}} Autocompletion using the TAB key

" ===========================================
" Show current git branch on vim's status line
" ===========================================
let g:gitCurrentBranch = ''
function! CurrentGitBranch()
    let cwd = getcwd()
    cd %:p:h
    let branch = matchlist(system('/usr/bin/git  branch -a --no-color'), '\v\* (\w*)\r?\n')
    execute 'cd ' . cwd
    if (len(branch))
      let g:gitCurrentBranch = '][git:' . branch[1] . ''
    else
      let g:gitCurrentBranch = ''
    endif
    return g:gitCurrentBranch
endfunction

autocmd BufEnter * :call CurrentGitBranch()

set statusline=%<[%n]%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff}%{g:gitCurrentBranch}%{']'}%y\ %F%=%l,%c%V%8P

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
"  snippetsEmu
" ============================================
let g:snippetsEmu_key = "<C-B>"

" ============================================
" mac
" ============================================
if has("mac") 
  if filereadable( $HOME . "/.vimrc-mac" )
    source ~/.vimrc-mac
  endif
endif

" ============================================
" win32
" ============================================
if has("gui_win32")
  if filereadable( $HOME . "/_vimrc-windows" )
    source ~/_vimrc-windows
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




