"
"===============================================================================
"==========  load example vimrc from the distribution  =========================
"===============================================================================
"
runtime vimrc_example.vim
"
filetype plugin on
"
"===============================================================================
"==========  CUSTOMIZATION (vimrc)  ============================================
"===============================================================================
"
" Platform specific items:
" - central backup directory (has to be created)
" - default dictionary
" Uncomment your choice.  
if  has("win16") || has("win32")     || has("win64") || 
  \ has("win95")
    "
"    runtime mswin.vim
"    set backupdir =$VIM\vimfiles\backupdir
"    set dictionary=$VIM\vimfiles\wordlists/german.list
else
"    set backupdir =$HOME/.vim.backupdir
"    set dictionary=$HOME/.vim/wordlists/german.list
endif
"
"-------------------------------------------------------------------------------
" Enable the use of the mouse for certain terminals.
"-------------------------------------------------------------------------------
if &term =~ "xterm"
  set mouse=a
endif
"
"-------------------------------------------------------------------------------
" Use of dictionaries
"-------------------------------------------------------------------------------
set complete+=k           " scan the files given with the 'dictionary' option
"
"-------------------------------------------------------------------------------
" Various settings
"-------------------------------------------------------------------------------
set autochdir             " change the current working directory
set autoread              " read open files again when changed outside Vim
set autowrite             " write a modified buffer on each :next , ...
set browsedir  =current   " which directory to use for the file browser
set incsearch             " use incremental search
set nowrap                " do not wrap lines
set shiftwidth =2         " number of spaces to use for each step of indent
set tabstop    =2         " number of spaces that a <Tab> in the file counts for
set visualbell            " visual bell instead of beeping
"
"-------------------------------------------------------------------------------
"  some additional hot keys
"-------------------------------------------------------------------------------
"    F2   -  write file without confirmation
"    F3   -  call file explorer Ex
"    F4   -  show tag under curser in the preview window (tagfile must exist!)
"    F5   -  close error window
"    F6   -  (re)open error window
"    F7   -  display previous error
"    F8   -  display next error   
"-------------------------------------------------------------------------------
map   <silent> <F2>    :write<CR>
map   <silent> <F3>    :Explore<CR>
nmap  <silent> <F4>    :exe ":ptag ".expand("<cword>")<CR>
map   <silent> <F5>    :cclose<CR>
map   <silent> <F6>    :copen<CR>
map   <silent> <F7>    :cp<CR>
map   <silent> <F8>    :cn<CR>
"
imap  <silent> <F2>    <Esc>:write<CR>
imap  <silent> <F3>    <Esc>:Explore<CR>
imap  <silent> <F4>    <Esc>:exe ":ptag ".expand("<cword>")<CR>
imap  <silent> <F5>    <Esc>:cclose<CR>
imap  <silent> <F6>    <Esc>:copen<CR>
imap  <silent> <F7>    <Esc>:cp<CR>
imap  <silent> <F8>    <Esc>:cn<CR>
"
"-------------------------------------------------------------------------------
" autocomplete parenthesis, brackets and braces
"-------------------------------------------------------------------------------
inoremap ( ()<Left>
inoremap [ []<Left>
inoremap { {}<Left>
"
vnoremap ( s()<Esc>P<Right>
vnoremap [ s[]<Esc>P<Right>
vnoremap { s{}<Esc>P<Right>
"
"-------------------------------------------------------------------------------
" Fast switching between buffers
" The current buffer will be saved before switching to the next one.
" Choose :bprevious or :bnext
"-------------------------------------------------------------------------------
 map  <silent> <s-tab>  <Esc>:if &modifiable && !&readonly && 
     \                  &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
imap  <silent> <s-tab>  <Esc>:if &modifiable && !&readonly && 
     \                  &modified <CR> :write<CR> :endif<CR>:bprevious<CR>
"
"-------------------------------------------------------------------------------
" Leave the editor with Ctrl-q (KDE): Write all changed buffers and exit Vim
"-------------------------------------------------------------------------------
nmap  <C-q>    :wqa<CR>
"
"-------------------------------------------------------------------------------
" Filename completion
" 
"   wildmenu : command-line completion operates in an enhanced mode
" wildignore : A file that matches with one of these
"              patterns is ignored when completing file or directory names.
"-------------------------------------------------------------------------------
set wildmenu
set wildignore=*.bak,*.o,*.e,*~
"
"-------------------------------------------------------------------------------
" print options  (pc = percentage of the media size)
"-------------------------------------------------------------------------------
set printoptions=left:8pc,right:3pc

"-------------------------------------------------------------------------------
" perl-support.vim
"-------------------------------------------------------------------------------
let g:Perl_AuthorName        = 'Dr.-Ing. Fritz Mehner'
let g:Perl_AuthorRef         = 'Mn'
let g:Perl_Company           = 'FH Südwestfalen, Iserlohn'
let g:Perl_Email             = 'mehner@fh-swf.de'
"                         
"-------------------------------------------------------------------------------
" taglist.vim : toggle the taglist window
" taglist.vim : define the title texts for Perl
"-------------------------------------------------------------------------------
 noremap <silent> <F11>  <Esc><Esc>:Tlist<CR>
inoremap <silent> <F11>  <Esc><Esc>:Tlist<CR>

let tlist_perl_settings  = 'perl;c:constants;f:formats;l:labels;p:packages;s:subroutines;d:subroutines;o:POD'

