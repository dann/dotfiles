" prove.vim - Snytax color for prove.
"
" Author:  Kazuhito Hokamura <http://webtech-walker.com>
" Version: 0.0.1
" License: MIT License <http://www.opensource.org/licenses/mit-license.php>

if exists('b:current_syntax')
  finish
endif

syntax match proveError /^Dubious.*/
syntax match proveError /^Failed.*/
syntax match proveError /^not ok.*/
syntax match proveError /Result: FAIL/
syntax match proveSuccess /All tests successful./

highlight proveError guifg=#CC0000 ctermfg=red
highlight proveSuccess guifg=green ctermfg=green

let b:current_syntax = "prove"
