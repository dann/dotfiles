if exists("current_compiler")
  finish
endif
let current_compiler = "scss"

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=sass\ --trace\ --unix-newlines\ --scss\ %\ %:s?\.scss$?.css?

CompilerSet errorformat=
        \%f:%l:%m\ (Sass::SyntaxError),
        \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
