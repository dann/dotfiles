if exists("current_compiler")
  finish
endif
let current_compiler = "scss"

if exists(":CompilerSet") != 2
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo&vim

CompilerSet makeprg=sass\ --unix-newlines\ --scss\ %\ %:s?\.scss$?.css?

CompilerSet errorformat=
	\%-G\ \ Use\ --trace\ for\ backtrace.,
        \%ESyntax\ error:\ %m,%Z\ \ \ \ \ \ \ \ on\ line\ %l\ of\ %f

let &cpo = s:cpo_save
unlet s:cpo_save
