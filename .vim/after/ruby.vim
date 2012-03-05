if exists("b:did_ruby_syntax_check_ftplugin") || &filetype !~ '\<ruby\>'
  finish
endif
let b:did_ruby_syntax_check_ftplugin = 1

"bail if the user doesnt have ruby installed
if !executable("ruby")
  finish
endif

function! s:ExecuteMake()
  if &filetype == 'ruby' && expand('%:t') !~? '^pry\d\{8}.\+\.rb'
    silent make! -c "%" | redraw!
  endif
endfunction

compiler ruby
augroup rbsytaxcheck
  autocmd! BufWritePost <buffer> call s:ExecuteMake()
augroup END
