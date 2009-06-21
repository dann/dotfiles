if exists("b:did_eruby_stl_syntax_warning_ftplugin") || &filetype !~ '\<eruby\>'
    finish
endif
let b:did_eruby_stl_syntax_warning_ftplugin = 1

"bail if the user doesnt have ruby or cat installed
if !executable("ruby") || !executable("cat")
    finish
endif

"inject the syntax warning into the statusline
let &l:statusline = substitute(&statusline, '\(%=\)',
            \ '%#warningmsg#%{StatuslineERubySyntaxWarning()}%*\1', '')

"recalculate after saving
autocmd bufwritepost * unlet! b:statusline_eruby_syntax_warning

"run the erb sections of the buffer though ruby -c
"
"return '' if no syntax errors detected
"return '[syntax:xxx]' if errors are detected, where xxx is the line num of
"the first error
function! StatuslineERubySyntaxWarning()
    if !exists("b:statusline_eruby_syntax_warning")
        let b:statusline_eruby_syntax_warning = ''
        if filereadable(expand("%"))
            let output = s:CheckSyntax(expand("%"))
            if v:shell_error != 0
                let b:statusline_eruby_syntax_warning =
                            \ '[syntax:'. s:ExtractErrorLine(output) . ']'
            endif
        endif
    endif
    return b:statusline_eruby_syntax_warning
endfunction

"extract the line num of the first syntax error for the given output
"from 'ruby -c'
function! s:ExtractErrorLine(error_msg)
    return substitute(a:error_msg, '.\{-}:\(\d*\): syntax error,.*', '\1', '')
endfunction

"run the erb sections of the given file through ruby -c and return the result
function! s:CheckSyntax(filename)
    return system('cat '. a:filename . ' | ruby -e "require \"erb\"; puts ERB.new(ARGF.read, nil, \"-\").src" | ruby -c')
endfunction
