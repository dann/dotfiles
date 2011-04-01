if exists("g:loaded_astyle")
       :finish
endif
let g:loaded_astyle = 1

let s:save_cpo = &cpo
set cpo&vim

function Astyle()
       let l:pos = getpos(".")
       %!astyle
       call setpos(".", pos)
endfunction
command Astyle :call Astyle()

let &cpo = s:save_cpo

