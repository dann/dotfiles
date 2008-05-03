imap <buffer> <C-x> var a=[],str='';for(p in )a.push(p);a.sort();alert(a.join(' '));<esc>5F)i

" 'Alert Here'
map <buffer> \ah :exec 'normal Oalert("' . expand('%:t') . ':' . line('.') . '");'<cr>


set dictionary=~/.vim/dict/javascript.dict
