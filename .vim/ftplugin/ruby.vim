setlocal tabstop=2 softtabstop=2 shiftwidth=2

set iskeyword+=@,$,?,:
set iskeyword-=.

" Completion
setlocal dictionary=~/.vim/dict/ruby.dict
setlocal tags+=~/.vim/tags/ruby/gems.tags
