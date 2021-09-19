" ===============================================
" Plugin Configs
" ===============================================

""""""""""""""""""""
" nvim-lspconfig
"""""""""""""""""""
" load .config/nvim/lua/lsp_config.lua
lua require('lsp_config')

autocmd BufWritePre *.go lua vim.lsp.buf.formatting()
autocmd BufWritePre *.go :silent! lua org_imports(300)

""""""""""""""""""""
" completion-nvim
"""""""""""""""""""
" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

let g:completion_enable_auto_popup = 1
imap <tab> <Plug>(completion_smart_tab)
imap <s-tab> <Plug>(completion_smart_s_tab)


""""""""""""""""""""
" lspsaga
""""""""""""""""""""
nnoremap <silent>gh :Lspsaga lsp_finder<CR>
nnoremap <silent>K :Lspsaga hover_doc<CR>
nnoremap <silent>gs :Lspsaga signature_help<CR>
nnoremap <silent>gr :Lspsaga rename<CR>

""""""""""""""""""""
" highlite
" for LSP Diagnoctics colors
""""""""""""""""""""
set termguicolors
colorscheme highlite

" vim: set foldmethod=marker foldlevel=0:
