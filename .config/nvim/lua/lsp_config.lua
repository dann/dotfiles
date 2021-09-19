
-- lspsaga
local saga = require 'lspsaga'
saga.init_lsp_saga {
  error_sign = '',
  warn_sign = '',
  hint_sign = '',
  infor_sign = '',
  border_style = "round",
}


local on_attach = function(client, bufnr)
  require('completion').on_attach()

  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  -- buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)

  -- luasaga
  --buf_set_keymap('n', 'K', '<cmd>lua require('lspsaga.hover').render_hover_doc()<CR>', opts)
  --buf_set_keymap('n', '<C-k>', '<cmd>lua require(lspsaga.signaturehelp).signature_help()<CR>', opts)
  --buf_set_keymap('n', 'gh', '<cmd>lua require('lspsaga.provider').lsp_finder()', opts)

  -- buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<leader>law', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>lrw', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>llw', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<leader>lt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<leader>lrn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>lrf', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<leader>ld', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '<leader>ll', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<leader>lca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap("n", "ff", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end

-- ============================
-- lspconfig settings
-- ============================
local nvim_lsp = require('lspconfig')

-- LSPs
--local servers = { "gopls","pyright","tsserver" }
--for _, lsp in ipairs(servers) do
--  nvim_lsp[lsp].setup {
--      on_attach = on_attach;
--  }
--end

nvim_lsp.gopls.setup {
--      cmd = {"gopls", "serve", "-rpc.trace", "--debug=localhost:6060"};
      on_attach = on_attach;
}

nvim_lsp.pyright.setup {
  on_attach = on_attach;
}

nvim_lsp.yamlls.setup{
  settings = {
        yaml = {
           schemas = { kubernetes = "/*.yml" },
      }
  };
  on_attach = on_attach;
}


-- ============================
-- auto import for golang
-- ============================
function org_imports(wait_ms)
    local params = vim.lsp.util.make_range_params()
    params.context = {only = {"source.organizeImports"}}
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, wait_ms)
    for _, res in pairs(result or {}) do
      for _, r in pairs(res.result or {}) do
        if r.edit then
          vim.lsp.util.apply_workspace_edit(r.edit)
        else
          vim.lsp.buf.execute_command(r.command)
        end
      end
    end
end


