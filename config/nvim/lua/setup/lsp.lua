-- LSP this is needed for LSP completions in CSS along with the snippets plugin
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.resolveSupport = {
  properties = {
    "documentation",
    "detail",
    "additionalTextEdits",
  },
}

-- Different machine VAR for office
local envMachine = os.getenv("MACHINE")
if envMachine == "work" then
  machineCmd =
    "/System/Volumes/Data/usr/local/lib/node_modules/vscode-langservers-extracted/bin/vscode-css-language-server"
else
  machineCmd = "vscode-css-language-server"
end

-- LSP Server config
require("lspconfig").cssls.setup({
  cmd = { machineCmd, "--stdio" },
  settings = {
    scss = {
      lint = {
        idSelector = "warning",
        zeroUnits = "warning",
        duplicateProperties = "warning",
      },
      completion = {
        completePropertyWithSemicolon = true,
        triggerPropertyValueCompletion = true,
      },
    },
  },
  capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities()),
  on_attach = function(client)
    client.server_capabilities.document_formatting = false
  end,
})
require("lspconfig").tsserver.setup({
  capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities()),
  on_attach = function(client)
    client.server_capabilities.document_formatting = false
  end,
})

require("lspconfig").html.setup({

  capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities()),
  on_attach = function(client)
    client.server_capabilities.document_formatting = false
  end,
})

require("lspconfig").pyright.setup({})
require("lspconfig").bashls.setup({})
require("lspconfig").gopls.setup({})
require("lspconfig").vimls.setup({})
require("lspconfig").yamlls.setup({})
require("lspconfig").rust_analyzer.setup({})

-- LSP Prevents inline buffer annotations
-- vim.lsp.diagnostic.show_line_diagnostics()
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = true,
  signs = true,
  underline = true,
  update_on_insert = false,
})

local signs = {
  Error = "ﰸ",
  Warn = "",
  Hint = "",
  Info = "",
}
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = nil })
end

-- vim.cmd([[autocmd CursorHold,CursorHoldI * lua vim.lsp.diagnostic.show_line_diagnostics({focusable=false})]])
