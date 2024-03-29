local function map(mode, lhs, rhs, opts)
  local options = { noremap = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Map leader to space
vim.g.mapleader = " "

-- Nvim Tree
map("n", "<leader>e", ":NvimTreeToggle<CR>", { silent = true })
map("n", "<leader>u", ":NvimTreeFindFile<CR>", { silent = true })

-- Update Plugins
map("n", "<Leader>u", ":PackerSync<CR>")

-- Open nvimrc file
map("n", "<Leader>v", "<cmd>e $MYVIMRC<CR>")

-- Source nvimrc file
map("n", "<Leader>sv", ":luafile %<CR>")

-- Quick new file
map("n", "<Leader>n", "<cmd>enew<CR>")

-- Easy select all of file
-- map("n", "<Leader>sa", "ggVG<c-$>")

-- Make visual yanks place the cursor back where started
map("v", "y", "ygv<Esc>")

-- Easier file save
-- map("n", "<Leader>w", "<cmd>:w<CR>")
-- map("n", "<Delete>", "<cmd>:w<CR>")

-- Tab to switch buffers in Normal mode
map("n", "<Tab>", ":bnext<CR>")
map("n", "<S-Tab>", ":bprevious<CR>")

-- Toggle show whitespace
map("n", "<F3>", ":set list!<CR>")

-- More molecular undo of text
-- map("i", ",", ",<c-g>u")
-- map("i", ".", ".<c-g>u")
-- map("i", "!", "!<c-g>u")
-- map("i", "?", "?<c-g>u")
-- map("i", ";", ";<c-g>u")
-- map("i", ":", ":<c-g>u")

-- Keep search results centred
-- map("n", "n", "nzzzv")
-- map("n", "N", "Nzzzv")
-- map("n", "J", "mzJ`z")

-- Make Y yank to end of the line
map("n", "Y", "y$")

-- Line bubbling
-- map("n", "<c-j>", "<cmd>m .+1<CR>==", { silent = true })
-- map("n", "<c-k>", "<cmd>m .-2<CR>==", { silent = true })
-- map("v", "<c-j>", ":m '>+1<CR>==gv=gv", { silent = true })
-- map("v", "<c-k>", ":m '<-2<CR>==gv=gv", { silent = true })

--After searching, pressing escape stops the highlight
map("n", "<esc>", ":noh<cr><esc>", { silent = true })

-- Easy add date/time
map("n", "<Leader>t", "\"=strftime('%c')<CR>Pa", { silent = true })

-- Telescope
map("n", "<Leader>1", ":Telescope sessions [save_current=true]<CR>")
map("n", "<leader>p", '<cmd>lua require("telescope.builtin").find_files()<cr>')
map("n", "<leader>r", '<cmd>lua require("telescope.builtin").registers()<cr>')
map("n", "<leader>g", '<cmd>lua require("telescope.builtin").live_grep()<cr>')
map("n", "<leader>b", '<cmd>lua require("telescope.builtin").buffers()<cr>')
map("n", "<leader>j", '<cmd>lua require("telescope.builtin").help_tags()<cr>')
map("n", "<leader>h", '<cmd>lua require("telescope.builtin").git_bcommits()<cr>')
map("n", "<leader>f", '<cmd>lua require("telescope").extensions.file_browser.file_browser()<CR>')
map("n", "<leader>s", '<cmd>lua require("telescope.builtin").spell_suggest()<cr>')
map("n", "<leader>i", '<cmd>lua require("telescope.builtin").git_status()<cr>')
map("n", "<leader>ca", '<cmd>lua require("telescope.builtin").lsp_code_actions()<cr>')
map("n", "<leader>cs", '<cmd>lua require("telescope.builtin").lsp_document_symbols()<cr>')
map("n", "<leader>cd", '<cmd>lua require("telescope.builtin").lsp_document_diagnostics()<cr>')
map("n", "<leader>cr", '<cmd>lua require("telescope.builtin").lsp_references()<cr>')

map("i", "<F2>", '<cmd>lua require("renamer").rename()<cr>', { noremap = true, silent = true })
map("n", "<leader>cn", '<cmd>lua require("renamer").rename()<cr>', { noremap = true, silent = true })
map("v", "<leader>cn", '<cmd>lua require("renamer").rename()<cr>', { noremap = true, silent = true })

map("n", "<leader>ci", "<cmd> lua vim.diagnostic.open_float()<cr>")

-- Easier split mappings
-- map("n", "<Leader><Down>", "<C-W><C-J>", { silent = true })
-- map("n", "<Leader><Up>", "<C-W><C-K>", { silent = true })
-- map("n", "<Leader><Right>", "<C-W><C-L>", { silent = true })
-- map("n", "<Leader><Left>", "<C-W><C-H>", { silent = true })
-- map("n", "<Leader>;", "<C-W>R", { silent = true })
-- map("n", "<Leader>[", "<C-W>_", { silent = true })
-- map("n", "<Leader>]", "<C-W>|", { silent = true })
-- map("n", "<Leader>=", "<C-W>=", { silent = true })

-- Hop
map("n", "<Leader>h", "<cmd>lua require'hop'.hint_words()<cr>")
map("n", "<Leader>l", "<cmd>lua require'hop'.hint_lines()<cr>")
map("v", "<Leader>h", "<cmd>lua require'hop'.hint_words()<cr>")
map("v", "<Leader>l", "<cmd>lua require'hop'.hint_lines()<cr>")

-- Symbols outline
map("n", "<leader>o", ":SymbolsOutline<cr>")

-- Trouble
map("n", "<leader>xx", "<cmd>TroubleToggle<cr>")
map("n", "<leader>xw", "<cmd>TroubleToggle lsp_workspace_diagnostics<cr>")
map("n", "<leader>xd", "<cmd>TroubleToggle lsp_document_diagnostics<cr>")
map("n", "<leader>xq", "<cmd>TroubleToggle quickfix<cr>")
map("n", "<leader>xl", "<cmd>TroubleToggle loclist<cr>")
map("n", "gR", "<cmd>TroubleToggle lsp_references<cr>")

-- Delete trailing whitespace
map("n", "<Leader>dtw", ":%s/\\s\\+$//e<cr>")
-- This doesn't work yet
-- map("v", "<Leader>dtw", ":'<',>s/\\s\\+$//e<cr>")

-- Toggle line numbers
map("n", "<F2>", ":let [&nu, &rnu] = [!&rnu, &nu+&rnu==1]<cr>")
