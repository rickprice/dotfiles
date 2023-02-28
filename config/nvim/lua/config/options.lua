local opt = vim.opt -- to set options

-- disable netrw at the very start of your init.lua (strongly advised) - From nvim-tree
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1

opt.backspace = { "indent", "eol", "start" }
-- opt.clipboard = "unnamedplus"
opt.colorcolumn = "100"
opt.completeopt = "menu,menuone,noselect"
opt.cursorcolumn = false
opt.cursorline = false
opt.encoding = "utf-8" -- Set default encoding to UTF-8
opt.expandtab = true -- Use spaces instead of tabs
opt.foldlevel = 99
opt.foldenable = false
opt.foldmethod = "expr"
opt.foldexpr = "nvim_treesitter#foldexpr()"
opt.foldtext = "nvim_treesitter#foldext()"
opt.formatoptions = "l"
opt.guicursor =
  "n-v-c-sm:block-blinkwait50-blinkon50-blinkoff50,i-ci-ve:ver25-Cursor-blinkon100-blinkoff100,r-cr-o:hor20"
opt.hidden = true
opt.hidden = true -- Enable background buffers
opt.hlsearch = true -- Highlight found searches
opt.ignorecase = true -- Ignore case
opt.inccommand = "split" -- Get a preview of replacements
opt.incsearch = true -- Shows the match while typing
opt.joinspaces = false -- No double spaces with join
opt.linebreak = true -- Stop words being broken on wrap
opt.list = true -- Show some invisible characters
opt.listchars = { tab = " ", trail = "·" }
opt.mouse = "a"
opt.wrapscan = false -- Don't wrap search over bottom, use ggn to start over
opt.number = false -- Show line numbers
opt.relativenumber = false
opt.scrolloff = 4 -- Lines of context
opt.shiftround = true -- Round indent
opt.shiftwidth = 4 -- Size of an indent
opt.showmode = false -- Don't display mode
opt.sidescrolloff = 8 -- Columns of context
-- opt.signcolumn = "yes:1" -- always show signcolumns
-- Auto show sign column up to 9 sign columns
opt.signcolumn = "auto:9"
opt.smartcase = true -- Do not ignore case with capitals
opt.smartindent = true -- Insert indents automatically
opt.spelllang = { "en_gb" }
opt.splitbelow = true -- Put new windows below current
opt.splitright = true -- Put new windows right of current
opt.tabstop = 4 -- Number of spaces tabs count for
opt.termguicolors = true -- You will have bad experience for diagnostic messages when it's default 4000.
opt.undodir = vim.fn.stdpath("config") .. "/undo"
opt.undofile = true
opt.undolevels = 10000
opt.wrap = true
vim.cmd("au TextYankPost * lua vim.highlight.on_yank {on_visual = true}") -- disabled in visual mode
vim.g.markdown_fenced_languages = { "html", "javascript", "typescript", "css", "scss", "lua", "vim" }
vim.g.suda_smart_edit = 1 -- Suda plugin
-- vim.o.lazyredraw = true
vim.o.whichwrap = vim.o.whichwrap .. "<,>" -- Wrap movement between lines in edit mode with arrows
-- This disables vimwiki thinking that every .md file is also a .wiki file
vim.g.vimwiki_global_ext = 0
-- Set history much higher
vim.g.history = 10000
-- Ensure that all characters are shown when list is enabled
opt.listchars = "eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣"
