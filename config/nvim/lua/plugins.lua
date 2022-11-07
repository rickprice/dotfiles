vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end
vim.api.nvim_command("packadd packer.nvim")
-- returns the require for use in `config` parameter of packer's use
-- expects the name of the config file
function get_setup(name)
  return string.format('require("setup/%s")', name)
end

return require("packer").startup({
  function(use)
    -- Packer can manage itself
    use("wbthomason/packer.nvim")

    use({ "nvim-lua/plenary.nvim" })

    use({ "nathom/filetype.nvim", config = get_setup("filetype") })
    use({ "EdenEast/nightfox.nvim", config = get_setup("nightfox") })
    use({ "kyazdani42/nvim-web-devicons" })
    use({
      "nvim-lualine/lualine.nvim",
      config = get_setup("lualine"),
      event = "VimEnter",
      requires = { "kyazdani42/nvim-web-devicons", opt = true },
    })
    use({
      "folke/zen-mode.nvim",
      config = get_setup("zen-mode"),
    })
    use({
      "norcalli/nvim-colorizer.lua",
      event = "BufReadPre",
      config = get_setup("colorizer"),
    })
    -- Post-install/update hook with neovim command
    use({
      "nvim-treesitter/nvim-treesitter",
      config = get_setup("treesitter"),
      run = ":TSUpdate",
    })
    use("nvim-treesitter/nvim-treesitter-textobjects")
    use({
      "windwp/nvim-autopairs",
      after = "nvim-cmp",
      config = get_setup("autopairs"),
    })
    use({
      "hrsh7th/nvim-cmp",
      requires = {
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-nvim-lua" },
        { "hrsh7th/cmp-buffer" },
        { "hrsh7th/cmp-path" },
        { "hrsh7th/cmp-cmdline" },
        { "hrsh7th/vim-vsnip", { "hrsh7th/vim-vsnip-integ" } },
        { "hrsh7th/cmp-vsnip" },
        { "hrsh7th/vim-vsnip-integ" },
        { "f3fora/cmp-spell", { "hrsh7th/cmp-calc" }, { "hrsh7th/cmp-emoji" } },
      },
      config = get_setup("cmp"),
    })
    use({ "kyazdani42/nvim-tree.lua", requires = { "nvim-tree/nvim-web-devicons" }, config = get_setup("tree") })

    use({
      "lewis6991/gitsigns.nvim",
      requires = { "nvim-lua/plenary.nvim" },
      event = "BufReadPre",
      config = get_setup("gitsigns"),
    })

    use("p00f/nvim-ts-rainbow")

    -- This needs plenary
    use({ "jose-elias-alvarez/null-ls.nvim", requires = "nvim-lua/plenary.nvim", config = get_setup("null-ls") })
    use({ "neovim/nvim-lspconfig", config = get_setup("lsp") })
    use({
      "numToStr/Comment.nvim",
      opt = true,
      keys = { "gc", "gcc" },
      config = get_setup("comment"),
    })
    use({
      "simrat39/rust-tools.nvim",
      module = "rust-tools",
      requires = {
        { "neovim/nvim-lspconfig" },
        { "nvim-lua/plenary.nvim" },
        { "mfussenegger/nvim-dap" },
      },
      config = get_setup("rust-tools"),
    })
    use({
      "folke/trouble.nvim",
      module = "trouble",
      requires = {
        { "kyazdani42/nvim-web-devicons" },
      },
      config = function()
        require("trouble").setup({})
      end,
    })
    use({
      "glepnir/lspsaga.nvim",
      module = "lspsaga",
      config = get_setup("lspsaga"),
    })
    use({
      "nvim-telescope/telescope.nvim",
      module = "telescope",
      cmd = "Telescope",
      requires = {
        { "glepnir/lspsaga.nvim" },
        { "nvim-lua/popup.nvim" },
        { "nvim-lua/plenary.nvim" },
        { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      },
      config = get_setup("telescope"),
    })
    use({ "nvim-telescope/telescope-file-browser.nvim" })
    use({ "onsails/lspkind-nvim", requires = { { "famiu/bufdelete.nvim" } } })
    use({ "tpope/vim-repeat" })
    use({ "tpope/vim-surround" })
    -- use({ "tpope/vim-fugitive" })
    use({ "wellle/targets.vim" })
    use({
      "phaazon/hop.nvim",
      event = "BufReadPre",
      config = get_setup("hop"),
    })
    use({ "Shatur/neovim-session-manager", requires = "nvim-lua/plenary.nvim", config = get_setup("session") })
    use({ "windwp/nvim-ts-autotag" })

    use({
      "winston0410/range-highlight.nvim",
      requires = { { "winston0410/cmd-parser.nvim" } },
      config = get_setup("range-highlight"),
    })
    use({ "filipdutescu/renamer.nvim", requires = "nvim-lua/plenary.nvim", config = get_setup("renamer") })
    use({ "goolord/alpha-nvim", config = get_setup("alpha") })

    use({ "luukvbaal/stabilize.nvim", config = get_setup("stabilize") })

    -- SymbolsOutline
    use({
      "simrat39/symbols-outline.nvim",
      cmd = { "SymbolsOutline" },
      setup = get_setup("outline"),
    })

    -- Manage sudo files better
    use({ "lambdalisue/suda.vim" })

    -- Improved incremental search
    use({ "haya14busa/is.vim" })

    -- Handle marks
    use({
      "chentoast/marks.nvim",
      config = function()
        require("marks").setup({})
      end,
    })

    -- Handle CSV files
    -- use({
    --   "chrisbra/csv.vim",
    --   ft = "csv",
    --   config = get_setup("csv"),
    -- })

    -- VimWiki
    --   use({ "ray-x/go.nvim", config = get_setup("golang") })

    -- VimWiki
    use({ "vimwiki/vimwiki" })

    -- Markdown
    use({ "ellisonleao/glow.nvim" })
    use({ "masukomi/vim-markdown-folding" })

    -- Better Selects
    use({
      "mfussenegger/nvim-ts-hint-textobject",
      config = function()
        vim.cmd([[omap     <silent> m :<C-U>lua require('tsht').nodes()<CR>]])
        vim.cmd([[vnoremap <silent> m :lua require('tsht').nodes()<CR>]])
      end,
    })

    -- ToDo management
    use({
      "folke/todo-comments.nvim",
      requires = "nvim-lua/plenary.nvim",
      config = get_setup("todo-comments"),
    })

    use({
      "nvim-neorg/neorg",
      tag = "*",
      -- ft = "norg",
      after = "nvim-treesitter", -- You may want to specify Telescope here as well
      config = get_setup("neorg"),
      requires = "nvim-lua/plenary.nvim",
      run = ":Neorg sync-parsers",
    })

    -- use({
    --   "folke/which-key.nvim",
    --   config = get_setup("which-key"),
    -- })

    -- Git and Diff stuff
    -- use({ "sindrets/diffview.nvim", requires = "nvim-lua/plenary.nvim", config = get_setup("diffview") })
    -- FIX: This one is causing problems
    -- use({ "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim", config = get_setup("neogit") })

    -- Is using a standard Neovim install, i.e. built from source or using a
    -- provided appimage.
    -- TODO: At some point this should be going away when NeoVim subsumes it
    use("lewis6991/impatient.nvim")

    use({ "eddiebergman/nvim-treesitter-pyfold", requires = "nvim-treesitter/nvim-treesitter" })

    -- Clojure
    --use("Olical/conjure")

    if packer_bootstrap then
      require("packer").sync()
    end
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
    profile = {
      enable = true,
      threshold = 1, -- the amount in ms that a plugins load time must be over for it to be included in the profile
    },
  },
})
