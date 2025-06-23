return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		opts = function(_, opts)
			-- Ensure opts.ensure_installed exists
			opts.ensure_installed = opts.ensure_installed or {}
			-- Add parsers
			vim.list_extend(opts.ensure_installed, {
				"bash",
				"typescript",
				"tsx",
				"javascript",
				-- "jsx",
				"json",
				"html",
				"css",
				"scss",
				"lua",
				"markdown",
				"markdown_inline",
				"python",
				"regex",
				"vim",
				"yaml",
				"rust",
				"haskell",
				-- "mdx",
			})
			-- Configure auto install and parser configs
			opts.auto_install = true
			opts.parser_install_dir = vim.fn.stdpath("data") .. "/lazy/nvim-treesitter"
		end,
	},
}
