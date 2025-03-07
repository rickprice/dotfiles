return {
	{
		"williamboman/mason.nvim",
		opts = {
			ensure_installed = {
				"stylua",
				"shellcheck",
				"shfmt",
				"flake8",
				"black",
				"rust-analyzer",
				"clangd",
				"dockerfile-language-server",
				"python-lsp-server",
				"haskell-language-server",
				"markdownlint",
				"marksman",
			},
		},
	},

	{
		"williamboman/mason-lspconfig.nvim",
		opts = {
			ensure_installed = { "rust_analyzer", "marksman" },
		},
	},
}
