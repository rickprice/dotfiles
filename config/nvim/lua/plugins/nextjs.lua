return {
	{
		"neovim/nvim-lspconfig",
		opts = function(_, opts)
			local lspconfig = require("lspconfig")
			
			-- Configure TailwindCSS LSP for Next.js projects
			lspconfig.tailwindcss.setup({
				filetypes = {
					"css",
					"scss",
					"sass",
					"html",
					"javascript",
					"javascriptreact",
					"typescript",
					"typescriptreact",
				},
				root_dir = lspconfig.util.root_pattern(
					"tailwind.config.js",
					"tailwind.config.ts",
					"tailwind.config.cjs",
					"tailwind.config.mjs"
				),
			})

			-- Configure CSS LSP
			lspconfig.cssls.setup({
				settings = {
					css = {
						validate = true,
						lint = {
							unknownAtRules = "ignore",
						},
					},
					scss = {
						validate = true,
						lint = {
							unknownAtRules = "ignore",
						},
					},
				},
			})

			-- Configure HTML LSP
			lspconfig.html.setup({
				filetypes = { "html", "javascriptreact", "typescriptreact" },
			})

			-- Configure JSON LSP
			lspconfig.jsonls.setup({
				settings = {
					json = {
						schemas = {
							{
								fileMatch = { "package.json" },
								url = "https://json.schemastore.org/package.json",
							},
							{
								fileMatch = { "tsconfig.json", "tsconfig.*.json" },
								url = "https://json.schemastore.org/tsconfig.json",
							},
							{
								fileMatch = { ".eslintrc", ".eslintrc.json" },
								url = "https://json.schemastore.org/eslintrc.json",
							},
							{
								fileMatch = { "next.config.js" },
								url = "https://json.schemastore.org/next.json",
							},
						},
					},
				},
			})

			-- Configure ESLint LSP
			lspconfig.eslint.setup({
				settings = {
					workingDirectory = { mode = "auto" },
				},
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		opts = function(_, opts)
			-- Ensure Next.js related file types are properly configured
			vim.filetype.add({
				extension = {
					mdx = "mdx",
				},
				filename = {
					[".eslintrc.json"] = "json",
					["next.config.js"] = "javascript",
					["next.config.ts"] = "typescript",
				},
			})
			
		end,
	},
}