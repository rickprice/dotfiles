return {
	-- change trouble config
	{
		"folke/trouble.nvim",
		-- opts will be merged with the parent spec
		opts = { use_diagnostic_signs = true },
		keys = {
			{ "<leader>xx", ":Trouble diagnostics toggle<CR>", desc = "Toggle trouble diagnostics" },
			{
				"<leader>xd",
				":Trouble diagnostics toggle filter.buf=0<CR>",
				desc = "Toggle trouble document diagnostics",
			},
			{ "<leader>xq", ":Trouble quickfix toggle<CR>", desc = "Toggle trouble quickfix list" },
			{ "<leader>xl", ":Trouble loclist toggle<CR>", desc = "Toggle trouble location list" },
			{ "<leader>xt", ":Trouble todo toggle<CR>", desc = "Toggle trouble todos list" },
			-- These don't really belong here, but they are diagnostics related
			{ "<leader>xh", ":lua vim.diagnostic.hide()<CR>", desc = "Disable LSP diagnostics" },
			{ "<leader>xs", ":lua vim.diagnostic.show()<CR>", desc = "Enable LSP diagnostics" },
		},
	},
}
