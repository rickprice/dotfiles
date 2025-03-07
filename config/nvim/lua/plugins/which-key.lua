return {
	{
		"folke/which-key.nvim",
		config = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 500
			require("which-key").setup({
				-- your configuration comes here
				-- or leave it empty to use the default settings
				-- refer to the configuration section below
			})
		end,
		keys = {
			{ "<F2>", ":let [&nu, &rnu] = [!&rnu, &nu+&rnu==1]<cr>", mode = "n", desc = "Toggle line numbers" },
			{ "<F3>", ":set list!<CR>", mode = "n", desc = "Toggle show whitespace" },
			{ "<leader>dtw", ":%s/\\s\\+$//e<cr>", mode = "n", desc = "Delete trailing whitespace" },
			open_mapping = true,
		},
	},
}
