return {
	{
		"smoka7/hop.nvim",
		config = function()
			vim.cmd("hi HopNextKey guifg=#ff9900")
			vim.cmd("hi HopNextKey1 guifg=#ff9900")
			vim.cmd("hi HopNextKey2 guifg=#ff9900")
			require("hop").setup()
		end,
		keys = {
			{ "<leader>h", "<cmd>lua require'hop'.hint_words()<cr>", mode = "n", desc = "Hop Anywhere" },
			{ "<leader>h", "<cmd>lua require'hop'.hint_words()<cr>", mode = "v", desc = "Hop Anywhere" },
			-- { "/", "<cmd>lua require'hop'.hint_patterns()<cr>", mode = "n", desc = "Hop Anywhere" },
		},
	},
}
