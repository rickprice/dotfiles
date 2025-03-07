return {
	{
		"filipdutescu/renamer.nvim",

		keys = {
			{ "<leader>cn", '<cmd>lua require("renamer").rename()<cr>', mode = "n", desc = "Rename" },
			{ "<leader>cn", '<cmd>lua require("renamer").rename()<cr>', mode = "v", desc = "Rename" },
		},
	},
}
