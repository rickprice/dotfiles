return {
  {
    "folke/noice.nvim",
    opts = {
      history = {
        -- Ensure history is properly configured
        view = "popup",
        opts = { enter = true, format = "details" },
        filter = {
          any = {
            { event = "notify" },
            { error = true },
            { warning = true },
            { event = "msg_show", kind = { "" } },
            { event = "lsp", kind = "message" },
          },
        },
      },
    },
  },
}