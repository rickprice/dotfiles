require("neorg").setup({
  load = {
    ["core.defaults"] = {},
    ["core.norg.concealer"] = {},
    -- ["core.gtd.base"] = {},
    ["core.norg.journal"] = {},
    ["core.norg.qol.toc"] = {},
    -- ["core.presenter"] = {},
    ["core.norg.dirman"] = {
      config = {
        workspaces = {
          work = "~/notes/work",
          home = "~/notes/home",
        },
      },
    },
  },
})
