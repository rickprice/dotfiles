return {
  "epwalsh/obsidian.nvim",
  version = "*",
  lazy = true,
  ft = "markdown",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "hrsh7th/nvim-cmp", -- optional, for completion
    "nvim-telescope/telescope.nvim", -- optional, for search
  },
  opts = {
    workspaces = {
      {
        name = "work",
        path = "~/obsidian/work-vault",
      },
      {
        name = "personal",
        path = "~/obsidian/personal-vault", -- optional
      },
    },

    -- Daily notes configuration
    daily_notes = {
      folder = "journal/daily",
      date_format = "%Y-%m-%d",
      alias_format = "%B %-d, %Y",
      template = "templates/daily-note.md"
    },

    -- Weekly notes for sprint planning, retrospectives
    weekly_notes = {
      folder = "journal/weekly",
      date_format = "%Y-W%V",
      alias_format = "Week of %B %-d, %Y",
      template = "templates/weekly-note.md"
    },

    -- Note completion and linking
    completion = {
      nvim_cmp = true,
      min_chars = 2,
    },

    -- Customize note ID generation
    note_id_func = function(title)
      local suffix = ""
      if title ~= nil then
        -- Convert to lowercase and replace spaces with dashes
        suffix = title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
      else
        -- Generate random ID if no title
        for _ = 1, 4 do
          suffix = suffix .. string.char(math.random(65, 90))
        end
      end
      return tostring(os.time()) .. "-" .. suffix
    end,

    -- Customize wiki link format
    wiki_link_func = function(opts)
      if opts.id == nil then
        return string.format("[[%s]]", opts.label)
      elseif opts.label ~= opts.id then
        return string.format("[[%s|%s]]", opts.id, opts.label)
      else
        return string.format("[[%s]]", opts.id)
      end
    end,

    -- Templates directory
    templates = {
      subdir = "templates",
      date_format = "%Y-%m-%d",
      time_format = "%H:%M",
      substitutions = {
        yesterday = function()
          return os.date("%Y-%m-%d", os.time() - 86400)
        end,
        tomorrow = function()
          return os.date("%Y-%m-%d", os.time() + 86400)
        end,
      },
    },

    -- Configure mappings
    mappings = {
      -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
      ["gf"] = {
        action = function()
          return require("obsidian").util.gf_passthrough()
        end,
        opts = { noremap = false, expr = true, buffer = true },
      },
      -- Toggle check-boxes.
      ["<leader>ch"] = {
        action = function()
          return require("obsidian").util.toggle_checkbox()
        end,
        opts = { buffer = true },
      },
      -- Smart action based on context
      ["<cr>"] = {
        action = function()
          return require("obsidian").util.smart_action()
        end,
        opts = { buffer = true, expr = true },
      }
    },

    -- Configure how notes are stored
    note_frontmatter_func = function(note)
      local out = { id = note.id, aliases = note.aliases, tags = note.tags }
      if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
        for k, v in pairs(note.metadata) do
          out[k] = v
        end
      end
      return out
    end,

    -- UI configuration
    ui = {
      enable = true,
      update_debounce = 200,
      checkboxes = {
        [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
        ["x"] = { char = "", hl_group = "ObsidianDone" },
        [">"] = { char = "", hl_group = "ObsidianRightArrow" },
        ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
      },
      bullets = { char = "•", hl_group = "ObsidianBullet" },
      external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
      reference_text = { hl_group = "ObsidianRefText" },
      highlight_text = { hl_group = "ObsidianHighlightText" },
      tags = { hl_group = "ObsidianTag" },
      block_ids = { hl_group = "ObsidianBlockID" },
    },

    -- Attachments configuration
    attachments = {
      img_folder = "assets/images",
      img_text_func = function(client, path)
        local link_path
        local vault_relative_path = client:vault_relative_path(path)
        if vault_relative_path ~= nil then
          link_path = vault_relative_path
        else
          link_path = tostring(path)
        end
        local display_name = vim.fs.basename(link_path)
        return string.format("![%s](%s)", display_name, link_path)
      end,
    },
  },

  keys = {
    -- Quick note creation
    { "<leader>on", "<cmd>ObsidianNew<cr>", desc = "New Obsidian note" },
    { "<leader>oo", "<cmd>ObsidianOpen<cr>", desc = "Open in Obsidian app" },
    { "<leader>ob", "<cmd>ObsidianBacklinks<cr>", desc = "Show backlinks" },
    { "<leader>ot", "<cmd>ObsidianTemplate<cr>", desc = "Insert template" },
    { "<leader>op", "<cmd>ObsidianPasteImg<cr>", desc = "Paste image" },
    { "<leader>or", "<cmd>ObsidianRename<cr>", desc = "Rename note" },

    -- Daily/Weekly notes
    { "<leader>od", "<cmd>ObsidianDailies<cr>", desc = "Open daily notes" },
    { "<leader>ot", "<cmd>ObsidianToday<cr>", desc = "Open today's note" },
    { "<leader>oy", "<cmd>ObsidianYesterday<cr>", desc = "Open yesterday's note" },
    { "<leader>otm", "<cmd>ObsidianTomorrow<cr>", desc = "Open tomorrow's note" },
    { "<leader>ow", "<cmd>ObsidianWeek<cr>", desc = "Open this week's note" },

    -- Search and navigation
    { "<leader>of", "<cmd>ObsidianQuickSwitch<cr>", desc = "Quick switch notes" },
    { "<leader>os", "<cmd>ObsidianSearch<cr>", desc = "Search notes" },
    { "<leader>ol", "<cmd>ObsidianLinks<cr>", desc = "Collect links" },
    { "<leader>otg", "<cmd>ObsidianTags<cr>", desc = "Show tags" },

    -- Work-specific shortcuts
    { "<leader>omm", "<cmd>ObsidianTemplate meeting-note<cr>", desc = "Meeting template" },
    { "<leader>opr", "<cmd>ObsidianTemplate project-note<cr>", desc = "Project template" },
    { "<leader>osr", "<cmd>ObsidianTemplate standup<cr>", desc = "Standup template" },
  },

  config = function(_, opts)
    require("obsidian").setup(opts)

    -- Custom commands for work journaling
    vim.api.nvim_create_user_command("ObsidianWeek", function()
      local week_format = os.date("%Y-W%V")
      vim.cmd("ObsidianNew " .. week_format)
    end, {})

    vim.api.nvim_create_user_command("ObsidianMeetingNote", function()
      local timestamp = os.date("%Y%m%d-%H%M")
      vim.cmd("ObsidianNew meeting-" .. timestamp)
    end, {})

    vim.api.nvim_create_user_command("ObsidianProjectNote", function(opts)
      if opts.args == "" then
        print("Usage: ObsidianProjectNote <project-name>")
        return
      end
      local project_name = opts.args:gsub(" ", "-"):lower()
      vim.cmd("ObsidianNew project-" .. project_name)
    end, { nargs = "?" })
  end,
}
