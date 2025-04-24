-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- lua/keymaps.lua  (loaded by Lazy)
return {
  -- existing mappings …

  -- ── Run external command on visual selection ──────────────────────────────
  {
    mode = "v",                   -- visual mode
    lhs  = "<leader>rp",           -- hit <leader>ap after selecting text
    desc = "Run shell pmd on selection → vert split",
    rhs  = function()
      -- capture visual selection
      -- local start_pos = vim.fn.getpos("'<")
      -- local end_pos   = vim.fn.getpos("'>")
      -- local lines     = vim.fn.getline(start_pos[2], end_pos[2])
      -- local text      = table.concat(lines, "\n")
      -- local arg       = vim.fn.shellescape(text)   -- one safe CLI argument

      local cmd = "my_command " .. arg             -- ← change my_command

      vim.cmd("vert new | setlocal buftype=nofile bufhidden=wipe nobuflisted")
      vim.cmd("0r !" .. cmd)
    end,
  },
}
