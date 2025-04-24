-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local function msg(text, hl)
  vim.api.nvim_echo({ { text, hl or "None" } }, false, {})
end

local function get_visual_selection()
  return table.concat(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos(".")), " ")
end

-- local map = vim.keymap.set

-- map("n", "<leader>H", "<cmd>msg('Leader H pressed')<cr>", { desc = "Lazy" })

--  Visual‑mode mapping:  <leader>x
--  Replace `my_command` below with whatever external program you need.
--  The entire visually‑selected text is passed as one argv entry.
vim.keymap.set("v", "<leader>H", function()

    msg('Leader H pressed')

  local arg = get_visual_selection()

  -- shell‑escape so it survives spaces, quotes, etc.
  -- local arg = vim.fn.shellescape(text)

  -- open a fresh vertical split
  vim.cmd("vert new | setlocal buftype=nofile bufhidden=wipe nobuflisted")

  -- !! put your program here !!
  local cmd = "Xinv-patch download  --force --filename /dev/stdout --patch-id " .. arg

    msg(arg)

    -- cmd = 'ls'

  -- run the command, insert its stdout at top of new split
  -- vim.cmd("0r !" .. cmd)
end, { desc = "Run external cmd on selection → vert split", silent = true })
-- c132daed-0177-4f87-b6a3-744d52436b55
