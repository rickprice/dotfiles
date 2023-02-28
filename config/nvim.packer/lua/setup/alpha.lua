-- Dashboard
local dashboard = require("alpha.themes.dashboard")
math.randomseed(os.time())

local function button(sc, txt, keybind, keybind_opts)
  local b = dashboard.button(sc, txt, keybind, keybind_opts)
  b.opts.hl = "Function"
  b.opts.hl_shortcut = "Type"
  return b
end

local function pick_color()
  local colors = { "String", "Identifier", "Keyword", "Number" }
  return colors[math.random(#colors)]
end

local function footer()
  local total_plugins = #vim.tbl_keys(packer_plugins)
  local datetime = os.date(" %d-%m-%Y   %H:%M:%S")
  return datetime
    .. "   "
    .. total_plugins
    .. " plugins"
    .. "   v"
    .. vim.version().major
    .. "."
    .. vim.version().minor
    .. "."
    .. vim.version().patch
end

dashboard.section.header.val = {
  -- https://manytools.org/hacker-tools/ascii-banner/
  -- Georgi16 font
  [[___      ___                 ____     ___                     ]],
  [[`MM\     `M'                 `Mb(     )d' 68b                 ]],
  [[ MMM\     M                   YM.     ,P  Y89                 ]],
  [[ M\MM\    M   ____     _____  `Mb     d'  ___ ___  __    __   ]],
  [[ M \MM\   M  6MMMMb   6MMMMMb  YM.   ,P   `MM `MM 6MMb  6MMb  ]],
  [[ M  \MM\  M 6M'  `Mb 6M'   `Mb `Mb   d'    MM  MM69 `MM69 `Mb ]],
  [[ M   \MM\ M MM    MM MM     MM  YM. ,P     MM  MM'   MM'   MM ]],
  [[ M    \MM\M MMMMMMMM MM     MM  `Mb d'     MM  MM    MM    MM ]],
  [[ M     \MMM MM       MM     MM   YM,P      MM  MM    MM    MM ]],
  [[ M      \MM YM    d9 YM.   ,M9   `MM'      MM  MM    MM    MM ]],
  [[_M_      \M  YMMMM9   YMMMMM9     YP      _MM__MM_  _MM_  _MM_]],
}
dashboard.section.header.opts.hl = pick_color()

dashboard.section.buttons.val = {
  button("<Leader>f", "  File Explorer"),
  button("<Leader>p", "  Find file"),
  button("<Leader>g", "  Find word"),
  button("<Leader>1", "  Open session"),
  button("<Leader>n", "  New file"),
  button("<Leader>v", "  Config"),
  button("<Leader>u", "  Update plugins"),
  button("q", "  Quit", "<Cmd>qa<CR>"),
}

dashboard.section.footer.val = footer()
dashboard.section.footer.opts.hl = "Constant"

require("alpha").setup(dashboard.opts)
