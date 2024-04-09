local wezterm = require("wezterm")
return {
	enable_tab_bar = false,
	-- color_scheme="Adventure",
	-- color_scheme="Jellybeans",
	-- color_scheme="BuiltinDark",

	-- Based off Jellybeans, but with pure black background
	colors = {
		foreground = "#dedede",
		background = "black",
		cursor_bg = "#ffa560",
		cursor_border = "#ffa560",
		cursor_fg = "#ffffff",
		selection_bg = "#474e91",
		selection_fg = "#f4f4f4",

		ansi = { "#929292", "#e27373", "#94b979", "#ffba7b", "#97bedc", "#e1c0fa", "#00988e", "#dedede" },
		brights = { "#bdbdbd", "#ffa1a1", "#bddeab", "#ffdca0", "#b1d8f6", "#fbdaff", "#1ab2a8", "#ffffff" },
	},

	-- font = wezterm.font_with_fallback({ "Iosevka", "Source Code Pro", "Source Code Variable", "PowerlineSymbols" }),
	-- font = wezterm.font_with_fallback({ "Fira Code","Symbols Nerd Font Mono"}),
	font = wezterm.font_with_fallback({ "Source Code Pro","Symbols Nerd Font Mono"}),
	font_size = 10.0,
	-- dpi = 96.0,
	bold_brightens_ansi_colors = true,
	-- font_dirs = {"/usr/share/fonts/TTF"},
	font_shaper = "Harfbuzz",
	harfbuzz_features = { "kern", "liga", "clig", "calt", "zero"},
	scrollback_lines = 10000,

	-- keys = {
	--   -- copy to clipboard and primary selection
	--   { key = "C", mods = "SHIFT|ALT", action = wezterm.action({ CopyTo = "ClipboardAndPrimarySelection" }) },
	--   -- paste from the clipboard
	--   { key = "V", mods = "SHIFT|ALT", action = wezterm.action({ PasteFrom = "Clipboard" }) },
	-- },
	-- mouse_bindings = {
	--   -- Bind 'Up' event of CTRL-Click to open hyperlinks
	--   {
	--     event = { Up = { streak = 1, button = "Left" } },
	--     mods = "CTRL",
	--     action = "OpenLinkAtMouseCursor",
	--   },
	--   -- Disable the 'Down' event of CTRL-Click to avoid weird program behaviors
	--   {
	--     event = { Down = { streak = 1, button = "Left" } },
	--     mods = "CTRL",
	--     action = "Nop",
	--   },
	-- -- Bind 'Up' event of Double-Click to copy to clipboard and selection
	-- {
	--   event = { Up = { streak = 2, button = "Left" } },
	--   mods = "",
	--   action = "ClipboardAndPrimarySelection",
	-- },
	-- -- Disable the 'Down' event of CTRL-Click to avoid weird program behaviors
	-- {
	--   event = { Down = { streak = 2, button = "Left" } },
	--   mods = "",
	--   action = "Nop",
	-- },
	-- },

	wezterm.on("user-var-changed", function(window, pane, name, value)
		local overrides = window:get_config_overrides() or {}
		if name == "ZEN_MODE" then
			local incremental = value:find("+")
			local number_value = tonumber(value)
			if incremental ~= nil then
				while number_value > 0 do
					window:perform_action(wezterm.action.IncreaseFontSize, pane)
					number_value = number_value - 1
				end
				overrides.enable_tab_bar = false
			elseif number_value < 0 then
				window:perform_action(wezterm.action.ResetFontSize, pane)
				overrides.font_size = nil
				overrides.enable_tab_bar = true
			else
				overrides.font_size = number_value
				overrides.enable_tab_bar = false
			end
		end
		window:set_config_overrides(overrides)
	end),
}
