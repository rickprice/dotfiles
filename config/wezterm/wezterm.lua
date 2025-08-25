local wezterm = require("wezterm")

local config = {
	enable_tab_bar = false,
	-- color_scheme="Adventure",
	-- color_scheme="Jellybeans",
	-- color_scheme="BuiltinDark",

	font = wezterm.font_with_fallback({ "Fira Code","Symbols Nerd Font Mono"}),
	font_size = 10.0,
	-- dpi = 96.0,
	bold_brightens_ansi_colors = true,
	-- font_dirs = {"/usr/share/fonts/TTF"},
	font_shaper = "Harfbuzz",
	-- harfbuzz_features = { "kern", "liga", "clig", "calt", "zero"},
	harfbuzz_features = { "kern", "liga", "clig", "calt"},
	scrollback_lines = 10000,
}

-- Platform-specific colors
if wezterm.target_triple:find("darwin") then
	-- macOS - white background
	config.colors = {
		foreground = "#333333",
		background = "#ffffff",
		cursor_bg = "#ff6600",
		cursor_border = "#ff6600",
		cursor_fg = "#ffffff",
		selection_bg = "#b3d4fc",
		selection_fg = "#000000",

		ansi = { "#000000", "#d70000", "#00d700", "#d7af00", "#0087d7", "#d700d7", "#00d7af", "#d7d7d7" },
		brights = { "#808080", "#ff5f5f", "#5fff5f", "#ffff5f", "#5f87ff", "#ff5fff", "#5fffff", "#ffffff" },
	}
else
	-- Other platforms - black background (original Jellybeans-based)
	config.colors = {
		foreground = "#dedede",
		background = "black",
		cursor_bg = "#ffa560",
		cursor_border = "#ffa560",
		cursor_fg = "#ffffff",
		selection_bg = "#474e91",
		selection_fg = "#f4f4f4",

		ansi = { "#929292", "#e27373", "#94b979", "#ffba7b", "#97bedc", "#e1c0fa", "#00988e", "#dedede" },
		brights = { "#bdbdbd", "#ffa1a1", "#bddeab", "#ffdca0", "#b1d8f6", "#fbdaff", "#1ab2a8", "#ffffff" },
	}
end

-- Event handler for zen mode
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
end)

return config
