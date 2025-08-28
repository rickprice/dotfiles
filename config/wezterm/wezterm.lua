local wezterm = require("wezterm")

local config = {
	enable_tab_bar = false,
	-- color_scheme="Adventure",
	-- color_scheme="Jellybeans",
	-- color_scheme="BuiltinDark",

	font_size = 10.0,
	initial_cols = 120,
	initial_rows = 70,
	-- dpi = 96.0,
	bold_brightens_ansi_colors = true,
	-- font_dirs = {"/usr/share/fonts/TTF"},
	font_shaper = "Harfbuzz",
	-- harfbuzz_features = { "kern", "liga", "clig", "calt", "zero"},
	harfbuzz_features = { "kern", "liga", "clig", "calt"},
	scrollback_lines = 10000,
}

-- Platform-specific colors and fonts
if wezterm.target_triple:find("darwin") then
	-- macOS - match Terminal.app default Basic theme
	config.colors = {
		foreground = "#000000",
		background = "#ffffff",
		cursor_bg = "#000000",
		cursor_border = "#000000",
		cursor_fg = "#ffffff",
		selection_bg = "#b3d4fc",
		selection_fg = "#000000",

		-- Standard macOS Terminal ANSI colors
		ansi = {
			"#000000", -- Black
			"#c23621", -- Red  
			"#25bc24", -- Green
			"#adad27", -- Yellow
			"#492ee1", -- Blue
			"#d338d3", -- Magenta
			"#33bbc8", -- Cyan
			"#cbcccd", -- White
		},
		brights = {
			"#818383", -- Bright Black (Gray)
			"#fc391f", -- Bright Red
			"#31e722", -- Bright Green  
			"#eaec23", -- Bright Yellow
			"#5833ff", -- Bright Blue
			"#f935f8", -- Bright Magenta
			"#14f0f0", -- Bright Cyan
			"#e9ebeb", -- Bright White
		},
	}
	-- macOS fonts with excellent ligature support
	config.font = wezterm.font_with_fallback({
		"JetBrains Mono",       -- Excellent ligatures, bundled with many IDEs
		"Fira Code",            -- Classic ligature font
		"SF Mono",              -- Apple's system monospace font
		"Menlo",                -- macOS default terminal font
		"Symbols Nerd Font Mono"
	})
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
	-- Other platforms - use original font configuration
	config.font = wezterm.font_with_fallback({ "Fira Code", "Symbols Nerd Font Mono"})
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
