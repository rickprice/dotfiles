local wezterm = require 'wezterm';
return {
  enable_tab_bar=false,
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

        ansi = {"#929292","#e27373","#94b979","#ffba7b","#97bedc","#e1c0fa","#00988e","#dedede"},
        brights = {"#bdbdbd","#ffa1a1","#bddeab","#ffdca0","#b1d8f6","#fbdaff","#1ab2a8","#ffffff"},
        },

    font = wezterm.font_with_fallback({"Iosevka","Source Code Pro","Source Code Variable","PowerlineSymbols"}),
    font_size = 11.0,
    -- dpi = 96.0,
    bold_brightens_ansi_colors = true,
    -- font_dirs = {"/usr/share/fonts/TTF"},
    font_shaper = "Harfbuzz",
    harfbuzz_features = {"kern", "liga", "clig", "calt"},
    scrollback_lines = 10000,
}

