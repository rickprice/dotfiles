import XMonad
import XMonad.Config.Desktop

main = xmonad desktopConfig
    { terminal    = "wezterm"
    , modMask     = mod4Mask
    }
