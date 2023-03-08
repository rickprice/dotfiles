import XMonad
-- import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders

myStartupHook = do
  -- spawnOnce "exec feh --bg-scale /home/lucask/Pictures/wallpapers/redwood.jpg"
  -- spawnOnce "picom -f --config /home/lucask/.config/picom/picom.conf &"
  -- spawnOnce "feh --bg-scale ~/Pictures/wallpaper.jpg"
  spawn "stalonetray &"
  spawn "picom &"
  spawn "dropbox &"
  spawn "nm-applet &"
  spawn "pamac-tray &"
  spawn "blueman-tray &"
  spawn "xfce4-power-manager &"
  spawn "volumeicon &"
  spawn "killall udiskie; udiskie --tray &"
  spawn "slack &"
  spawn "discord &"
  spawn "autorandr mobile; autorandr docked"


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
        {
        terminal    = "wezterm"
        , modMask     = mod4Mask
        , layoutHook=myLayout
        , manageHook=myManageHook
        , startupHook = myStartupHook
        } `additionalKeysP` myAdditionalKeys


-- myAdditionalKeys =
--     [ ((mod4Mask, xK_F8), spawn "firefox-developer-edition")
--     , ((mod4Mask, xK_F9), spawn "pcmanfm")
--     , ((mod4Mask .|. shiftMask, xK_Return), spawn "wezterm")
--      ]
myAdditionalKeys =
    [ ("M-F8", spawn "firefox-developer-edition")
    , ("M-F9", spawn "pcmanfm")
    , ("M-S-Return", spawn "wezterm")
     ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
