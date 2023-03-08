import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders

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


myAdditionalKeys =
    [ ((mod4Mask, xK_F8), spawn "firefox-developer-edition")
    , ((mod4Mask, xK_F9), spawn "pcmanfm")
    , ((mod4Mask .|. shiftMask, xK_Return), spawn "wezterm")
     ]


main = xmonad $ ewmhFullscreen . ewmh $ docks def 
        { 
        terminal    = "wezterm"
        , modMask     = mod4Mask
        , layoutHook=avoidStruts $ smartBorders $ layoutHook def
        , manageHook=manageHook def <+> manageDocks
        , startupHook = myStartupHook
        } `additionalKeys` myAdditionalKeys 
