import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks

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

main=do 
    xmonad $ docks def 
        { 
        terminal    = "wezterm"
        , modMask     = mod4Mask
        , layoutHook=avoidStruts $ layoutHook def
        , manageHook=manageHook def <+> manageDocks
        , startupHook = myStartupHook
        } 
