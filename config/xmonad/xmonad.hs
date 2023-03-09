{-# LANGUAGE ImportQualifiedPost #-}

import           XMonad
-- import XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP

import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Ungrab

import           XMonad.Layout.Magnifier
import           XMonad.Layout.ThreeColumns

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.UrgencyHook

import           Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet              as W

-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders

myStartupHook = do
  -- spawnOnce "exec feh --bg-scale /home/lucask/Pictures/wallpapers/redwood.jpg"
  -- spawnOnce "picom -f --config /home/lucask/.config/picom/picom.conf &"
  -- spawnOnce "feh --bg-scale ~/Pictures/wallpaper.jpg"
  spawn "trayer --monitor 0 --edge top --align right --width 15"
  spawn "picom"
  spawn "dropbox"
  spawn "nm-applet"
  spawn "pamac-tray"
  spawn "blueman-applet"
  spawn "xfce4-power-manager"
  spawn "volumeicon"
  spawn "killall udiskie; udiskie --tray"
  spawn "slack"
  spawn "discord"
  -- spawn "autorandr mobile; autorandr docked"


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . setEwmhActivateHook doAskUrgent
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
        -- , normalBorderColor=myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        } `additionalKeys` myAdditionalKeys


myWorkspaces = ["W1.0","W1.1","W1.2","W2.0","W2.1","W2.2","W3.0","W3.1","W3.2"] ++ (map snd myExtraWorkspaces) -- you can customize the names of the default workspaces by changing the list

myExtraWorkspaces = [(xK_0, "IM")] -- list of (key, name)

myAdditionalKeys =
    [
   ((0, xF86XK_PowerDown),         spawn "sudo systemctl suspend")
 , ((0, xF86XK_AudioRaiseVolume),  spawn "amixer -D pulse sset Master 10%+")
 , ((0, xF86XK_AudioLowerVolume),  spawn "amixer -D pulse sset Master 10%-")
 , ((0, xF86XK_AudioMute),         spawn "amixer -D pulse sset Master toggle")
 , ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +10%")
 , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")
   , ((mod4Mask, xK_F8), spawn "firefox-developer-edition")
    , ((mod4Mask, xK_F9), spawn "pcmanfm")
    , ((mod4Mask .|. shiftMask, xK_Return), spawn "wezterm")
     ] ++ [
        ((mod4Mask, key), (windows $ W.greedyView ws))
        | (key, ws) <- myExtraWorkspaces
    ] ++ [
        ((mod4Mask .|. shiftMask, key), (windows $ W.shift ws))
        | (key, ws) <- myExtraWorkspaces
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

myNormalBorderColor="#dddddd"
myFocusedBorderColor="#FFB53A"

desktops=4
desktop_panes=3

desktop_names = [ [ "W" ++  show x ++ show y ] | x <- [1..desktops] , y <- [1..desktop_panes]]
desktop_panel_keys = [ [x,y] | x <- [1..desktops], y <- [1..desktop_panes]]
