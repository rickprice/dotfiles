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

import           XMonad.Actions.SpawnOn
import qualified XMonad.StackSet              as W
import           XMonad.Util.SpawnOnce

-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders

myModMask = mod4Mask
myBrowser = "firefox-developer-edition"
myTerminal = "wezterm"
myFileManager = "pcmanfm"
myScanner = "simple-scan"
myEbookViewer = "ebook-viewer"


myStartupHook = do
  -- spawnOnce "exec feh --bg-scale /home/lucask/Pictures/wallpapers/redwood.jpg"
  -- spawnOnce "picom -f --config /home/lucask/.config/picom/picom.conf &"
  -- spawnOnce "feh --bg-scale ~/Pictures/wallpaper.jpg"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawn "killall trayer; trayer --monitor 0 --edge top --align right --width 15"
  spawnOnce "wired --run"
  spawnOnce "picom -b"
  spawnOnce "xfce4-power-manager"
  spawnOnce "dropbox"
  spawnOnce "nm-applet"
  spawnOnce "pamac-tray"
  spawnOnce "blueman-applet"
  spawnOnce "volumeicon"
  spawnOnce "killall udiskie; udiskie --tray"
  spawnOn "IM" "slack"
  spawnOn "IM" "discord"
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
        terminal    = myTerminal
        , modMask     = myModMask
        , layoutHook=myLayout
        , manageHook=myManageHook
        , startupHook = myStartupHook
        , normalBorderColor=myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        } `additionalKeys` myAdditionalKeys `additionalKeysP` myNewStyleKeys


myWorkspaces = workspaceNames ++ (map snd myExtraWorkspaces) -- you can customize the names of the default workspaces by changing the list

myExtraWorkspaces = [(xK_1, "IM"),(xK_2, "ZM")] -- list of (key, name)

myAdditionalKeys =
    [
   ((0, xF86XK_PowerDown),         spawn "sudo systemctl suspend")
 , ((0, xF86XK_AudioRaiseVolume),  spawn "amixer -D pulse sset Master 10%+")
 , ((0, xF86XK_AudioLowerVolume),  spawn "amixer -D pulse sset Master 10%-")
 , ((0, xF86XK_AudioMute),         spawn "amixer -D pulse sset Master toggle")
 , ((0, xF86XK_MonBrightnessUp),   spawn "brightnessctl set +10%")
 , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")
   , ((myModMask, xK_F8), spawn "firefox-developer-edition")
    , ((myModMask, xK_F9), spawn "pcmanfm")
    , ((myModMask .|. shiftMask, xK_Return), spawn "wezterm")
     ] ++ [
        ((myModMask, key), (windows $ W.greedyView ws))
        | (key, ws) <- myExtraWorkspaces
    ] ++ [
        ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
        | (key, ws) <- myExtraWorkspaces
    ]

myManageHook :: ManageHook
myManageHook = composeAll
    [
    manageSpawn
    , manageZoomHook
    , className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayout = threeCol ||| tiled ||| Mirror tiled ||| Full
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

desktops=6
desktop_panes=3
workspacePrefix = "W"
workspaceFocusKey = "M-w"
workspaceMoveKey = "M-S-w"

workspaceNames = map desktopNameFromTuple workspace_panel_tuples
workspaceShowDesktopKeys = map desktopShowDesktopKeymapFromTuple workspace_panel_tuples
workspaceMoveFocusedWindowKeys = map desktopMoveFocusedKeyFromTuple workspace_panel_tuples

workspace_panel_tuples = [ (x,y) | x <- [1..desktops], y <- [1..desktop_panes]]

desktopNameFromTuple = desktopNameFromTuple' workspacePrefix

desktopNameFromTuple' :: Show a => String -> (a,a) -> String
desktopNameFromTuple' p t = p ++ show  (fst t) ++ show (snd t)

desktopKeyMapFromTuple :: Show a => String -> (a,a) -> String
desktopKeyMapFromTuple p t = p ++ " " ++ show  (fst t) ++ " " ++ show (snd t)

showDesktop :: String -> X ()
showDesktop d = windows $ W.greedyView d

moveFocusedWindowToDesktop :: String -> X ()
moveFocusedWindowToDesktop d = windows $ W.shift d

desktopShowDesktopKeymapFromTuple t = (desktopKeyMapFromTuple workspaceFocusKey t, showDesktop (desktopNameFromTuple t))

desktopMoveFocusedKeyFromTuple t = (desktopKeyMapFromTuple workspaceMoveKey t, moveFocusedWindowToDesktop (desktopNameFromTuple t))

myNewStyleKeys =
    workspaceShowDesktopKeys
    ++ workspaceMoveFocusedWindowKeys
    ++ [
        ("M-a b", spawn myBrowser)
        , ("M-a f", spawn myFileManager)
        , ("M-S-<Enter>", spawn myTerminal)
        , ("M-a s", spawn myScanner)
        , ("M-a e", spawn myEbookViewer)
        ]
    -- ++ [("M-w 9 8", showDesktop "W13")]
    -- ++ [("M-w 9 9", spawn "firefox"       )]


manageZoomHook =
  composeAll $
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

