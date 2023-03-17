{-# LANGUAGE ImportQualifiedPost #-}

import           XMonad
-- import XMonad.Config.Desktop
import           XMonad.Config.Desktop        (desktopConfig,
                                               desktopLayoutModifiers)
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
import           XMonad.Hooks.UrgencyHook     (NoUrgencyHook (NoUrgencyHook),
                                               withUrgencyHook)

import           Graphics.X11.ExtraTypes.XF86

import           XMonad.Actions.SpawnOn
import qualified XMonad.StackSet              as W
import           XMonad.Util.SpawnOnce

-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders
import           XMonad.Layout.NoBorders      (noBorders, smartBorders)
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import           XMonad.Layout.ToggleLayouts

myModMask = mod4Mask
myBrowser = "firefox-developer-edition"
myTerminal = "wezterm"
myFileManager = "pcmanfm"
myScanner = "simple-scan"
myEbookViewer = "ebook-viewer"
myBackgrounds = "~/Documents/Personal/Dropbox/FrederickDocuments/Backgrounds/"
mySystemMonitor = "gnome-system-monitor"
myDMenu = "dmenu-frecency"
myDarkTable = "darktable"
myExtraWorkspaces = ["IM","ZM", "ADM","DOC", "TP", "FP1","FP2"]
myCustomKeys = [
         ("M-f", sendMessage ToggleLayout)
        , ("M-S-<Enter>", spawn myTerminal)
        , ("M-a b", spawn myBrowser)
        , ("M-a d", spawn myDarkTable)
        , ("M-a e", spawn myEbookViewer)
        , ("M-a f", spawn myFileManager)
        , ("M-a m", spawn mySystemMonitor)
        , ("M-a s", spawn myScanner)

        , ("M-1", showDesktop "IM")
        , ("M-S-1", moveFocusedWindowToDesktop "IM")

        , ("M-2", showDesktop "ZM")
        , ("M-S-2", moveFocusedWindowToDesktop "ZM")

        , ("M-w d 1", showDesktop "DOC")
        , ("M-S-w d 1", moveFocusedWindowToDesktop "DOC")

        , ("M-3", showDesktop "ADM")
        , ("M-S-3", moveFocusedWindowToDesktop "ADM")

        , ("M-w t 1", showDesktop "TP")
        , ("M-S-w t 1", moveFocusedWindowToDesktop "TP")

        , ("M-w f 1", showDesktop "FP1")
        , ("M-S-w f 1", moveFocusedWindowToDesktop "FP1")
        , ("M-w f 2", showDesktop "FP2")
        , ("M-S-w f 2", moveFocusedWindowToDesktop "FP2")

        , ("M-p", spawn myDMenu)
        ]
    -- ++ [("M-w 9 8", showDesktop "W13")]
    -- ++ [("M-w 9 9", spawn "firefox"       )]

myStartupHook = do
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawn "killall trayer; trayer --monitor 0 --edge top --align right --width 10"
  spawnOnce "wired --run"
  spawnOnce "picom -b"
  spawnOnce "xfce4-power-manager"
  spawnOnce "dropbox"
  spawnOnce "nm-applet"
  spawnOnce "pamac-tray"
  spawnOnce "blueman-applet"
  spawnOnce "volumeicon"
  spawnOnce "killall udiskie; udiskie --tray"
  spawn ("feh --no-fehbg --bg-max --random " ++ myBackgrounds ++ " " ++ myBackgrounds)
  spawnOn "IM" "slack"
  spawnOn "IM" "discord"
  -- spawn "autorandr mobile; autorandr docked"


main :: IO ()
main = xmonad
     . ewmh
     . ewmhFullscreen
     . withUrgencyHook NoUrgencyHook -- no popups only bar
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
        {
        terminal    = myTerminal
        , modMask     = myModMask
        , layoutHook= smartBorders $ desktopLayoutModifiers $ myLayouts
        , manageHook=myManageHook
        , startupHook = myStartupHook
        , normalBorderColor=myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        } `additionalKeysP` myNewStyleKeys


myWorkspaces = workspaceNames ++ myExtraWorkspaces

myManageHook :: ManageHook
myManageHook = composeAll
    [
    manageSpawn
    , manageZoomHook
    , className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

myLayouts = toggleLayouts ( noBorders Full) (threeCol ||| tiled ||| Mirror tiled ||| Full)
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
    , ppOrder           = \[ws, l, _, _] -> [ws, l]
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
    ++ myCustomKeys


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

