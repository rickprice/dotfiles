{-# LANGUAGE ImportQualifiedPost #-}

import Data.Ratio

-- import XMonad.Config.Desktop

import Control.Concurrent

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Config.Desktop (
    desktopConfig,
    desktopLayoutModifiers,
 )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook (
    NoUrgencyHook (NoUrgencyHook),
    withUrgencyHook,
 )
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiColumns

-- import XMonad.Layout.Named
-- import XMonad.Layout.NoBorders

import XMonad.Layout.GridVariants
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ThreeColumns

-- import XMonad.Layout.Grid
-- import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.ToggleLayouts
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

-- ScratchPads
import XMonad.Util.NamedScratchpad

-- import XMonad.Util.Run

myModMask = mod4Mask

myBrowser = "firefox-developer-edition"

myBrowserNyxt = "nyxt --no-socket"

myAudioManager = "pavucontrol"

myTerminal = "wezterm"

myFileManager = "pcmanfm"

myScanner = "simple-scan"

myEbookViewer = "ebook-viewer"

myBackgrounds = "~/Documents/Personal/Dropbox/FrederickDocuments/Backgrounds/"

mySystemMonitor = "gnome-system-monitor"

myDMenu = "dmenu-frecency"

myDarkTable = "darktable"
myDarkTablePersonalLibrary = "~/Documents/Personal/DarktablePersonal/library.db"
myDarkTableCommercialLibrary = "~/Documents/Personal/DarktableCommercial/library.db"

myInkScape = "inkscape"

myCalculator = "gnome-calculator"

myRDPClient = "remmina"

myExtraWorkspaces = ["IM", "ZM", "ADM", "DOC", "NSP"]

myRunBackgrounds = "feh --no-fehbg --bg-max --random " ++ myBackgrounds

myFixScreens = "autorandr --change"

myCustomKeys =
    [ ("M-f", sendMessage ToggleLayout)
    , ("M-S-<Enter>", spawn myTerminal)
    , (appRunKey ++ "b", spawn myBrowser)
    , (appRunKey ++ "n", spawn myBrowserNyxt)
    , (appRunKey ++ "d", spawn (myDarkTable ++ " --library " ++ myDarkTablePersonalLibrary))
    , (appRunKey ++ "S-d", spawn (myDarkTable ++ " --library " ++ myDarkTableCommercialLibrary))
    , (appRunKey ++ "i", spawn myInkScape)
    , (appRunKey ++ "e", spawn myEbookViewer)
    , (appRunKey ++ "f", spawn myFileManager)
    , (appRunKey ++ "m", spawn mySystemMonitor) -- performance monitor
    , (appRunKey ++ "s", spawn myScanner)
    , (appRunKey ++ "c", spawn myCalculator)
    , (appRunKey ++ "r", spawn myRDPClient)
    , (appRunKey ++ "w", setupWorkWindow)
    , (appRunKey ++ "z", fixScreens)
    , ("M-1", showDesktop "W11")
    , ("M-S-1", moveFocusedWindowToDesktop "W11")
    , ("M-2", showDesktop "IM")
    , ("M-S-2", moveFocusedWindowToDesktop "IM")
    , ("M-3", showDesktop "ZM")
    , ("M-S-3", moveFocusedWindowToDesktop "ZM")
    , (workspaceFocusKey ++ "d 1", showDesktop "DOC")
    , (workspaceMoveKey ++ "d 1", moveFocusedWindowToDesktop "DOC")
    , ("M-4", showDesktop "ADM")
    , ("M-S-4", moveFocusedWindowToDesktop "ADM")
    , ("M-5", showDesktop "NSP")
    , ("M-S-5", moveFocusedWindowToDesktop "NSP")
    , -- , ("M-p", spawn myDMenu)
      -- Dynamic ScratchPads
      ("M-S-[", withFocused $ toggleDynamicNSP "dyn1")
    , ("M-S-]", withFocused $ toggleDynamicNSP "dyn2")
    , ("M-[", dynamicNSPAction "dyn1")
    , ("M-]", dynamicNSPAction "dyn2")
    ]

setupWorkWindow = do
    spawnHere myBrowser
    spawnHere myBrowser
    liftIO (threadDelay 5000000)
    spawnHere myTerminal
    spawnHere myTerminal

fixScreens = do
    spawn myFixScreens

-- liftIO (threadDelay 7000000)
-- This is now handled by a script in autorandr
-- spawn myRunBackgrounds

warpMouseKeys =
    [ ("M-C-w", warpToScreen 0 (1 % 2) (1 % 2))
    , ("M-C-e", warpToScreen 1 (1 % 2) (1 % 2))
    , ("M-C-r", warpToScreen 2 (1 % 2) (1 % 2))
    ]

myStartupHook = do
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawn "killall trayer; sleep 10; trayer --monitor primary --edge top --align right --width 15"
    spawnOnce "wired --run"
    spawnOnce "picom -b"
    -- spawnOnce "dropbox"
    spawnOnce "nm-applet"
    spawnOnce "pamac-tray"
    spawnOnce "blueman-applet"
    spawn "killall volumeicon; sleep 15; volumeicon"
    -- spawnOnce "cbatticon"
    spawnOnce "xfce4-power-manager"
    spawnOnce "meteo-qt"
    spawnOnce "killall udiskie; udiskie --tray"
    fixScreens
    -- Setup initial work window
    spawnOn "ADM" myBrowser
    liftIO (threadDelay 7000000)
    -- Setup IM programs
    spawnOn "IM" "slack"
    liftIO (threadDelay 7000000)
    spawnOn "IM" "discord"

main :: IO ()
main =
    xmonad
        . ewmh
        . ewmhFullscreen
        . withUrgencyHook NoUrgencyHook -- no popups only bar
        . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
        $ myConfig

myConfig =
    def
        { terminal = myTerminal
        , modMask = myModMask
        , layoutHook = smartBorders $ desktopLayoutModifiers $ myLayouts
        , manageHook = myManageHook
        , startupHook = myStartupHook
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspaces
        , logHook = updatePointer (0.5, 0.5) (0, 0)
        }
        `additionalKeysP` myNewStyleKeys

myWorkspaces = asWorkspaces ++ myExtraWorkspaces ++ tWorkspaces ++ fWorkspaces

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ manageSpawn
        , -- , manageZoomHook
          className =? "zoom" --> doSink
        , className =? "Gimp" --> doFloat
        , className =? "meteo-qt" --> doFloat
        , isDialog --> doFloat
        ]

myLayouts = toggleLayouts (noBorders Full) (smartBorders (mainGrid ||| magnifier mainGrid ||| multiColumn))
  where
    magnifier = magnifiercz 1.4

    orientation = XMonad.Layout.GridVariants.L
    masterRows = 2
    masterColumns = 2
    masterPortion = (2 / 3)
    slaveAspectRatio = (16 / 10)
    resizeIncrement = (5 / 100)

    mainGrid = SplitGrid orientation masterRows masterColumns masterPortion slaveAspectRatio resizeIncrement
    -- mirrorTall = Mirror (Tall 1 (3 / 100) (3 / 5))
    multiColumn = multiCol [1] 1 0.01 (-0.5)

myXmobarPP :: PP
myXmobarPP =
    def
        { ppSep = magenta " • "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
        , ppHidden = white . wrap " " ""
        , ppHiddenNoWindows = lowWhite . wrap " " ""
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppOrder = \[ws, l, _, _] -> [ws, l]
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myNormalBorderColor = "#dddddd"

myFocusedBorderColor = "#FFB53A"

workspaceFocusKey = "M-d "
workspaceMoveKey = "M-S-d "

appRunKey = "M-a "

workspace_panel_tuples desktops 1 = [(x, Nothing) | x <- [1 .. desktops]]
workspace_panel_tuples desktops desktop_panes = [(x, Just y) | x <- [1 .. desktops], y <- [1 .. desktop_panes]]

workspaceNames workspacePrefix desktops desktop_panes = map (desktopNameFromTuple workspacePrefix) (workspace_panel_tuples desktops desktop_panes)
workspaceKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes ++ workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes

workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = map (desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspace_panel_tuples desktops desktop_panes)

workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = map (desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspace_panel_tuples desktops desktop_panes)

desktopNameFromTuple :: Show a => String -> (a, Maybe a) -> String
desktopNameFromTuple workspacePrefix = desktopNameFromTuple' workspacePrefix

desktopNameFromTuple' :: Show a => String -> (a, Maybe a) -> String
desktopNameFromTuple' p (x, Nothing) = p ++ show x
desktopNameFromTuple' p (x, Just y) = p ++ show x ++ show y

desktopKeyMapFromTuple :: Show a => String -> (a, Maybe a) -> String
desktopKeyMapFromTuple p (x, Nothing) = p ++ " " ++ show x
desktopKeyMapFromTuple p (x, Just y) = p ++ " " ++ show x ++ " " ++ show y

showDesktop :: String -> X ()
showDesktop d = windows $ W.greedyView d

moveFocusedWindowToDesktop :: String -> X ()
moveFocusedWindowToDesktop d = windows $ W.shift d

desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (desktopKeyMapFromTuple (workspaceFocusKey ++ workspaceKeyPrefix) t, showDesktop ((desktopNameFromTuple workspaceWindowPrefix) t))

desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (desktopKeyMapFromTuple (workspaceMoveKey ++ workspaceKeyPrefix) t, moveFocusedWindowToDesktop ((desktopNameFromTuple workspaceWindowPrefix) t))

-- ActiveState workspaces
asWorkspaceDisplayPrefix = "W"
asWorkspaceKeyPrefix = ""
asDesktops = 4
asDesktop_panes = 2
asWorkspaces = workspaceNames asWorkspaceDisplayPrefix asDesktops asDesktop_panes
asWorkspaceKeys = workspaceKeys asWorkspaceKeyPrefix asWorkspaceDisplayPrefix asDesktops asDesktop_panes

-- Tamara workspaces
tWorkspaceDisplayPrefix = "TP"
tWorkspaceKeyPrefix = "t"
tDesktops = 2
tDesktop_panes = 1
tWorkspaces = workspaceNames tWorkspaceDisplayPrefix tDesktops tDesktop_panes
tWorkspaceKeys = workspaceKeys tWorkspaceKeyPrefix tWorkspaceDisplayPrefix tDesktops tDesktop_panes

-- Tamara workspaces
fWorkspaceDisplayPrefix = "FP"
fWorkspaceKeyPrefix = "f"
fDesktops = 5
fDesktop_panes = 1
fWorkspaces = workspaceNames fWorkspaceDisplayPrefix fDesktops fDesktop_panes
fWorkspaceKeys = workspaceKeys fWorkspaceKeyPrefix fWorkspaceDisplayPrefix fDesktops fDesktop_panes

myNewStyleKeys =
    asWorkspaceKeys
        ++ tWorkspaceKeys
        ++ fWorkspaceKeys
        ++ myCustomKeys
        ++ warpMouseKeys

-- manageZoomHook =
--     composeAll $
--         [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat
--         , (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
--         ]
--   where
--     zoomClassName = "zoom"
--     tileTitles =
--         [ "Zoom - Free Account" -- main window
--         , "Zoom - Licensed Account" -- main window
--         , "Zoom" -- meeting window on creation
--         , "Zoom Meeting" -- meeting window shortly after creation
--         , "Zoom Cloud Meetings" -- The window they create after the Zoom meeting closes
--         , "Meeting Chat" -- Meeting chat window
--         ]
--     shouldFloat title = title `notElem` tileTitles
--     shouldSink title = title `elem` tileTitles
--     doSink = (ask >>= doF . W.sink) <+> doF W.swapDown
