{-# LANGUAGE ImportQualifiedPost #-}

import Data.Ratio

-- import XMonad.Config.Desktop

import Control.Concurrent

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.SpawnOn
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
import XMonad.Util.Ungrab

-- ScratchPads
import XMonad.Util.NamedScratchpad

-- import XMonad.Util.Run

myModMask = mod4Mask

myBrowser = "firefox-developer-edition"

myBrowserNyxt = "nyxt --no-socket"

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

myExtraWorkspaces = ["IM", "ZM", "ADM", "DOC", "NSP", "TP1", "TP2", "FP1", "FP2", "FP3"]

myRunBackgrounds = "feh --no-fehbg --bg-max --random " ++ myBackgrounds

myFixScreens = "autorandr --change"

myCustomKeys =
    [ ("M-f", sendMessage ToggleLayout)
    , ("M-S-<Enter>", spawn myTerminal)
    , ("M-a b", spawn myBrowser)
    , ("M-a n", spawn myBrowserNyxt)
    , ("M-a d", spawn (myDarkTable ++ " --library " ++ myDarkTablePersonalLibrary))
    , ("M-a S-d", spawn (myDarkTable ++ " --library " ++ myDarkTableCommercialLibrary))
    , ("M-a i", spawn myInkScape)
    , ("M-a e", spawn myEbookViewer)
    , ("M-a f", spawn myFileManager)
    , ("M-a m", spawn mySystemMonitor) -- performance monitor
    , ("M-a s", spawn myScanner)
    , ("M-a c", spawn myCalculator)
    , ("M-a r", spawn myRDPClient)
    , ("M-a w", setupWorkWindow)
    , ("M-a z", fixScreens)
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
    , (workspaceFocusKey ++ "t 1", showDesktop "TP1")
    , (workspaceMoveKey ++ "t 1", moveFocusedWindowToDesktop "TP1")
    , (workspaceFocusKey ++ "t 2", showDesktop "TP2")
    , (workspaceMoveKey ++ "t 2", moveFocusedWindowToDesktop "TP2")
    , (workspaceFocusKey ++ "f 1", showDesktop "FP1")
    , (workspaceMoveKey ++ "f 1", moveFocusedWindowToDesktop "FP1")
    , (workspaceFocusKey ++ "f 2", showDesktop "FP2")
    , (workspaceMoveKey ++ "f 2", moveFocusedWindowToDesktop "FP2")
    , (workspaceFocusKey ++ "f 3", showDesktop "FP3")
    , (workspaceMoveKey ++ "f 3", moveFocusedWindowToDesktop "FP3")
    -- , ("M-p", spawn myDMenu)
    -- Dynamic ScratchPads
    , ("M-S-[", withFocused $ toggleDynamicNSP "dyn1")
    , ("M-S-]", withFocused $ toggleDynamicNSP "dyn2")
    , ("M-["  , dynamicNSPAction "dyn1")
    , ("M-]"  , dynamicNSPAction "dyn2")
    ]

setupWorkWindow = do
    spawnHere myBrowser
    spawnHere myBrowser
    liftIO (threadDelay 5000000)
    spawnHere myTerminal
    spawnHere myTerminal

fixScreens = do
    spawn myFixScreens
    liftIO (threadDelay 7000000)
    spawn myRunBackgrounds

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
    spawnOnce "xfce4-power-manager"
    -- spawnOnce "dropbox"
    spawnOnce "nm-applet"
    spawnOnce "pamac-tray"
    spawnOnce "blueman-applet"
    spawn "killall volumeicon; sleep 15; volumeicon"
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
        }
        `additionalKeysP` myNewStyleKeys

myWorkspaces = workspaceNames ++ myExtraWorkspaces

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ manageSpawn
        , manageZoomHook
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

desktops = 3

desktop_panes = 3

workspacePrefix = "W"

workspaceFocusKey = "M-d "

workspaceMoveKey = "M-S-d "

workspaceNames = map desktopNameFromTuple workspace_panel_tuples

workspaceShowDesktopKeys = map desktopShowDesktopKeymapFromTuple workspace_panel_tuples

workspaceMoveFocusedWindowKeys = map desktopMoveFocusedKeyFromTuple workspace_panel_tuples

workspace_panel_tuples = [(x, y) | x <- [1 .. desktops], y <- [1 .. desktop_panes]]

desktopNameFromTuple = desktopNameFromTuple' workspacePrefix

desktopNameFromTuple' :: Show a => String -> (a, a) -> String
desktopNameFromTuple' p t = p ++ show (fst t) ++ show (snd t)

desktopKeyMapFromTuple :: Show a => String -> (a, a) -> String
desktopKeyMapFromTuple p t = p ++ " " ++ show (fst t) ++ " " ++ show (snd t)

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
        ++ warpMouseKeys

manageZoomHook =
    composeAll $
        [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat
        , (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
        ]
  where
    zoomClassName = "zoom"
    tileTitles =
        [ "Zoom - Free Account" -- main window
        , "Zoom - Licensed Account" -- main window
        , "Zoom" -- meeting window on creation
        , "Zoom Meeting" -- meeting window shortly after creation
        , "Zoom Cloud Meetings" -- The window they create after the Zoom meeting closes
        , "Meeting Chat" -- Meeting chat window
        ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown
