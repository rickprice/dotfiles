{-# LANGUAGE ImportQualifiedPost #-}

import Data.Ratio

-- import XMonad.Config.Desktop

import Control.Concurrent

import Network.HostName (getHostName)

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
import XMonad.Hooks.UrgencyHook
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

-- Workspace Groups
import XMonad.Actions.DynamicWorkspaceGroups as ADWG
import qualified XMonad.Layout.IndependentScreens as LIS

-- import XMonad.Util.Run

myModMask = mod4Mask

-- Hostnames
hostnameWork = "fwork"
hostnameDAW = "daw"

-- Programs we run frequently
myBrowser = "firefox-developer-edition"
myEmailer = "thunderbird"
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
myRunBackgrounds = "feh --no-fehbg --bg-max --random " ++ myBackgrounds
myFixScreens = "autorandr --change"
myArdour = "ardour8"

-- Define extra workspaces that I use all the time, by hostname
myExtraWorkspaces hostname | hostname == hostnameWork = ["IM", "MAIL", "ADM", "SCRATCH", "ZM", "DOC", "NSP"]
myExtraWorkspaces _ = ["SCRATCH", "DOC", "NSP"]

spawnKey key program = (appRunKey ++ key, spawn program)

workspaceKeys key desktop = [(workspaceFocusKey ++ key, showDesktop desktop), (workspaceMoveKey ++ key, moveFocusedWindowToDesktop desktop)]

dynamicScratchPadKeys key scratchPadName = [("M-S-" ++ key, withFocused $ toggleDynamicNSP scratchPadName), ("M-" ++ key, dynamicNSPAction scratchPadName) ]

dynamicWorkspaceGroupKeys key viewGroup = [("M-" ++ key, ADWG.viewWSGroup viewGroup), ("M-S-" ++ key, ADWG.addCurrentWSGroup viewGroup)]

viewGroupKeys key viewGroup = [("M-s w " ++ key , ADWG.viewWSGroup viewGroup)]

myCustomKeys hostname =
    [ ("M-f", sendMessage ToggleLayout)
    , ("M-S-<Enter>", spawn myTerminal)
    , spawnKey "b" myBrowser
    , spawnKey "d" (myDarkTable ++ " --library " ++ myDarkTablePersonalLibrary)
    , spawnKey "S-d" (myDarkTable ++ " --library " ++ myDarkTableCommercialLibrary)
    , spawnKey "i" myInkScape
    , spawnKey "e" myEbookViewer
    , spawnKey "f" myFileManager
    , spawnKey "m" mySystemMonitor -- performance monitor
    , spawnKey "s" myScanner
    , spawnKey "c" myCalculator
    , spawnKey "r" myRDPClient
    , spawnKey "a" myArdour
    , spawnKey "z" myFixScreens

    -- Handle powerkeys
    , ("M-1", powerkeys 1 hostname)
    , ("M-2", powerkeys 2 hostname)
    , ("M-3", powerkeys 3 hostname)
    , ("M-4", powerkeys 4 hostname)
    , ("M-5", powerkeys 5 hostname)
    , ("M-6", powerkeys 6 hostname)
    , ("M-7", powerkeys 7 hostname)
    , ("M-8", powerkeys 8 hostname)

    , ("M-S-1", moveFocusedWindowToDesktop "W11")
    , ("M-S-2", moveFocusedWindowToDesktop "IM")
    , ("M-S-3", moveFocusedWindowToDesktop "MAIL")
    , ("M-S-4", moveFocusedWindowToDesktop "ADM")
    , ("M-S-5", moveFocusedWindowToDesktop "SCRATCH")
    , ("M-S-6", moveFocusedWindowToDesktop "ZM")
    , ("M-S-7", moveFocusedWindowToDesktop "NSP")
    ]

    ++ workspaceKeys "i" "IM"
    ++ workspaceKeys "m" "MAIL"
    ++ workspaceKeys "d" "DOC"
    ++ workspaceKeys "s" "SCRATCH"
    ++ workspaceKeys "z" "ZM"
    ++ workspaceKeys "n" "NSP"

    -- Dynamic ScratchPads
    ++ dynamicScratchPadKeys "[" "dyn1"
    ++ dynamicScratchPadKeys "]" "dyn2"

    ++ dynamicWorkspaceGroupKeys "/" "modslash"

    ++ viewGroupKeys "w 1" "Work1"
    ++ viewGroupKeys "w 2" "Work2"
    ++ viewGroupKeys "w 3" "Work3"

    ++ viewGroupKeys "f 1" "Frederick1"
    ++ viewGroupKeys "f 2" "Frederick2"
    ++ viewGroupKeys "f 3" "Frederick3"

    ++ viewGroupKeys "t 1" "Tamara1"
    ++ viewGroupKeys "t 2" "Tamara2"

    ++ viewGroupKeys "z 1" "Zoom"
    ++ viewGroupKeys "z 2" "Zoom2"

    -- ++ [
    -- Dynamic Workspace Groups
    -- , ("M-y n", ADWG.promptWSGroupAdd myXPConfig "Name this group: ")
    -- , ("M-y g", ADWG.promptWSGroupView myXPConfig "Go to group: ")
    -- , ("M-y d", ADWG.promptWSGroupForget myXPConfig "Forget group: ")]
    -- mod-/ and mod-? %! Jump to or memorize a workspace group

setupWorkWindow = do
    spawnHere myBrowser
    spawnHere myBrowser
    liftIO (threadDelay 5000000)
    spawnHere myTerminal
    spawnHere myTerminal

-- liftIO (threadDelay 7000000)
-- This is now handled by a script in autorandr
-- spawn myRunBackgrounds

warpMouseKeys =
    [ ("M-C-w", warpToScreen 0 (1 % 2) (1 % 2))
    , ("M-C-e", warpToScreen 1 (1 % 2) (1 % 2))
    , ("M-C-r", warpToScreen 2 (1 % 2) (1 % 2))
    ]

myStartupHook  hostname= do
    setupWorkspaceGroups hostname
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawn "killall trayer; sleep 10; trayer --monitor primary --edge top --align right --width 10"
    spawnOnce "wired --run"
    spawnOnce "picom -b"
    spawnOnce "nm-applet"
    spawnOnce "pamac-tray"
    spawnOnce "blueman-applet"
    spawn "killall volumeicon; sleep 15; volumeicon"
    -- spawnOnce "cbatticon"
    spawnOnce "xfce4-power-manager"
    spawnOnce "killall udiskie; udiskie --tray"
    -- spawnOnce "qmidinet -n 6"
    spawn myFixScreens
    if hostname == hostnameWork
        then
            do
                spawnOnce "meteo-qt"
                -- Setup initial work window
                spawnOn "MAIL" myEmailer
                -- liftIO (threadDelay 7000000)
                -- Setup IM programs
                spawnOn "IM" "slack"
                -- liftIO (threadDelay 7000000)
                spawnOn "IM" "discord"
                -- Browser
                -- liftIO (threadDelay 7000000)
                spawnOn "ADM" myBrowser
        else
            do
                spawnOn "FP12" myArdour


main :: IO ()
main = do
    hostname <- getHostName
    xmonad $ withUrgencyHook NoUrgencyHook
        $ setEwmhActivateHook doAskUrgent
        . ewmh
        . ewmhFullscreen
        . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
        $ createMyConfig hostname


createMyConfig hostname = 
            def
                { terminal = myTerminal
                , modMask = myModMask
                , layoutHook = smartBorders $ desktopLayoutModifiers myLayouts
                , manageHook = myManageHook
                , startupHook = myStartupHook hostname
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , workspaces = myWorkspaces hostname
                , logHook = updatePointer (0.5, 0.5) (0, 0)
                }
                `additionalKeysP` myNewStyleKeys hostname

myWorkspaces hostname | hostname == hostnameWork = asWorkspaces ++ myExtraWorkspaces hostname ++ tWorkspaces ++ fWorkspaces
myWorkspaces hostname = fWorkspaces ++ myExtraWorkspaces hostname



myManageHook :: ManageHook
myManageHook =
    composeAll
        [ manageSpawn
        -- , ewmhDesktopsManageHook
        -- , manageZoomHook
        -- , className =? "zoom" --> doSink
        , className =? "simple-scan" --> doSink
        , className =? "zoom" --> doShift "ZM"
        , className =? "Gimp" --> doFloat
        , className =? "meteo-qt" --> doFloat
        , className =? "discord" --> doShift "IM"
        , className =? "Slack" --> doShift "IM"
        , className =? "thunderbird" --> doShift "MAIL"
        , isDialog --> doFloat
        ]

myLayouts = toggleLayouts (noBorders Full) (smartBorders (multiColumn ||| mainGrid ||| magnifier mainGrid ))
  where
    magnifier = magnifiercz 1.4

    orientation = XMonad.Layout.GridVariants.L
    masterRows = 2
    masterColumns = 2
    masterPortion = 2 / 3
    slaveAspectRatio = 16 / 10
    resizeIncrement = 5 / 100

    mainGrid = SplitGrid orientation masterRows masterColumns masterPortion slaveAspectRatio resizeIncrement
    -- mirrorTall = Mirror (Tall 1 (3 / 100) (3 / 5))
    multiColumn = multiCol [1] 1 0.01 (-0.5)

myXmobarPP :: PP
myXmobarPP =
    def
        { ppSep = magenta " • "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
        -- , ppHidden = white . wrap " " ""
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

workspacePanelTuples desktops 1 = [(x, Nothing) | x <- [1 .. desktops]]
workspacePanelTuples desktops desktop_panes = [(x, Just y) | x <- [1 .. desktops], y <- [1 .. desktop_panes]]

workspaceNames workspacePrefix desktops desktop_panes = map (desktopNameFromTuple workspacePrefix) (workspacePanelTuples desktops desktop_panes)
wsKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes ++ workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes

workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = map (desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspacePanelTuples desktops desktop_panes)

workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes = map (desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspacePanelTuples desktops desktop_panes)

desktopNameFromTuple :: Show a => String -> (a, Maybe a) -> String
desktopNameFromTuple p (x, Nothing) = p ++ show x
desktopNameFromTuple p (x, Just y) = p ++ show x ++ show y

fixPrefix Nothing = ""
fixPrefix (Just p) = p ++ " "

-- desktopKeyMapFromTuple :: Show a => String -> (Maybe a, Maybe a) -> String
desktopKeyMapFromTuple p (x, Nothing) = fixPrefix p ++ show x
desktopKeyMapFromTuple p (x, Just y) = fixPrefix p ++ show x ++ " " ++ show y

showDesktop :: String -> X ()
showDesktop d = windows $ W.greedyView d

moveFocusedWindowToDesktop :: String -> X ()
moveFocusedWindowToDesktop d = windows $ W.shift d

-- calculateKeymap workspaceKeyPrefix workspaceWindowPrefix metaPrefix function = (metaPrefix ++ desktopKeyMapFromTuple workspaceKeyPrefix t, function ((desktopNameFromTuple workspaceWindowPrefix) t))

desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (workspaceFocusKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, showDesktop (desktopNameFromTuple workspaceWindowPrefix t))

desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (workspaceMoveKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, moveFocusedWindowToDesktop (desktopNameFromTuple workspaceWindowPrefix t))

-- ActiveState workspaces
asWorkspaceDisplayPrefix = "W"
asWorkspaceKeyPrefix = Nothing
asDesktops = 3
asDesktopPanes = 2
asWorkspaces = workspaceNames asWorkspaceDisplayPrefix asDesktops asDesktopPanes
asWorkspaceKeys = wsKeys asWorkspaceKeyPrefix asWorkspaceDisplayPrefix asDesktops asDesktopPanes

-- Tamara workspaces
tWorkspaceDisplayPrefix = "TP"
tWorkspaceKeyPrefix = Just "t"
tDesktops = 2
tDesktopPanes = 2
tWorkspaces = workspaceNames tWorkspaceDisplayPrefix tDesktops tDesktopPanes
tWorkspaceKeys = wsKeys tWorkspaceKeyPrefix tWorkspaceDisplayPrefix tDesktops tDesktopPanes

-- Frederick workspaces
fWorkspaceDisplayPrefix = "FP"
fWorkspaceKeyPrefix = Just "f"
fDesktops = 3
fDesktopPanes = 2
fWorkspaces = workspaceNames fWorkspaceDisplayPrefix fDesktops fDesktopPanes
fWorkspaceKeys = wsKeys fWorkspaceKeyPrefix fWorkspaceDisplayPrefix fDesktops fDesktopPanes

myNewStyleKeys hostname =
    asWorkspaceKeys
        ++ tWorkspaceKeys
        ++ fWorkspaceKeys
        ++ myCustomKeys hostname
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

setupWorkspaceGroups hostname |  hostname == hostnameWork = do
    ADWG.addRawWSGroup "Work1"      [(2, "W11"),(1, "W12")]
    ADWG.addRawWSGroup "Work2"      [(2, "W21"),(1, "W22")]
    ADWG.addRawWSGroup "Work3"      [(2, "W31"),(1, "W32")]

    ADWG.addRawWSGroup "Frederick1" [(2, "FP11"),(1, "FP12")]
    ADWG.addRawWSGroup "Frederick2" [(2, "FP21"),(1, "FP22")]
    ADWG.addRawWSGroup "Frederick3" [(2, "FP31"),(1, "FP32")]

    ADWG.addRawWSGroup "Tamara1"    [(2, "TP11"),(1, "TP12")]
    ADWG.addRawWSGroup "Tamara2"    [(2, "TP21"),(1, "TP22")]

    ADWG.addRawWSGroup "Messaging"  [(2, "IM"), (1, "MAIL")]
    ADWG.addRawWSGroup "Zoom"  [(2, "MAIL"), (1, "IM"),(0,"ZM")]
    ADWG.addRawWSGroup "Zoom2"  [(2, "W11"), (1, "W12"),(0,"ZM")]
    ADWG.addRawWSGroup "StandardWork"  [(2, "W11"), (1, "W12"),(0,"ADM")]

setupWorkspaceGroups _ = do
    screenCount <- LIS.countScreens
    let topMainScreen = screenCount - 2
    let bottomMainScreen = screenCount - 1

    ADWG.addRawWSGroup "Work1"      [(bottomMainScreen, "W11"),(topMainScreen, "W12")]
    ADWG.addRawWSGroup "Work2"      [(bottomMainScreen, "W21"),(topMainScreen, "W22")]
    ADWG.addRawWSGroup "Work3"      [(bottomMainScreen, "W31"),(topMainScreen, "W32")]

    ADWG.addRawWSGroup "Frederick1" [(bottomMainScreen, "FP11"),(topMainScreen, "FP12")]
    ADWG.addRawWSGroup "Frederick2" [(bottomMainScreen, "FP21"),(topMainScreen, "FP22")]
    ADWG.addRawWSGroup "Frederick3" [(bottomMainScreen, "FP31"),(topMainScreen, "FP32")]

    ADWG.addRawWSGroup "Tamara1"    [(bottomMainScreen, "TP11"),(topMainScreen, "TP12")]
    ADWG.addRawWSGroup "Tamara2"    [(bottomMainScreen, "TP21"),(topMainScreen, "TP22")]

    ADWG.addRawWSGroup "Messaging"  [(bottomMainScreen, "IM"), (1, "MAIL")]
    ADWG.addRawWSGroup "Zoom"  [(bottomMainScreen, "MAIL"), (1, "IM"),(0,"ZM")]
    ADWG.addRawWSGroup "Zoom2"  [(bottomMainScreen, "W11"), (1, "W12"),(0,"ZM")]
    ADWG.addRawWSGroup "StandardWork"  [(bottomMainScreen, "W11"), (1, "W12"),(0,"ADM")]

powerkeys key hostname = do
    -- case (screenCount, key) of
    screenCount <- LIS.countScreens
    case (screenCount, key, hostname) of
        -- 3 Screen Setup
        (3,1, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "StandardWork"
        (3,2, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "Messaging"
        (3,3, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "Frederick1"
        (3,4, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "Tamara1"
        (3,6, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "Zoom"
        (3,7, hostname) | hostname == hostnameWork -> ADWG.viewWSGroup "Zoom2"


        -- 2 Screen Setup
        (2,1, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick1"
        (2,2, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick2"
        (2,3, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick3"

        -- Default Screen Setup
        (_,1, hostname) | hostname == hostnameWork -> showDesktop "W11"
        (_,1,_) -> showDesktop "F11"

        (_,2, _) -> showDesktop "IM"
        (_,3, _) -> showDesktop "MAIL"
        (_,4, _) -> showDesktop "ADM"
        (_,6, _) -> showDesktop "ZM"

        (_,5, _) -> showDesktop "SCRATCH"
        (_,8, _) -> showDesktop "NSP"
