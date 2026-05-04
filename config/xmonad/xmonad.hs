{-# LANGUAGE ImportQualifiedPost #-}

-- Base imports
import Control.Concurrent
import Data.Char (toLower)
import Data.List
import Data.Ratio
import Network.HostName (getHostName)

-- XMonad core
import XMonad
import XMonad.StackSet qualified as W

-- XMonad configuration
import XMonad.Config.Desktop (
    desktopConfig,
    desktopLayoutModifiers,
 )

-- XMonad actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Actions.DynamicWorkspaceGroups as ADWG

-- XMonad hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

-- XMonad layouts
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.IndependentScreens as LIS

-- XMonad utilities
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- X11 extras
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib

-- =============================================================================
-- CONFIGURATION CONSTANTS
-- =============================================================================

-- Main modifier key
myModMask = mod4Mask

-- Hostnames
hostnameWork = "fwork"
hostnameDAW = "daw"

-- Applications
myTerminal = "wezterm"
-- myBrowser = "firefox-developer-edition"
myBrowser = "google-chrome-stable --new-window https://www.google.com"
myBrowserNyxt = "nyxt --no-socket"
-- myEmailer = "wezterm start -- neomutt -F /home/fprice/.mutt/muttrc"
myEmailer = "trojita"
myFileManager = "pcmanfm"
myDMenu = "dmenu-frecency"

-- Creative applications
myDarkTable = "darktable"
myDarkTablePersonalLibrary = "~/Documents/Personal/DarktablePersonal/library.db"
myDarkTableCommercialLibrary = "~/Documents/Personal/DarktableCommercial/library.db"
myInkScape = "inkscape"
myArdour = "ardour9"
myGuitarix = "guitarix"
myCarlaKeyboardProject = "/home/fprice/Documents/Personal/Dropbox/FrederickDocuments/Music/KeyboardWorking.carxp"
myCarla = "carla" ++" "++ myCarlaKeyboardProject
myQPWGraph = "qpwgraph"
myMidiSnoop = "midisnoop"
myEbookViewer = "ebook-viewer"
myMarkdownEditor = "obsidian"

-- System utilities
myAudioManager = "pavucontrol"
mySystemMonitor = "gnome-system-monitor"
myCalculator = "gnome-calculator"
myScanner = "simple-scan"
myRDPClient = "remmina"
myPrinterConfig = "system-config-printer"
myScreenLock = "xscreensaver-command -lock"

-- Scripts and commands
myBackgrounds = "~/Documents/Personal/Dropbox/FrederickDocuments/Backgrounds/"
myRunBackgrounds = "feh --no-fehbg --bg-max --random " ++ myBackgrounds
myFixScreens = "autorandr --change"
myFixLogitechMouse = "xinput --set-prop 'Logitech M325' 'libinput Accel Speed' -0.4"
myFixKensingtonTrackball = "kensington-reset.sh"

-- Colors
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#FFB53A"

-- Key prefixes
workspaceFocusKey = "M-d "
workspaceMoveKey = "M-S-d "
appRunKey = "M-a "

-- =============================================================================
-- KEY BINDING HELPER FUNCTIONS
-- =============================================================================

spawnKey key program = (appRunKey ++ key, spawn program)

workspaceKeys key desktop = [(workspaceFocusKey ++ key, showDesktop desktop), (workspaceMoveKey ++ key, moveFocusedWindowToDesktop desktop)]

dynamicScratchPadKeys key scratchPadName = [("M-S-" ++ key, withFocused $ toggleDynamicNSP scratchPadName), ("M-" ++ key, dynamicNSPAction scratchPadName) ]

dynamicWorkspaceGroupKeys key viewGroup = [("M-" ++ key, ADWG.viewWSGroup viewGroup), ("M-S-" ++ key, ADWG.addCurrentWSGroup viewGroup)]

viewGroupKeys keys viewGroup = [("M-s " ++ keys , ADWG.viewWSGroup viewGroup)]

myCustomKeys hostname =
    [ ("M-f", sendMessage ToggleLayout)
    , ("M-S-<Enter>", spawn myTerminal)
    -- , ("M-y", withFocused $ windows . W.sink)
    , spawnKey "b" myBrowser
    , spawnKey "d" (myDarkTable ++ " --library " ++ myDarkTablePersonalLibrary)
    , spawnKey "S-d" (myDarkTable ++ " --library " ++ myDarkTableCommercialLibrary)
    , spawnKey "i" myInkScape
    , spawnKey "e" myEbookViewer
    , spawnKey "f" myFileManager
    , spawnKey "p" mySystemMonitor -- performance monitor
    , spawnKey "s" myScanner
    , spawnKey "c" myCalculator
    , ("<XF86Calculator>", spawn myCalculator)
    , ("C-M-'", spawn myScreenLock)
    , ("calc", spawn myCalculator)
    , spawnKey "r" myRDPClient
    , spawnKey "a a" myArdour
    , spawnKey "a g" myGuitarix
    , spawnKey "a c" myCarla
    , spawnKey "a q" myQPWGraph
    , spawnKey "a m" myMidiSnoop
    , spawnKey "z" myFixScreens
    -- , spawnKey "m" myEmailer
    , spawnKey "o" myMarkdownEditor
    , spawnKey "l" myScreenLock 
    , spawnKey "m" myFixKensingtonTrackball

    -- Handle powerkeys
    , ("M-1", powerkeys 1 hostname)
    , ("M-2", powerkeys 2 hostname)
    , ("M-3", powerkeys 3 hostname)
    , ("M-4", powerkeys 4 hostname)
    , ("M-5", powerkeys 5 hostname)

    -- Powekey for Quick Mobile jumping particularly
    , ("M-i", showDesktop "IM")
    -- , ("M-t", showDesktop "TP11")
    , ("M-S-f", showDesktop "FP11")

    -- , ("M-6", powerkeys 6 hostname)
    -- , ("M-7", powerkeys 7 hostname)
    -- , ("M-8", powerkeys 8 hostname)

    -- Handle powergroups
    -- , ("M-s w w", powergroups 1)
    -- , ("M-s w 1", powergroups 1)
    -- , ("M-s w 2", powergroups 2)
    -- , ("M-s w 3", powergroups 3)
    --
    -- , ("M-s c", powergroups 4)
    --
    -- , ("M-s z z", powergroups 5)
    -- , ("M-s z 1", powergroups 5)
    -- , ("M-s z 2", powergroups 6)
    --
    -- , ("M-s t t", powergroups 7)
    -- , ("M-s t 1", powergroups 7)
    -- , ("M-s t 2", powergroups 8)
    --
    -- , ("M-s f f", powergroups 9)
    -- , ("M-s f 1", powergroups 9)
    -- , ("M-s f 2", powergroups 10)

    -- Handle moves
    , ("M-S-1", moveFocusedWindowToDesktop "W11")
    , ("M-S-2", moveFocusedWindowToDesktop "IM")
    , ("M-S-3", moveFocusedWindowToDesktop "MAIL")
    , ("M-S-4", moveFocusedWindowToDesktop "ADM")
    , ("M-S-5", moveFocusedWindowToDesktop "SCRATCH")
    , ("M-S-6", moveFocusedWindowToDesktop "ZM")
    , ("M-S-7", moveFocusedWindowToDesktop "NSP")
    ]

    ++ workspaceKeys "a" "ADM"
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

    ++ viewGroupKeys "w w" "StandardWork4"
    ++ viewGroupKeys "w 1" "Work1"
    ++ viewGroupKeys "w 2" "Work2"
    ++ viewGroupKeys "w 3" "Work3"

    ++ viewGroupKeys "f f" "StandardFrederick1"
    ++ viewGroupKeys "f 1" "Frederick1"
    ++ viewGroupKeys "f 2" "Frederick2"
    ++ viewGroupKeys "f 3" "Frederick3"

    ++ viewGroupKeys "t t" "Tamara1"
    ++ viewGroupKeys "t 1" "Tamara1"
    ++ viewGroupKeys "t 2" "Tamara2"

    ++ viewGroupKeys "z z" "Zoom"
    ++ viewGroupKeys "z 1" "Zoom"
    ++ viewGroupKeys "z 2" "Zoom2"

    -- ++ viewGroupKeys "m m" "M1"
    -- ++ viewGroupKeys "m 1" "M1"
    -- ++ viewGroupKeys "m 2" "M2"

    ++ viewGroupKeys "c" "Messaging"

    -- ++ [
    -- Dynamic Workspace Groups
    -- , ("M-y n", ADWG.promptWSGroupAdd myXPConfig "Name this group: ")
    -- , ("M-y g", ADWG.promptWSGroupView myXPConfig "Go to group: ")
    -- , ("M-y d", ADWG.promptWSGroupForget myXPConfig "Forget group: ")]
    -- mod-/ and mod-? %! Jump to or memorize a workspace group

-- Helper function for setting up work windows
setupWorkWindow = do
    spawnHere myBrowser
    spawnHere myBrowser
    liftIO (threadDelay 5000000)
    spawnHere myTerminal
    spawnHere myTerminal

-- Mouse warp keys
warpMouseKeys =
    [ ("M-C-w", warpToScreen 0 (1 % 2) (1 % 2))
    , ("M-C-e", warpToScreen 1 (1 % 2) (1 % 2))
    , ("M-C-r", warpToScreen 2 (1 % 2) (1 % 2))
    ]

-- =============================================================================
-- STARTUP AND MAIN CONFIGURATION
-- =============================================================================

-- Startup hook configuration
myStartupHook  hostname= do
    setupWorkspaceGroups hostname
    -- System services
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawnOnce "dunst"
    spawnOnce "picom -b"
    spawnOnce "pamac-tray"
    spawnOnce "blueman-applet"
    spawn "killall pasystray; sleep 15; pasystray"
    spawnOnce "xfce4-power-manager"
    spawnOnce "killall udiskie; udiskie --tray"
    -- spawnOnce "easyeffects --service-mode --hide-window"
    spawn myFixScreens
    spawn myFixLogitechMouse
    
    -- Host-specific configuration
    if hostnameWork `isPrefixOf` hostname
        then do
            spawnOnce "system-config-printer-applet"
            spawnOnce "meteo-qt"
            spawnOn "MAIL" myEmailer
            spawnOn "IM" "discord"
            spawnOn "ADM" myMarkdownEditor
            spawnOn "ADM" myBrowser
            spawnOn "U1" myCarla
            spawnOn "U1" myGuitarix
            spawnOn "U2" myQPWGraph
            spawnOnce "syncthing serve"
        else do
            spawnOn "FP12" myArdour
    
    -- System tray and utilities
    spawnOnce "snixembed"
    spawnOnce "nm-applet"
    spawnOnce "xscreensaver --no-splash"
    spawnOnce "trayer --monitor primary --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --tint 0xffffff --height 21 --iconspacing 2"
    setWMName "LG3D"

-- Main configuration
main :: IO ()
main = do
    hostname <- getHostName
    xmonad $ withUrgencyHook NoUrgencyHook
        $ setEwmhActivateHook doAskUrgent
        . ewmh
        . ewmhFullscreen
        . docks
        . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
        $ createMyConfig hostname

createMyConfig hostname = 
            def
                { terminal = myTerminal
                , modMask = myModMask
                , layoutHook = avoidStruts $ smartBorders $ desktopLayoutModifiers myLayouts
                , manageHook = manageDocks <+> myManageHook
                , startupHook = myStartupHook hostname
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , workspaces = myWorkspaces hostname
                , logHook = updatePointer (0.5, 0.5) (0, 0)
                }
                `additionalKeysP` myNewStyleKeys hostname


-- =============================================================================
-- LAYOUTS AND VISUAL CONFIGURATION
-- =============================================================================

-- Window layouts
myLayouts = toggleLayouts (noBorders Full) (smartBorders (multiColumn ||| mainGrid ||| magnifier mainGrid ||| churchSetup ))
  where
    magnifier = magnifiercz 1.4

    orientation = XMonad.Layout.GridVariants.L
    masterRows = 2
    masterColumns = 2
    masterPortion = 2 / 3
    slaveAspectRatio = 16 / 10
    resizeIncrement = 5 / 100

    mainGrid = SplitGrid orientation masterRows masterColumns masterPortion slaveAspectRatio resizeIncrement
    multiColumn = multiCol [1] 1 0.01 (-0.5)
    tall = (Tall 1 (10/100) (80/100))
    churchSetup = ( (tall ****|* tall ) ****/* tall )

-- Window management rules
myManageHook :: ManageHook
myManageHook =
    composeAll
        [ manageSpawn
        , manageDocks
        , customInsertPosition
        , resource =? "trayer" --> doIgnore
        , className =? "simple-scan" --> doSink
        , className =? "zoom" --> doShift "ZM"
        -- , className =? "Gimp" --> doFloat
        , className =? "meteo-qt" --> doFloat
        , className =? "discord" --> doShift "IM"
        , className =? "Slack" --> doShift "IM"
        , className =? "thunderbird" --> doShift "MAIL"
        , isDialog --> doFloat
        ]

-- Custom insertion position logic based on WM_CLASS, WM_TRANSIENT_FOR, and dialog windows
customInsertPosition :: ManageHook  
customInsertPosition = do
    w <- ask
    dpy <- liftX $ asks display
    wmClass <- liftX $ io $ do
        wmClassAtom <- internAtom dpy "WM_CLASS" False
        getWindowProperty8 dpy wmClassAtom w
    wmTransientFor <- liftX $ io $ do
        wmTransientForAtom <- internAtom dpy "WM_TRANSIENT_FOR" False  
        getWindowProperty32 dpy wmTransientForAtom w
    isDialogWindow <- isDialog
    case (wmClass, wmTransientFor, isDialogWindow) of
        (Just _, Nothing, False) -> insertPosition End Newer
        (_, Just _, _) -> idHook
        (_, _, True) -> idHook
        _ -> idHook 


-- Status bar configuration
myXmobarPP :: PP
myXmobarPP =
    def
        { ppSep = magenta " • "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
        , ppHiddenNoWindows = lowWhite . wrap " " ""
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppOrder = \[ws, l, _, _] -> [ws, l]
        , ppExtras = [logTitles formatFocused formatUnfocused]
        }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""


-- =============================================================================
-- WORKSPACE MANAGEMENT
-- =============================================================================

-- Define extra workspaces that I use all the time, by hostname
myExtraWorkspaces hostname | isPrefixOf hostnameWork hostname = ["IM", "MAIL", "ADM", "SCRATCH", "ZM", "DOC", "NSP"]
myExtraWorkspaces _ = ["SCRATCH", "DOC", "NSP"]

myWorkspaces hostname | isPrefixOf hostnameWork hostname = wWorkspaces ++ myExtraWorkspaces hostname ++ tWorkspaces ++ fWorkspaces ++ uWorkspaces
myWorkspaces hostname = fWorkspaces ++ myExtraWorkspaces hostname

-- Workspace helper functions
showDesktop :: String -> X ()
showDesktop d = windows $ W.greedyView d

moveFocusedWindowToDesktop :: String -> X ()
moveFocusedWindowToDesktop d = windows $ W.shift d

-- Workspace generation functions
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

desktopKeyMapFromTuple p (x, Nothing) = fixPrefix p ++ show x
desktopKeyMapFromTuple p (x, Just y) = fixPrefix p ++ show x ++ " " ++ show y

desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (workspaceFocusKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, showDesktop (desktopNameFromTuple workspaceWindowPrefix t))

desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix t = (workspaceMoveKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, moveFocusedWindowToDesktop (desktopNameFromTuple workspaceWindowPrefix t))

-- Workspace definitions
-- Work workspaces
wWorkspaceDisplayPrefix = "W"
wWorkspaceKeyPrefix = Nothing
wDesktops = 4
wDesktopPanes = 1
wWorkspaces = workspaceNames wWorkspaceDisplayPrefix wDesktops wDesktopPanes
wWorkspaceKeys = wsKeys wWorkspaceKeyPrefix wWorkspaceDisplayPrefix wDesktops wDesktopPanes

-- Tamara workspaces
tWorkspaceDisplayPrefix = "TP"
tWorkspaceKeyPrefix = Just "t"
tDesktops = 4
tDesktopPanes = 1
tWorkspaces = workspaceNames tWorkspaceDisplayPrefix tDesktops tDesktopPanes
tWorkspaceKeys = wsKeys tWorkspaceKeyPrefix tWorkspaceDisplayPrefix tDesktops tDesktopPanes

-- Frederick workspaces
fWorkspaceDisplayPrefix = "FP"
fWorkspaceKeyPrefix = Just "f"
fDesktops = 6
fDesktopPanes = 1
fWorkspaces = workspaceNames fWorkspaceDisplayPrefix fDesktops fDesktopPanes
fWorkspaceKeys = wsKeys fWorkspaceKeyPrefix fWorkspaceDisplayPrefix fDesktops fDesktopPanes

-- Utility workspaces
uWorkspaceDisplayPrefix = "U"
uWorkspaceKeyPrefix = Just "u"
uDesktops = 2
uDesktopPanes = 1
uWorkspaces = workspaceNames uWorkspaceDisplayPrefix uDesktops uDesktopPanes
uWorkspaceKeys = wsKeys uWorkspaceKeyPrefix uWorkspaceDisplayPrefix uDesktops uDesktopPanes

-- =============================================================================
-- MAIN KEY BINDINGS
-- =============================================================================

myNewStyleKeys hostname =
    wWorkspaceKeys
        ++ tWorkspaceKeys
        ++ fWorkspaceKeys
        ++ uWorkspaceKeys
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

-- =============================================================================
-- WORKSPACE GROUPS SETUP
-- =============================================================================

-- Screen position constants
farLeftScreen = 3
topMiddleScreen = 0
bottomMiddleScreen = 1
farRightScreen = 2

setupWorkspaceGroups hostname | isPrefixOf hostnameWork hostname = do
    ADWG.addRawWSGroup "Work1"      [(farLeftScreen, "W4"),(topMiddleScreen, "W3"),(bottomMiddleScreen,"W2"),(farRightScreen,"W1")]
    ADWG.addRawWSGroup "Work2"      [(bottomMiddleScreen, "W4"),(farRightScreen, "W3")]
    ADWG.addRawWSGroup "Work3"      [(bottomMiddleScreen, "W6"),(farRightScreen, "W5")]

    -- ADWG.addRawWSGroup "StandardFrederick1"  [(farLeftScreen, "ADM"),(topMiddleScreen, "MAIL"),(bottomMiddleScreen,"IM"),(farRightScreen,"FP1")]
    -- ADWG.addRawWSGroup "StandardFrederick1"  [(farLeftScreen, "MAIL"),(topMiddleScreen, "IM"),(bottomMiddleScreen,"ADM"),(farRightScreen,"FP1")]
    ADWG.addRawWSGroup "StandardFrederick1"  [(farLeftScreen, "MAIL"),(topMiddleScreen, "ADM"),(farRightScreen,"DOC"),(bottomMiddleScreen,"FP1")]
    ADWG.addRawWSGroup "Frederick1"  [(farLeftScreen, "FP4"),(topMiddleScreen, "FP3"),(bottomMiddleScreen,"FP2"),(farRightScreen,"FP1")]
    ADWG.addRawWSGroup "Frederick2" [(bottomMiddleScreen, "FP4"),(farRightScreen, "FP3")]
    ADWG.addRawWSGroup "Frederick3" [(bottomMiddleScreen, "FP6"),(farRightScreen, "FP5")]

    ADWG.addRawWSGroup "Tamara1"  [(farLeftScreen, "TP4"),(topMiddleScreen, "TP3"),(bottomMiddleScreen,"TP2"),(farRightScreen,"TP1")]
    ADWG.addRawWSGroup "Tamara2" [(bottomMiddleScreen, "TP5"),(farRightScreen, "TP6")]

    ADWG.addRawWSGroup "Messaging"  [(topMiddleScreen, "IM"), (bottomMiddleScreen, "MAIL")]

    ADWG.addRawWSGroup "StandardWork3"  [(farLeftScreen, "IM"),(bottomMiddleScreen,"MAIL"),(farRightScreen,"W1")]
    -- ADWG.addRawWSGroup "StandardWork4"  [(farLeftScreen, "ADM"),(topMiddleScreen, "MAIL"),(bottomMiddleScreen,"IM"),(farRightScreen,"W1")]
    ADWG.addRawWSGroup "StandardWork4"  [(farLeftScreen, "MAIL"),(topMiddleScreen, "ADM"),(farRightScreen,"DOC"),(bottomMiddleScreen,"W1")]

setupWorkspaceGroups _ = do
    ADWG.addRawWSGroup "Work1"      [(bottomMiddleScreen, "W2"),(farRightScreen, "W1")]
    ADWG.addRawWSGroup "Work2"      [(bottomMiddleScreen, "W4"),(farRightScreen, "W3")]
    ADWG.addRawWSGroup "Work3"      [(bottomMiddleScreen, "W6"),(farRightScreen, "W5")]

    ADWG.addRawWSGroup "StandardFrederick1"  [(farLeftScreen, "ADM"),(topMiddleScreen, "MAIL"),(bottomMiddleScreen,"IM"),(farRightScreen,"FP1")]
    ADWG.addRawWSGroup "Frederick1"  [(farLeftScreen, "FP4"),(topMiddleScreen, "FP3"),(bottomMiddleScreen,"FP2"),(farRightScreen,"FP1")]
    ADWG.addRawWSGroup "Frederick2" [(bottomMiddleScreen, "FP2"),(farRightScreen, "FP3")]
    ADWG.addRawWSGroup "Frederick3" [(bottomMiddleScreen, "FP4"),(farRightScreen, "FP5")]

    -- ADWG.addRawWSGroup "Tamara1" [(bottomMiddleScreen, "TP2"),(farRightScreen, "TP1")]
    ADWG.addRawWSGroup "Tamara1"  [(farLeftScreen, "TP4"),(topMiddleScreen, "TP3"),(bottomMiddleScreen,"TP2"),(farRightScreen,"TP1")]
    ADWG.addRawWSGroup "Tamara2" [(bottomMiddleScreen, "TP5"),(farRightScreen, "TP6")]

    ADWG.addRawWSGroup "Messaging"  [(topMiddleScreen, "IM"), (bottomMiddleScreen, "MAIL")]

    ADWG.addRawWSGroup "StandardWork3"  [(farLeftScreen, "IM"),(bottomMiddleScreen,"MAIL"),(farRightScreen,"W1")]
    ADWG.addRawWSGroup "StandardWork4"  [(farLeftScreen, "ADM"),(topMiddleScreen, "MAIL"),(bottomMiddleScreen,"IM"),(farRightScreen,"W1")]

-- Power keys function - context-aware workspace switching
powerkeys key hostname = do
    screenCount <- LIS.countScreens
    case (screenCount, key, hostname) of
        -- 4 Screen Setup
        (4,1, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "StandardWork4"
        (4,2, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Messaging"
        (4,3, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Frederick1"
        (4,4, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Tamara1"
        (4,6, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Zoom"
        (4,7, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Zoom2"

        -- 3 Screen Setup
        -- (3,1, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "StandardWork3"
        -- (3,2, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Messaging"
        -- (3,3, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Frederick1"
        -- (3,4, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Tamara1"
        -- (3,6, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Zoom"
        -- (3,7, hostname) | isPrefixOf hostnameWork hostname -> ADWG.viewWSGroup "Zoom2"
        --
        -- -- 2 Screen Setup
        -- (2,1, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick1"
        -- (2,2, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick2"
        -- (2,3, hostname) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick3"

        -- Default Screen Setup
        (_,1, hostname) | isPrefixOf hostnameWork hostname -> showDesktop "W1"
        (_,1,_) -> showDesktop "FP1"
        (_,2, _) -> showDesktop "IM"
        (_,3, _) -> showDesktop "MAIL"
        (_,4, _) -> showDesktop "ADM"
        (_,5, _) -> showDesktop "SCRATCH"
        (_,6, _) -> showDesktop "ZM"
        (_,8, _) -> showDesktop "NSP"


-- powergroups key = do
--     case key of
--         1 -> ADWG.viewWSGroup "Work1"
--         2 -> ADWG.viewWSGroup "Work2"
--         3 -> ADWG.viewWSGroup "Work3"
--         4 -> ADWG.viewWSGroup "Messaging"
--         5 -> ADWG.viewWSGroup "Zoom"
--         6 -> ADWG.viewWSGroup "Zoom2"
--         7 -> ADWG.viewWSGroup "Tamara1"
--         8 -> ADWG.viewWSGroup "Tamara2"
--         9 -> ADWG.viewWSGroup "Frederick1"
--         10 -> ADWG.viewWSGroup "Frederick2"
--
