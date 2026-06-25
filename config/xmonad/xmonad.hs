{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}

-- Base imports
import Data.List
import Data.Ratio
import Network.HostName (getHostName)

-- XMonad core
import XMonad
import XMonad.StackSet qualified as W

-- XMonad configuration
import XMonad.Config.Desktop (desktopLayoutModifiers)

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
import XMonad.Hooks.UrgencyHook

-- XMonad layouts
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.IndependentScreens as LIS

-- XMonad utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.ExtensibleState as XS

-- =============================================================================
-- CONSTANTS
-- =============================================================================

-- Main modifier key
myModMask = mod4Mask

-- Hostnames
hostnameWork = "fwork"

-- Applications
myTerminal = "wezterm"
-- myBrowser = "firefox-developer-edition"
myBrowser = "google-chrome-stable --new-window https://www.google.com"
-- myEmailer = "wezterm start -- neomutt -F /home/fprice/.mutt/muttrc"
myEmailer = "trojita"
myFileManager = "pcmanfm"

-- Creative applications
myDarkTable = "darktable"
myDarkTablePersonalLibrary = "~/Documents/Personal/DarktablePersonal/library.db"
myDarkTableCommercialLibrary = "~/Documents/Personal/DarktableCommercial/library.db"
myInkScape = "inkscape"
myArdour = "ardour9"
myGuitarix = "guitarix"
myCarlaKeyboardProject = "/home/fprice/Documents/Personal/Dropbox/FrederickDocuments/Music/KeyboardWorking.carxp"
myCarla = "carla" ++" "++ myCarlaKeyboardProject
myTouchOSCProject = "/usr/share/doc/midi-daemon/examples/TouchOSC/ComplexSetup.tosc"
myTouchOSC = "TouchOSC" ++ " --general.ui.editor false --general.ui.fullscreen true"++" "++ myTouchOSCProject
myQPWGraph = "qpwgraph"
myMidiSnoop = "midisnoop"
myEbookViewer = "ebook-viewer"
myMarkdownEditor = "obsidian"

-- System utilities
mySystemMonitor = "gnome-system-monitor"
myCalculator = "gnome-calculator"
myScanner = "simple-scan"
myRDPClient = "remmina"
myScreenLock = "xscreensaver-command -lock"

-- Scripts and commands
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

-- Screen positions
displayPort2 = 3
hdmiA0 = 0
displayPort1 = 1
displayPort0 = 2

-- Work workspaces
wWorkspaceDisplayPrefix = "W"
wWorkspaceKeyPrefix = Nothing
wDesktops = 2
wDesktopPanes = 3

-- Tamara workspaces
tWorkspaceDisplayPrefix = "TP"
tWorkspaceKeyPrefix = Just "t"
tDesktops = 2
tDesktopPanes = 3

-- Frederick workspaces
fWorkspaceDisplayPrefix = "FP"
fWorkspaceKeyPrefix = Just "f"
fDesktops = 3
fDesktopPanes = 5

-- Utility workspaces
uWorkspaceDisplayPrefix = "U"
uWorkspaceKeyPrefix = Just "u"
uDesktops = 1
uDesktopPanes = 3

-- =============================================================================
-- WORKSPACE MANAGEMENT
-- =============================================================================

-- Derived workspace lists and key bindings
wWorkspaces = workspaceNames wWorkspaceDisplayPrefix wDesktops wDesktopPanes
wWorkspaceKeys = wsKeys wWorkspaceKeyPrefix wWorkspaceDisplayPrefix wDesktops wDesktopPanes

tWorkspaces = workspaceNames tWorkspaceDisplayPrefix tDesktops tDesktopPanes
tWorkspaceKeys = wsKeys tWorkspaceKeyPrefix tWorkspaceDisplayPrefix tDesktops tDesktopPanes

fWorkspaces = workspaceNames fWorkspaceDisplayPrefix fDesktops fDesktopPanes
fWorkspaceKeys = wsKeys fWorkspaceKeyPrefix fWorkspaceDisplayPrefix fDesktops fDesktopPanes

uWorkspaces = workspaceNames uWorkspaceDisplayPrefix uDesktops uDesktopPanes
uWorkspaceKeys = wsKeys uWorkspaceKeyPrefix uWorkspaceDisplayPrefix uDesktops uDesktopPanes

-- Workspace sets by hostname
myExtraWorkspaces hostname | hostnameWork `isPrefixOf` hostname = ["IM", "MAIL", "ADM", "SCRATCH", "ZM", "DOC", "NSP"]
myExtraWorkspaces _ = ["SCRATCH", "DOC", "NSP"]

myWorkspaces hostname | hostnameWork `isPrefixOf` hostname = wWorkspaces ++ myExtraWorkspaces hostname ++ tWorkspaces ++ fWorkspaces ++ uWorkspaces
myWorkspaces hostname = fWorkspaces ++ myExtraWorkspaces hostname

-- Workspace navigation helpers
showDesktop :: String -> X ()
showDesktop = windows . W.greedyView

moveFocusedWindowToDesktop :: String -> X ()
moveFocusedWindowToDesktop = windows . W.shift

-- Workspace name/key generation
workspacePanelTuples desktops 1 = [(x, Nothing) | x <- [1 .. desktops]]
workspacePanelTuples desktops desktop_panes = [(x, Just y) | x <- [1 .. desktops], y <- [1 .. desktop_panes]]

workspaceNames workspacePrefix desktops desktop_panes = map (desktopNameFromTuple workspacePrefix) (workspacePanelTuples desktops desktop_panes)

wsKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes =
    workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes
    ++ workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes

workspaceShowDesktopKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes =
    map (desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspacePanelTuples desktops desktop_panes)

workspaceMoveFocusedWindowKeys workspaceKeyPrefix workspaceWindowPrefix desktops desktop_panes =
    map (desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix) (workspacePanelTuples desktops desktop_panes)

desktopNameFromTuple :: Show a => String -> (a, Maybe a) -> String
desktopNameFromTuple p (x, Nothing) = p ++ show x
desktopNameFromTuple p (x, Just y) = p ++ show x ++ show y

fixPrefix = maybe "" (++ " ")

desktopKeyMapFromTuple p (x, Nothing) = fixPrefix p ++ show x
desktopKeyMapFromTuple p (x, Just y) = fixPrefix p ++ show x ++ " " ++ show y

desktopShowDesktopKeymapFromTuple workspaceKeyPrefix workspaceWindowPrefix t =
    let ws = desktopNameFromTuple workspaceWindowPrefix t
    in (workspaceFocusKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, addName ("Focus " ++ ws) $ showDesktop ws)

desktopMoveFocusedKeyFromTuple workspaceKeyPrefix workspaceWindowPrefix t =
    let ws = desktopNameFromTuple workspaceWindowPrefix t
    in (workspaceMoveKey ++ desktopKeyMapFromTuple workspaceKeyPrefix t, addName ("Move to " ++ ws) $ moveFocusedWindowToDesktop ws)

-- =============================================================================
-- WORKSPACE GROUPS
-- =============================================================================

setupWorkspaceGroups hostname | hostnameWork `isPrefixOf` hostname = do
    ADWG.addRawWSGroup "Work1"      [(displayPort2, "W13"),(hdmiA0, "W13"),(displayPort1,"W12"),(displayPort0,"W11")]
    ADWG.addRawWSGroup "Work2"      [(displayPort1, "W4"),(displayPort0, "W3")]
    ADWG.addRawWSGroup "Work3"      [(displayPort1, "W6"),(displayPort0, "W5")]

    ADWG.addRawWSGroup "StandardUtility1"  [(displayPort2, "U13"),(displayPort0,"U12"),(displayPort1,"U11"),(hdmiA0, "U13")]

    ADWG.addRawWSGroup "StandardFrederick1"  [(displayPort2, "FP14"),(hdmiA0, "FP11"),(displayPort1,"FP13"),(displayPort0,"FP12")]
    ADWG.addRawWSGroup "Frederick1"  [(displayPort2, "FP14"),(hdmiA0, "FP11"),(displayPort1,"FP13"),(displayPort0,"FP12")]
    ADWG.addRawWSGroup "Frederick2" [(displayPort1, "FP21"),(displayPort0, "FP22")]
    ADWG.addRawWSGroup "Frederick3" [(displayPort1, "FP31"),(displayPort0, "FP32")]

    ADWG.addRawWSGroup "Tamara1"  [(displayPort2, "TP13"),(hdmiA0, "TP11"),(displayPort1,"TP13"),(displayPort0,"TP12")]

    ADWG.addRawWSGroup "Messaging"  [(hdmiA0, "IM"), (displayPort0, "MAIL")]

    ADWG.addRawWSGroup "StandardWork3"  [(displayPort2, "IM"),(displayPort1,"MAIL"),(displayPort0,"W11")]
    ADWG.addRawWSGroup "StandardWork4"  [(displayPort2, "MAIL"),(hdmiA0, "ADM"),(displayPort1,"DOC"),(displayPort0,"W11")]

setupWorkspaceGroups _ = do
    -- ADWG.addRawWSGroup "Work1"      [(displayPort1, "W2"),(displayPort0, "W1")]
    -- ADWG.addRawWSGroup "Work2"      [(displayPort1, "W4"),(displayPort0, "W3")]
    -- ADWG.addRawWSGroup "Work3"      [(displayPort1, "W6"),(displayPort0, "W5")]
    --
    -- ADWG.addRawWSGroup "StandardFrederick1"  [(displayPort2, "ADM"),(hdmiA0, "MAIL"),(displayPort1,"IM"),(displayPort0,"FP1")]
    -- ADWG.addRawWSGroup "Frederick1"  [(displayPort2, "FP4"),(hdmiA0, "FP3"),(displayPort1,"FP2"),(displayPort0,"FP1")]
    -- ADWG.addRawWSGroup "Frederick2" [(displayPort1, "FP2"),(displayPort0, "FP3")]
    -- ADWG.addRawWSGroup "Frederick3" [(displayPort1, "FP4"),(displayPort0, "FP5")]

    -- ADWG.addRawWSGroup "Tamara1" [(displayPort1, "TP2"),(displayPort0, "TP1")]
    -- ADWG.addRawWSGroup "Tamara1"  [(displayPort2, "TP4"),(hdmiA0, "TP3"),(displayPort1,"TP2"),(displayPort0,"TP1")]
    -- ADWG.addRawWSGroup "Tamara2" [(displayPort1, "TP5"),(displayPort0, "TP6")]

    ADWG.addRawWSGroup "Messaging"  [(hdmiA0, "IM"), (displayPort1, "MAIL")]

    ADWG.addRawWSGroup "StandardWork3"  [(displayPort2, "IM"),(displayPort1,"MAIL"),(displayPort0,"W1")]
    ADWG.addRawWSGroup "StandardWork4"  [(displayPort2, "ADM"),(hdmiA0, "MAIL"),(displayPort1,"IM"),(displayPort0,"W1")]

-- Power keys function - context-aware workspace switching
powerkeys key hostname = do
    numScreens <- LIS.countScreens
    case (numScreens, key) of
        -- 4 Screen Setup
        (4,1) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "StandardWork4"
        (4,2) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Messaging"
        (4,3) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Frederick1"
        (4,4) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Tamara1"
        (4,6) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Zoom"
        (4,7) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Zoom2"

        -- 3 Screen Setup
        -- (3,1) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "StandardWork3"
        -- (3,2) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Messaging"
        -- (3,3) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Frederick1"
        -- (3,4) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Tamara1"
        -- (3,6) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Zoom"
        -- (3,7) | hostnameWork `isPrefixOf` hostname -> ADWG.viewWSGroup "Zoom2"
        --
        -- -- 2 Screen Setup
        -- (2,1) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick1"
        -- (2,2) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick2"
        -- (2,3) | hostname == hostnameDAW -> ADWG.viewWSGroup "Frederick3"

        -- Default Screen Setup
        (_,1) | hostnameWork `isPrefixOf` hostname -> showDesktop "W11"
        (_,1) -> showDesktop "FP11"
        (_,2) -> showDesktop "IM"
        (_,3) -> showDesktop "MAIL"
        (_,4) -> showDesktop "ADM"
        (_,5) -> showDesktop "SCRATCH"
        (_,6) -> showDesktop "ZM"
        (_,8) -> showDesktop "NSP"
        _ -> return ()

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

-- =============================================================================
-- LAYOUTS
-- =============================================================================

myLayouts = toggleLayouts (noBorders Full) (smartBorders (multiColumn ||| mainGrid ||| magnifyLayout mainGrid ||| churchSetup ))
  where
    magnifyLayout = magnifiercz 1.4

    orientation = XMonad.Layout.GridVariants.L
    masterRows = 2
    masterColumns = 2
    masterPortion = 2 / 3
    slaveAspectRatio = 16 / 10
    resizeIncrement = 5 / 100

    mainGrid = SplitGrid orientation masterRows masterColumns masterPortion slaveAspectRatio resizeIncrement
    multiColumn = multiCol [1] 1 0.01 (-0.5)
    tall = Tall 1 (10/100) (80/100)
    churchSetup = (tall ****|* tall) ****/* tall

-- =============================================================================
-- MANAGE HOOKS
-- =============================================================================

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
        _ -> idHook

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
-- STATUS BAR
-- =============================================================================

newtype HideEmptyWS = HideEmptyWS Bool deriving (Read, Show)

instance ExtensionClass HideEmptyWS where
    initialValue = HideEmptyWS True

toggleHideEmptyWS :: X ()
toggleHideEmptyWS = XS.modify (\(HideEmptyWS b) -> HideEmptyWS (not b))

myXmobarPP :: X PP
myXmobarPP = do
    HideEmptyWS hideEmpty <- XS.get
    return $ def
        { ppSep = magenta " • "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
        , ppHidden = \ws -> if ws `elem` ["U11", "U12", "U13"] then "" else lowWhite . wrap " " "" $ ws
        , ppHiddenNoWindows = \ws -> if hideEmpty || ws `elem` ["U11", "U12", "U13"] then "" else lowWhite . wrap " " "" $ ws
        , ppUrgent = red . wrap (yellow "!") (yellow "!")
        , ppOrder = \case { (ws:l:_) -> [ws, l]; _ -> [] }
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
-- KEY BINDINGS
-- =============================================================================

spawnKey key desc program = (appRunKey ++ key, addName desc $ spawn program)

workspaceKeys key ws = [(workspaceFocusKey ++ key, addName ("Focus " ++ ws) $ showDesktop ws), (workspaceMoveKey ++ key, addName ("Move to " ++ ws) $ moveFocusedWindowToDesktop ws)]

dynamicScratchPadKeys key scratchPadName = [("M-S-" ++ key, addName ("Assign to scratchpad " ++ scratchPadName) $ withFocused $ toggleDynamicNSP scratchPadName), ("M-" ++ key, addName ("Show scratchpad " ++ scratchPadName) $ dynamicNSPAction scratchPadName)]

dynamicWorkspaceGroupKeys key viewGroup = [("M-" ++ key, addName ("View group " ++ viewGroup) $ ADWG.viewWSGroup viewGroup), ("M-S-" ++ key, addName ("Save group " ++ viewGroup) $ ADWG.addCurrentWSGroup viewGroup)]

viewGroupKeys keys viewGroup = [("M-s " ++ keys, addName ("View group " ++ viewGroup) $ ADWG.viewWSGroup viewGroup)]

myCustomKeys hostname =
    [ ("M-f", addName "Toggle fullscreen" $ sendMessage ToggleLayout)
    , ("M-S-h", addName "Toggle hide empty workspaces" toggleHideEmptyWS)
    , ("M-S-<Enter>", addName "Open terminal" $ spawn myTerminal)
    , spawnKey "b" "Browser" myBrowser
    , spawnKey "d" "DarkTable (personal)" (myDarkTable ++ " --library " ++ myDarkTablePersonalLibrary)
    , spawnKey "S-d" "DarkTable (commercial)" (myDarkTable ++ " --library " ++ myDarkTableCommercialLibrary)
    , spawnKey "i" "Inkscape" myInkScape
    , spawnKey "e" "Ebook viewer" myEbookViewer
    , spawnKey "f" "File manager" myFileManager
    , spawnKey "p" "System monitor" mySystemMonitor
    , spawnKey "s" "Scanner" myScanner
    , spawnKey "c" "Calculator" myCalculator
    , ("<XF86Calculator>", addName "Calculator" $ spawn myCalculator)
    , ("C-M-'", addName "Screen lock" $ spawn myScreenLock)
    , ("calc", addName "Calculator" $ spawn myCalculator)
    , spawnKey "r" "RDP client" myRDPClient
    , spawnKey "a a" "Ardour DAW" myArdour
    , spawnKey "a g" "Guitarix" myGuitarix
    , spawnKey "a c" "Carla" myCarla
    , spawnKey "a q" "QPWGraph" myQPWGraph
    , spawnKey "a m" "MidiSnoop" myMidiSnoop
    , spawnKey "z" "Fix screens" myFixScreens
    , spawnKey "o" "Markdown editor (Obsidian)" myMarkdownEditor
    , spawnKey "l" "Screen lock" myScreenLock
    , spawnKey "m" "Fix Kensington trackball" myFixKensingtonTrackball

    -- Handle powerkeys
    , ("M-1", addName "Power key 1" $ powerkeys 1 hostname)
    , ("M-2", addName "Power key 2" $ powerkeys 2 hostname)
    , ("M-3", addName "Power key 3" $ powerkeys 3 hostname)
    , ("M-4", addName "Power key 4" $ powerkeys 4 hostname)
    , ("M-5", addName "Power key 5" $ powerkeys 5 hostname)

    , ("M-i", addName "Jump to IM" $ showDesktop "IM")
    , ("M-S-f", addName "Jump to FP11" $ showDesktop "FP11")

    -- Handle moves
    , ("M-S-1", addName "Move to W11" $ moveFocusedWindowToDesktop "W11")
    , ("M-S-2", addName "Move to IM" $ moveFocusedWindowToDesktop "IM")
    , ("M-S-3", addName "Move to MAIL" $ moveFocusedWindowToDesktop "MAIL")
    , ("M-S-4", addName "Move to ADM" $ moveFocusedWindowToDesktop "ADM")
    , ("M-S-5", addName "Move to SCRATCH" $ moveFocusedWindowToDesktop "SCRATCH")
    , ("M-S-6", addName "Move to ZM" $ moveFocusedWindowToDesktop "ZM")
    , ("M-S-7", addName "Move to NSP" $ moveFocusedWindowToDesktop "NSP")
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

    ++ viewGroupKeys "u u" "StandardUtility1"
    ++ viewGroupKeys "u 1" "StandardUtility1"

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

    ++ viewGroupKeys "c" "Messaging"

-- Mouse warp keys
warpMouseKeys =
    [ ("M-C-w", addName "Warp mouse to screen 0" $ warpToScreen 0 (1 % 2) (1 % 2))
    , ("M-C-e", addName "Warp mouse to screen 1" $ warpToScreen 1 (1 % 2) (1 % 2))
    , ("M-C-r", addName "Warp mouse to screen 2" $ warpToScreen 2 (1 % 2) (1 % 2))
    ]

myNewStyleKeys hostname =
    wWorkspaceKeys
        ++ tWorkspaceKeys
        ++ fWorkspaceKeys
        ++ uWorkspaceKeys
        ++ myCustomKeys hostname
        ++ warpMouseKeys

-- =============================================================================
-- STARTUP
-- =============================================================================

myStartupHook hostname = do
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
            spawnOn "U11" myCarla
            spawnOn "U11" myGuitarix
            spawnOn "U12" myQPWGraph
            spawnOn "U13" myTouchOSC
            spawnOnce "syncthing serve"
        else do
            spawnOn "FP11" myArdour
            spawnOnce "cbatticon"

    -- System tray and utilities
    spawnOnce "snixembed"
    spawnOnce "nm-applet"
    spawnOnce "xscreensaver --no-splash"
    spawnOnce "trayer --monitor primary --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --tint 0xffffff --height 21 --iconspacing 2"
    setWMName "LG3D"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    hostname <- getHostName
    xmonad $ withUrgencyHook NoUrgencyHook
        $ setEwmhActivateHook doAskUrgent
        . ewmh
        . ewmhFullscreen
        . docks
        . withEasySB (statusBarProp "xmobar" myXmobarPP) defToggleStrutsKey
        $ createMyConfig hostname

createMyConfig hostname =
    addDescrKeys ((myModMask, xK_F1), xMessage)
        (\c -> mkNamedKeymap c (myNewStyleKeys hostname))
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
