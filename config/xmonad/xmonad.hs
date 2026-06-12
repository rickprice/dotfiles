{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import qualified Data.Map.Strict as M
import System.Exit (exitSuccess)

import XMonad
import XMonad.StackSet qualified as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- =============================================================================
-- CONSTANTS
-- =============================================================================

myModMask  = mod4Mask
myTerminal = "wezterm"

myNormalBorderColor  = "#2F3D44"
myFocusedBorderColor = "#556064"
myBorderWidth        = 1

myWorkspaces = map show [1..9 :: Int]

-- =============================================================================
-- LAYOUTS
-- =============================================================================

myLayouts = toggleLayouts (noBorders Full)
    $ smartBorders
    $ myTall ||| Mirror myTall ||| simpleTabbed
  where
    myTall = Tall 1 (3/100) (1/2)

-- =============================================================================
-- MANAGE HOOK
-- =============================================================================

myManageHook = composeAll
    [ manageDocks
    , namedScratchpadManageHook myScratchpads
    , title     =? "alsamixer"          --> doFloat
    , className =? "calamares"          --> doFloat
    , className =? "Clipgrab"           --> doFloat
    , className =? "fpakman"            --> doFloat
    , className =? "Galculator"         --> doFloat
    , className =? "GParted"            --> doFloat
    , className =? "Lightdm-settings"   --> doFloat
    , className =? "Lxappearance"       --> doFloat
    , className =? "Manjaro-hello"      --> doFloat
    , className =? "Oblogout"           --> doFullFloat
    , className =? "octopi"             --> doFloat
    , className =? "Pamac-manager"      --> doFloat
    , className =? "Pavucontrol"        --> doFloat
    , className =? "qt5ct"              --> doFloat
    , className =? "Qtconfig-qt4"       --> doFloat
    , className =? "Skype"              --> doFloat
    , className =? "Timeset-gui"        --> doFloat
    , className =? "Xfburn"             --> doFloat
    , className =? "Variety"            --> doFloat
    , className =? "meteo-qt"           --> doFloat
    , isDialog                          --> doFloat
    ]

-- =============================================================================
-- SCRATCHPAD
-- =============================================================================

myScratchpads =
    [ NS "scratchpad" myTerminal (title =? "scratchpad")
        (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
    ]

-- =============================================================================
-- STATUS BAR
-- =============================================================================

myXmobarPP = def
    { ppSep             = " | "
    , ppCurrent         = xmobarColor "#292F34" "#16a085" . wrap " " " "
    , ppHidden          = xmobarColor "#EEE8D5" "" . wrap " " " "
    , ppHiddenNoWindows = const ""
    , ppUrgent          = xmobarColor "#E5201D" "#FDF6E3" . wrap "!" "!"
    , ppTitle           = xmobarColor "#F9FAF9" "" . shorten 50
    , ppLayout          = xmobarColor "#F9FAF9" ""
    }

-- =============================================================================
-- KEY BINDINGS
-- =============================================================================

toggleFloat :: Window -> X ()
toggleFloat w = windows $ \s ->
    if M.member w (W.floating s)
        then W.sink w s
        else W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)) s

moveAndFollow :: WorkspaceId -> X ()
moveAndFollow ws = windows (W.greedyView ws . W.shift ws)

systemModeSubmap :: X ()
systemModeSubmap = submap $ M.fromList
    [ ((0,         xK_l), spawn "i3exit lock")
    , ((0,         xK_s), spawn "i3exit suspend")
    , ((0,         xK_u), spawn "i3exit switch_user")
    , ((0,         xK_e), spawn "i3exit logout")
    , ((0,         xK_h), spawn "i3exit hibernate")
    , ((0,         xK_r), spawn "i3exit reboot")
    , ((shiftMask, xK_s), spawn "i3exit shutdown")
    ]

myKeys =
    -- Terminal
    [ ("M-S-<Return>",   spawn myTerminal)
    -- Kill window
    , ("M-S-c",          kill)
    , ("M-S-q",          kill)
    -- Launchers
    , ("M-d",            spawn "dmenu-frecency")
    , ("M-r",            spawn "dmenu-frecency")
    , ("M-z",            spawn "morc_menu")
    -- Audio mixer
    , ("M-C-m",          spawn "wezterm start -- alsamixer")
    -- Applications
    , ("M-<F2>",         spawn "firefox-developer-edition")
    , ("M-<F8>",         spawn "firefox-developer-edition")
    , ("M-<F3>",         spawn "pcmanfm")
    , ("M-<F9>",         spawn "pcmanfm")
    , ("M-<F4>",         spawn "rm -f ~/.cache/selected-background.txt && autorandr --change --force")
    , ("M-<F5>",         spawn "background-picker --selected-image-file ~/.cache/selected-background.txt --directory ~/Documents/Dropbox/Pictures/SharedBackgrounds")
    , ("M-<F6>",         spawn "ManageTouchpad on")
    -- Compositor
    , ("M-t",            spawn "pkill picom")
    , ("M-C-t",          spawn "picom -b")
    -- Notifications
    , ("M-S-d",          spawn "killall dunst; notify-send 'restart dunst'")
    -- Screenshots
    , ("<Print>",        spawn "i3-scrot")
    , ("M-<Print>",      spawn "i3-scrot -w")
    , ("M-S-<Print>",    spawn "i3-scrot -s")
    -- Utilities
    , ("M-C-x",          spawn "xkill")
    -- Focus (vim-style: j=down, k=up; l/; mirror j/k for right-hand reach)
    , ("M-j",            windows W.focusDown)
    , ("M-k",            windows W.focusUp)
    , ("M-l",            windows W.focusDown)
    , ("M-;",            windows W.focusUp)
    -- Focus (arrow keys)
    , ("M-<Left>",       windows W.focusDown)
    , ("M-<Down>",       windows W.focusDown)
    , ("M-<Up>",         windows W.focusUp)
    , ("M-<Right>",      windows W.focusUp)
    -- Move windows (vim-style)
    , ("M-S-j",          windows W.swapDown)
    , ("M-S-k",          windows W.swapUp)
    , ("M-S-l",          windows W.swapDown)
    , ("M-S-;",          windows W.swapUp)
    -- Move windows (arrow keys)
    , ("M-S-<Left>",     windows W.swapDown)
    , ("M-S-<Down>",     windows W.swapDown)
    , ("M-S-<Up>",       windows W.swapUp)
    , ("M-S-<Right>",    windows W.swapUp)
    -- Workspace cycling
    , ("M-C-<Right>",    nextWS)
    , ("M-C-<Left>",     prevWS)
    -- Layout (h/v resize master area; q/e/s/w cycle through layouts)
    , ("M-h",            sendMessage Shrink >> spawn "notify-send 'tile horizontally'")
    , ("M-v",            sendMessage Expand >> spawn "notify-send 'tile vertically'")
    , ("M-q",            sendMessage NextLayout)
    , ("M-e",            sendMessage NextLayout)
    , ("M-s",            sendMessage NextLayout)
    , ("M-w",            sendMessage NextLayout)
    -- Fullscreen
    , ("M-f",            sendMessage ToggleLayout)
    -- Floating
    , ("M-S-<Space>",    withFocused toggleFloat)
    , ("M-<Space>",      windows W.focusMaster)
    -- Sticky (copy window to all workspaces)
    , ("M-S-s",          windows copyToAll)
    -- Focus master / parent approximation
    , ("M-a",            windows W.focusMaster)
    -- Scratchpad
    , ("M--",            namedScratchpadAction myScratchpads "scratchpad")
    , ("M-S--",          withFocused $ windows . W.sink)
    -- Move workspace between monitors (i3 uses M-C-> / M-C-< but those require Shift on US keyboard)
    , ("M-C-.",          shiftNextScreen >> nextScreen)
    , ("M-C-,",          shiftPrevScreen >> prevScreen)
    -- Restart / quit
    , ("M-S-r",          restart "xmonad" True)
    , ("M-S-e",          io exitSuccess)
    -- System mode: M-0 then l/s/u/e/h/r/S-s
    , ("M-0",            systemModeSubmap)
    ]
    ++
    [ ("M-" ++ show i,   windows $ W.greedyView ws)
    | (i, ws) <- zip [1..9 :: Int] myWorkspaces
    ]
    ++
    [ ("M-C-" ++ show i, windows $ W.shift ws)
    | (i, ws) <- zip [1..9 :: Int] myWorkspaces
    ]
    ++
    [ ("M-S-" ++ show i, moveAndFollow ws)
    | (i, ws) <- zip [1..9 :: Int] myWorkspaces
    ]

-- =============================================================================
-- STARTUP
-- =============================================================================

myStartupHook = do
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
    spawnOnce "dunst"
    spawnOnce "picom -b"
    spawnOnce "dropbox"
    spawnOnce "meteo-qt"
    spawnOnce "nm-applet"
    spawnOnce "xfce4-power-manager"
    spawnOnce "udiskie --tray"
    spawnOnce "system-config-printer-applet"
    spawnOnce "pamac-tray"
    spawnOnce "blueman-applet"
    spawnOnce "snixembed"
    spawn "sleep 3 && autorandr --change"
    spawn "~/.local/bin/ManageTouchpad off"
    spawn "sleep 20 && pasystray"
    spawn "firefox-developer-edition"
    spawn "xmodmap -e 'keycode 94 = Shift_L'"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = xmonad
    $ withUrgencyHook NoUrgencyHook
    $ setEwmhActivateHook doAskUrgent
    . ewmh
    . ewmhFullscreen
    . docks
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) (\_ -> (myModMask, xK_m))
    $ def
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces         = myWorkspaces
        , layoutHook         = avoidStruts myLayouts
        , manageHook         = myManageHook
        , startupHook        = myStartupHook
        , focusFollowsMouse  = True
        } `additionalKeysP` myKeys
