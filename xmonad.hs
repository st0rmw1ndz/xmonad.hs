{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.Map qualified as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Text.Printf
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.ToggleFullFloat
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, manageDocks)
import XMonad.Hooks.Place
import XMonad.Hooks.StatusBar
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (named)
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.NamedScratchpad

myTerminal, myEditor :: String
myTerminal = "st"
myEditor = "nvim"

myWorkspaces :: [String]
myWorkspaces = ["1:term", "2:www", "3:mus", "4:chat", "5:file", "6:dev", "7:vol", "8:sys", "9"]

myBorderWidth :: Dimension
myBorderWidth = 1

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#222222"
myFocusedBorderColor = "#80b7ff"

floatClasses, swallowClasses :: [String]
floatClasses = ["Arandr", "Nsxiv"]
swallowClasses = ["St", "XTerm"]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {XMonad.modMask = modMask, XMonad.workspaces = workspaces}) =
  M.fromList $
    [ -- Common programs
      ((modMask, xK_Return), spawn myTerminal),
      ((modMask, xK_p), spawnDmenu "dmenu_run"),
      ((modMask, xK_w), spawn "firefox"),
      ((modMask .|. controlMask, xK_q), spawn "qutebrowser"),
      ((modMask, xK_e), spawnTerminal myEditor),
      ((modMask, xK_f), spawnTerminal "lf"),
      ((modMask, xK_t), spawnTerminal "ncmpcpp"),
      ((modMask .|. controlMask, xK_e), spawnTerminal "ncspot"),
      ((modMask, xK_v), spawnTerminal "pulsemixer"),
      ((modMask .|. controlMask, xK_y), spawn "pavucontrol"),
      ((modMask .|. controlMask, xK_semicolon), spawn "simplescreenrecorder"),
      ((modMask .|. controlMask, xK_apostrophe), spawn "obs"),
      ((modMask .|. controlMask, xK_o), spawnTerminal "cava"),
      ((modMask .|. controlMask, xK_d), spawn "arandr"),
      ((modMask .|. controlMask, xK_v), spawn "screenlayouts-open"),
      ((modMask .|. controlMask, xK_w), spawn "wallpapers-open"),
      ((modMask .|. controlMask, xK_bracketright), spawn "find ~/pictures/screenshots -type f | sort -r | nsxiv -ti"),
      -- Screenshotting
      ((0, xK_Print), spawn "epicshot -cs select"),
      ((controlMask, xK_Print), spawn "epicshot -cs full"),
      ((modMask, xK_Print), spawn "epicshot -so select"),
      ((modMask .|. controlMask, xK_r), spawn "epicshot -cs select"),
      ((modMask .|. controlMask, xK_t), spawn "epicshot -cs full"),
      ((modMask .|. controlMask, xK_g), spawn "epicshot -so select"),
      -- Faux function row
      ((modMask .|. controlMask, xK_F5), spawn "mpc prev"),
      ((modMask .|. controlMask, xK_F6), spawn "mpc next"),
      ((modMask .|. controlMask, xK_F7), spawn "mpc toggle"),
      ((modMask .|. controlMask, xK_F8), spawn "mpc stop"),
      ((modMask .|. controlMask, xK_F9), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && pipe_volume"),
      ((modMask .|. controlMask, xK_F10), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%- && pipe_volume"),
      ((modMask .|. controlMask, xK_F11), spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+ && pipe_volume"),
      ((modMask .|. controlMask, xK_F12), spawn "run-i3lock"),
      -- Special keys
      ((0, xF86XK_Explorer), spawnTerminal "nnn"),
      ((0, xF86XK_Search), spawnDmenu "dmenu_run"),
      ((0, xF86XK_Calculator), spawnTerminal "bc -i"),
      ((0, xF86XK_Tools), spawnTerminal "ncmpcpp"),
      ((0, xF86XK_AudioPrev), spawn "mpc prev"),
      ((0, xF86XK_AudioNext), spawn "mpc next"),
      ((0, xF86XK_AudioPlay), spawn "mpc toggle"),
      ((0, xF86XK_AudioStop), spawn "mpc stop"),
      ((0, xF86XK_AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle && pipe_volume"),
      ((0, xF86XK_AudioLowerVolume), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ 5%- && pipe_volume"),
      ((0, xF86XK_AudioRaiseVolume), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ 5%+ && pipe_volume"),
      -- Layout switching
      ( (modMask, xK_a),
        submap . M.fromList $
          [ ((modMask, xK_t), sendMessage $ JumpToLayout "Tall"),
            ((modMask, xK_y), sendMessage $ JumpToLayout "Wide"),
            ((modMask, xK_g), sendMessage $ JumpToLayout "Grid"),
            ((modMask, xK_f), sendMessage $ JumpToLayout "Full"),
            ((modMask, xK_s), sendMessage $ JumpToLayout "Spiral"),
            ((modMask, xK_a), sendMessage NextLayout)
          ]
      ),
      ((modMask, xK_b), sendMessage ToggleStruts),
      -- Basic window management
      ((modMask, xK_j), windows W.focusDown),
      ((modMask, xK_k), windows W.focusUp),
      ((modMask .|. shiftMask, xK_j), windows W.swapDown),
      ((modMask .|. shiftMask, xK_k), windows W.swapUp),
      -- Master control
      ((modMask, xK_h), sendMessage Shrink),
      ((modMask, xK_l), sendMessage Expand),
      ((modMask, xK_i), sendMessage $ IncMasterN 1),
      ((modMask, xK_d), sendMessage $ IncMasterN $ -1),
      ((modMask, xK_s), windows W.focusMaster),
      ((modMask .|. shiftMask, xK_s), windows W.swapMaster),
      -- Window actions
      ((modMask .|. shiftMask, xK_c), kill),
      ((modMask .|. shiftMask, xK_f), withFocused toggleFullFloat),
      ((modMask .|. shiftMask, xK_space), withFocused toggleFloat),
      -- Scratchpads
      ((modMask .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "terminal"),
      ((modMask .|. controlMask, xK_c), namedScratchpadAction myScratchPads "ncmpcpp"),
      -- Session
      ((modMask .|. controlMask, xK_l), spawn "xscreensaver-command -lock"),
      ((modMask .|. controlMask, xK_s), spawn "xmonad --restart && notify-send 'xmonad' 'Successfully recompiled and restarted.'"),
      ((modMask .|. controlMask, xK_Delete), io exitSuccess)
    ]
      -- Workspace viewing and shifting
      ++ [ ((modMask .|. m, k), windows $ f i)
           | (k, i) <- zip [xK_1 .. xK_9] workspaces,
             (m, f) <- [(0, W.view), (shiftMask, W.shift)]
         ]
  where
    toggleFloat :: Window -> X ()
    toggleFloat w = do
      isFloating <- gets (M.member w . W.floating . windowset)
      if isFloating
        then windows $ W.sink w
        else withFocused float

    spawnTerminal :: String -> X ()
    spawnTerminal x = do
      spawn $ printf "%s -e %s" myTerminal x

    spawnDmenu :: String -> X ()
    spawnDmenu x = do
      spawn $ printf "%s -h %s -fn %s -nb '%s' -nf '%s' -sb '%s' -sf '%s'" x "17" "Terminus-8" "#1d2021" "#ebdbb2" "#fabd2f" "#282828"

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ -- Float and move window
      ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster),
      -- Shift window to master
      ((modMask, button2), \w -> focus w >> windows W.shiftMaster),
      -- Float and resize window
      ((modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster),
      -- Go to previous workspace
      ((modMask, button4), const prevWS),
      -- Go to next workspace
      ((modMask, button5), const nextWS),
      -- Shift window to previous workspace
      ((modMask .|. shiftMask, button4), \_ -> shiftToPrev >> prevWS),
      -- Shift window to next workspace
      ((modMask .|. shiftMask, button5), \_ -> shiftToNext >> nextWS)
    ]

myLayoutHook =
  lessBorders OnlyScreenFloat $
    spacing 8 $
      avoidStruts $
        tall ||| wide ||| grid ||| full
  where
    tall =
      named "Tall" $
        Tall 1 (3 / 100) (1 / 2)
    wide =
      named "Wide" $
        Mirror tall
    grid =
      named
        "Grid"
        Grid
    full =
      named
        "Full"
        Full

-- SCRATCHPADS ---------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ constructScratchpad "terminal" "scTerminal" Nothing,
    constructScratchpad "ncmpcpp" "scNcmpcpp" (Just "ncmpcpp")
  ]
  where
    constructScratchpad :: String -> String -> Maybe String -> NamedScratchpad
    constructScratchpad name cls maybeExec =
      NS
        name
        ( case maybeExec of
            Just exec -> printf "%s -c %s -e %s" myTerminal cls exec
            Nothing -> printf "%s -c %s" myTerminal cls
        )
        (className =? cls)
        (customFloating $ W.RationalRect (3 / 5) (4 / 6) (1 / 5) (1 / 6))

-- HOOKS ---------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ placeHook simpleSmart,
      manageDocks,
      namedScratchpadManageHook myScratchPads,
      insertPosition End Newer,
      composeAll [className =? c --> doFloat | c <- floatClasses]
    ]

myEventHook :: Event -> X All
myEventHook =
  swallowEventHook (foldr1 (<||>) $ map (className =?) swallowClasses) (return True)

myStartupHook :: X ()
myStartupHook = do
  spawn "initialize_pipes"

-- XMOBAR --------------------------------------------------------------

myPP :: PP
myPP =
  def
    { ppSep = sep " | ",
      ppCurrent = current . wrap "" "*",
      ppHidden = noScratchPad,
      ppHiddenNoWindows = const ""
    }
  where
    noScratchPad :: String -> String
    noScratchPad ws = if ws == "NSP" then "" else ws

    sep, current :: String -> String
    sep = xmobarColor "#777777" ""
    current = xmobarColor "#6eadff" ""

-- MAIN ----------------------------------------------------------------

main :: IO ()
main =
  xmonad
    . withSB (statusBarProp "xmobar-top" $ pure myPP)
    . withSB (statusBarProp "xmobar-bottom" $ pure myPP)
    . toggleFullFloatEwmhFullscreen
    . ewmhFullscreen
    . ewmh
    $ def
      { borderWidth = myBorderWidth,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        layoutHook = myLayoutHook,
        workspaces = myWorkspaces,
        manageHook = myManageHook,
        handleEventHook = myEventHook,
        startupHook = myStartupHook,
        mouseBindings = myMouseBindings,
        keys = myKeys,
        focusFollowsMouse = True,
        clickJustFocuses = False,
        modMask = mod4Mask
      }
