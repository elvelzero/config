-------------------------------------------------------------------------- IMPORT --------------------------------------------------------------------------
  -- Base
import XMonad
import System.IO
import System.Exit
import qualified XMonad.StackSet as W

  -- Action
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS

  -- Data
import qualified Data.Map as M

  -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

  -- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Ungrab

--------------------------------------------------------------------------- VARS ---------------------------------------------------------------------------
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor
myBorderWidth = 2           -- Sets border width for windows
myNormColor   = "#282c34"   -- Border color of normal windows
myFocusColor  = "#46d9ff"   -- Border color of focused windows
windowCount   = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

-- Start my vars
    -- font
myFont = "xft:Baekmuk Batang:style=bold italic:size=10"
    -- modkey
myModMask       = mod4Mask
    -- apps
-- myLauncher0     = "dmenu_run"
myLauncher      = "rofi -show drun -config ~/.config/rofi/themes/solarized.rasi -display-drun \"Run: \" -drun-display-format \"{name}\" -show-icons"
myTerminal      = "kitty"
myFM            = "spacefm"
myBrowser       = "google-chrome-stable"
aBrowser        = "google-chrome-stable --incognito"

myWallpaperSet  = "nitrogen"

myDocViewer     = "zathura"
myDocEditor     = "libreoffice"

myRasterImgEdit = "gimp"
myVectorImgEdit = "inkscape"

myVidPlayer     = "celluloid"
myVidRecorder   = "obs"
myVidEditor     = "olive-editor"

myLockScreen    = "./.config/i3lock-color/lock.sh"
myScreenshot    = "scrot ~/my/pictures/screenshots/scrot.png"

myNetworkMan    = "kitty -e nmtui-connect"
myTaskMan       = "kitty -e htop"

    -- system
brightSet       = "xbacklight = 70"
brightUp        = "xbacklight + 10"
brightDown      = "xbacklight - 10"
volUp           = "amixer set Master 5%+ unmute"
volDown         = "amixer set Master 5%- unmute"
volMute         = "amixer set Master toggle"

------------------------------------------------------------------------- KEYBINDS -------------------------------------------------------------------------
  -- Keyboard
myKeys :: [(String, X ())]
myKeys =
    [ 
      -- core apps
      ( "M-r"           , spawn myLauncher )
    , ( "M-<Return>"    , spawn myTerminal )
    , ( "M-e"           , spawn myFM )
    , ( "M-b"           , spawn myBrowser )
    , ( "M-S-b"         , spawn aBrowser )
      -- basic apps
    , ( "M-w"           , spawn myWallpaperSet )
    , ( "M-o"           , spawn myDocEditor )
    , ( "M-S-o"         , spawn myDocViewer )
    , ( "M-g"           , spawn myRasterImgEdit )
    , ( "M-S-g"         , spawn myVectorImgEdit )
    , ( "M-v"           , spawn myVidPlayer )
    , ( "M-C-v"         , spawn myVidRecorder )
    , ( "M-S-v"         , spawn myVidEditor )
      -- other apps 
    , ( "M-S-s"         , spawn myLockScreen )
    , ( "M-p"           , unGrab *> spawn myScreenshot )
      -- utilities
    , ( "M-m"           , spawn myNetworkMan )
    , ( "M-S-m"         , spawn myTaskMan )
      -- multimedia
    , ( "<XF86AudioMute>"        , spawn volMute )
    , ( "<XF86AudioLowerVolume>" , spawn volDown )
    , ( "<XF86AudioRaiseVolume>" , spawn volUp )
      -- Brightness, my xf86 key doesnt work :(
    , ( "C-<XF86AudioMute>"         , spawn brightSet )
    , ( "C-<XF86AudioLowerVolume>"  , spawn brightDown )
    , ( "C-<XF86AudioRaiseVolume>"  , spawn brightUp )
      -- Workspace cycle
    , ( "M-<Up>"               , nextWS )
    , ( "M-<Down>"             , prevWS )
    , ( "M-<Page_Up>"          , moveTo Next NonEmptyWS )
    , ( "M-<Page_Down>"        , moveTo Prev NonEmptyWS )
    , ( "M-S-<Page_Up>"        , moveTo Next EmptyWS )
    , ( "M-S-<Page_Down>"      , moveTo Prev EmptyWS )
      -- Window cycle
    , ( "M-h"      , windows W.focusUp ) -- focus to previous window
    , ( "M-j"      , windows W.focusUp ) -- focus to previous window
    , ( "M-k"      , windows W.focusDown ) -- focus to next window
    , ( "M-l"      , windows W.focusDown ) -- focus to next window
      -- Window resizer
    , ("M-C-h", sendMessage Shrink)       -- resize window left
    , ("M-C-l", sendMessage Expand)       -- resize window right
    , ("M-C-j", sendMessage MirrorShrink) -- resize window down
    , ("M-C-k", sendMessage MirrorExpand) -- resize window up
      -- Window swap
    , ( "M-S-h"      , windows W.focusMaster ) -- focus to master
    , ( "M-S-l"      , windows W.swapMaster ) -- swap focused window to master
    , ( "M-S-j"      , windows W.swapDown ) -- swap focused window with the next window
    , ( "M-S-k"      , windows W.swapUp ) -- swap focused window with the previous window
      -- Copy windows
    , ( "M-x"      , windows copyToAll )
    , ( "M-S-x"    , killAllOtherCopies )
      -- toogle fullscreen no border
    , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    ]

  -- Mouse
myMouse (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ -- mod-button1, Set the window to floating mode and move by dragging
        ( (modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
      --   mod-button2, Raise the window to the top of the stack
      , ( (modm, button2), (\w -> focus w >> windows W.shiftMaster))
      --   mod-button3, Set the window to floating mode and resize by dragging
      , ( (modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
      --   Workspaces cycles with mouse scrolls; ALT = mod1Mask, CTRL = controlMask, SHIFT = shiftMask
      , ( (modm, button4)                 , (\w -> nextWS) )
      , ( (modm, button5)                 , (\w -> prevWS) )
      , ( (modm .|. shiftMask, button4)   , (\w -> moveTo Next NonEmptyWS) )
      , ( (modm .|. shiftMask, button5)   , (\w -> moveTo Prev NonEmptyWS) )
      , ( (modm .|. controlMask, button4) , (\w -> moveTo Next EmptyWS) )
      , ( (modm .|. controlMask, button5) , (\w -> moveTo Prev EmptyWS) )
    ]

------------------------------------------------------------------------- WORKSPACE -------------------------------------------------------------------------
  -- Space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
  -- Workspace names
-- myWorkspaces = ["나", "정", "모", "사", "지", "미", "다", "채", "쯔"]
myWorkspaces = [" 나 ", " 정 ", " 모 ", " 사 ", " 지 ", " 미 ", " 다 ", " 채 ", " 쯔 "]

--------------------------------------------------------------------------- LAYOUT ---------------------------------------------------------------------------
  -- Defining layouts
-- tall        = renamed [Replace "tall"]
--             $ smartBorders
--             $ mySpacing 7
--             $ Tall 1 (3/100) (1/2)

rszTall     = renamed [Replace "rszTile"]
            $ smartBorders
            $ mySpacing 7
            $ ResizableTall 1 (3/100) (1/2) []

mirrorTall  = renamed [Replace "mirrorTall"]
            $ smartBorders
            $ mySpacing 7
            $ Mirror (Tall 1 (3/100) (1/2))

full        = renamed [Replace "tall"]
            $ smartBorders
            $ Full

-- spirals     = renamed [Replace "spirals"]
--             $ smartBorders
--             $ mySpacing 7
--             $ spiral (6/7)

-- rspirals    = renamed [Replace "rspirals"]
--             $ smartBorders
--             $ mySpacing 7
--             $ reflectVert
--             $ spiral (6/7)

  -- Layout hook
myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout  = rszTall -- tall
                                ||| mirrorTall
                                ||| full
                                -- ||| spirals 
                                -- ||| rspirals 

--------------------------------------------------------------------------- HOOKS ---------------------------------------------------------------------------
  -- Startup hook
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"

  -- myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = insertPosition Below Newer <+> manageDocks <+> composeAll
    [ 
      className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "notification"    --> doFloat
    , className =? "splash"          --> doFloat
    , className =? "toolbar"         --> doFloat
    -- , wmName    =? "Open File"       --> doFloat   

    , className =? "Gimp"               --> doCenterFloat <+> doShift ( myWorkspaces !! 7 )
    , className =? "obs"                --> doShift ( myWorkspaces !! 8 )
    , className =? "jetbrains-idea-ce"  --> doCenterFloat
    , className =? "jetbrains-studio"   --> doCenterFloat
    , className =? "PacketTracer"       --> doCenterFloat
    
    -- , className =? "VirtualBox Machine" --> doFloat
    , className =? "VirtualBox" --> doFloat
    , title =? "Oracle VM VirtualBox Manager"  --> doFloat

    , isFullscreen -->  doFullFloat
    ]

  -- Log hook
myLogHook h = dynamicLogWithPP $ xmobarPP
              -- the following variables beginning with 'pp' are settings for xmobar.
              { ppOutput          = hPutStrLn h
              , ppCurrent         = xmobarColor "#eceff4" "" . wrap "<box type=Bottom width=2 mb=2 color=#63c5ea>" "</box>"                   -- Current workspace
              , ppVisible         = xmobarColor "#eceff4" "" -- . clickable                                                                   -- Visible but not current workspace
              , ppHidden          = xmobarColor "#eceff4" "" -- . wrap "<box type=Bottom width=2 mt=2 color=#63c5ea>" "</box>" -- . clickable -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#63c5ea" ""  -- . clickable                                                                  -- Hidden workspaces (no windows)
              , ppTitle           = xmobarColor "#b3afc2" "" . shorten 60                                                                     -- Title of active window
              , ppSep             =  " | "                                                                        -- Separator character
              , ppUrgent          = xmobarColor "#FA946E" "" . wrap "!" "!"                                                                   -- Urgent workspace
              , ppExtras          = [windowCount]                                                                                             -- # of windows current workspace
              -- , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]                                                                           -- order of things in xmobar
              , ppOrder           = \(ws:l:t:ex) -> [ws]++ex                                                                                -- order of things in xmobar
              }
  
myHandleEventHook = docksEventHook <+> fullscreenEventHook
                    -- docks <+> fullscreenEventHook

--------------------------------------------------------------------------- MAIN ---------------------------------------------------------------------------
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ ewmh def
        { 
        -- simple stuff
          modMask            = myModMask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        -- border
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        -- hooks  
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook xmproc
        , startupHook        = myStartupHook
        -- keybinds
        , mouseBindings      = myMouse
        } `additionalKeysP`    myKeys
------------------------------------------------------------------------------------------------------------------------------------------------------------
