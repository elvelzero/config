-------------------------------------------------------------------------- IMPORT --------------------------------------------------------------------------
  -- Base
import XMonad
import System.IO
import System.Exit
import qualified XMonad.StackSet as W

  -- Action
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

  -- Data -- try to delete this
import Data.Monoid
import qualified Data.Map as M

  -- Hooks -- try to delete things in ()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

  -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
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
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"
    -- modkey
myModMask       = mod4Mask
    -- apps
myLauncher      = "dmenu_run"
myTerminal      = "kitty"
myFM            = "spacefm"
myBrowser       = "google-chrome-stable"
aBrowser        = "google-chrome-stable --incognito"

myWifi          = "nmcli device wifi connect Cloud"

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

------------------------------------------------------------------------- GRID SELECT -------------------------------------------------------------------------
spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    -- where conf = defaultGSConfig
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [   
              --   ("Terminal"             , myTerminal)
              -- , ("Browser"              , myBrowser)
              -- , ("File Manager"         , myFM)
              -- , ("Docs Editor"          , myDocEditor)
              -- , ("Docs Viewer"          , myDocViewer)
                ("Raster Image Editor"  , myRasterImgEdit)
              , ("Vector Image Editor"  , myVectorImgEdit)
              , ("Video Player"         , myVidPlayer)
              , ("Video Recorder"       , myVidRecorder)
              , ("Video Editor"         , myVidEditor)
              , ("Android Studio"       , "android-studio")
              , ("Intellij IDEA"        , "idea-ce")
              , ("Visual Studio Code"   , "code")
            ]
------------------------------------------------------------------------- KEYBINDS -------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [ 
      -- core apps
      ( "M-r"           , spawn myLauncher )
    , ( "M-S-r"         , spawnSelected' myAppGrid )
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
    , ( "M-n"           , spawn myWifi )
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
    -- , ( "M-<Page_Up>"               , nextWS )
    -- , ( "M-<Page_Down>"             , prevWS )
    , ( "M-<Up>"               , nextWS )
    , ( "M-<Down>"             , prevWS )
    , ( "M-<Page_Up>"          , moveTo Next NonEmptyWS )
    , ( "M-<Page_Down>"        , moveTo Prev NonEmptyWS )
    , ( "M-S-<Page_Up>"        , moveTo Next EmptyWS )
    , ( "M-S-<Page_Down>"      , moveTo Prev EmptyWS )
      -- Copy windows
    , ( "M-x"      , windows copyToAll )
    , ( "M-S-x"    , killAllOtherCopies )
      -- toogle fullscreen no border
    , ("M-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    ]

  -- Mouse bind
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
tall        = renamed [Replace "tall"]
            $ smartBorders
            $ mySpacing 7
            $ Tall 1 (3/100) (1/2)

mirrorTall  = renamed [Replace "mirrorTall"]
            $ mySpacing 7
            $ Mirror (Tall 1 (3/100) (1/2))

full        = renamed [Replace "tall"]
            $ smartBorders
            $ Full

  -- Layout hook
myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout  = tall
                                ||| mirrorTall
                                ||| full 

--------------------------------------------------------------------------- HOOKS ---------------------------------------------------------------------------
  -- Startup hook
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"

  -- myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
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

    , className =? "Gimp"           --> doFloat
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
              , ppSep             =  "<fc=#666666> <fn=1>|</fn> </fc>"                                                                        -- Separator character
              , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"                                                                   -- Urgent workspace
              , ppExtras          = [windowCount]                                                                                             -- # of windows current workspace
              -- , ppOrder           = \(ws:l:t:ex) -> [ws,l]++ex++[t]                                                                           -- order of things in xmobar
              , ppOrder           = \(ws:l:_:ex) -> [ws,l]++ex                                                                                -- order of things in xmobar
              }

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
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        -- hooks  
        , manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
                               -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                               -- This works perfect on SINGLE monitor systems. On multi-monitor systems,
                               -- it adds a border around the window if screen does not have focus. So, my solution
                               -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                               -- <+> fullscreenEventHook
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook -- showWName' myShowWNameTheme $ myLayoutHook
        , logHook            = myLogHook xmproc
        -- keybinds
        , mouseBindings      = myMouse
        } 
        `additionalKeysP`      myKeys
------------------------------------------------------------------------------------------------------------------------------------------------------------
