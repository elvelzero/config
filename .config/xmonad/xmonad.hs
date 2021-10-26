---------------------------------------------------------------------------------------------------------------------------
-- Base
import XMonad
import System.IO

-- Layout modifier
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Util
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
---------------------------------------------------------------------------------------------------------------------------
-- My Variables
myBar           = xmobar

myBorderWidth   :: Dimension
myBorderWidth   = 2           -- Sets border width for windows

myNormColor     :: String
myNormColor     = "#282c34"   -- Border color of normal windows

myFocusColor    :: String
myFocusColor    = "#46d9ff"   -- Border color of focused windows

-- My vars
    -- modkey
myModMask       = mod4Mask
    -- apps
myLauncher      = "dmenu_run"
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

-- Keybinds
myKeys      :: [(String, X ())]
myKeys      =
    [ 
      -- core apps
      ( "M-r"           , spawn myLauncher )
    , ( "M-S-<Return>"  , spawn myTerminal )
    , ( "M-e"           , spawn myFM )
    , ( "M-["           , spawn myBrowser )
    , ( "M-]"           , spawn aBrowser )
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
    ]

-- Space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Defining Layout
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

-- My Layout   
myLayout    = dLayout
            where
                dLayout     =   tall
                            ||| mirrorTall
                            ||| full 

-- My WorkSpace
-- workspace name
-- Na = 나, Jeong = 정, Mo = 모, Sa = 사, Ji = 지, Mi = 미, Da = 다, Chae = 채, Tzu = 쯔
-- myWorkspaces = [" 1 나 ", " 2 정 ", " 3 모 ", " 4 사 ", " 5 지 ", " 6 미 ", " 7 다 ", " 8 채 ", " 9 쯔 "]
-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" 나 ", " 정 ", " 모 ", " 사 ", " 지 ", " 미 ", " 다 ", " 채 ", " 쯔 "]
myWorkspaces = [" \xf8a3 ", " \xf8a6 ", " \xf8a9 ", " \xf8ac ", " \xf8af ", " \xf8b2 ", " \xf8b5 ", " \xf8b8 ", " \xf8bb "]

-- My logHook
myLogHook   = dynamicLogWithPP $ xmobarPP 
                {
                -- ppOutput = hPutStrLn
                ppCurrent = xmobarColor "#c792ea" "" . wrap "<box type=Bottom width=2 mb=2 color=#c792ea>" "</box>"
                , ppVisible = xmobarColor "#c792ea" ""
                , ppHidden  = xmobarColor "#82AAFF" "" . wrap "<box type=Top width=2 mt=2 color=#82AAFF>" "</box>"
                , ppHiddenNoWindows = xmobarColor "#82AAFF" ""
                -- , ppTitle = xmobarColor "#b3afc2" "" . shorten 0
                , ppOrder   = \(ws:l:_:_) -> [ws,l]
                }

-- My manage hook
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

    , className =? "Gimp"           --> doFloat
    , title =? "Oracle VM VirtualBox Manager"  --> doFloat
    , isFullscreen -->  doFullFloat
    ]

-- My Config
myConfig    = def
                { 
                -- Variables
                  modMask           = myModMask
                
                -- Border
                , borderWidth       = myBorderWidth
                , normalBorderColor = myNormColor
                , focusedBorderColor= myFocusColor

                --Hook
                , layoutHook        = myLayout
                , logHook           = myLogHook
                , manageHook        = myManageHook

                --Workspace
                , workspaces        = myWorkspaces
                }
              -- Keybinds  
              `additionalKeysP`  myKeys

-- Main Fun
main :: IO ()
main = xmonad . ewmh =<< myBar myConfig
