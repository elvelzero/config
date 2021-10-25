-- Base
import XMonad

-- Layout
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab

-- My Variables
myBar           = xmobar

myBorderWidth   :: Dimension
myBorderWidth   = 2           -- Sets border width for windows

myNormColor     :: String
myNormColor     = "#282c34"   -- Border color of normal windows

myFocusColor    :: String
myFocusColor    = "#46d9ff"   -- Border color of focused windows

-- Apps var
myModMask       = mod4Mask

myTerminal      = "kitty"

myLauncher      = "dmenu_run"

myBrowser       = "chromium"
aBrowser        = "chromium --incognito"

myScreenshot    = "scrot /home/arch/documents/pictures/screenshots/scrot.png"

-- Keybinds
myKeys      :: [(String, X ())]
myKeys      =
    [ 
      ("M-S-<Return>", spawn myTerminal     )
    , ("M-r"  , spawn myLauncher            )
    , ("M-["  , spawn myBrowser             )
    , ("M-]"  , spawn aBrowser              )
    , ("M-S-=", unGrab *> spawn myScreenshot)
    ]

-- Space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Defining Layout
tall        = renamed [Replace "tall"]
            $ mySpacing 7
            $ Tall 1 (3/100) (1/2)

mirrorTall  = renamed [Replace "mirrorTall"]
            $ mySpacing 7
            $ Mirror (Tall 1 (3/100) (1/2))

full        = renamed [Replace "tall"]
            $ Full

-- My Layout   
-- myLayout    = Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)) ||| Full 
myLayout    = dLayout
            where
                dLayout     =   tall
                            ||| mirrorTall
                            ||| full 

-- My WorkSpace
myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = ["미나 dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]

-- My logHook
myLogHook   = dynamicLogWithPP 
              xmobarPP {
                  ppTitle   = shorten 4
                , ppOrder   = \(ws: l : _ : _ ) -> [ws,l]
                }

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

                --Workspace
                , workspaces        = myWorkspaces
                }
              -- Keybinds  
              `additionalKeysP`  myKeys

-- Main Fun
main :: IO ()
main = xmonad . ewmh =<< myBar myConfig
