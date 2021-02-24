{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Control.Exception ( try, SomeException )
import Control.Monad
import Control.Monad.Fix
import Data.List ( intercalate, isPrefixOf, nub )
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Directory ( getCurrentDirectory )
import System.Environment
import System.IO.Unsafe
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types

import XMonad.Actions.CycleWS
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers hiding (pid)
import XMonad.Hooks.Place (placeHook, fixed)
-- import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.TrackFloating
import XMonad.Layout.WorkspaceDir
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Stack
import XMonad.Util.Ungrab
import qualified XMonad.Util.PureX as P
import XMonad.Util.EZConfig

import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Hooks.InsertPosition
import XMonad.Layout.SimpleDecoration (shrinkText)
import XMonad.Actions.GroupNavigation
-- Imitate dynamicLogXinerama layout
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.ManageHelpers
-- Order screens by physical location
import XMonad.Actions.PhysicalScreens
import Data.Default
-- For getSortByXineramaPhysicalRule
import XMonad.Layout.LayoutCombinators
-- smartBorders and noBorders
import XMonad.Layout.NoBorders
-- spacing between tiles
import XMonad.Layout.Spacing
-- Insert new tabs to the right: https://stackoverflow.com/questions/50666868/how-to-modify-order-of-tabbed-windows-in-xmonad?rq=1
-- import XMonad.Hooks.InsertPosition
import XMonad.Layout.Dwindle

-- Aplicaciones

myTerminal = "alacritty"
-- myTerminal = "termite"
--myStatusBar = "polybar aenami --log=error"
myStatusBar = "xmobar -o /home/blackraider/.xmobarrc"
myBottomStatusBar = "i3status -c /home/blackraider/.config/i3status/config | xmobar -b /home/blackraider/.xmobarrcbottom"

myTearFlickerCmd = "nvidia-settings --assign CurrentMetaMode='HDMI-0:1920x1080_60 +0+0 { ForceFullCompositionPipeline = On }'"

myRofiRun = "rofi -show run"
myRofiWindows = "rofi -show window"
myRofiDRun = "rofi -show drun"

myDefaultBrowser = "vivaldi"
myAlternateBrowser = "firefox"

myDefaultFileBrowser = "mc"
myAlternateFileBrowser = "ranger"

myEditor = "emacs"
myHome = "/home/blackraider"

myTray = "stalonetray"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myTabConfig = def { activeColor = "#556064"
                  , inactiveColor = "#2F3D44"
                  , urgentColor = "#FDF6E3"
                  , activeBorderColor = "#454948"
                  , inactiveBorderColor = "#454948"
                  , urgentBorderColor = "#268BD2"
                  , activeTextColor = "#80FFF9"
                  , inactiveTextColor = "#1ABC9C"
                  , urgentTextColor = "#1ABC9C"
                  , fontName = "xft:Noto Sans CJK:size=10:antialias=true"
                  }


myLayout = avoidStruts $ -- layoutHook defaultConfig $
  tiled
  ||| noBorders (tabbed shrinkText myTabConfig)
  ||| Mirror tiled
  ||| noBorders Full
  ||| twopane
  ||| Mirror twopane
  -- ||| emptyBSP
  ||| Spiral L XMonad.Layout.Dwindle.CW (3/2) (11/10) -- L means the non-main windows are put to the left.
  where
     -- The last parameter is fraction to multiply the slave window heights
     -- with. Useless here.
     tiled = ResizableTall nmaster delta ratio []              -- spacing 1 $ 
     -- In this layout the second pane will only show the focused window.
     twopane = TwoPane delta ratio                             -- spacing 1 $ 
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myPP = def { ppCurrent = xmobarColor "#1ABC9C" "" . wrap "[" "]"
           , ppTitle = xmobarColor "#1ABC9C" "" . shorten 60
           , ppVisible = wrap "(" ")"
           , ppUrgent  = xmobarColor "red" "yellow"
           , ppSort = getSortByXineramaPhysicalRule def
           }





myManageHook = composeAll [ isFullscreen --> doFullFloat ]  -- $
                   -- auto-float certain windows
--                 [ className =? c --> doCenterFloat | c <- myFloats ] -- (4)
--                 ++
--                 [ fmap (t `isPrefixOf`) title --> doFloat | t <- myFloatTitles ]
--                 -- windows to auto-float
--                 where myFloats = [ ]
--                       myFloatTitles = ["Euro Truck Simulator 2"]




-- Main
main = do
  -- Startup Apps
  --clipmenu <- spawnPipe "clipmenu"
  --polybar <- spawnPipe myStatusBar

  xmproc <- spawnPipe myStatusBar                    --"xmobar -o /home/blackraider/.xmobarrc"
  xmprocBottom <- spawnPipe myBottomStatusBar        --"i3status -c /home/blackraider/.config/i3status/config | xmobar -b /home/blackraider/.xmobarrcbottom"

  -- xmprocBottom <- spawnPipe "i3status | xmobar -b -t --font='Terminus' '%StdinReader%' -c '[Run StdinReader]'"
 
  tray <- spawnPipe myTray

  xnvidia <- spawn myTearFlickerCmd;


  -- XMonad
  xmonad $ ewmh $ docks $ def
    { terminal           = myTerminal
    , XMonad.modMask = mod4Mask
    , focusFollowsMouse  = True
    , borderWidth        = 2
    , keys               = myKeys
    , layoutHook         = myLayout  --avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppHiddenNoWindows = xmobarColor "grey" ""
                        , ppTitle   = xmobarColor "green"  "" . shorten 80
                        , ppVisible = wrap "(" ")"
                        , ppUrgent  = xmobarColor "red" "yellow"
                        }
    , manageHook         = myManageHook
    , workspaces         = myWorkspaces
    } -- `additionalKeys` myKeys




-- Key Bindings

myKeys conf@(XConfig {XMonad.modMask = mod4Mask}) =   M.fromList $[
           -- Menus
           (  (mod4Mask .|. controlMask , xK_p  ), unGrab >> spawn myRofiRun)
           , ((mod4Mask .|. controlMask, xK_m  ), unGrab >> spawn myRofiDRun)
           , ((mod4Mask .|. shiftMask,   xK_m  ), unGrab >> spawn myRofiWindows)

           -- Terminales
           , ((mod4Mask , xK_t ), spawn myTerminal)
           , ((mod4Mask , xK_slash ), spawn myTerminal)

           -- Programas
           , ((mod4Mask ,                xK_e ), spawn $ myTerminal ++ " -e " ++ myDefaultFileBrowser )
           , ((mod4Mask .|. shiftMask,   xK_e ), spawn $ myTerminal ++ " -e " ++ myAlternateFileBrowser )
           , ((mod4Mask .|. controlMask, xK_n ), spawn $ myDefaultBrowser )
           , ((mod4Mask .|. shiftMask,   xK_n ), spawn $ myAlternateBrowser )

           , ((mod4Mask ,               xK_backslash ), spawn myEditor )

           -- Teclas de Volumen
           , ((0,         xF86XK_AudioMute       ), spawn "exec amixer -D default sset Master mute")
           , ((0,         xF86XK_AudioLowerVolume), spawn "exec amixer -D default sset Master 5%- unmute")
           , ((0,         xF86XK_AudioRaiseVolume), spawn "exec amixer -D default sset Master 5%+ unmute")

           -- Cerrar Ventana
           , ((mod4Mask,               xK_Escape), kill)

           -- Movimientos
           , ((mod4Mask,                               xK_j     ), windows W.focusDown   )
           , ((mod4Mask,                               xK_k     ), windows W.focusUp     )
           , ((mod4Mask .|. controlMask,               xK_Down  ), windows W.focusDown   )
           , ((mod4Mask .|. controlMask,               xK_Up    ), windows W.focusUp     )
           , ((mod4Mask,                               xK_Return), windows W.swapMaster  )
           , ((mod4Mask .|. shiftMask,                 xK_j     ), windows W.swapDown    )
           , ((mod4Mask .|. shiftMask,                 xK_k     ), windows W.swapUp      )

           -- Tamanios
           , ((mod4Mask,               xK_h     ), sendMessage Shrink            )
           , ((mod4Mask,               xK_l     ), sendMessage Expand            )
           , ((mod4Mask,               xK_u     ), sendMessage MirrorShrink      )
           , ((mod4Mask,               xK_i     ), sendMessage MirrorExpand      )
           , ((mod4Mask,               xK_m     ), sendMessage (Toggle REFLECTX) )
           -- Reiniciar xmonad
           , ((mod4Mask,	       xK_q     ), spawn "xmonad --recompile; xmonad --restart" )
           -- Ventanas y Escritorios
           , ((mod4Mask .|. shiftMask, xK_Return  ), shiftTo Next EmptyWS)  -- Envia ventana a priner escritorio libre
           , ((mod4Mask .|. shiftMask, xK_Right   ), shiftToNext)           -- Envia ventana a siguiente escritorio
           , ((mod4Mask .|. shiftMask, xK_Left    ), shiftToPrev)           -- Envia ventana a escritorio anterior
           , ((mod4Mask,               xK_n       ), toggleWS    )          -- Cicla entre dos escritorios
           , ((mod4Mask,               xK_Left    ), prevWS      )          -- Escritorio anterior
           , ((mod4Mask,               xK_Right   ), nextWS      )          -- Siguiente escritorio
           , ((mod4Mask .|. controlMask, xK_Left  ), swapTo Prev )          -- Intercambia escritorios de posicion - Actual y Anterior
           , ((mod4Mask .|. controlMask, xK_Right ), swapTo Next )          -- Intercambia escritorios de posicion - Acturla y Siguiente
           -- Rotate through the available layout algorithms
           , ((mod4Mask,               xK_space ), sendMessage NextLayout)
           --  Reset the layouts on the current workspace to default
           , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
           , ((mod4Mask .|. shiftMask, xK_h), sendMessage $ JumpToLayout "Mirror Tall")
           , ((mod4Mask .|. shiftMask, xK_v), sendMessage $ JumpToLayout "Tall")
           , ((mod4Mask .|. shiftMask, xK_f), sendMessage $ JumpToLayout "Full")
           , ((mod4Mask .|. shiftMask, xK_t), sendMessage $ JumpToLayout "Tabbed Simplest")
           ]

-- Layouts


