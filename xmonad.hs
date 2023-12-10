import System.IO(hPutStrLn)

import XMonad
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

import qualified XMonad.Hooks.EwmhDesktops as E

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

-- deb packages required:
-- - libghc-xmonad-dev
-- - xmobar

myConfig = def {
    modMask = mod4Mask, -- left windows super
    focusFollowsMouse = False
  } `additionalKeysP` [
    ("M-S-p", spawn "kupfer"),
    ("M-c", spawn "anamnesis -b"),
    ("M-<Print>", spawn "shutter -s"),
    ("M-S-l", spawn "xscreensaver-command --lock"),
    ("<XF86MonBrightnessUp>", spawn "brightness.sh +"),
    ("<XF86MonBrightnessDown>", spawn "brightness.sh -"),
    ("<XF86AudioMute>", spawn "amixer -D pulse set Master 1+ toggle"),
    ("<XF86AudioLowerVolume>", spawn "amixer -D pulse set Master 10%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse set Master 10%+")
  ]

myLogHook spw = dynamicLogWithPP xmobarPP {
  ppOutput = hPutStrLn $ spw
}

myManageHook = manageDocks <+>
  manageHook myConfig <+> 
  fullscreenManageHook

myLayoutHook = smartBorders $ fullscreenFull $ avoidStruts $ layoutHook myConfig

myEventHook = E.ewmhDesktopsEventHook <+> 
  E.fullscreenEventHook <+> 
  fullscreenEventHook <+>
  docksEventHook 

main = do
  spwXMobar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  spwXMonadRc <- spawnPipe "~/.xmonad/xmonadrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ E.ewmh myConfig {
    logHook = myLogHook spwXMobar,
    manageHook = myManageHook,
    layoutHook = myLayoutHook,
    handleEventHook = myEventHook
  }
