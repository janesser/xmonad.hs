import System.IO(hPutStrLn)

import XMonad
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

myConfig = defaultConfig {
    modMask = mod4Mask, -- left windows super
    focusFollowsMouse = False
  } `additionalKeysP` [
    -- bzr branch bzr://anamnesis.bzr.sourceforge.net/bzrroot/anamnesis trunk
    ("M-c", spawn "anamnesis -b"),
    ("M-<Print>", spawn "shutter -s")]

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
    xmonad $ withUrgencyHook NoUrgencyHook $ E.ewmh myConfig {
	logHook = myLogHook spwXMobar,
	manageHook = myManageHook,
	layoutHook = myLayoutHook,
	handleEventHook = myEventHook
    }
