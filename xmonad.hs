import System.IO(hPutStrLn)

import XMonad

import XMonad.Actions.UpdateFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen

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
    ("C-ö", spawn "diodon"),
    ("<Print>", spawn "shutter -s"),
    ("M-S-l", spawn "light-locker-command -l"),
    ("<XF86MonBrightnessUp>", spawn "brightness.sh +"),
    ("<XF86MonBrightnessDown>", spawn "brightness.sh -"),
    ("<XF86AudioMute>", spawn "amixer set Master 1+ toggle"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
  ]

myLogHook spw = dynamicLogWithPP xmobarPP {
  ppOutput = hPutStrLn spw
}

myManageHook = manageDocks <+> fullscreenManageHook

myLayoutHook = avoidStruts $ smartBorders (tall ||| half ||| full) where
    full = noBorders Full
    tall = Tall 1 (3/100) (2/3) -- M-S-Space to reset
    half = Tall 1 (3/100) (1/2) 

main = do
  spwXMobar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  spwXMonadRc <- spawnPipe "~/.xmonad/xmonadrc"
  xmonad $ docks . ewmhFullscreen . ewmh . withUrgencyHook NoUrgencyHook $ myConfig {
    logHook = myLogHook spwXMobar,
    manageHook = myManageHook,
    layoutHook = myLayoutHook,
    startupHook = adjustEventInput,
    handleEventHook = focusOnMouseMove
  }
