import Control.Monad (liftM2)
import Data.List
import Data.Time
import System.IO (hPutStrLn)
import System.IO.Unsafe
import Text.Printf
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdateFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

import XMonad.Prompt.Man

import XMonad.Prompt
import XMonad.Prompt.OrgMode

{-# NOINLINE orgToday #-}
orgToday = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y]" <$> getCurrentTime
{-# NOINLINE orgNow #-}
orgNow = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y %H:%M:%S]" <$> getCurrentTime

myConfig =
  def
    { modMask = mod4Mask -- left windows super
    , focusFollowsMouse = True
    , workspaces =
        [ "1:htop"
        , "2:comm"
        , "3:web"
        , "4:ide"
        , "5:entertain"
        , "6:adult"
        , "7:games"
        , "8:education"
        , "9:admin"
        ]
    }
    `additionalKeysP` [ ("M-e", spawn "pcmanfm")
                      , ("M-S-p", spawn "kupfer")
                      , ("C-ö", spawn "copyq toggle")
                      , ("<Print>", spawn "shutter -s")
                      , ("M-S-l", spawn "light-locker-command -l")
                      , ("C-ä", spawn "killall xcompmgr; xcompmgr -cCfF")
                      , ("C-ü", spawn "killall xcompmgr")
                      , ("<XF86MonBrightnessUp>", spawn "brightness.sh +")
                      , ("<XF86MonBrightnessDown>", spawn "brightness.sh -")
                      ,
                        ( "<XF86AudioMute>"
                        , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
                        )
                      ,
                        ( "<XF86AudioLowerVolume>"
                        , spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"
                        )
                      ,
                        ( "<XF86AudioRaiseVolume>"
                        , spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"
                        )
                      , ("M-o j", orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/orgzly/journal.org")
                      , ("M-o t", orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/orgzly/todo.org")
                      , ("M-o e", orgPrompt def orgToday "~/Dropbox/orgzly/tochter1.org")
                      , ("M-o S-j", spawn "emacs ~/Dropbox/orgzly/journal.org")
                      , ("M-o S-t", spawn "emacs ~/Dropbox/orgzly/todo.org")
                      , ("M-o S-e", spawn "emacs ~/Dropbox/orgzly/tochter1.org")
                      , ("M-m", manPrompt def)
                      ]

myLogHook spw = dynamicLogWithPP xmobarPP{ppOutput = hPutStrLn spw}

myManageHook =
  composeAll
    [ className =? "panel" --> doIgnore -- FIXME trayer on top - not reliable yet
    , className =? "google-chrome" --> doShift "3:web"
    , className =? "code" --> doShift "4:ide"
    , title =? "WhatsApp Web" --> doShift "2:comm"
    , fmap ("Element" `isPrefixOf`) title --> doShift "2:comm"
    , className =? "signal" --> doShift "2:comm"
    , className =? "thunderbird" --> doShift "2:comm"
    , fmap ("Youtube" `isPrefixOf`) title --> doShift "5:entertain"
    , fmap ("Spotify" `isPrefixOf`) title --> doShift "5:entertain"
    , className =? "vlc" --> doSideFloat CE
    ]
 where
  role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook =
  avoidStruts
    $ onWorkspaces ["1:htop", "3:web"] full
    $ onWorkspace "2:comm" tabbed
    $ onWorkspace "4:ide" tall
    $ smartBorders (tall ||| full ||| tabbed)
 where
  full = noBorders Full
  tall = Tall 1 (3 / 100) (2 / 3) -- M-S-Space to reset
  tabbed = simpleTabbed

myStartupHook :: X ()
myStartupHook = do
  spawnOnOnce "1:htop" "x-terminal-emulator -e htop"
  spawnOnOnce "2:comm" "comm.sh"
  spawnOnOnce "3:web" "google-chrome --restore-last-session"

myFadeHook =
  composeAll
    [ opaque
    , isUnfocused --> opacity (8 / 10)
    , liftM2 (&&) isFloating isUnfocused --> opacity (4 / 10)
    , className =? "vlc" --> opaque
    ]

main = do
  spwXMobar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  spwXMonadRc <- spawnPipe ". ~/.xmonad/xmonadrc"
  xmonad
    $ docks
    . ewmhFullscreen
    . ewmh
    . withUrgencyHook NoUrgencyHook
    $ myConfig
      { logHook = myLogHook spwXMobar <+> fadeWindowsLogHook myFadeHook
      , manageHook =
          manageDocks <+> myManageHook <+> fullscreenManageHook
      , layoutHook = myLayoutHook
      , startupHook = myStartupHook
      , handleEventHook = focusOnMouseMove <+> fadeWindowsEventHook
      }
