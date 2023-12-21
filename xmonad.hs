import System.IO (hPutStrLn)
import Data.List
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdateFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce

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
                      ]

myLogHook spw = dynamicLogWithPP xmobarPP{ppOutput = hPutStrLn spw}

myManageHook =
  composeAll
    [ className =? "stalonetray" --> doIgnore
    , role =? "browser" --> doShift "3:web"
    , className =? "code" --> doShift "4:ide"
    , title =? "WhatsApp Web" --> doShift "2:comm"
    , fmap ( "Element" `isPrefixOf`) title --> doShift "2:comm"
    , className =? "signal" --> doShift "2:comm"
    , className =? "thunderbird" --> doShift "2:comm"
    , title =? "YouTube" --> doShift "5:entertain"
    ] where role = stringProperty "WM_WINDOW_ROLE"

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

-- start comm.sh
-- start google-chrome
main = do
  spwXMobar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  spwXMonadRc <- spawnPipe ". ~/.xmonad/xmonadrc"
  xmonad
    $ docks
    . ewmhFullscreen
    . ewmh
    . withUrgencyHook NoUrgencyHook
    $ myConfig
      { logHook = myLogHook spwXMobar
      , manageHook =
          myManageHook <+> manageDocks <+> fullscreenManageHook
      , layoutHook = myLayoutHook
      , startupHook = myStartupHook
      , handleEventHook = focusOnMouseMove
      }
