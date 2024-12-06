import Control.Monad
import Data.List.Split
import Data.Time
import System.IO (hPutStrLn)
import System.IO.Unsafe
import System.Directory
import Network.HostName

import XMonad
import XMonad.Config
import XMonad.Prelude (toLower)

import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Accordion
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad

import Data.List (intercalate)
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.Hacks
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

{-# NOINLINE orgToday #-}
orgToday = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y]" <$> getCurrentTime
{-# NOINLINE orgNow #-}
orgNow = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y %H:%M:%S]" <$> getCurrentTime

myWindowPromptConfig =
  def
    { searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    }

myKeysConfig :: XConfig l -> XConfig l
myKeysConfig =
  addDescrKeys
    ((mod4Mask .|. shiftMask, xK_h), noName . xKeysPrompt def)
    (myBasicKeys <+> myWindowKeys <+> myJournalKeys)

myBasicKeyMap =
  [ ("M-x", noName $ xmonadPrompt def)
  , ("M-e", spawn' "pcmanfm")
  , ("C-ö", spawn' "copyq toggle")
  , ("<Print>", spawn' "shutter -s")
  , ("M-S-l", spawn' "light-locker-command -l")
  , ("<XF86MonBrightnessUp>", spawn' "brightness.sh +")
  , ("<XF86MonBrightnessDown>", spawn' "brightness.sh -")
  ,
    ( "<XF86AudioMute>"
    , spawn' "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    )
  ,
    ( "<XF86AudioLowerVolume>"
    , spawn' "pactl set-sink-volume @DEFAULT_SINK@ -10%"
    )
  ,
    ( "<XF86AudioRaiseVolume>"
    , spawn' "pactl set-sink-volume @DEFAULT_SINK@ +10%"
    )
  , ("M-b", addName "sendMessage ToggleStruts" $ sendMessage ToggleStruts)
  , ("M-C-k", spawn' "xkill")
  , ("M-C-p", addName "xprops" $ spawn "x-terminal-emulator -e bash -c \"xprop && read -n 1 -p 'Press any key to continue..'\"")
  , ("M-m", addName "manPrompt" $ manPrompt def)
  ]
myBasicKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myBasicKeys c =
  subtitle "My Basic Keys"
    : mkNamedKeymap c myBasicKeyMap

xKeysPrompt :: XPConfig -> [((KeyMask, KeySym), NamedAction)] -> X ()
xKeysPrompt c keylist = do
  mkXPrompt Bindings c (mkComplFunFromList' c $ showKmSimple keylist) doIt
 where
  doIt k = spawn $ "xdotool key " ++ doToolKey k
  doToolKey = intercalate "+" . map doControlKey . splitOn "-"
  doControlKey "C" = "Control_L"
  doControlKey "S" = "Shift_L"
  doControlKey "M4" = "Super_L"
  doControlKey k = filter (/= '>') $ filter (/= '<') k

data Bindings = Bindings
instance XPrompt Bindings where
  showXPrompt Bindings = "Bindings: "

myWindowKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myWindowKeys c =
  subtitle "My Window Keys"
    : mkNamedKeymap
      c
      [ ("M-C-ä", spawn' "killall xcompmgr; xcompmgr -cCfF")
      , ("M-C-S-ä", spawn' "killall xcompmgr")
      , ("M-C-g", addName "windowPrompt goto" $ windowPrompt myWindowPromptConfig Goto allWindows)
      , ("M-C-b", addName "windowPrompt bring" $ windowPrompt myWindowPromptConfig Bring allWindows)
      , ("M-C-<Space>", addName "layoutScreens 4 Grid" $ layoutScreens 4 Grid)
      , ("M-C-S-<Space>", addName "rescreen" rescreen)
      , ("M-C-a", addName "copy window to all workspaces" $ windows copyToAll)
      , ("M-C-S-a", addName "kill all other window copies" $ killAllOtherCopies)
      ]

myJournalKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myJournalKeys c =
  subtitle "My Keys"
    : mkNamedKeymap
      c
      [ ("M-o j", addName "add TODO to journal" $ orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/journal.org")
      , ("M-o t", addName "add TODO to family todos" $ orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/orgzly/todos.org")
      , ("M-o e", addName "add entry to tochter1" $ orgPrompt def orgToday "~/Dropbox/orgzly/tochter1.org")
      , ("M-o S-j", spawn' "emacs ~/Dropbox/journal.org")
      , ("M-o S-t", spawn' "emacs ~/Dropbox/orgzly/todos.org")
      , ("M-o S-e", spawn' "emacs ~/Dropbox/orgzly/tochter1.org")
      ]

myConfig =
  myKeysConfig $
    def
      { modMask = mod4Mask -- left windows super
      , focusFollowsMouse = False
      , workspaces =
          [ "1:top"
          , "2:comm"
          , "3:web"
          , "4:ide"
          , "5:entertain"
          , "6:private"
          , "7:games"
          , "8:education"
          , "9:admin"
          ]
      }

myLogHook spw = dynamicLogWithPP xmobarPP{ppOutput = hPutStrLn spw}

myManageHook =
  composeAll
    [ -- comm
      className =? "Signal" --> doShift "2:comm"
    , className =? "Element" --> doShift "2:comm"
    , className =? "WhatSie" --> doShift "2:comm"
    , className =? "dev.geopjr.Tuba" --> doShift "2:comm"
    , className =? "Thunderbird" --> doShift "2:comm"
    , className =? "Evolution" --> doShift "2:comm"
    , className =? "Claws-mail" --> doShift "2:comm"
    , -- ide
      className =? "vscodium" --> doShift "4:ide"
    , -- entertain
      className =? "vlc" --> doSideFloat CE
    , role =? "PictureInPicture" --> doSideFloat CE
    , className =? "LibreWolf" --> doShift "3:web"
    , -- admin
      className =? "easyeffects" --> doShift "9:admin"
    ]
 where
  role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook =
  avoidStruts $
    onWorkspace "1:top" tall $
      onWorkspace "2:comm" tabbed $
        onWorkspace "3:web" accordion $
          onWorkspace "7:games" full $
            smartBorders (tall ||| tallM ||| full ||| tabbed ||| accordion)
 where
  full = renamed [Replace "Full"] $ noBorders Full
  tall = Tall 1 (3 / 100) (2 / 3) -- M-S-Space to reset
  tallM = renamed [Replace "Mirror Tall"] $ Mirror tall
  tabbed = renamed [Replace "Tabbed"] simpleTabbed
  accordion = Accordion

myStartupHook :: X ()
myStartupHook = do
  spawnOnOnce "1:top" "x-terminal-emulator -e btop"
  spawnOnOnce "3:web" "x-www-browser --restore-last-session"
  -- height needs to be explicit, check ToggleStruts
  spawnOnce "gtk-sni-tray-standalone --bottom --beginning --watcher"

myFadeHook =
  composeAll
    [ opaque
    , isUnfocused --> opacity (8 / 10)
    , liftM2 (&&) isFloating isUnfocused --> opacity (4 / 10)
    , className =? "vlc" --> opaque
    ]

main :: IO ()
main = do
  xmobarrcHostspecific <- ("~/.config/xmonad/xmobarrc." ++) <$> getHostName
  xmobarrcHostspecificExists <- doesFileExist xmobarrcHostspecific
  spwXMobar <-
    if xmobarrcHostspecificExists
      then spawnPipe $ "xmobar " ++ xmobarrcHostspecific
      else spawnPipe $ "xmobar ~/.config/xmonad/xmobarrc"
  spwXMonadRc <- spawnPipe "bash ~/.config/xmonad/xmonadrc" -- writes ~/.ssh/env
  xmonad
    $ docks
      . ewmhFullscreen
      . ewmh
      . withUrgencyHook NoUrgencyHook
      . javaHack
    $ myConfig
      { logHook = myLogHook spwXMobar <+> fadeWindowsLogHook myFadeHook
      , manageHook =
          manageDocks <+> myManageHook <+> fullscreenManageHook
      , layoutHook = myLayoutHook
      , startupHook = myStartupHook
      , handleEventHook = fadeWindowsEventHook <+> fixSteamFlicker -- <+> focusOnMouseMove
      , terminal = "x-terminal-emulator"
      }
