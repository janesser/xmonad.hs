-- stack build --stack-yaml /home/jan/.config/xmonad/stack.yaml

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Semigroup (Endo)
import FloatingVideos (
  PlaceVideos (PlaceVideos),
  RotateVideoFloat (RotateVideoFloat),
  ToggleSizeVideoFloat (ToggleSizeVideoFloat),
  floatingVideos,
  floatingVideosEventHook,
 )
import MouseGestures
import Network.HostName
import ScreenCornersToggled (
  ToggleScreenCorner (..),
  addVerticalScreenCorners,
  screenCornerToggledEventHook,
  screenCornerToggledLayoutHook,
 )
import System.Directory (doesFileExist)
import System.IO.Unsafe
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.Focus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.Hacks
import XMonad.Util.NamedActions
import XMonad.Util.SpawnOnce

myWindowPromptConfig :: XPConfig
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

myBasicKeyMap :: [(String, NamedAction)]
myBasicKeyMap =
  [ ("M-x", noName $ xmonadPrompt def)
  , ("M-e", spawn' "pcmanfm")
  , ("C-ö", spawn' "copyq toggle")
  , ("<Print>", spawn' "shutter -s")
  , ("M-S-l", spawn' "xautolock -locknow")
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
  , ("M-f", addName "sendMessage ToggleStruts" $ sendMessage ToggleStruts)
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
      [ ("M-C-ä", spawn' compositorRestart)
      , ("M-C-S-ä", spawn' compositorStop)
      , ("M-ü", sendMessage' RotateVideoFloat)
      , ("M-C-ü", sendMessage' ToggleSizeVideoFloat)
      , ("M-C-s", sendMessage' ToggleScreenCorner)
      , ("M-C-g", addName "windowPrompt goto" $ windowPrompt myWindowPromptConfig Goto allWindows)
      , ("M-C-b", addName "windowPrompt bring" $ windowPrompt myWindowPromptConfig Bring allWindows)
      , ("M-C-<Space>", addName "layoutScreens 4 Grid" $ layoutScreens 4 Grid)
      , ("M-C-S-<Space>", addName "rescreen" rescreen)
      , ("M-C-a", addName "copy window to all workspaces" $ windows copyToAll)
      , ("M-C-S-a", addName "kill all other window copies" killAllOtherCopies)
      ]
 where
  compositorRestart = compositorStop ++ " ; xcompmgr -cCfF"
  compositorStop = "killall xcompmgr"

myJournalKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myJournalKeys c =
  subtitle "My Keys"
    : mkNamedKeymap
      c
      [ ("M-o S-j", spawn' "emacs ~/pCloudDrive/journal.org")
      , ("M-o S-t", spawn' "emacs ~/pCloudDrive/orgzly/todos.org")
      , ("M-o S-e", spawn' "emacs ~/pCloudDrive/orgzly/tochter1.org")
      , ("M-o S-a", spawn' "emacs ~/pCloudDrive/orgzly/tochter2.org")
      ]

monWs :: String
monWs = "top"

commWs :: String
commWs = "comm"

browseWs :: String
browseWs = "web"

devWs :: String
devWs = "ide"

leasureWs :: String
leasureWs = "entertain"

privWs :: String
privWs = "private"

gamesWs :: String
gamesWs = "games"

eduWs :: String
eduWs = "education"

adminWs :: String
adminWs = "admin"

myWorkspaces :: [String]
myWorkspaces =
  [ monWs
  , commWs
  , browseWs
  , devWs
  , eduWs
  , privWs
  , gamesWs
  , leasureWs
  , adminWs
  ]

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =
  myKeysConfig $
    withMouseGestures $
      def
        { modMask = mod4Mask -- left windows super
        , focusFollowsMouse = False
        , workspaces = myWorkspaces
        }

{- FOURMOLU_DISABLE -}
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeOne
    [ isDialog -?> doCenterFloat
    , isNotification -?> doSideFloat NE
    -- requires https://github.com/benruijl/sflock/commit/6d1998b177c381baff3abe70748ed92ae6e4a262
    , className =? "sflock" -?> doFullFloat 
    , -- games & private
      className =? "Lutris" -?> doSink <+> doShift gamesWs
    , currentWs =? gamesWs -?> doSink <+> doFullFloat
    , currentWs =? privWs -?> doSink
    , -- comm
      className =? "Signal"           <||>
      className =? "Element"          <||>
      className =? "WhatSie"          <||>
      className =? "ZapZap"           <||>
      className =? "dev.geopjr.Tuba"  <||>
      className =? "Thunderbird"      <||>
      className =? "Evolution"        <||>
      className =? "Claws-mail" -?> doShift commWs
    , -- ide
      className =? "vscodium" -?> doShift devWs
    , -- entertain
      className =? "vlc" -?> doSideFloat C
    , className =? "Clementine" -?> doShift leasureWs
    -- browse
    , className =? "LibreWolf" -?> doShift browseWs
    , -- admin
      className =? "easyeffects"      <||>
      className =? "Pavucontrol"      <||>
      className =? "KeePassXC" -?> doShift adminWs
    ]
{- FOURMOLU_ENABLE -}

myFocusHook :: Query (Endo WindowSet)
myFocusHook =
  manageFocus $
    composeOne
      [ new (className =? "copyq") -?> switchFocus
      , newOnCur <&&> focused (currentWs =? gamesWs) -?> keepFocus
      , return True -?> switchFocus
      ]

myLayoutHook =
  avoidStruts $
    floatingVideos $
      windowArrange $
        onWorkspace monWs tall $
          onWorkspace commWs tabsL $
            onWorkspace browseWs accordion $
              onWorkspace gamesWs full $
                screenCornerToggledLayoutHook $
                  smartBorders (tabsL ||| tallM ||| full ||| tall ||| accordion)
 where
  full = renamed [Replace "Full"] $ noBorders Full
  tall = Tall 1 (3 / 100) (2 / 3) -- M-S-Space to reset
  tallM = renamed [Replace "Mirror Tall"] $ Mirror tall
  tabsL = renamed [Replace "Tabbed"] simpleTabbed
  accordion = Accordion

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "bash ~/.config/xmonad/xmonadrc.sh"
  addVerticalScreenCorners
  spawnOnOnce monWs "x-terminal-emulator -e btop"  -- writes ~/.ssh/env

myFadeHook :: FadeHook
myFadeHook =
  composeAll
    [ opaque
    , isUnfocused --> opacity (8 / 10)
    , liftM2 (&&) isFloating isUnfocused --> opacity (4 / 10)
    , className =? "vlc" --> opaque
    ]

myStatusBar :: StatusBarConfig
{-# NOINLINE myStatusBar #-}
myStatusBar = do
  statusBarProp determineStartUp (clickablePP xmobarPP)
 where
  determineStartUp :: String
  determineStartUp = do
    let xmobarrcHostspecificExists = unsafePerformIO $ doesFileExist xmobarrcHostspecific
    "xmobar "
      ++ if xmobarrcHostspecificExists
        then xmobarrcHostspecific
        else xmobarrcDefault
  hostname = unsafePerformIO getHostName
  xmobarrcDefault = "~/.config/xmonad/xmobarrc"
  xmobarrcHostspecific = "~/.config/xmonad/xmobarrc." ++ hostname

main :: IO ()
main = do
  xmonad
    . withSB myStatusBar
    . docks
    . ewmhFullscreen
    . ewmh
    . withUrgencyHook NoUrgencyHook
    . javaHack
    $ myConfig
      { logHook = fadeWindowsLogHook myFadeHook
      , manageHook =
          manageDocks <+> myFocusHook <> myManageHook
      , layoutHook = myLayoutHook
      , startupHook = myStartupHook
      , handleEventHook = screenCornerToggledEventHook <+> fadeWindowsEventHook <+> fixSteamFlicker <+> floatingVideosEventHook -- <+> focusOnMouseMove
      , terminal = "x-terminal-emulator"
      }
