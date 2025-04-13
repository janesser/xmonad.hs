-- stack build --stack-yaml /home/jan/projs/xmonad.hs/.config/xmonad/stack.yaml

import Control.Monad
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (fromList)
import Data.Semigroup (Endo)
import Data.Time
--import FloatingVideos
import GHC.IO.Handle (Handle)
import Network.HostName (getHostName)
import System.Directory (doesFileExist)
import System.IO.Unsafe
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.MouseGestures
import XMonad.Actions.MouseResize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.Fullscreen
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
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.StackSet (sink)
import XMonad.Util.EZConfig (
  additionalMouseBindings,
  mkNamedKeymap,
 )
import XMonad.Util.Hacks
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

{-# NOINLINE orgToday #-}
orgToday :: String
orgToday = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y]" <$> getCurrentTime

{-# NOINLINE orgNow #-}
orgNow :: String
orgNow = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y %H:%M:%S]" <$> getCurrentTime

mouseGestureHook :: Window -> X ()
mouseGestureHook = mouseGesture gestures
 where
  gestures =
    fromList
      [ ([], focus)
      , ([U], const prevWS)
      , ([D], const nextWS)
      ]

mouseMoveHook :: Window -> X ()
mouseMoveHook w = focus w >> mouseMoveWindow w >> ifClick (windows $ sink w)

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
      [ ("M-C-ä", spawn' "killall xcompmgr; xcompmgr -cCfF")
      , ("M-C-S-ä", spawn' "killall xcompmgr")
--      , ("M-ü", addName "sendMessage RotateVideoFloat" $ sendMessage RotateVideoFloat)
      , ("M-C-g", addName "windowPrompt goto" $ windowPrompt myWindowPromptConfig Goto allWindows)
      , ("M-C-b", addName "windowPrompt bring" $ windowPrompt myWindowPromptConfig Bring allWindows)
      , ("M-C-<Space>", addName "layoutScreens 4 Grid" $ layoutScreens 4 Grid)
      , ("M-C-S-<Space>", addName "rescreen" rescreen)
      , ("M-C-a", addName "copy window to all workspaces" $ windows copyToAll)
      , ("M-C-S-a", addName "kill all other window copies" killAllOtherCopies)
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
    def
      { modMask = mod4Mask -- left windows super
      , focusFollowsMouse = False
      , workspaces = myWorkspaces
      }
      `additionalMouseBindings` [ ((mod4Mask .|. shiftMask, button1), mouseMoveHook)
                                , ((mod4Mask .|. shiftMask, button3), mouseGestureHook)
                                ]

myLogHook :: Handle -> X ()
myLogHook spw = dynamicLogWithPP xmobarPP{ppOutput = hPutStrLn spw}

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeOne
    [ isDialog -?> doCenterFloat
    , isNotification -?> doSideFloat NE
    , -- games & private
      className =? "Lutris" -?> doSink <+> doShift gamesWs
    , currentWs =? gamesWs -?> doSink <+> doFullFloat
    , currentWs =? privWs -?> doSink
    , -- comm
      className
        =? "Signal"
        <||> className
        =? "Element"
        <||> className
        =? "WhatSie"
        <||> className
        =? "ZapZap"
        <||> className
        =? "dev.geopjr.Tuba"
        <||> className
        =? "Thunderbird"
        <||> className
        =? "Evolution"
        <||> className
        =? "Claws-mail"
        -?> doShift commWs
    , -- ide
      className =? "vscodium" -?> doShift devWs
    , -- entertain
      className =? "vlc" -?> doSideFloat C
    , role =? "PictureInPicture" -?> doF copyToAll
    , className =? "LibreWolf" -?> doShift browseWs
    , -- admin
      className
        =? "easyeffects"
        <||> className
        =? "Pavucontrol"
        <||> className
        =? "KeePassXC"
        -?> doShift adminWs
    ]
 where
  role = stringProperty "WM_WINDOW_ROLE"

myLayoutHook =
  avoidStruts $
    mouseResize $
      windowArrange $
        onWorkspace monWs tall $
          onWorkspace commWs tabsL $
            onWorkspace browseWs accordion $
              onWorkspace gamesWs full $
--                floatingVideos $
                  screenCornerLayoutHook $
                    smartBorders (tabsL ||| tallM ||| full ||| tall ||| accordion)
 where
  full = renamed [Replace "Full"] $ noBorders Full
  tall = Tall 1 (3 / 100) (2 / 3) -- M-S-Space to reset
  tallM = renamed [Replace "Mirror Tall"] $ Mirror tall
  tabsL = renamed [Replace "Tabbed"] simpleTabbed
  accordion = Accordion

myStartupHook :: X ()
myStartupHook = do
  spawnOnOnce monWs "x-terminal-emulator -e btop"
  -- height needs to be explicit, check ToggleStruts
  spawnOnce "gtk-sni-tray-standalone --bottom --beginning --watcher"
  spawnOnce "blueman-applet" -- requires tray activated

myFadeHook :: FadeHook
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
      else spawnPipe "xmobar ~/.config/xmonad/xmobarrc"
  _ <- spawnPipe "bash ~/.config/xmonad/xmonadrc" -- writes ~/.ssh/env
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
