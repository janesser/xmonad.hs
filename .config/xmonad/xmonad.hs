import Control.Monad
import Data.IORef
import Data.List (intercalate)
import Data.List.Split
import Data.Semigroup (Endo)
import Data.Time
import GHC.IO.Handle (Handle)
import Network.HostName
import System.Directory
import System.IO.Unsafe
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
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
import XMonad.Prelude (All (..))
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.StackSet (sink)
import XMonad.Util.EZConfig (additionalMouseBindings, mkNamedKeymap)
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

{-# NOINLINE screenCornerRef #-}
screenCornerRef :: IORef Bool
screenCornerRef = unsafePerformIO $ newIORef True

myScreenCornerEventHook :: IORef Bool -> Event -> X All
myScreenCornerEventHook ref e =
  io (readIORef ref) >>= \isOn ->
    if isOn
      then screenCornerEventHook e
      else return (All True)

addMyScreenCorners :: X ()
addMyScreenCorners =
  addScreenCorners
    [ (SCTop, prevWS),
      (SCBottom, nextWS)
    ]

myWindowPromptConfig :: XPConfig
myWindowPromptConfig =
  def
    { searchPredicate = fuzzyMatch,
      sorter = fuzzySort
    }

myKeysConfig :: XConfig l -> XConfig l
myKeysConfig =
  addDescrKeys
    ((mod4Mask .|. shiftMask, xK_h), noName . xKeysPrompt def)
    (myBasicKeys <+> myWindowKeys <+> myJournalKeys)

myBasicKeyMap :: [(String, NamedAction)]
myBasicKeyMap =
  [ ("M-x", noName $ xmonadPrompt def),
    ("M-e", spawn' "pcmanfm"),
    ("C-ö", spawn' "copyq toggle"),
    ("<Print>", spawn' "shutter -s"),
    ("M-S-l", spawn' "light-locker-command -l"),
    ("<XF86MonBrightnessUp>", spawn' "brightness.sh +"),
    ("<XF86MonBrightnessDown>", spawn' "brightness.sh -"),
    ( "<XF86AudioMute>",
      spawn' "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    ),
    ( "<XF86AudioLowerVolume>",
      spawn' "pactl set-sink-volume @DEFAULT_SINK@ -10%"
    ),
    ( "<XF86AudioRaiseVolume>",
      spawn' "pactl set-sink-volume @DEFAULT_SINK@ +10%"
    ),
    ("M-b", addName "sendMessage ToggleStruts" $ sendMessage ToggleStruts),
    ("M-f", addName "toggle screen corners" $ io $ modifyIORef screenCornerRef not),
    ("M-C-k", spawn' "xkill"),
    ("M-C-p", addName "xprops" $ spawn "x-terminal-emulator -e bash -c \"xprop && read -n 1 -p 'Press any key to continue..'\""),
    ("M-m", addName "manPrompt" $ manPrompt def)
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
      [ ("M-C-ä", spawn' "killall xcompmgr; xcompmgr -cCfF"),
        ("M-C-S-ä", spawn' "killall xcompmgr"),
        ("M-C-g", addName "windowPrompt goto" $ windowPrompt myWindowPromptConfig Goto allWindows),
        ("M-C-b", addName "windowPrompt bring" $ windowPrompt myWindowPromptConfig Bring allWindows),
        ("M-C-<Space>", addName "layoutScreens 4 Grid" $ layoutScreens 4 Grid),
        ("M-C-S-<Space>", addName "rescreen" rescreen),
        ("M-C-a", addName "copy window to all workspaces" $ windows copyToAll),
        ("M-C-S-a", addName "kill all other window copies" $ killAllOtherCopies)
      ]

myJournalKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myJournalKeys c =
  subtitle "My Keys"
    : mkNamedKeymap
      c
      [ ("M-o j", addName "add TODO to journal" $ orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/journal.org"),
        ("M-o t", addName "add TODO to family todos" $ orgPrompt def ("TODO " ++ orgNow) "~/Dropbox/orgzly/todos.org"),
        ("M-o e", addName "add entry to tochter1" $ orgPrompt def orgToday "~/Dropbox/orgzly/tochter1.org"),
        ("M-o S-j", spawn' "emacs ~/Dropbox/journal.org"),
        ("M-o S-t", spawn' "emacs ~/Dropbox/orgzly/todos.org"),
        ("M-o S-e", spawn' "emacs ~/Dropbox/orgzly/tochter1.org")
      ]

monitorWs :: String
monitorWs = "1:top"

commWs :: String
commWs = "2:comm"

browseWs :: String
browseWs = "3:web"

devWs :: String
devWs = "4:ide"

leasureWs :: String
leasureWs = "5:entertain"

privateWs :: String
privateWs = "6:private"

gamesWs :: String
gamesWs = "7:games"

eduWs :: String
eduWs = "8:education"

adminWs :: String
adminWs = "9:admin"

myWorkspaces :: [String]
myWorkspaces =
  [ monitorWs,
    commWs,
    browseWs,
    devWs,
    leasureWs,
    privateWs,
    gamesWs,
    eduWs,
    adminWs
  ]

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =
  myKeysConfig $
    def
      { modMask = mod4Mask, -- left windows super
        focusFollowsMouse = False,
        workspaces = myWorkspaces
      }
      `additionalMouseBindings` [ ((0, button2), \w -> focus w >> mouseMoveWindow w >> ifClick (windows $ sink w))
                                ]

myLogHook :: Handle -> X ()
myLogHook spw = dynamicLogWithPP xmobarPP {ppOutput = hPutStrLn spw}

myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeOne
    [ isDialog -?> doCenterFloat,
      isNotification -?> doSideFloat NE,
      -- comm
      className =? "Signal" -?> doShift commWs,
      className =? "Element" -?> doShift commWs,
      className =? "WhatSie" -?> doShift commWs,
      className =? "dev.geopjr.Tuba" -?> doShift commWs,
      className =? "Thunderbird" -?> doShift commWs,
      className =? "Evolution" -?> doShift commWs,
      className =? "Claws-mail" -?> doShift commWs,
      -- ide
      className =? "vscodium" -?> doShift devWs,
      -- entertain
      className =? "vlc" -?> doSideFloat C,
      role =? "PictureInPicture" -?> doSideAndCopy,
      className =? "LibreWolf" -?> doShift browseWs,
      -- admin
      className =? "easyeffects" -?> doShift adminWs,
      -- games
      currentWs =? gamesWs -?> doFullFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    doSideAndCopy = doSideFloat NE <+> doF copyToAll

myLayoutHook =
  screenCornerLayoutHook $
    avoidStruts $
      onWorkspace "1:top" tall $
        onWorkspace "2:comm" tabsL $
          onWorkspace "3:web" accordion $
            onWorkspace "7:games" full $
              smartBorders (tabsL ||| tallM ||| full ||| tall ||| accordion)
  where
    full = renamed [Replace "Full"] $ noBorders Full
    tall = Tall 1 (3 / 100) (2 / 3) -- M-S-Space to reset
    tallM = renamed [Replace "Mirror Tall"] $ Mirror tall
    tabsL = renamed [Replace "Tabbed"] simpleTabbed
    accordion = Accordion

myStartupHook :: X ()
myStartupHook = do
  spawnOnOnce "1:top" "x-terminal-emulator -e btop"
  spawnOnOnce "3:web" "x-www-browser --restore-last-session"
  -- height needs to be explicit, check ToggleStruts
  spawnOnce "gtk-sni-tray-standalone --bottom --beginning --watcher"
  addMyScreenCorners

myFadeHook :: FadeHook
myFadeHook =
  composeAll
    [ opaque,
      isUnfocused --> opacity (8 / 10),
      liftM2 (&&) isFloating isUnfocused --> opacity (4 / 10),
      className =? "vlc" --> opaque
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
      { logHook = myLogHook spwXMobar <+> fadeWindowsLogHook myFadeHook,
        manageHook =
          manageDocks <+> myManageHook <+> fullscreenManageHook,
        layoutHook = myLayoutHook,
        startupHook = myStartupHook,
        handleEventHook = fadeWindowsEventHook <+> myScreenCornerEventHook screenCornerRef <+> fixSteamFlicker, -- <+> focusOnMouseMove
        terminal = "x-terminal-emulator"
      }
