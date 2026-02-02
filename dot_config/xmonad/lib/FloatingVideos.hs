{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FloatingVideos (
  floatingVideos,
  RotateVideoFloat (..),
  ToggleSizeVideoFloat (..),
  PlaceVideos (..),
  floatingVideosEventHook,
) where

import Control.Monad
import Data.Enum.Circular
import Data.Maybe
import Data.Monoid
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.ManageHelpers (composeOne, doRectFloat, isDialog, isNotification, (-?>))
import XMonad.Layout.LayoutModifier
import XMonad.StackSet (RationalRect (..), Stack (..), Workspace (..), allWindows, integrate)
import qualified XMonad.StackSet as W (filter)
import XMonad.Util.WindowProperties (getProp32)

-- | similar to ManageHelpers.Side(..)
data VideoFloatMode = NC | NE | SE | SW | WE deriving (Eq, Enum, Bounded, Read, Show)

videoFloatRectangle :: Rational -> VideoFloatMode -> RationalRect
videoFloatRectangle r NC = RationalRect ((1 - r) / 2) 0 r r
videoFloatRectangle r NE = RationalRect (1 - r) 0 r r
videoFloatRectangle r SE = RationalRect (1 - r) (1 - r) r r
videoFloatRectangle r SW = RationalRect 0 (1 - r) r r
videoFloatRectangle r WE = RationalRect 0 ((1-r) / 2) r r
smallR :: Rational
smallR = 2 / 7
largeR :: Rational
largeR = 2 / 5

data RotateVideoFloat = RotateVideoFloat deriving (Show)
instance Message RotateVideoFloat

data ToggleSizeVideoFloat = ToggleSizeVideoFloat deriving (Show)
instance Message ToggleSizeVideoFloat

data PlaceVideos = PlaceVideos deriving (Show)
instance Message PlaceVideos

data PlaceVideosAltered = PlaceVideosAltered Rational VideoFloatMode
instance Message PlaceVideosAltered

data VideoFloating a = VideoFloating Rational VideoFloatMode deriving (Read, Show)

{- | make video-windows (with WM_WINDOW_ROLE == 'PictureInPicture') float lightly around screen
    example EZconfig
      ("M-ü", sendMessage' RotateVideoFloat)
      ("M-C-ü", sendMessage' ToggleSizeVideoFloat)

built by the example of:
https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Layout.Fullscreen.html
-}
instance LayoutModifier VideoFloating Window where
  -- \| layout everything but video-windows
  modifyLayout _ wk sr = do
    let st = stack wk
    if isJust st
      then do
        let st' :: Stack Window = fromJust st
        let allWins = integrate st'
        vidWins :: [Window] <- filterM isVideo allWins
        let st'' = W.filter (`notElem` vidWins) st'
        runLayout wk{stack = st''} sr
      else
        runLayout wk sr

  modifierDescription :: VideoFloating Window -> String
  modifierDescription (VideoFloating _ vf) = show vf

  handleMess :: VideoFloating Window -> SomeMessage -> X (Maybe (VideoFloating Window))
  handleMess (VideoFloating r vf) m
    | Just RotateVideoFloat <- fromMessage m = do
        let nvf = csucc vf
        sendMessage $ PlaceVideosAltered r nvf
        return $ Just $ VideoFloating r nvf
    | Just ToggleSizeVideoFloat <- fromMessage m = do
        let nr = if r == largeR then smallR else largeR
        sendMessage $ PlaceVideosAltered nr vf
        return $ Just $ VideoFloating nr vf
    | Just PlaceVideos <- fromMessage m = do
        sendMessage $ PlaceVideosAltered r vf
        return Nothing
    | Just (PlaceVideosAltered nr nvf) <- fromMessage m = do
        doPlaceVideos nr nvf
        clearEvents propertyChangeMask -- break loop, see `floatingVideosEventHook PropertyEvent` below
        return Nothing
    | otherwise = return Nothing

-- | place videos using a `windows` transaction which will cause refresh
doPlaceVideos :: Rational -> VideoFloatMode -> X ()
doPlaceVideos r vf = do
  wss <- asks (XMonad.workspaces . config)
  placeVideos $ floatHook rect wss
 where
  rect = videoFloatRectangle r vf
  floatHook :: RationalRect -> [String] -> Query (Endo WindowSet)
  floatHook nrect wss = composeOne [fmap not (isNotification <||> isDialog) -?> doRectFloat nrect <+> copyToAllWorkspaces wss]
  copyToAllWorkspaces :: [String] -> Query (Endo WindowSet)
  copyToAllWorkspaces wss = do
    let copied :: [Query (Endo WindowSet)] = map copyWindowToWorkspace wss
    foldr (<>) (head copied) (tail copied)
  copyWindowToWorkspace :: String -> Query (Endo WindowSet)
  copyWindowToWorkspace ws = ask >>= \w -> doF $ copyWindow w ws
  placeVideos :: Query (Endo WindowSet) -> X ()
  placeVideos q = withWindowSet $ \ws -> do
    let allWins = allWindows ws
    vidWins <- filterM isVideo allWins
    mapM_ (placeVideo q) vidWins
  placeVideo :: ManageHook -> Window -> X ()
  placeVideo q w = do
    g <- appEndo <$> userCodeDef (Endo id) (runQuery q w)
    windows g

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating smallR SE

{- | catch new windows or property change of "_NET_WM_STATE"

_NET_WM_STATE(ATOM) = _NET_WM_STATE_ABOVE, _NET_WM_STATE_STAYS_ON_TOP

built by example of https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Hooks.OnPropertyChange.html
-}
floatingVideosEventHook :: Event -> X All
floatingVideosEventHook MapRequestEvent{ev_window = w} = do
  whenX (isVideo w) $ sendMessage PlaceVideos
  return $ All True
floatingVideosEventHook PropertyEvent{ev_window = w, ev_atom = a, ev_propstate = ps} = do
  pa <- getAtom "_NET_WM_STATE"
  when (ps == propertyNewValue && a == pa) $ do
    whenX (isVideo w) $ sendMessage PlaceVideos
  return mempty
floatingVideosEventHook _ = return mempty

-- | built by example of https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Hooks.EwmhDesktops.html#fullscreenEventHook.html
isVideo :: Window -> X Bool
isVideo w = do
  -- _NET_WM_STATE(ATOM) = _NET_WM_STATE_ABOVE, _NET_WM_STATE_STAYS_ON_TOP
  wmState <- getAtom "_NET_WM_STATE"
  aboveAtom <- getAtom "_NET_WM_STATE_ABOVE"
  wState <- fromMaybe [] <$> getProp32 wmState w
  return $ fromIntegral aboveAtom `elem` wState
