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
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import XMonad.StackSet as W

wmWindowRole :: Query String
wmWindowRole = stringProperty "WM_WINDOW_ROLE"

videoRole :: String
videoRole = "PictureInPicture"

data VideoFloatMode = NorthCenter | NorthEast | SouthEast | SouthWest deriving (Eq, Enum, Bounded, Read, Show)

videoFloatRectangle :: Rational -> VideoFloatMode -> RationalRect
videoFloatRectangle r NorthCenter = RationalRect ((1 - r) / 2) 0 r r
videoFloatRectangle r NorthEast = RationalRect (1 - r) 0 r r
videoFloatRectangle r SouthEast = RationalRect (1 - r) (1 - r) r r
videoFloatRectangle r SouthWest = RationalRect 0 (1 - r) r r

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
        let allWins = W.integrate st'
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
        let nr = if r == (1 / 2) then 1 / 4 else 1 / 2
        sendMessage $ PlaceVideosAltered nr vf
        return $ Just $ VideoFloating nr vf
    | Just PlaceVideos <- fromMessage m = do
        sendMessage $ PlaceVideosAltered r vf
        return Nothing
    | Just (PlaceVideosAltered nr nvf) <- fromMessage m = do
        doPlaceVideos nr nvf
        return Nothing
    | otherwise = return Nothing

-- | place videos using a `windows` transaction which will cause refresh
doPlaceVideos :: Rational -> VideoFloatMode -> X ()
doPlaceVideos r vf = do
  placeVideos $ floatHook rect
 where
  rect = videoFloatRectangle r vf
  floatHook :: RationalRect -> Query (Endo WindowSet)
  floatHook nrect =
    composeOne
      [wmWindowRole =? videoRole -?> (doRectFloat nrect <+> copyToAllWorkspaces)]
  placeVideos :: Query (Endo WindowSet) -> X ()
  placeVideos q = do
    -- TODO withWindowSet
    st <- get
    let allWins = W.allWindows $ windowset st
    vidWins <- filterM isVideo allWins
    mapM_ (placeVideo q) vidWins
  placeVideo :: ManageHook -> Window -> X ()
  placeVideo q w = do
    g <- appEndo <$> runQuery q w
    windows g
  copyToAllWorkspaces :: Query (Endo WindowSet)
  copyToAllWorkspaces = do
    -- foldEndo $ forM copyWindowToWorkspace wss
    copyWindowToWorkspace "ide"

  copyWindowToWorkspace :: String -> Query (Endo WindowSet)
  copyWindowToWorkspace ws = ask >>= \w -> doF $ copyWindow w ws
  wss :: X [String]
  wss = do
    let wks = asks (XMonad.workspaces . config)
    wks

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating (1 / 4) SouthEast

floatingVideosEventHook :: Event -> X All
floatingVideosEventHook MapRequestEvent{ev_window = w} = do
  whenX (isVideo w) $ sendMessage PlaceVideos
  return $ All True
floatingVideosEventHook _ = return $ All True

isVideo :: Window -> X Bool
isVideo w = do
  role <- runQuery wmWindowRole w
  return $ role == videoRole