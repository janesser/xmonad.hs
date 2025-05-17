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

import Data.Enum.Circular
import Data.Monoid
import XMonad
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import XMonad.StackSet as W

data VideoFloatMode = NorthCenter | SouthEast | SouthWest deriving (Eq, Enum, Bounded, Read, Show)

videoFloatRectangle :: Rational -> VideoFloatMode -> RationalRect
videoFloatRectangle r NorthCenter = RationalRect ((1 - r) / 2) 0 r r
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

-- | https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Layout.Fullscreen.html
instance LayoutModifier VideoFloating Window where
  -- FIXME applies only on certain workspaces
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
    | Just (PlaceVideosAltered nr nvf) <- fromMessage m = do
        let nrect = videoFloatRectangle nr nvf
        _ <- placeVideos $ floatHook nrect
        return Nothing
    | Just PlaceVideos <- fromMessage m = do
        let nrect = videoFloatRectangle r vf
        _ <- placeVideos $ floatHook nrect
        return Nothing
    | otherwise = return Nothing
   where
    floatHook :: RationalRect -> Query (Endo WindowSet)
    floatHook nrect = composeOne [stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture" -?> doRectFloat nrect <+> doF copyToAll]
    placeVideos :: Query (Endo WindowSet) -> X ()
    placeVideos q = do
      st <- get
      let ws = windowset st
          wins = W.allWindows ws
      mapM_ (placeVideo q) wins
    placeVideo :: ManageHook -> Window -> X ()
    placeVideo q w = do
      g <- appEndo <$> userCodeDef (Endo id) (runQuery q w)
      windows g

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating (1 / 4) SouthEast

floatingVideosEventHook :: Event -> X All
floatingVideosEventHook MapRequestEvent{ev_window = _} = do
  broadcastMessage PlaceVideos
  return $ All True
floatingVideosEventHook _ = return $ All True
