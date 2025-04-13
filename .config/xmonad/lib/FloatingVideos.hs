{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FloatingVideos (
  floatingVideos,
  RotateVideoFloat (..),
) where

import Data.Enum.Circular (csucc)
import Data.Maybe (catMaybes, fromJust, isJust)
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet as W (
  Workspace (stack),
  filter,
  integrate',
 )
import XMonad.Util.Rectangle (
  PointRectangle (PointRectangle),
  coordinatesToRectangle,
  pixelsToCoordinates,
 )

-- very similar to XMonad.Layout.CenteredMaster
data VideoFloatMode = SouthEast | NorthCenter deriving (Eq, Enum, Bounded, Read, Show)
videoFloatRectangle :: VideoFloatMode -> PointRectangle Integer -> PointRectangle Integer
videoFloatRectangle SouthEast (PointRectangle _ _ x2 y2) = PointRectangle (x2 - 200) (y2 - 200) x2 y2
videoFloatRectangle NorthCenter (PointRectangle x1 y1 x2 _) = PointRectangle (xcenter - 100) y1 (xcenter + 100) (y1 + 200)
 where
  xcenter :: Integer
  xcenter = (x2 - x1) `div` 2

data RotateVideoFloat = RotateVideoFloat
instance Message RotateVideoFloat

newtype VideoFloating a = VideoFloating VideoFloatMode deriving (Read, Show)

instance LayoutModifier VideoFloating Window where
  modifierDescription :: VideoFloating Window -> String
  modifierDescription (VideoFloating vf) = show vf -- FIXME applies only on certain workspaces

  pureMess :: VideoFloating Window -> SomeMessage -> Maybe (VideoFloating Window)
  pureMess (VideoFloating vf) m
    | Just RotateVideoFloat <- fromMessage m = Just $ VideoFloating $ csucc vf
    | otherwise = Nothing

  modifyLayout (VideoFloating vf) wk sr = do
    let ss = W.stack wk
    let ws = W.integrate' ss
    case ws of
      [] -> runLayout wk sr
      wins -> do
        maybeVideos <- placeVideos wins
        let vWs = map (fst . fromJust) (Prelude.filter isJust maybeVideos)
        let nvWs = ss >>= W.filter (`notElem` vWs)
        wrs <- runLayout wk{W.stack = nvWs} sr
        let vWRs = catMaybes maybeVideos
        return (fst wrs ++ vWRs, snd wrs) -- FIXME no effect ??
   where
    placeVideos = mapM placeVideo

    placeVideo :: Window -> X (Maybe (Window, Rectangle))
    placeVideo w = do
      wRole <- runQuery (stringProperty "WM_WINDOW_ROLE") w
      if wRole == "PictureInPicture"
        then
          return $ Just (w, coordinatesToRectangle $ videoFloatRectangle vf (pixelsToCoordinates sr))
        else
          return Nothing

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating SouthEast
