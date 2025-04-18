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
    ( fromMessage,
      runQuery,
      stringProperty,
      float,
      Window,
      Rectangle,
      LayoutClass(runLayout),
      Message,
      SomeMessage,
      X )
import XMonad.Layout.LayoutModifier
    ( LayoutModifier(modifierDescription, modifyLayout, pureMess),
      ModifiedLayout(..) )
import XMonad.StackSet as W
    ( Workspace(stack), filter, integrate' )
import XMonad.Util.Rectangle (
  PointRectangle (PointRectangle),
  coordinatesToRectangle,
  pixelsToCoordinates,
 )

-- very similar to XMonad.Layout.CenteredMaster
data VideoFloatMode = SouthEast | NorthCenter deriving (Eq, Enum, Bounded, Read, Show)
videoFloatRectangle :: VideoFloatMode -> PointRectangle Integer -> PointRectangle Integer
videoFloatRectangle SouthEast (PointRectangle _ _ x2 y2) = PointRectangle (x2 - 400) (y2 - 400) x2 y2
videoFloatRectangle NorthCenter (PointRectangle x1 y1 x2 _) = PointRectangle (xcenter - 200) y1 (xcenter + 200) (y1 + 400)
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
        return (fst wrs ++ vWRs, snd wrs) 
        -- FIXME won't work with Accordion
        -- FIXME won't float
        -- FIXME won't copyAll
   where
    placeVideos = mapM placeVideo

    placeVideo :: Window -> X (Maybe (Window, Rectangle))
    placeVideo w = do
      wRole <- runQuery (stringProperty "WM_WINDOW_ROLE") w
      if wRole == "PictureInPicture"
        then do
          -- TODO mimmick https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Layout.Fullscreen.html#line-151
          -- TODO set floating rectangle instead, since now needs `sink` to snap to preferred rect
          let rect = coordinatesToRectangle $ videoFloatRectangle vf (pixelsToCoordinates sr)
          _ <- float w
          return $ Just (w, rect)
        else
          return Nothing

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating SouthEast
