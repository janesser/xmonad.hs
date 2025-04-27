{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ManagementHooks apply on WindowSets of xmonad.Core
-- ManagementHooks apply in xmonad.Main
-- LayoutModifiers apply to runLayout before/after in xmonad.Core
-- layoutHooks are part of xmonad.Core
-- layoutHooks are triggered event-based

-- where is the post-layout rendering ?
-- can Workspace and WindowSet be translated into eachother ?
--- are subsets of WindowSet supported

-- https://stackoverflow.com/questions/59315118/how-can-i-modify-the-windowset-in-xmonad

module FloatingVideos (
  floatingVideos,
  RotateVideoFloat (..),
) where

import Data.Enum.Circular (csucc)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Monoid
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutModifier
import XMonad.StackSet as W

-- very similar to XMonad.Layout.CenteredMaster
data VideoFloatMode = SouthEast | NorthCenter deriving (Eq, Enum, Bounded, Read, Show)
videoFloatRectangle :: VideoFloatMode -> RationalRect
videoFloatRectangle SouthEast = RationalRect (1 / 2) (1 / 2) (1 / 2) (1 / 2)
videoFloatRectangle NorthCenter = RationalRect (1 / 4) 0 (1 / 2) (1 / 2)

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
    let st = W.stack wk
    let ws = W.integrate' st
    case ws of
      [] -> runLayout wk sr
      wins -> do
        _ <- manageVideos wins
        maybeVideos <- placeVideos wins
        let vWs = map (fst . fromJust) (Prelude.filter isJust maybeVideos)
        let nvWs = st >>= W.filter (`notElem` vWs)
        wrs <- runLayout wk{W.stack = nvWs} sr
        let vWRs = catMaybes maybeVideos
        -- FIXME requires sink to fall into position
        return (fst wrs ++ vWRs, snd wrs)
   where
    rect = videoFloatRectangle vf

    placeVideos :: [Window] -> X [Maybe (Window, Rectangle)]
    placeVideos = mapM placeVideo

    mapW l f = mapM f l
    
    manageVideos :: [Window] -> X [()]
    manageVideos ws = do
      let q = runQuery (stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture" -?> doRectFloat rect <+> doF copyToAll)
      q2 <- mapW ws q
      let q3 = catMaybes q2
      mapM (windows . appEndo) q3


    placeVideo :: Window -> X (Maybe (Window, Rectangle))
    placeVideo w = do
      wRole <- runQuery (stringProperty "WM_WINDOW_ROLE") w
      if wRole == "PictureInPicture"
        then do
          return $ Just (w, scaleRationalRect sr rect)
        else
          return Nothing

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating SouthEast
