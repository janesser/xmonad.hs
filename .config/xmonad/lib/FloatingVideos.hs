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

import Control.Monad (filterM)
import Data.Enum.Circular
import Data.Monoid (Endo (appEndo))
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

  -- https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Layout.Fullscreen.html
  modifyLayout (VideoFloating vf) wk sr = do
    let st = W.stack wk
    let ws = W.integrate' st
    vws <- videoWindowsF ws
    --q <- mapW vws $ runQuery videoHook
    --let e = fmap appEndo q
    --mapM_ windows e -- FIXME no effect, runs runLayout inside
    (wrs, _) <- runLayout wk{W.stack = st >>= W.filter (`notElem` vws)} sr
    return (wrs, Nothing)
   where
    rect = videoFloatRectangle vf

    videoHook :: ManageHook
    videoHook = doRectFloat rect <+> doF copyToAll
    videoWindowsF :: [Window] -> X [Window]
    videoWindowsF = filterM isVideoX
    isVideoX :: Window -> X Bool
    isVideoX w = do
      role <- runQuery (stringProperty "WM_WINDOW_ROLE") w
      return $ role == "PictureInPicture"

    mapW l f = mapM f l

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating SouthEast
