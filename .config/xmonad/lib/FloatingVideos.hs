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
  ToggleSizeVideoFloat (..),
) where

import Control.Monad
import Data.Enum.Circular
import Data.Map as M
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet as W
import Data.Maybe

-- very similar to XMonad.Layout.CenteredMaster
data VideoFloatMode = SouthEast | NorthCenter deriving (Eq, Enum, Bounded, Read, Show)

videoFloatRectangle :: Rational -> VideoFloatMode -> RationalRect
videoFloatRectangle r SouthEast = RationalRect (1 - r) (1 - r) r r
videoFloatRectangle r NorthCenter = RationalRect ((1 - r) / 2) 0 r r

data RotateVideoFloat = RotateVideoFloat deriving (Show)
instance Message RotateVideoFloat
data ToggleSizeVideoFloat = ToggleSizeVideoFloat deriving (Show)
instance Message ToggleSizeVideoFloat

data VideoFloating a = VideoFloating Rational VideoFloatMode deriving (Read, Show)

instance LayoutModifier VideoFloating Window where
  modifierDescription :: VideoFloating Window -> String
  modifierDescription (VideoFloating _ vf) = show vf -- FIXME applies only on certain workspaces

  pureMess (VideoFloating r vf) m
    | Just RotateVideoFloat <- fromMessage m = Just $ VideoFloating r $ csucc vf
    | Just ToggleSizeVideoFloat <- fromMessage m = Just $ VideoFloating (if r == (1 / 2) then 1 / 4 else 1 / 2) vf
    | otherwise = Nothing

  -- https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/src/XMonad.Layout.Fullscreen.html
  -- \| given any situation
  --- split normal windows and forward to underlying `runLayout`
  --- put video windows to floating layer
  modifyLayout (VideoFloating r vf) wk sr = do
    -- st <- get
    -- let ws = windowset st
    -- flt = W.floating ws
    -- flt' <- videoWindowsR
    vws <- videoWindowsW
    --mapM_ placeVideo vws
    -- put st{windowset = ws{W.floating = M.union flt flt'}}
    layoutNonVideos vws
   where
    rect = videoFloatRectangle r vf

    placeVideo w = do return ()
      --windows $ W.float w rect

    layoutNonVideos vws = do
      --let st = W.stack wk
      --if isJust st
      --  then runLayout wk{W.stack = W.filter (`notElem` vws) (fromJust st)} sr
      --  else 
      runLayout wk sr

    videoWindowsR :: X (Map Window RationalRect)
    videoWindowsR = do
      vws <- videoWindowsW
      let wrs = zip vws (repeat rect)
      return $ M.fromList wrs

    videoWindowsW :: X [Window]
    videoWindowsW = do
      let tiled_wins = W.integrate' . W.stack $ wk
      
      sts <- get
      let floating_wins = M.keys . W.floating . windowset $ sts
      videoWindowsF (tiled_wins ++ floating_wins)

    videoWindowsF :: [Window] -> X [Window]
    videoWindowsF = filterM isVideoX

    isVideoX :: Window -> X Bool
    isVideoX w = do
      role <- runQuery (stringProperty "WM_WINDOW_ROLE") w
      return $ role == "PictureInPicture"

floatingVideos :: l a -> ModifiedLayout VideoFloating l a
floatingVideos = ModifiedLayout $ VideoFloating (1 / 4) SouthEast
