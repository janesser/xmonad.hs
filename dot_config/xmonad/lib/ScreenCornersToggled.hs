{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ScreenCornersToggled (
    ToggleScreenCorner (..),
    screenCornerToggledLayoutHook,
    addVerticalScreenCorners,
    screenCornerToggledEventHook,
) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.IO (unsafePerformIO)
import XMonad (Event, Message, X, fromMessage, io)
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Hooks.ScreenCorners (
    ScreenCorner (SCBottom, SCTop),
    addScreenCorners,
    screenCornerEventHook,
 )
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (All (..))

{-# NOINLINE screenCornerToggle #-}
screenCornerToggle :: IORef Bool
screenCornerToggle = unsafePerformIO $ newIORef False

data ToggleScreenCorner = ToggleScreenCorner deriving (Show)
instance Message ToggleScreenCorner

data ScreenCornerToggledLayout a = ScreenCornerToggledLayout deriving (Show, Read)
instance LayoutModifier ScreenCornerToggledLayout a where
    handleMess _ m
        | Just ToggleScreenCorner <- fromMessage m = do
            io $ modifyIORef' screenCornerToggle not
            return Nothing
        | otherwise = return Nothing

screenCornerToggledLayoutHook :: l a -> ModifiedLayout ScreenCornerToggledLayout l a
screenCornerToggledLayoutHook = ModifiedLayout ScreenCornerToggledLayout

screenCornerToggledEventHook :: Event -> X All
screenCornerToggledEventHook e = do
    toggle <- io $ readIORef screenCornerToggle
    if toggle
        then screenCornerEventHook e
        else return (All True)

addVerticalScreenCorners :: X ()
addVerticalScreenCorners =
    addScreenCorners
        [ (SCTop, nextWS)
        , (SCBottom, prevWS)
        ]
