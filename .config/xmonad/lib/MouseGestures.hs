module MouseGestures (withMouseGestures) where

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.FloatSnap (Direction2D (D, U), ifClick)
import XMonad.Actions.MouseGestures (mouseGesture)
import XMonad.Prelude
import XMonad.StackSet (sink)
import XMonad.Util.EZConfig (additionalMouseBindings)

-- import XMonad.Actions.MouseResize

-- TODO gestures without modifies
withMouseGestures :: XConfig l -> XConfig l
withMouseGestures c =
    c
        `additionalMouseBindings` [ ((mod4Mask .|. shiftMask, button1), mouseMoveHook)
                                  , ((mod4Mask .|. shiftMask, button3), mouseGestureHook)
                                  ]

mouseGestureHook :: Window -> X ()
mouseGestureHook = mouseGesture gestures
  where
    gestures =
        fromList
            [ ([], focus)
            , ([U], const prevWS)
            , ([D], const nextWS)
            ]

mouseMoveHook :: Window -> X ()
mouseMoveHook w = focus w >> mouseMoveWindow w >> ifClick (windows $ sink w)
