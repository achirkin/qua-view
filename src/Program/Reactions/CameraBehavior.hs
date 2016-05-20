{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Model.Reactions.CameraBehavior
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.CameraBehavior () where


import Reactive

import Program
import Program.Model.Camera

import Controllers.Pointer
import Controllers.ElementResizing

--import GHCJS.Useful

transformCamera :: (Camera -> Camera) -> Program -> Program
transformCamera f program@Program{controls = Controls{ selectedObject = 0 }}
    = program{camera = f (camera program) }
transformCamera _ program = program

instance Reaction Program PView WheelEvent "Zoom Camera" 0 where
    react _ (WheelDelta wd) = transformCamera (scroll (wd*0.18+0.03))
--    response _ _ _ Program{camera = Camera { newState = st }} view = logText ("Zoomed: " ++ show st) >> return (Left view)
--    response _ ev _ _ view = logText ("Zoomed: " ++ show ev) >> return (Left view)

instance Reaction Program PView PointerDownEvent "Remember Camera state" 0 where
    react _ _ = transformCamera (\c@Camera{ newState = nstate} -> c{oldState = nstate})
--    response _ ev _ _ view = logText ("Down: " ++ show ev) >> return (Left view)

instance Reaction Program PView PointerUpEvent "Remember Camera state" 0 where
    react _ _ = transformCamera (\c@Camera{ newState = nstate} -> c{oldState = nstate})
--    response _ ev _ _ view = logText ("Up: " ++ show ev) >> return (Left view)

instance Reaction Program PView PointerCancelEvent "Remember Camera state" 0 where
    react _ _ = transformCamera (\c@Camera{ newState = nstate} -> c{oldState = nstate})
--    response _ ev _ _ view = logText ("Cancel: " ++ show ev) >> return (Left view)

instance Reaction Program PView PointerMoveEvent "Move Camera" 0 where
    react _ (PMove _            _ []             )   = id
    react _ (PMove LeftButton   _ ((npos,opos):_))   = transformCamera (dragHorizontal opos npos)
    react _ (PMove RightButton  _ ((npos,opos):_))   = transformCamera (rotateCentered opos npos)
    react _ (PMove MiddleButton _ ((npos,opos):_))   = transformCamera (dragVertical   opos npos)
    react _ (PMove Touches      _ [(npos,opos)]  )   = transformCamera (dragHorizontal opos npos)
    react _ (PMove Touches      _ [(n1,o1),(n2,o2)]) = transformCamera (twoFingerControl (o1,o2) (n1,n2))
    react _ (PMove Touches      _ ((n1,o1):_:_:_))   = transformCamera (rotateCentered o1 n1)
--    response _ ev _ _ view = logText ("Move: " ++ show ev) >> return (Left view)

instance Reaction Program PView ResizeEvent "Resize viewport" 0 where
    react _ (ResizeEvent w h) = transformCamera (\Camera{ newState = st} -> initCamera w h st)
--    response _ ev _ _ view = logText ("Resize: " ++ show ev) >> return (Left view)
