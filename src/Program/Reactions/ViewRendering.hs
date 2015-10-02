{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.ViewRendering
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.ViewRendering () where

import Reactive

import Program

import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents

import GHCJS.Useful
import Program.View

import Program.Reactions.ServiceFinish

renderScene :: Program -> PView -> IO (Either PView (EventBox Program PView))
renderScene program view = do
    -- prepare rendering
    ctime <- getTime
    ctx <- prepareRenderState (context view) (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- done!
    return $ Left view{context = ctx}

instance Reaction Program PView WheelEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerUpEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerCancelEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerMoveEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView ResizeEvent "Resize & Render" 1 where
    response _ _ _ p v@PView{context = ctx} = do
        ctx' <- updateViewPortSize (camera p) ctx
        renderScene p v{context = ctx'}


instance Reaction Program PView SelectionEvent "Render" 9 where
    response _ _ _ = renderScene


instance Reaction Program PView ServiceRunFinish "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView ClearingGeometry "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView GeoJSONLoaded "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView ClearServiceResults "Render" 9 where
    response _ _ _ = renderScene

-- In order to show a preview of the scenario as an image,
-- we need to render staff in a buffer (not to get empty image)
instance Reaction Program PView SubmitScenario "Render" 0 where
    response _ _ _ = renderScene
