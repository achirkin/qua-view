{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.SelectorRendering
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This module generates SelectionEvent, so it must import all modules which process this event
--
-----------------------------------------------------------------------------

module Program.Reactions.SelectorRendering (
    ) where

import GHCJS.WebGL
--import Geometry.Space


import Reactive

import Program

import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents

--import GHCJS.Useful
--import Program.View


import Program.Reactions.CitySelection ()
import Program.Reactions.ViewRendering ()
-- $(createEventSense ''SelectionEvent)
-- $(createEventSense ''SelectionConfirmEvent)


----- Marking area for selection and firing an event with selection Id

--selectOnScene :: Program -> PView -> IO (Either PView (EventBox Program PView))
--selectOnScene program view = do
--    ctx <- applySelector (context view) (camera program) (city program) (cityView view)
--    return $ Left view{context = ctx}

instance Reaction Program PView WheelEvent "Mark selection" 10 where
--    response _ _ _ = selectOnScene

instance Reaction Program PView PointerUpEvent "Mark selection" 10 where
--    response _ _ _ = selectOnScene

instance Reaction Program PView PointerCancelEvent "Mark selection" 10 where
--    response _ _ _ = selectOnScene

instance Reaction Program PView ResizeEvent "Mark selection" 10 where
--    response _ _ _ = selectOnScene


instance Reaction Program PView ClearingGeometry "Clear selection mark" 10 where
--    response _ _ _ = selectOnScene

instance Reaction Program PView GeoJSONLoaded "Mark selection" 10 where
--    response _ _ _ = selectOnScene
--
------- Selecting object on click
--
--getSelectId :: Vector2 GLfloat -> Program -> PView -> IO SelectionEvent
--getSelectId v program view = getSelection (context view) (camera program) v
--
--wrapEvent :: (EventSense Program PView e) => IO e -> IO (Either PView (EventBox Program PView))
--wrapEvent = liftM (Right . EBox)


instance Reaction Program PView PointerClickEvent "Get selection" 1 where
--    response _ (PClick LeftButton (p:_)) _ prog  = wrapEvent . getSelectId p prog
--    response _ (PClick Touches [p]) _ prog = wrapEvent . getSelectId p prog
--    response _ _ _ _ = return . Left

----- Selecting onbject on mouse down to allow moving objects

instance Reaction Program PView PointerDownEvent "Fire selection action" 0 where
--    react _ _ prog@Program{controls = c} = prog{controls = c{selectedObject = 0}}
--    response _
--             (PDown _ (p:_))
--             _ program
--             view = do
--        SelectionEvent i <- getSelectId p program view
--        if i == 0 then return $ Left view
--        else return . Right . EBox $ SelectionConfirmEvent i
--    response _ _ _ _ view = return $ Left view
