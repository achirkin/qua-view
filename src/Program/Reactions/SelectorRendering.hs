{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.SelectorRendering
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This module generates SelectionEvent, so it must import all modules which process this event
--
-----------------------------------------------------------------------------

module Program.Reactions.SelectorRendering (
    ) where

import JsHs.WebGL
import Data.Geometry


import Reactive

import Program

import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents

import Control.Monad (when)

--import GHCJS.Useful
import Program.View


import Program.Reactions.CitySelection ()
import Program.Reactions.ViewRendering ()
$(createEventSense ''SelectionEvent)
$(createEventSense ''SelectionConfirmEvent)


--- Marking area for selection and firing an event with selection Id

selectOnScene :: Program -> PView -> IO PView
selectOnScene program view = do
    ctx <- applySelector (context view) (camera program) (city program) (cityView view)
    return view{context = ctx}

instance Reaction Program PView WheelEvent "Mark selection" 10 where
    response _ _ _ = selectOnScene

instance Reaction Program PView PointerUpEvent "Mark selection" 10 where
    response _ _ _ = selectOnScene

instance Reaction Program PView PointerCancelEvent "Mark selection" 10 where
    response _ _ _ = selectOnScene

instance Reaction Program PView ResizeEvent "Mark selection" 10 where
    response _ _ _ = selectOnScene


instance Reaction Program PView ClearingGeometry "Clear selection mark" 10 where
    response _ _ _ = selectOnScene

instance Reaction Program PView GeoJSONLoaded "Mark selection" 10 where
    response _ _ _ = selectOnScene

--- Selecting object on click

getSelectId :: Vector2 GLfloat -> Program -> PView -> IO SelectionEvent
getSelectId v program view = getSelection (context view) (camera program) v


instance Reaction Program PView PointerClickEvent "Get selection" 1 where
    response _ hole (PClick LeftButton (p:_)) prog pview = getSelectId p prog pview >>= reqEvent hole . EBox >> return pview
    response _ hole (PClick Touches [p])      prog pview = getSelectId p prog pview >>= reqEvent hole . EBox >> return pview
    response _ _ _ _ pview = return pview

--- Selecting onbject on mouse down to allow moving objects

instance Reaction Program PView PointerDownEvent "Fire selection action" 0 where
    react _ _ prog@Program{controls = c} = prog{controls = c{selectedObject = 0}}
    response _ hole (PDown _ (p:_)) program view = do
        SelectionEvent i <- getSelectId p program view
        when (i /= 0) . reqEvent hole . EBox $ SelectionConfirmEvent i
        return view
    response _ _ _ _ view = return view
