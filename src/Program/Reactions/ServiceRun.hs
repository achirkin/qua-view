{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.ServiceRun
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.ServiceRun where

--import Control.Concurrent (forkIO)
import Control.Monad (liftM)
import Geometry.Space
import Geometry.Structure

import GHCJS.Useful
import Reactive
import Program
import Program.Model.City
import Program.Model.CityGround
import Program.Model.ScalarField

import Controllers.GUIEvents
import Services


import Program.Reactions.ServiceFinish
import Program.Reactions.ViewRendering ()
$(createEventSense ''ServiceRunFinish)

instance Reaction Program PView ServiceRunBegin "Run service" 1 where
    react _ _ program@Program{city = ci}
        = program{city = ci{ground = rebuildGround (minBBox ci) (ground ci)}}
    response _ _ Program
            { controls = Controls {activeService = ServiceBox service}
            , city = ci
            } pview = if barea < 0.1
                      then do
        logText "No geometry to run a service."
        getElementById "clearbutton" >>= elementParent >>= hideElement
        return $ Left pview
                      else do
        programInProgress
        logText ("Running service " ++ show service)
        liftM (Right . EBox . ServiceRunFinish) (runService service sf)
            where cs = 1
                  barea = let Vector2 w h = (highBound . groundBox $ ground ci)
                                         .- (lowBound . groundBox $ ground ci)
                          in w*h
                  evalGrid = groundEvalGrid (ground ci) cs
                  sf = ScalarField
                    { cellSize  = cs
                    , sfPoints  = evalGrid
                    , sfRange   = zeros
                    , sfValues  = []
                    }
