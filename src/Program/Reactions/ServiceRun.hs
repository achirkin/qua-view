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
            } _ = do
        programInProgress
        logText ("Running service " ++ show service)
        runService service sf >>= return . Right . EBox . ServiceRunFinish
            where cs = 1
                  evalGrid = groundEvalGrid (ground ci) cs
                  sf = ScalarField
                    { cellSize  = cs
                    , sfPoints  = evalGrid
                    , sfRange   = zeros
                    , sfValues  = []
                    }
