{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Maybe (isJust)
--import Control.Concurrent (forkIO)
--import Control.Monad (liftM)
--import Geometry.Space
--import Geometry.Structure

import Data.JSArray

import GHCJS.Useful
import Reactive
import Program
import Program.Model.City
import Program.Model.CityGround
import Program.Model.ScalarField
--import Program.Model.GeoJSON

import Controllers.GUIEvents
import Controllers.LuciClient


import Services


import Program.Reactions.ServiceFinish
import Program.Reactions.ViewRendering ()
$(createEventSense ''ServiceRunFinish)

instance Reaction Program PView ServiceRunBegin "Run service" 1 where
    react _ _ program@Program{city = ci}
        = program{city = ci{ground = buildGround (groundDilate $ settings ci) $ objectsIn ci}}
    response _ hole _ Program
            { controls = Controls {activeService = ServiceBox service}
            , city = ci
            } pview = if isEmptyGround (ground ci)
                      then do
        logText "No geometry to run a service."
        getElementById "clearbutton" >>= elementParent >>= hideElement
        return pview
                      else do
        programInProgress
        logText ("Running service " ++ show service)
        runService service (luciClient pview) (luciScenario pview) sf >>= \r -> case r of
            Nothing -> programIdle >> return pview
            Just sfin -> (reqEvent hole . EBox $ ServiceRunFinish sfin) >> return pview
            where cs = evalCellSize $ settings ci
                  sf = ScalarField
                    { cellSize  = cs
                    , sfPoints  = groundEvalGrid (ground ci) cs
                    , sfRange   = (0,0)
                    , sfValues  = emptyJSArray
                    }

instance Reaction Program PView ServiceRunBegin "Update Scenario" 0 where
    response _ _ _ _ pview@PView{luciClient = Nothing} = return pview
    response _ _ _ _ pview@PView{scUpToDate = True, luciScenario = Just _} = return pview
    response _ _ _ program pview@PView{scUpToDate = False, luciClient = Just lc} = do
        programInProgress
        t0 <- getTime
        mscenario <- case luciScenario pview of
            _lcs -> do
              logText "Updating scenario on Luci..."
--              printVal . asJSVal $ storeCityAsIs (city program)
              tryscenario <- createLuciScenario lc "Visualizer scenario" (storeCityAsIs $ city program)
                    -- undefined -- . geometries2features . cityGeometryRoofs $ city program
              case tryscenario of
                Left err -> logText' err >> return Nothing
                Right scenario -> return (Just scenario)
        getTime >>= \t1 -> logText ("Scenario updated in " ++ show (t1-t0) ++ " seconds.")
        programIdle
        return pview{luciScenario = mscenario, scUpToDate = isJust mscenario}
    response _ _ _ _ pview = return pview
