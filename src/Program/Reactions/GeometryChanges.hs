{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.GeometryChanges
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.GeometryChanges () where

--import Geometry.Space
--import Geometry.Space.Transform
--import Geometry.Math
--import GHCJS.Marshal
import GHCJS.Useful
--import JsHs.Debug
--import JsHs.LikeJS.Class

import Reactive
import Controllers.GUIEvents
--import Controllers.LuciClient

import Program
import Program.Model.City
--import Program.Model.CityObject
--import Program.Model.GeoJSON
import Program.View

--import Data.Geometry
--import Data.Geometry.Transform
--import Data.Geometry.Structure.PointSet
--import Debug.Trace
--import Geometry.Structure
--import Control.Monad

--JsHs.Callback
--import JsHs.Types
--import JsHs.Types.Prim
--import Data.Coerce




updateProgramView :: String -> Program -> PView -> IO PView
updateProgramView msg program pview = do
        getElementById "clearbutton" >>= elementParent >>= hideElement
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        logText msg
        return pview{cityView = cityView', scUpToDate = False}

instance Reaction Program PView ClearingGeometry "Clearing City Geometry" 0 where
    react _ ClearingGeometry program = program
        { city = clearCity (city program)
--        , controls = (controls program){placeTransform = Nothing}
        }
    response _ _ _ = updateProgramView "Cleared geometry."


instance Reaction Program PView GeoJSONLoaded "Updating City Geometry after GeoJSON is loaded" 0 where
    react _ GeoJSONLoaded
        {
        -- isDynamic = dyn
        --,
        featureCollection = col }
        program@Program{city = ci} =
            let (_errs, c) = if isEmptyCity ci
                            then buildCity citySettings col
                            else updateCity col ci
                citySettings = defaultCitySettings {
                        defScale = objectScale . controls $ program
                    }
            in program {city = c}
    response _ _ GeoJSONLoaded
        {
        -- isDynamic = dyn
        -- ,
        -- featureCollection = col
         } prog view = do
            cview <- createView (glctx $ context view) (city prog)
--            let (errors, city) = buildCity 3 200 col
--            let (scale,shift) = scenarioViewScaling 200 col
--                (errors, cityObjs) = processScenario 3 scale shift col
--            mapM_ print errors
--            printVal . unsafeCoerce . clutter $ city prog
            return view{cityView = cview}
--    react _ geoJSONI program@Program{controls = c@Controls{placeTransform = Nothing}}
--        = program{controls = c{placeTransform = Just (scal, shift)}}
--        { city = addCityStaticWires lns $ addCityObjects geoms (city program)
--        }
--        where ((geoms, lns),_msgs,scal,shift) = featureCollectionToObjectsAndScale
--                    behav (featureCollection geoJSONI) -- TODO: how to log this?
--              behav = if isDynamic geoJSONI then Dynamic else Static
--    react _ geoJSONI program@Program{controls = Controls{placeTransform = Just (scal, shift)}}
--        = program
--        { city = addCityStaticWires lns $ addCityObjects geoms (city program)
--        }
--        where ((geoms, lns),_msgs) = featureCollectionToObjects
--                    behav scal shift (featureCollection geoJSONI) -- TODO: how to log this?
--              behav = if isDynamic geoJSONI then Dynamic else Static
--    response _ _ _ = updateProgramView "Updated geometry."
