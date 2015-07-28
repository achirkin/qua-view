{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.GeometryChanges
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.GeometryChanges () where

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Math

import Reactive
import Controllers.GUIEvents

import Program
import Program.Model.City
import Program.Model.CityObject
import Program.Model.GeoJSON
import Program.View

updateProgramView :: Program -> PView -> IO (Either PView e)
updateProgramView program pview = do
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        return $ Left pview{cityView = cityView'}

instance Reaction Program PView ClearingGeometry "Clearing City Geometry" 0 where
    react _ ClearingGeometry program = program{city = clearCity (city program)}
    response _ _ = updateProgramView


instance Reaction Program PView GeoJSONLoaded "Updating City Geometry after GeoJSON is loaded" 0 where
    react _ geoJSONI program = program{city = addCityObjects geoms' (city program)}
        where (geoms,_msgs) = featureCollection2DtoObjects behav 0.15 (featureCollection geoJSONI) -- TODO: how to log this?
              sh = neg . mean $ map (applyV3 . wrap zeros) geoms
              geoms' = map (translate sh () >>) geoms
              behav = if isDynamic geoJSONI then Dynamic else Static
    response _ _ = updateProgramView


