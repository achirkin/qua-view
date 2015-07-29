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

import GHCJS.Useful

import Reactive
import Controllers.GUIEvents

import Program
import Program.Model.City
import Program.Model.CityObject
import Program.Model.GeoJSON
import Program.View

updateProgramView :: Program -> PView -> IO (Either PView e)
updateProgramView program pview = do
        getElementById "clearbutton" >>= elementParent >>= hideElement
        cityView' <- updateView (glctx $ context pview) (city program) (cityView pview)
        return $ Left pview{cityView = cityView'}

instance Reaction Program PView ClearingGeometry "Clearing City Geometry" 0 where
    react _ ClearingGeometry program = program{city = clearCity (city program)}
    response _ _ = updateProgramView


instance Reaction Program PView GeoJSONLoaded "Updating City Geometry after GeoJSON is loaded" 0 where
    react _ geoJSONI program = program
        { city = addCityStaticWires lns' $ addCityObjects geoms' (city program)
        }
        where ((geoms, lns),_msgs) = featureCollectionToObjects
                    behav 0.15 (featureCollection geoJSONI) -- TODO: how to log this?
              sh = neg . mean $ map (applyV3 . wrap zeros) geoms
              geoms' = map (translate sh () >>) geoms
              shlns = neg . mean $ map mean lns
              lns' = map (map ((0.001..*) . (.+ shlns))) lns
--              centers = map mean lns'
              behav = if isDynamic geoJSONI then Dynamic else Static
    response _ _ = updateProgramView


