{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FromJSON and ToJSON instances for `Model.Scenario`.
--
--   Should not be included into `qua-view`!
--
module Model.GeoJSON.Scenario
    (
    ) where


--import Control.Applicative
--import qualified Data.Map.Strict as Map
--import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
--import Numeric.DataFrame

import Commons
import SmallGL.Types
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import Model.GeoJSON.Coordinates

instance FromJSON Scenario.Scenario where
    parseJSON v = flip (withObject "Scenario object") v $ \scObj -> do
        -- basic properties are all optional,
        -- they are parsed only if we are given scenario object (wrapped FeatureCollection
        _name       <- scObj .:? "name"
        -- msrid       <- scObj .:? "srid"
        mlon        <- scObj .:? "lon"
        mlat        <- scObj .:? "lat"
        alt         <- scObj .:? "alt" .!= 0
        _properties <- scObj .:? "properties" .!= def
        let _geoLoc = (,,) <$> mlon <*> mlat <*> Just alt

        -- Feature collection maybe this object itself or 'geometry' sub-object
        fc <- scObj .:? "geometry" .!= scObj
        _objects <- fc .: "features"

        pure Scenario.Scenario {..}


instance FromJSON Object.Object where
    parseJSON v = flip (withObject "Feature") v $ \fObj -> do
        let _renderingId = RenderedObjectId 0
            _center = 0
        _properties <- fObj .:? "properties" .!= def
        (_geometry, PaddedZeros padded) <- fObj .: "geometry"

        pure Object.Object {..}
