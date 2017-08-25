{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FromJSON and ToJSON instances for `Model.Scenario`.
--
--   Should not be included into `qua-view`!
--
module Model.GeoJSON.Scenario
    (
    ) where


import System.IO.Unsafe (unsafePerformIO)
--import Control.Applicative
--import qualified Data.Map.Strict as Map
--import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import Numeric.DataFrame
import Numeric.DataFrame.IO

import Commons
import SmallGL.Types
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import qualified Model.Scenario.Object.Geometry as Geometry
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
        _properties <- fObj .:? "properties" .!= def
        (_geometry, PaddedZeros _padded) <- fObj .: "geometry"
        let _center = unsafePerformIO $ do
               SomeIODataFrame mdf <- Geometry.allData _geometry
               df <- unsafeFreezeDataFrame mdf
               return $ let t = ewfoldl (\a x -> a + fromScalar (4 !. x) * x )
                                        (vec4 0 0 0 0) df
                        in t / fromScalar (4 !. t)


        pure Object.Object {..}
