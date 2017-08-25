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


import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.List (mapAccumL)
import Control.Lens
import System.IO.Unsafe (unsafePerformIO)
--import Control.Applicative
import qualified Data.Map.Strict as Map
--import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import Numeric.DataFrame
import Numeric.DataFrame.IO

import Commons
import SmallGL.Types
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Properties
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
        objList <- fc .: "features"

        -- get maximum presented geomID to set up currentGeomID counter
        let (Max maxObjId) = foldMap (Max . fromMaybe 0 . view (Object.properties.property "geomID") )
                                     objList

        -- set up all missing geomIDs and construct a map
        let _objects = Map.fromList
                     . snd
                     $ mapAccumL (\i o -> case o ^. Object.properties.property "geomID" of
                                            Nothing -> (i+1, (Object.ObjectId i, o))
                                            Just k  -> (i  , (Object.ObjectId k, o))
                                 )
                                 (maxObjId+1)
                                 objList


        pure Scenario.Scenario {..}


instance FromJSON Object.Object where
    parseJSON v = flip (withObject "Feature") v $ \fObj -> do
        let _renderingId = RenderedObjectId 0
        _properties <- fObj .:? "properties" .!= def
        (_geometry, PaddedZeros _padded) <- fObj .: "geometry"
        let _center = unsafePerformIO $ do
               SomeIODataFrame mdf <- Geometry.allData _geometry
               df <- unsafeFreezeDataFrame mdf
               return $ let t = ewfoldl (\a x -> let (x1,x2,x3,x4) = unpackV4 x
                                                 in if abs x4 < 0.0001
                                                    then a
                                                    else a + vec4 (x1/x4) (x2/x4) (x3/x4) 1
                                        )
                                        (vec4 0 0 0 0) df
                        in t / fromScalar (4 !. t)


        pure Object.Object {..}
