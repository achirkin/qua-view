{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FromJSON and ToJSON instances for `Model.Scenario`.
--
--   Should not be included into `qua-view`!
--
module Model.GeoJSON.Scenario
    ( prepareScenario
    ) where


import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.List (mapAccumL)
import Control.Lens hiding (indices)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Instances
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits
import Unsafe.Coerce

import Commons.NoReflex
import SmallGL.Types
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Properties
import Model.GeoJSON.Coordinates

instance FromJSON (Scenario.Scenario' 'Object.NotReady) where
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
        let (_objIdSeq, _objects)
                     = Object.ObjectId *** Map.fromList
                     $ mapAccumL (\i o -> case o ^. Object.properties.property "geomID" of
                                            Nothing -> (i+1, (Object.ObjectId i, o))
                                            Just k  -> (i  , (Object.ObjectId k, o))
                                 )
                                 (maxObjId+1)
                                 objList


        pure Scenario.Scenario {..}


instance FromJSON (Object.Object' 'Object.NotReady) where
    parseJSON v = flip (withObject "Feature") v $ \fObj -> do
        let _renderingData = Object.ORDN
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


prepareScenario :: Scenario.Scenario' 'Object.NotReady
                -> IO (Scenario.Scenario' 'Object.Prepared)
prepareScenario sc = Scenario.objects (traverse $ prepareObject sc) sc

prepareObject :: Scenario.Scenario' s
              -> Object.Object' 'Object.NotReady
              -> IO (Object.Object' 'Object.Prepared)
prepareObject sc obj = do
    mindices <- setNormalsAndComputeIndices (obj^.Object.geometry)
    case obj^.Object.geometry of

      Geometry.Points (SomeIODataFrame pts) -> do
        colors <- unsafeArrayThaw . ewgen $
           obj^.Object.viewColor.non (sc^.Scenario.defaultPointColor).colorVeci
        return $ obj & Object.renderingData .~ Object.ORDP (PointData  (Coords pts) (Colors colors))

      lins@(Geometry.Lines _) -> case mindices of
          Nothing -> error "Could not get indices for a line string"
          Just (SomeIODataFrame indices) -> do
            SomeIODataFrame coords <- Geometry.allData lins
            colors <- unsafeArrayThaw . ewgen $
               obj^.Object.viewColor.non (sc^.Scenario.defaultLineColor).colorVeci
            return $ obj & Object.renderingData .~ Object.ORDP (LineData (Coords coords)
                                                                         (Colors colors)
                                                                         (Indices indices))

      polys@(Geometry.Polygons _) -> case mindices of
          Nothing -> error "Could not get indices for a polygon"
          Just (SomeIODataFrame indices) -> do
            SomeIODataFrame (crsnrs' :: IODataFrame Float ns) <- Geometry.allData polys
            case someIntNatVal (dimVal (dim @ns) `div` 8) of
              Nothing -> error "Data size for a polygon"
              Just (SomeIntNat (_::Proxy n)) -> do
                let crsnrs = unsafeCoerce crsnrs' :: IODataFrame Float '[4,2,n]
                colors <- unsafeArrayThaw . ewgen $
                  obj^.Object.viewColor.non (sc^.Scenario.defaultLineColor).colorVeci
                return $ obj & Object.renderingData .~ Object.ORDP (ColoredData
                                                                     (CoordsNormals crsnrs)
                                                                     (Colors colors)
                                                                     (Indices indices))

