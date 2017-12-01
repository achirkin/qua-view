{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Workers.LoadGeometry.Parser
    ( parseScenarioJSON
    , prepareScenario
    ) where

import Data.Semigroup
import Data.Maybe (fromMaybe)
import Data.List (mapAccumL)
import Control.Lens hiding (indices)
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Internal
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
import           Model.Scenario.Statistics
import Model.GeoJSON.Scenario ()
import Model.GeoJSON.Coordinates
import Model.GeoJSON.Coordinates.Wgs84



parseScenarioJSON :: Value -> Parser (Scenario.Scenario' 'Object.NotReady)
parseScenarioJSON v = flip (withObject "Scenario object") v $ \scObj -> do
        -- basic properties are all optional,
        -- they are parsed only if we are given scenario object (wrapped FeatureCollection)
        _name       <- scObj .:? "name"
        -- msrid       <- scObj .:? "srid"
        mlon        <- scObj .:? "lon"
        mlat        <- scObj .:? "lat"
        alt         <- scObj .:? "alt" .!= 0
        _properties <- scObj .:? "properties" .!= def
        let _geoLoc = (,,) <$> mlon <*> mlat <*> Just alt
            _viewState = def

        -- Feature collection may be this object itself or 'geometry' sub-object
        fc <- scObj .:? "geometry" .!= scObj
        objList <- fc .: "features"

        -- get maximum presented geomID to set up currentGeomID counter
        let (Max maxObjId) = foldMap (Max . fromMaybe 0 . view (Object.properties.property "geomID") )
                                     objList

        -- set up all missing geomIDs and construct a map
        let (_objIdSeq, _objects)
                     = Object.ObjectId *** Map.fromList
                     $ mapAccumL (\i o -> case o ^. Object.properties.property "geomID" of
                                    Nothing -> (i+1, (Object.ObjectId i
                                                     , o & Object.properties
                                                                 .property "geomID" .~ (Just i)
                                                     ))
                                    Just k  -> (i  , (Object.ObjectId k, o))
                                 )
                                 (maxObjId+1)
                                 objList


        pure Scenario.Scenario {..}





type PrepScenario = RWST (ScenarioStatistics, Scenario.Scenario' 'Object.NotReady)
                         () Scenario.ScenarioState IO

prepareScenario :: ScenarioStatistics
                -> Scenario.ScenarioState
                -> Scenario.Scenario' 'Object.NotReady
                -> IO (Scenario.Scenario' 'Object.Prepared)
prepareScenario st ss sc = do
    (newSc, newSs, ()) <- (\m -> runRWST m (st, sc) ss)
                $ flip Scenario.objects sc
                  $ Map.traverseWithKey
                    $ \i -> performGTransform st
                        >=> performExtrude
                        >=> prepareObject i
    return $ newSc & Scenario.viewState .~ newSs


-- | extrude geometry if we find it necessary
performExtrude :: Object.Object' 'Object.NotReady -> PrepScenario (Object.Object' 'Object.NotReady)
performExtrude o | Object.was2D o = do
                     sc <- view _2
                     let h = Scenario.resolvedObjectHeight sc o
                     liftIO $ Object.geometry (Geometry.extrudeSolidGeometry h) o
                 | otherwise      = pure o


-- | Transform from WGS'84 if we find it necessary
performGTransform :: ScenarioStatistics -> Object.Object' 'Object.NotReady
                  -> PrepScenario (Object.Object' 'Object.NotReady)
performGTransform st =
  if guessIsWgs84 st
    then \obj -> liftIO $ do
      Geometry.applyGeomCoords (obj^.Object.geometry) (wgs84ToMetric (centerPoint st))
      return $ obj & Object.center %~ (\v -> let (x,y,z,t) = unpackV4 v
                                                 v' = wgs84ToMetric (centerPoint st) (vec2 x y)
                                                 (x',y') = unpackV2 v'
                                             in  vec4 x' y' z t
                                      )
    else return



prepareObject :: Object.ObjectId
              -> Object.Object' 'Object.NotReady
              -> PrepScenario (Object.Object' 'Object.Prepared)
prepareObject (Object.ObjectId objId) obj = (view _2 >>=) $ \sc -> liftIO $ do
    mindices <- setNormalsAndComputeIndices (obj^.Object.geometry)
    let ocolor = Scenario.resolvedObjectColor sc obj ^. colorVeci
    case obj^.Object.geometry of

      Geometry.Points (SomeIODataFrame pts) -> do
        colors <- unsafeArrayThaw $ ewgen ocolor
        return $ obj & Object.renderingData .~ Object.ORDP
                                                (ObjPointData  (Coords pts) (Colors colors) objId)

      lins@(Geometry.Lines _) -> case mindices of
          Nothing -> error "Could not get indices for a line string"
          Just (SomeIODataFrame indices) -> do
            SomeIODataFrame coords <- Geometry.allData lins
            colors <- unsafeArrayThaw $ ewgen ocolor
            return $ obj & Object.renderingData .~ Object.ORDP
                                                     (ObjLineData (Coords coords)
                                                                  (Colors colors)
                                                                   objId
                                                                  (Indices indices))

      polys@(Geometry.Polygons _) -> case mindices of
          Nothing -> error "Could not get indices for a polygon"
          Just (SomeIODataFrame indices) -> do
            SomeIODataFrame (crsnrs' :: IODataFrame Float ns) <- Geometry.allData polys
            case someIntNatVal (dimVal (dim @ns) `div` 8) of
              Nothing -> error "Data size for a polygon"
              Just (SomeIntNat (_::Proxy n)) -> do
                let crsnrs = unsafeCoerce crsnrs' :: IODataFrame Float '[4,2,n]
                colors <- unsafeArrayThaw $ ewgen ocolor
                return $ obj & Object.renderingData .~ Object.ORDP
                                                         (ObjColoredData (CoordsNormals crsnrs)
                                                                         (Colors colors)
                                                                         objId
                                                                         (Indices indices))



