{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}

module Workers.LoadGeometry.Parser
    ( parseScenarioJSON
    , prepareScenario
    ) where

import Data.Semigroup
import Data.Maybe (fromMaybe, isNothing)
import Data.List (mapAccumL)
import Data.Sequence
import Control.Applicative ((<|>))
import Control.Lens hiding (indices)
import Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import JavaScript.JSON.Types.Instances hiding ((.=))
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
        mlon        <- scObj .:? "lon"
        mlat        <- scObj .:? "lat"
        alt         <- scObj .:? "alt" .!= 0
        _srid       <- scObj .:? "srid"
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
                                                                 .property "geomID" .~ Just i
                                                     ))
                                    Just k  -> (i  , (Object.ObjectId k, o))
                                 )
                                 (maxObjId+1)
                                 objList


        pure Scenario.Scenario {..}





type PrepScenario = RWST (Scenario.Scenario' 'Object.NotReady)
                         (Seq JSError) Scenario.ScenarioState IO

report :: JSError -> PrepScenario ()
report = tell . singleton

prepareScenario :: ScenarioStatistics
                -> Scenario.ScenarioState
                -> Scenario.Scenario' 'Object.NotReady
                -> IO (Scenario.Scenario' 'Object.Prepared, Seq JSError)
prepareScenario st ss sc = do
    (newSc, newSs, errs) <- (\m -> runRWST m sc oldSs)
      . flip Scenario.objects sc
        . (fmap (Map.mapMaybe id) .)
          . Map.traverseWithKey -- use Map.traverseMapWithKey when 0.5.8 at least is available
            $ \i -> transGeom
                >=> performExtrude
                >=> checkGroupId i
                >=> checkSpecial i
                >=> prepareObject i
    return (newSc & Scenario.viewState .~ newSs
                  & Scenario.geoLoc %~ (<|> mScLoc) -- infer location if it is not given
                  & Scenario.srid .~ newSRID
           , errs)
  where
    -- updated statistics is a corrected version after transform from lon-lat coordinates
    (st', mScLoc, newSRID, transGeom) = performGTransform (sc^.Scenario.srid) st
    foundVD = sc^.Scenario.viewDistance.non (inferViewDistance st')
    oldSs = ss -- update clipping distance if it is given in properties
             & Scenario.clippingDist .~ foundVD
               -- update zoom levels if it is given in properties
             & Scenario.zoomLimits   .~
                 ( sc^.Scenario.minCameraDistance.non (fst $ ss^.Scenario.zoomLimits)
                 , sc^.Scenario.maxCameraDistance.non (foundVD * 0.6667)
                 )
               -- set default camera position
             & Scenario.cameraPos .~ inferCameraLookAt st'
               -- parse service plugins
             & Scenario.servicePlugins .~ sc^.Scenario.servicePluginsProp


-- | extrude geometry if we find it necessary
performExtrude :: Object.Object' 'Object.NotReady -> PrepScenario (Object.Object' 'Object.NotReady)
performExtrude o | Object.was2D o = do
                     sc <- ask
                     let h = Scenario.resolvedObjectHeight sc o
                     liftIO $ Object.geometry (Geometry.extrudeSolidGeometry h) o
                 | otherwise      = pure o


-- | Transform from WGS'84 if we find it necessary
performGTransform :: Maybe Int -- ^ forced was WGS'84
                  -> ScenarioStatistics
                  -> ( ScenarioStatistics
                     , Maybe (Double, Double, Double) -- ^ updated loc
                     , Maybe Int -- ^ updated SRID
                     , Object.Object' 'Object.NotReady -> PrepScenario (Object.Object' 'Object.NotReady)
                     )
performGTransform forcedSRID st
  = if (forcedSRID == Just 4326) || (guessIsWgs84 st && isNothing forcedSRID)
    then ( st'
         , Just (realToFrac lon, realToFrac lat, 0)
         , Nothing
         , \obj -> liftIO $ do
               Geometry.applyGeomCoords (obj^.Object.geometry) f
               return $ obj & Object.center %~ f4
         )
    else (st, Nothing, forcedSRID, return)
  where
    f = wgs84ToMetric (centerPoint st)
    (lon, lat) = unpackV2 $ centerPoint st
    st' = st
      { lowerCorner = f $ lowerCorner st
      , upperCorner = f $ upperCorner st
      , centerPoint = 0
      }
    f4 v = let (x,y,z,t) = unpackV4 v
               v' = wgs84ToMetric (centerPoint st) (vec2 x y)
               (x',y') = unpackV2 v'
           in  vec4 x' y' z t


-- | Add an GroupId-ObjectId pair if there is one to ScenarioState
checkGroupId :: Object.ObjectId
             -> Object.Object' s -> PrepScenario (Object.Object' s)
checkGroupId objId obj = do
  forM_ (obj^.Object.groupID)
    $ \gId -> do
        Scenario.objectGroups . at gId . non [] %= (objId:)
        Scenario.groupIdSeq %= max gId
  return obj



prepareObject :: Object.ObjectId
              -> Object.Object' 'Object.NotReady
              -> PrepScenario (Maybe (Object.Object' 'Object.Prepared))
prepareObject (Object.ObjectId objId) obj = (ask >>=) $ \sc -> do
    mindices <- liftIO $ setNormalsAndComputeIndices (obj^.Object.geometry)
    let ocolor = Scenario.resolvedObjectColor sc obj ^. colorVeci
        -- parse property "selectable" to check whether an object can be selected or not.
        selectorId = if obj^.Object.selectable
                     then objId
                     else 0xFFFFFFFF
        reportFail s = Nothing <$ report
            ( "Error preparing GeoJSON Feature [geomID="
              <> JSError (toJSString $ show objId)
              <> "]: " <> s)
    case obj^.Object.geometry of

      Geometry.Points (SomeIODataFrame pts) -> do
        colors <- liftIO . unsafeArrayThaw $ ewgen ocolor
        return . Just $ obj & Object.renderingData .~ Object.ORDP
                                                (ObjPointData  (Coords pts)
                                                               (Colors colors)
                                                               selectorId)

      lins@(Geometry.Lines _) -> case mindices of
          Nothing -> reportFail "Could not get indices for a line string"
          Just (SomeIODataFrame indices) -> liftIO $  do
            SomeIODataFrame coords <- Geometry.allData lins
            colors <- unsafeArrayThaw $ ewgen ocolor
            return . Just $ obj & Object.renderingData .~ Object.ORDP
                                                     (ObjLineData (Coords coords)
                                                                  (Colors colors)
                                                                  selectorId
                                                                  (Indices indices))

      polys@(Geometry.Polygons _) -> case mindices of
          Nothing -> reportFail "Could not get indices for a polygon"
          Just (SomeIODataFrame indices) -> do
            SomeIODataFrame (crsnrs' :: IODataFrame Float ns) <- liftIO $ Geometry.allData polys
            case someIntNatVal (dimVal (dim @ns) `div` 8) of
              Nothing -> reportFail "Data size for a polygon"
              Just (SomeIntNat (_::Proxy n)) -> do
                let crsnrs = unsafeCoerce crsnrs' :: IODataFrame Float '[4,2,n]
                colors <- liftIO $ unsafeArrayThaw $ ewgen ocolor
                return . Just $ obj & Object.renderingData .~ Object.ORDP
                                                         (ObjColoredData (CoordsNormals crsnrs)
                                                                         (Colors colors)
                                                                         selectorId
                                                                         (Indices indices))


-- | Check property "special" and act accordingly
checkSpecial :: Object.ObjectId
             -> Object.Object' s -> PrepScenario (Object.Object' s)
checkSpecial objId obj = case obj ^. Object.special of
  Nothing -> return obj
  Just Object.SpecialForcedArea    ->
    obj <$ prepareSpecialForcedArea objId
  Just Object.SpecialTemplate      ->
    obj <$ prepareSpecialTemplate objId obj
  Just Object.SpecialCamera        ->
    obj <$ prepareSpecialCamera obj
  Just Object.SpecialCreationPoint ->
    obj <$ prepareSpecialCreationPoint obj


prepareSpecialForcedArea :: Object.ObjectId -> PrepScenario ()
prepareSpecialForcedArea objId
  = Scenario.forcedAreaObjId .= Just objId

prepareSpecialTemplate :: Object.ObjectId -> Object.Object' s -> PrepScenario ()
prepareSpecialTemplate objId obj
  = Scenario.templates %= Set.insert (obj^.Object.groupID.to (fmap Right).non (Left objId))


prepareSpecialCamera :: Object.Object' s -> PrepScenario ()
prepareSpecialCamera obj = do
    cp <-  getTwoPoints $ obj ^. Object.geometry
    forM_ cp $ assign Scenario.cameraPos
  where
    getTwoPoints :: Geometry.Geometry -> PrepScenario (Maybe (Vec3f, Vec3f))
    getTwoPoints (Geometry.Points (SomeIODataFrame (df :: IODataFrame Float ns)))
      | (Evidence :: Evidence ([4,n] ~ ns)) <- unsafeCoerce (Evidence @(ns ~ ns))
      , dimVal' @n == 2
      = liftIO $ do
      camP <- unsafeSubArrayFreeze @Float @'[] @3 df (1:!1:!Z)
      lookAtP <- unsafeSubArrayFreeze @Float @'[] @3 df (1:!2:!Z)
      return $ Just (camP, lookAtP)
    getTwoPoints _ = Nothing <$ report "special:camera must be a MultiPoint with exactly two points!"



prepareSpecialCreationPoint :: Object.Object' s -> PrepScenario ()
prepareSpecialCreationPoint obj = do
    cp <- getPoint $ obj ^. Object.geometry
    assign Scenario.creationPoint cp
  where
    getPoint :: Geometry.Geometry -> PrepScenario (Maybe Vec3f)
    getPoint (Geometry.Points (SomeIODataFrame (df :: IODataFrame Float ns)))
      | (Evidence :: Evidence ([4,n] ~ ns)) <- unsafeCoerce (Evidence @(ns ~ ns))
      , dimVal' @n == 1
      = liftIO $ Just <$> unsafeSubArrayFreeze @Float @'[] @3 df (1:!1:!Z)
    getPoint _ = Nothing <$ report "special:creationPoint must be a single Point type!"
