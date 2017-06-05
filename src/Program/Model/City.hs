{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.City
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
module Program.Model.City
    ( City (..), buildCity, updateCity, isEmptyCity, emptyCity, clearCity
    , CitySettings (..), defaultCitySettings
    , CityObjectCollection ()
    , ClearingGeometry (..)
    , getObject, setObject
    -- | Load geometry
    , processScenario, scenarioViewScaling
    -- | Store geometry
    , storeCityAsIs, storeObjectsAsIs
    -- | reactive-banana
    , cityBehavior
    , CityUpdate (..)
    , GroundUpdated (..)
    , GroundUpdateRequest (..)
    , colorizeObjects
    ) where

import Control.Arrow ((***))
import JsHs.Types
import JsHs.WebGL
--import GHCJS.Marshal.Pure
import Data.Maybe (fromMaybe)

import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import JsHs.JSString hiding (foldr1)
import Data.Geometry
import Data.Geometry.Structure.Feature
import qualified Data.Geometry.Transform as T
import qualified Data.Geometry.Structure.LineString as LS
import qualified Data.Geometry.Structure.PointSet as PS
--import Data.Geometry.Transform
--import Geometry.Structure

import Program.Model.CityObject
import Program.Model.CityGround
import Program.Model.WiredGeometry
import Program.Model.Camera
import Program.Settings
import Program.Types
import Program.VisualService

import Reactive.Banana.Combinators
--import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer.Strict
--import JsHs.Debug
--import JsHs.Debug
--import Debug.Trace

-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObjId       :: !Int
--    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !CityObjectCollection
    , cityTransform     :: !(GLfloat, Vector2 GLfloat)
    , ground            :: !CityGround
    , csettings         :: !CitySettings
    , clutter           :: !(LS.MultiLineString 3 GLfloat, WiredGeometry)
    , buildingColors    :: !(Maybe (PS.PointArray 4 GLfloat))
    --, drawTextures      :: !Bool
    , oriLatLonAlt      :: !(Maybe (Vector 3 GLfloat))
    , srid              :: !(Maybe Int)
    }

data CitySettings = CitySettings
    { defHeight    :: !GLfloat
    , diagFunction :: Int -> GLfloat
    , groundDilate :: !GLfloat
    , evalCellSize :: !GLfloat
    , defElevation :: !GLfloat
    , defScale     :: !(Maybe GLfloat)
    , scLonLat     :: !(Maybe (Vector 2 GLfloat))
    }

-- | This indicates removal of all geometry from the city
data ClearingGeometry = ClearingGeometry

defaultCitySettings :: CitySettings
defaultCitySettings = CitySettings
    { defHeight    = 1.5
    , diagFunction = (*5) . sqrt . fromIntegral
    , groundDilate = 1
    , evalCellSize = 0.5
    , defElevation = 0.01
    , defScale     = Nothing
    , scLonLat     = Nothing
    }

emptyCity :: City
emptyCity = City
    { activeObjId = 0
--    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , ground = emptyGround
    , cityTransform = (0, 0)
    , csettings = defaultCitySettings
    , clutter = emptyLineSet (vector4 0.8 0.4 0.4 1)
    , buildingColors = Nothing
    , oriLatLonAlt = Nothing
    , srid         = Nothing
    }

-- | An event that represents all possible city changes
data CityUpdate
  = CityErase
    -- ^ clear all geometry
  | CityUpdate GeometryInput
    -- ^ update geometry
  | CityNew GeometryInput
    -- ^ create whole new geometry

-- | This is a main module export.
--   Describes logic of city changes.
cityBehavior :: (MonadMoment m, MonadFix m)
             => Behavior Settings
             -> Behavior (Maybe Int) -- ^ selected object id
             -> Event (Maybe JSString) -- ^ click on some property to view colormap
             -> Event (Maybe Int) -- ^ held object id
             -> Event (ObjectTransform T.QFTransform CityObject)
             -> Event CityUpdate
             -> Event GroundUpdateRequest
             -> Event VisualServiceResult
             -> m (Event (RequireViewUpdate City), Behavior City, Event [JSString], Event (GeomId, Matrix4 GLfloat), Event GroundUpdated)
cityBehavior psets selIdB colorizeE heldIdE otransform cityChange grounUpdateRequestE vsResultE = mdo
    activeObjectSnapshot <- stepper Nothing $ osnapshotF <$> cityBeh <*> selIdB <@> heldIdE
    let objectMove = fmap ((,) (Nothing, []) .)
                    $ objectMoveF <$> activeObjectSnapshot <@> otransform
        objectMovedRecord = filterJust $ objectMotionRecord <$> activeObjectSnapshot <@> otransform
    (cityUE',cityBeh) <- mapAccum emptyCity
              $ unionsChanges
                [ objectMove
                , cityUpdates
                , cityPropResultsF <$> vsResultE
                ]
    (groundB, groundUpdatedE) <- groundBehavior cityBeh grounUpdateRequestE
    let cityUE = filterJust $ fst <$> cityUE'
        cityErrors = filterE (not . Prelude.null) $ snd <$> cityUE'
        cityB2 = addGroundB <$> groundB <*> (addSelId <$> selIdB <*> cityBeh)
    colorizeObjectsB <- colorizeObjects colorizeE cityB2
    return (cityUE, addColorObject <$> colorizeObjectsB <*> cityB2, cityErrors, objectMovedRecord, groundUpdatedE)
  where
    addSelId Nothing city = city{activeObjId = 0}
    addSelId (Just i) city = city{activeObjId = i}
    addGroundB gr city = city{ground = gr}
    addColorObject col city = city{buildingColors = col}
    cityUpdates = manageCityUpdates psets cityChange
    cityPropResultsF (VisualServiceResultObjects _ ps) city = ((Nothing, []), city {objectsIn = js_updateProps (objectsIn city) ps})
    cityPropResultsF _  city = ((Nothing, []), city)
    osnapshotF _ Nothing _ = Nothing
    osnapshotF _ _ Nothing = Nothing
    osnapshotF city (Just i) (Just j) | i /= j = Nothing
                                      | otherwise = (,) i <$> getObject i city
    objectMoveF Nothing   _                  city = city
    objectMoveF (Just (i,o)) TransformCancel       city = setObject i o city
    objectMoveF (Just (i,o)) (TransformProgress t) city = setObject i (t o) city
    objectMoveF (Just (i,o)) (ObjectTransform   t) city = setObject i (t o) city
    objectMotionRecord (Just (_,o)) (ObjectTransform t) = Just (geomId $ T.unwrap o,m)
        where T.MTransform m _ = T.mergeSecond (pure id) $ t (pure $ T.unwrap o)
    objectMotionRecord _ _ = Nothing



--unionsSnd :: [Event (a -> (Maybe b, a))] -> Event (a -> (Maybe b, a))
--unionsSnd [] = never
--unionsSnd xs = foldr1 (unionWith comb) xs
--  where
--    comb f g x = case f x of
--        (Nothing, r) -> g r
--        (Just y , r) -> case g r of
--            (Nothing, v) -> (Just y, v)
--            (Just z , v) -> (Just z, v)

unionsChanges :: [Event (a -> ((Maybe b, [JSString]), a))] -> Event (a -> ((Maybe b, [JSString]), a))
unionsChanges [] = never
unionsChanges as = foldr1 (unionWith comb) as
  where
    comb f g x = case f x of
        ((Nothing, xs), r) -> ins xs $ g r
        ((Just y, xs), r) -> case g r of
            ((Nothing, ys), v) -> ((Just y, xs++ys), v)
            ((Just z, ys), v) -> ((Just z, xs++ys), v)
    ins xs ((ma, ys), b) = ((ma, xs ++ ys), b)


-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObjectCollection = CityObjectCollection JSVal
instance JS.LikeJS "Array" CityObjectCollection
instance JS.LikeJSArray "Object" CityObjectCollection where
    type ArrayElem CityObjectCollection = LocatedCityObject


-- | Pure city transform given a feature collection to apply
manageCityUpdate :: Settings -> CityUpdate -> City -> ([JSString], City)
manageCityUpdate _ CityErase _ = ([], emptyCity)
manageCityUpdate sets (CityNew gi) _ = buildCity (defaultCitySettings { defScale = objectScale sets}) gi
manageCityUpdate sets (CityUpdate gi) city | isEmptyCity city = buildCity (defaultCitySettings { defScale = objectScale sets}) gi
                                           | otherwise = updateCity gi city

-- | City transforms in Event style
manageCityUpdates :: Behavior Settings
                  -> Event CityUpdate
                  -> Event (City -> ((Maybe (RequireViewUpdate City), [JSString]), City))
manageCityUpdates bsets ev = u <$> transforms
  where
    transforms :: Event (City -> ([JSString], City))
    transforms = manageCityUpdate <$> bsets <@> ev
    u f x = let (ss, y) = f x in ((Just (RequireViewUpdate y), ss), y)

--loadingCityJSONEvent :: Event GeoJSONLoaded -> Event (CitySettings -> City -> City)
--loadingCityJSONEvent = fmap c
--  where
--    c e sets = snd . loadingCityJSON sets e
--
--loadingCityJSON :: CitySettings -> GeoJSONLoaded -> City -> ([JSString], City)
--loadingCityJSON citySettings GeoJSONLoaded { featureCollection = col } ci =
--  if isEmptyCity ci then buildCity citySettings col
--                    else updateCity col ci


buildCity :: CitySettings -- ^ desired diagonal length of the city
          -> GeometryInput -- ^ scenario to build city of
          -> ([JSString], City) -- ^ Errors and the city itself
buildCity sets scenario = (,) errors City
    { activeObjId = 0
--    , activeObjSnapshot = Nothing
    , objectsIn = objects
    , ground = buildGround (groundDilate sets) objects
    , cityTransform = (cscale, cshift)
    , csettings = sets { scLonLat = pfcLonLat parsedCollection}
    , clutter = createLineSet (vector4 0.8 0.4 0.4 1) liness
    , buildingColors = Nothing
    , oriLatLonAlt = giOriginLatLonAlt
    , srid = giSrid
    }
    where (rcscale,cshift)  = scenarioViewScaling (diagFunction sets) parsedCollection
          errors = giErrors ++ fcErrors;
          (fcErrors,objects, liness) = processScenario (defHeight sets) (defElevation sets) cscale cshift parsedCollection
          cscale = fromMaybe rcscale (defScale sets)
          (giSrid, giOriginLatLonAlt, giErrors, parsedCollection) = smartProcessGeometryInput 2 (vector3 0 0 (defElevation sets)) scenario

--  { pfcPoints  :: JS.Array Feature
--  , pfcLines   :: JS.Array Feature
--  , pfcPolys   :: JS.Array Feature
--  , pfcDeletes :: JS.Array Int
--  , pfcErrors  :: JS.Array JSString
--  , pfcMin     :: Vector n x
--  , pfcMax     :: Vector n x
--  , pfcDims    :: Int


updateCity :: GeometryInput -> City -> ([JSString], City)
-- TODO: Improve updateCity logic.
updateCity scenario
           city@City{cityTransform = (cscale, cshift)} = (,)
        errors
        city { objectsIn = allobjects
             , ground = buildGround (groundDilate $ csettings city) allobjects
             , clutter = appendLineSet liness (clutter city)
             }
    where (errors,objects, liness) = processScenario (defHeight $ csettings city)  (defElevation $ csettings city) cscale cshift parsedCollection
--          updates = JS.map (geomId . T.unwrap) objects
--          deletes = JS.toList $ JS.concat (JS.map GeomId $ pfcDeletes parsedCollection) updates
--          afterDelete = JS.filter (\o -> geomId (T.unwrap o) `notElem` deletes) $ objectsIn city
--          allobjects = JS.concat afterDelete objects
          allobjects = js_smartUpdateCity (objectsIn city) objects (JS.map GeomId $ pfcDeletes parsedCollection)
          prevMaxGeomId = max 2 . fromIntegral . Prelude.maximum . JS.toList . JS.map (geomId . T.unwrap) $ objectsIn city
          (giSrid, giOriginLatLonAlt, giErrors, parsedCollection) = smartProcessGeometryInput prevMaxGeomId (vector3 0 0 (defElevation $ csettings city)) scenario


foreign import javascript unsafe "gm$smartUpdateBArray($1, $2, $3)"
    js_smartUpdateCity :: CityObjectCollection -> CityObjectCollection -> JS.Array GeomId -> CityObjectCollection

foreign import javascript unsafe "gm$updateProps($1, $2)"
    js_updateProps :: CityObjectCollection -> JSTA.TypedArray GLfloat -> CityObjectCollection



foreign import javascript unsafe "[]"
    emptyCollection :: CityObjectCollection

foreign import javascript "$1.length"
    collectionLength :: CityObjectCollection -> Int

getObject :: Int -> City -> Maybe LocatedCityObject
getObject i City{objectsIn=objects} = JS.asLikeJS $ js_getObject (i-1) objects

setObject :: Int -> LocatedCityObject -> City -> City
setObject i obj city@City{objectsIn=objects} = city{objectsIn = js_setObject (i-1) (JS.asJSVal obj) objects}


foreign import javascript unsafe "$2[$1]"
    js_getObject :: Int -> CityObjectCollection -> JSVal

foreign import javascript unsafe "$r = $3.slice(); $r[$1] = $2;"
    js_setObject :: Int-> JSVal -> CityObjectCollection -> CityObjectCollection

isEmptyCity :: City -> Bool
isEmptyCity c = collectionLength (objectsIn c) == 0

-- | Remove all geometry from city
clearCity :: City -> City
clearCity city = city
    { activeObjId = 0
--    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , cityTransform = (0, 0)
    , ground = emptyGround
    , csettings   = defaultCitySettings
    , clutter = emptyLineSet (vector4 0.8 0.4 0.4 1) --  createLineSet (Vector4 0.8 0.4 0.4 1) []
    } -- where objs' = IM.empty :: IM.IntMap LocatedCityObject

----------------------------------------------------------------------------------------------------
-- Scenario Processing
----------------------------------------------------------------------------------------------------

processScenario :: GLfloat -- ^ default height of buildings in camera space
                -> GLfloat -- ^ default elevation of lines in camera space
                -> GLfloat -- ^ scale objects before processing
                -> Vector2 GLfloat -- ^ shift objects before processing
                -> ParsedFeatureCollection 3 GLfloat -> ([JSString],CityObjectCollection, LS.MultiLineString 3 GLfloat)
processScenario h e sc sh collection | sc <= 0 = ([pack $ "processScenario: Scale is not possible (" ++ show sc ++ ")"], JS.fromList [], JS.fromList [])
                                     | otherwise = (JS.toList (pfcErrors collection) ++ berrs ++ lerrs, buildings, mlns)
    where (berrs, buildings) = (JS.toList *** JS.fromJSArray) $ JS.mapEither (processPolygonFeature h sc sh) (pfcPolys collection)
          (lerrs, mlns) = (JS.toList *** (JS.fromJSArray . JS.join)) $ JS.mapEither (processLineFeature e sc sh) (pfcLines collection)


processLineFeature :: GLfloat -- ^ default z-position in camera space
                   -> GLfloat -- ^ scale objects before processing
                   -> Vector2 GLfloat -- ^ shift objects before processing
                   -> Feature -> Either JSString (JS.Array (LS.LineString 3 GLfloat))
processLineFeature defz scale shift sObj = JS.mapSame (PS.mapSet (\vec -> (vec - resizeVector shift) * broadcastVector scale )) <$>
    (getSizedGeoJSONGeometry (vector3 0 0 (defz / scale)) sObj >>= toMLS)
    where toMLS :: GeoJsonGeometry 3 GLfloat -> Either JSString (JS.Array (LS.LineString 3 GLfloat))
          toMLS (GeoLineString x)      = Right $ JS.fromList [x]
          toMLS (GeoMultiLineString x) = Right $ JS.toJSArray x
          toMLS _                      = Left "processLineFeature: wrong geometry type (not a line)"


-- | Calculate scale and shift coefficients for scenario
--   dependent on desired diameter of the scene
scenarioViewScaling :: (Int -> GLfloat)
                    -> ParsedFeatureCollection 3 GLfloat
                    -> (GLfloat, Vector2 GLfloat)
scenarioViewScaling diam scenario = ( 2 * diam n / normL2 (h-l) , vector2 x y)
    where
      n = JS.length (pfcPolys scenario) + 1 -- + JS.length (pfcLines scenario) * 2
      l = pfcMin scenario
      h = pfcMax scenario
      (x,y,_) = unpackV3 $ (l + h) / 2




--scenarioViewScaling :: (Int->GLfloat) -- ^ desired diameter of a scenario based on number of objects
--                    -> FeatureCollection
--                    -> (GLfloat, Vector2 GLfloat) -- ^ scale and shift coefficients
--scenarioViewScaling diam scenario = ( diam n / normL2 (h-l) , (l + h) / 2)
--    where (n,l,h) = js_boundScenario scenario
--
--foreign import javascript unsafe "var r = gm$boundNestedArray($1['features'].map(function(co){return co['geometry']['coordinates'];}));\
--                          \if(!r){ $r2 = [Infinity,Infinity];\
--                          \        $r3 = [-Infinity,-Infinity];}\
--                          \else { $r2 = r[0].slice(0,2); $r3 = r[1].slice(0,2); } $r1storeObjectsAsIs = $1['features'].length;"
--    js_boundScenario :: FeatureCollection -> (Int, Vector2 x, Vector2 x)

----------------------------------------------------------------------------------------------------
-- Scenario Store
----------------------------------------------------------------------------------------------------

-- TODO: I discarded grid scaling, but need to decidehow to treat it later.
storeCityAsIs :: City -> GeometryInput
-- TODO: Proper change in the logic?
storeCityAsIs City
    { objectsIn = buildings
    , clutter = (mline, _)
    , cityTransform = (scale, shift)
    } = js_FCToGI $ JS.fromJSArray . JS.fromList $
       (feature . PS.mapSet (\x -> x*scale3 + shift3) . GeoMultiLineString $ mline)
        : JS.toList (JS.map (storeCityObject scale shift PlainFeature) buildings)
  where
    shift3 = resizeVector shift
    scale3 = broadcastVector (1/scale)

storeObjectsAsIs :: [GeomId] -> City -> GeometryInput
-- TODO: Proper change in the logic?
storeObjectsAsIs xs City
    { objectsIn = buildings
--    , clutter = (mline, _)
    , cityTransform = (scale, shift)
    } = js_FCToGI $ JS.fromJSArray . JS.map (storeCityObject scale shift PlainFeature) $ JS.filter (\o -> geomId (T.unwrap o) `elem` xs) buildings



----------------------------------------------------------------------------------------------------
-- City Ground behavior Store
----------------------------------------------------------------------------------------------------

data GroundUpdateRequest = GroundUpdateRequest | GroundClearRequest
  deriving (Eq, Show)
data GroundUpdated = GroundUpdated (PS.PointArray 3 GLfloat) | GroundCleared

-- | Behavior of a colourfull grid under the city
groundBehavior :: (MonadMoment m, MonadFix m) -- , MonadFix m
             => Behavior City
             -> Event GroundUpdateRequest
             -> m (Behavior CityGround, Event GroundUpdated)
groundBehavior cityB updateE = do
    groundB <- stepper emptyGround (fst <$> groundE)
    return (groundB, snd <$> groundE)
  where
    groundE = updateGround <$> cityB <@> updateE
    updateGround ci GroundUpdateRequest = let g = buildGround (groundDilate $ csettings ci) $ objectsIn ci
                                          in ( g
                                             , GroundUpdated $ groundEvalGrid g (evalCellSize $ csettings ci) (cityTransform ci)
                                             )
    updateGround _  GroundClearRequest  = (emptyGround, GroundCleared)



----------------------------------------------------------------------------------------------------
-- Coloring buildings
----------------------------------------------------------------------------------------------------

colorizeObjects ::  (MonadMoment m, MonadFix m)
                => Event (Maybe JSString)
                -> Behavior City -- ^ selected object id
                -> m (Behavior (Maybe (PS.PointArray 4 GLfloat)))
colorizeObjects colorizeE cityB = stepper Nothing propValuesE
  where
    propValuesE :: Event (Maybe (PS.PointArray 4 GLfloat))
    propValuesE = propValues <$> cityB <@> colorizeE
    propValues ci (Just pname) = case js_smartColors (-1) $ JS.map (js_getThisProp pname . allProps . T.unwrap) (objectsIn ci) of
      (vals, True) -> Just . JS.fromJSArray
                                      . JS.map (\x -> fromIntegral x / 255)
                                      $ applyPalette palette subs vals
      (colors, False) -> Just colors
    propValues _ Nothing = Nothing
    subs = Just (broadcastVector $ -1, vector4 100 100 100 200)
    palette = Bezier3Palette (vector4 50 50 240 255)
                             (vector4 50 240 100 255)
                             (vector4 100 240 50 255)
                             (vector4 240 50 50 255)


-- foreign import javascript safe "gm$smartNormalizeValues($2,$1)" js_smartColors :: GLfloat -> JS.Array JSVal -> PS.PointArray 4 GLfloat
foreign import javascript unsafe "var snv = gm$smartNormalizeValues($2,$1); $r1 = snv[0]; $r2 = snv[1];"
  js_smartColors :: GLfloat -> JS.Array JSVal -> (PS.PointArray 4 GLfloat, Bool)
foreign import javascript unsafe "$2[$1]" js_getThisProp :: JSString -> JSVal -> JSVal



