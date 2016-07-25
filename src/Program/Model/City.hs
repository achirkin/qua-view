{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
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
    , getObject, setObject
    -- | Load geometry
    , processScenario, scenarioViewScaling
    -- | Store geometry
    , storeCityAsIs
    -- | reactive-banana
    , cityBehavior
    ) where

import Control.Arrow ((***), first)
import JsHs.Types
import JsHs.WebGL
--import GHCJS.Marshal.Pure
import Data.Maybe (fromMaybe)

import qualified JsHs.Array as JS
import JsHs.JSString hiding (foldr1)
import Data.Geometry
import Data.Geometry.Structure.Feature
import qualified Data.Geometry.Structure.LineString as LS
import qualified Data.Geometry.Structure.PointSet as PS
--import Data.Geometry.Transform
--import Geometry.Structure

import Program.Model.CityObject
import Program.Model.CityGround
import Program.Model.WiredGeometry
import Program.Settings
import Program.Types

import Controllers.GUIEvents

import Reactive.Banana.Combinators
--import JsHs.Debug

--import JsHs.Debug
--import Debug.Trace

-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObjId       :: !Int
    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !CityObjectCollection
    , cityTransform     :: !(GLfloat, Vector2 GLfloat)
    , ground            :: !CityGround
    , csettings         :: !CitySettings
    , clutter           :: !(LS.MultiLineString 3 GLfloat, WiredGeometry)
    --, drawTextures      :: !Bool
    }

data CitySettings = CitySettings
    { defHeight    :: !GLfloat
    , diagFunction :: Int -> GLfloat
    , groundDilate :: !GLfloat
    , evalCellSize :: !GLfloat
    , defElevation :: !GLfloat
    , defScale     :: !(Maybe GLfloat)
    }


defaultCitySettings :: CitySettings
defaultCitySettings = CitySettings
    { defHeight    = 1
    , diagFunction = (*5) . sqrt . fromIntegral
    , groundDilate = 1
    , evalCellSize = 0.5
    , defElevation = 0.01
    , defScale     = Nothing
    }

emptyCity :: City
emptyCity = City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , ground = emptyGround
    , cityTransform = (0, 0)
    , csettings = defaultCitySettings
    , clutter = emptyLineSet (vector4 0.8 0.4 0.4 1)
    }

-- | This is a main module export.
--   Describes logic of city changes.
cityBehavior :: MonadMoment m
             => Behavior Settings
             -> Event GeoJSONLoaded
             -> Event ClearingGeometry
             -> m (Event (RequireViewUpdate City), Behavior City)
cityBehavior psets updateEvent clearEvent
    = fmap (first filterJust)
    . mapAccum emptyCity
    $ unionsSnd
      [ cityUpdate
      , cityClear
      ]
  where
    applySets = (\s f -> f $ defaultCitySettings { defScale = objectScale s}) <$> psets
    cityUpdate = fmap u $ applySets <@> loadingCityJSONEvent updateEvent
    cityClear  = fmap u $ clearCity <$ clearEvent
    u f x = let y = f x in (Just (RequireViewUpdate y), y)

unionsSnd :: [Event (a -> (Maybe b, a))] -> Event (a -> (Maybe b, a))
unionsSnd [] = never
unionsSnd xs = foldr1 (unionWith comb) xs
  where
    comb f g x = case f x of
        (Nothing, r) -> g r
        (Just y , r) -> case g r of
            (Nothing, v) -> (Just y, v)
            (Just z , v) -> (Just z, v)


-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObjectCollection = CityObjectCollection JSVal
instance JS.LikeJS "Array" CityObjectCollection
instance JS.LikeJSArray "Object" CityObjectCollection where
    type ArrayElem CityObjectCollection = LocatedCityObject

loadingCityJSONEvent :: Event GeoJSONLoaded -> Event (CitySettings -> City -> City)
loadingCityJSONEvent = fmap c
  where
    c e sets = snd . loadingCityJSON sets e

loadingCityJSON :: CitySettings -> GeoJSONLoaded -> City -> ([JSString], City)
loadingCityJSON citySettings GeoJSONLoaded { featureCollection = col } ci =
  if isEmptyCity ci then buildCity citySettings col
                    else updateCity col ci


buildCity :: CitySettings -- ^ desired diagonal length of the city
          -> FeatureCollection -- ^ scenario to build city of
          -> ([JSString], City) -- ^ Errors and the city itself
buildCity sets scenario = (,) errors City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = objects
    , ground = buildGround (groundDilate sets) objects
    , cityTransform = (cscale, cshift)
    , csettings = sets
    , clutter = createLineSet (vector4 0.8 0.4 0.4 1) liness
    }
    where (rcscale,cshift)  = scenarioViewScaling (diagFunction sets) scenario
          (errors,objects, liness) = processScenario (defHeight sets) (defElevation sets) cscale cshift scenario
          cscale = fromMaybe rcscale (defScale sets)

updateCity ::FeatureCollection -> City -> ([JSString], City)
updateCity scenario
           city@City{cityTransform = (cscale, cshift)} = (,)
        errors
        city { objectsIn = allobjects
             , ground = buildGround (groundDilate $ csettings city) allobjects
             , clutter = appendLineSet liness (clutter city)
             }
    where (errors,objects, liness) = processScenario (defHeight $ csettings city)  (defElevation $ csettings city) cscale cshift scenario
          allobjects = JS.concat (objectsIn city) objects




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
    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , cityTransform = (0, 0)
    , ground = emptyGround
    , clutter = emptyLineSet (vector4 0.8 0.4 0.4 1) --  createLineSet (Vector4 0.8 0.4 0.4 1) []
    } -- where objs' = IM.empty :: IM.IntMap LocatedCityObject

----------------------------------------------------------------------------------------------------
-- Scenario Processing
----------------------------------------------------------------------------------------------------

processScenario :: GLfloat -- ^ default height of buildings in camera space
                -> GLfloat -- ^ default elevation of lines in camera space
                -> GLfloat -- ^ scale objects before processing
                -> Vector2 GLfloat -- ^ shift objects before processing
                -> FeatureCollection -> ([JSString],CityObjectCollection, LS.MultiLineString 3 GLfloat)
processScenario h e sc sh collection | sc <= 0 = ([pack $ "processScenario: Scale is not possible (" ++ show sc ++ ")"], JS.fromList [], JS.fromList [])
                                     | otherwise = (berrs ++ lerrs, buildings, mlns)
    where (berrs, buildings) = (JS.toList *** JS.fromJSArray) $ JS.mapEither (processPolygonFeature h sc sh) plgs
          (_pts,lns,plgs) = filterGeometryTypes collection
          (lerrs, mlns) = (JS.toList *** (JS.fromJSArray . JS.join)) $ JS.mapEither (processLineFeature e sc sh) lns


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
scenarioViewScaling :: (Int->GLfloat) -- ^ desired diameter of a scenario based on number of objects
                    -> FeatureCollection
                    -> (GLfloat, Vector2 GLfloat) -- ^ scale and shift coefficients
scenarioViewScaling diam scenario = ( diam n / normL2 (h-l) , (l + h) / 2)
    where (n,l,h) = js_boundScenario scenario


{-# INLINE js_boundScenario #-}
foreign import javascript unsafe "var r = gm$boundNestedArray($1['features'].map(function(co){return co['geometry']['coordinates'];}));\
                          \if(!r){ $r2 = [Infinity,Infinity];\
                          \        $r3 = [-Infinity,-Infinity];}\
                          \else { $r2 = r[0].slice(0,2); $r3 = r[1].slice(0,2); } $r1 = $1['features'].length;"
    js_boundScenario :: FeatureCollection -> (Int, Vector2 x, Vector2 x)

----------------------------------------------------------------------------------------------------
-- Scenario Store
----------------------------------------------------------------------------------------------------

-- TODO: I discarded grid scaling, but need to decidehow to treat it later.
storeCityAsIs :: City -> FeatureCollection
storeCityAsIs City
    { objectsIn = buildings
    , clutter = (mline, _)
--    , cityTransform = (scale, shift)
    } = JS.fromJSArray . JS.fromList $
        (feature . GeoMultiLineString $ mline)
        : JS.toList (JS.map (storeCityObject 1 0 PlainFeature) buildings)
--        (feature . GeoMultiLineString $ PS.mapSet (\vec -> vec * broadcastVector (1/scale) + resizeVector shift ) mline)
--        : toList (JS.map (storeCityObject scale shift PlainFeature) buildings)





