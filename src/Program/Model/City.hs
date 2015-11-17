{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.City
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------
module Program.Model.City
    ( City (..), buildCity, updateCity, isEmptyCity, emptyCity
    , CityObjectCollection ()
    , processScenario, scenarioViewScaling
    , getObject, setObject
    , CitySettings (..), defaultCitySettings
--    , buildCity
--    , addCityObjects
    , clearCity
--    , addCityStaticWires
    --, cityToJS
    ) where

import Control.Arrow ((***))
import GHCJS.Types
import GHCJS.WebGL
import GHCJS.Marshal.Pure

import Data.JSArray
import Data.Geometry
import Data.Geometry.Structure.Feature
--import Data.Geometry.Transform
--import Geometry.Structure

import Program.Model.CityObject
import Program.Model.CityGround
--import Program.Model.WiredGeometry




-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObjId       :: !Int
    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !CityObjectCollection
    , cityTransform     :: !(GLfloat, Vector2 GLfloat)
    , ground            :: !CityGround
    , settings          :: !CitySettings
--    , clutter           :: !WiredGeometry
    --, drawTextures      :: !Bool
    }

data CitySettings = CitySettings
    { defHeight    :: !GLfloat
    , diagFunction :: Int -> GLfloat
    , groundDilate :: !GLfloat
    , evalCellSize :: !GLfloat
    }

defaultCitySettings :: CitySettings
defaultCitySettings = CitySettings
    { defHeight    = 1
    , diagFunction = (*5) . sqrt . fromIntegral
    , groundDilate = 1
    , evalCellSize = 0.5
    }

emptyCity :: City
emptyCity = City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , ground = emptyGround
    , cityTransform = (0, 0)
    , settings = defaultCitySettings
    }

-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObjectCollection = CityObjectCollection JSVal
instance LikeJS CityObjectCollection
instance LikeJSArray CityObjectCollection where
    type JSArrayElem CityObjectCollection = LocatedCityObject




buildCity :: CitySettings -- ^ desired diagonal length of the city
          -> FeatureCollection -- ^ scenario to build city of
          -> ([JSString], City) -- ^ Errors and the city itself
buildCity sets scenario = (,) errors City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = objects
    , ground = buildGround (groundDilate sets) objects
    , cityTransform = (cscale, cshift)
    , settings = sets
    }
    where (cscale,cshift)  = scenarioViewScaling (diagFunction sets) scenario
          (errors,objects) = processScenario (defHeight sets) cscale cshift scenario

updateCity ::FeatureCollection -> City -> ([JSString], City)
updateCity scenario
           city@City{cityTransform = (cscale, cshift)} = (,)
        errors
        city { objectsIn = allobjects
             , ground = buildGround (groundDilate $ settings city) allobjects
             }
    where (errors,objects) = processScenario (defHeight $ settings city) cscale cshift scenario
          allobjects = jsconcat (objectsIn city) objects




foreign import javascript unsafe "[]"
    emptyCollection :: CityObjectCollection

foreign import javascript "$1.length"
    collectionLength :: CityObjectCollection -> Int

getObject :: Int -> City -> Maybe LocatedCityObject
getObject i City{objectsIn=objects} = pFromJSVal $ js_getObject (i-1) objects

setObject :: Int -> LocatedCityObject -> City -> City
setObject i obj city@City{objectsIn=objects} = city{objectsIn = js_setObject (i-1) (pToJSVal obj) objects}


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
--    , clutter = createLineSet (Vector4 0.8 0.4 0.4 1) []
    } -- where objs' = IM.empty :: IM.IntMap LocatedCityObject

----------------------------------------------------------------------------------------------------
-- Scenario Processing
----------------------------------------------------------------------------------------------------

processScenario :: GLfloat -- ^ default height in camera space
                -> GLfloat -- ^ scale objects before processing
                -> Vector2 GLfloat -- ^ shift objects before processing
                -> FeatureCollection -> ([JSString],CityObjectCollection)
processScenario h sc sh = (toList *** fromJSArray)
                        . jsmapEither (processFeature h sc sh)


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

