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
--import Program.Model.CityGround
--import Program.Model.WiredGeometry



-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObjectCollection = CityObjectCollection JSVal
instance LikeJS CityObjectCollection
instance LikeJSArray CityObjectCollection where
    type JSArrayElem CityObjectCollection = LocatedCityObject


-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObjId       :: !Int
    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !CityObjectCollection
    , cityTransform     :: !(GLfloat, Vector2 GLfloat)
--    , ground            :: !CityGround
--    , clutter           :: !WiredGeometry
    --, drawTextures      :: !Bool
    }




buildCity :: GLfloat -- ^ default height of objects represented as footprints
          -> (Int -> GLfloat) -- ^ desired diagonal length of the city
          -> FeatureCollection -- ^ scenario to build city of
          -> ([JSString], City) -- ^ Errors and the city itself
buildCity defHeight diam scenario = (,) errors City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = objects
    , cityTransform = (cscale, cshift)
    }
    where (cscale,cshift)  = scenarioViewScaling diam scenario
          (errors,objects) = processScenario defHeight cscale cshift scenario

updateCity :: GLfloat -- ^ default height of builginds
           -> FeatureCollection -> City -> ([JSString], City)
updateCity defHeight scenario
           city@City{cityTransform = (cscale, cshift)} = (,)
        errors
        city { objectsIn = js_concatObjectCollections (objectsIn city) objects }
    where (errors,objects) = processScenario defHeight cscale cshift scenario


emptyCity :: City
emptyCity = City
    { activeObjId = 0
    , activeObjSnapshot = Nothing
    , objectsIn = emptyCollection
    , cityTransform = (0, 0)
    }

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
--    , ground = rebuildGround (boundingBox zeros zeros) (ground city)
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

{-# INLINE js_concatObjectCollections #-}
foreign import javascript unsafe "$1.concat($2)"
    js_concatObjectCollections :: CityObjectCollection -> CityObjectCollection -> CityObjectCollection





---- | Helper for creation of the city from the list of city objects
--buildCity :: [CityObject]
--          -> [Vector3 GLfloat] -- ^ positions
--          -> [GLfloat] -- ^ rotations (w.r.t. Y axis)
--          -> [[Vector3 GLfloat]] -- ^ static wired geometry
--          -> City
--buildCity bs ps rs clut =  City
--        { activeObj = 0
--        , activeObjSnapshot = Nothing
--        , objectsIn = objects
--        , ground = buildGround bb
--        , clutter = createLineSet (Vector4 0.8 0.4 0.4 1) clut
--        --, drawTextures = False
--        }
--    where trans p r t = translate p t >>= rotateY r
--          objects = IM.fromAscList . zip [1512,11923..] $ zipWith3 trans ps rs bs
--          bb = if IM.null objects then boundingBox zeros zeros else boundMap3d2d objects


--instance Boundable City 2 GLfloat where
--    minBBox City{ objectsIn = objs } = if IM.null objs
--                                       then boundingBox zeros zeros
--                                       else boundMap3d2d objs
--
--boundMap3d2d :: Boundable a 3 GLfloat
--             => IM.IntMap (STransform "Quaternion" GLfloat a)
--             -> BoundingBox 2 GLfloat
--boundMap3d2d objs = boundingBox (Vector2 xl zl) (Vector2 xh zh)
--        where bb3d = boundSet (fmap (fmap minBBox) objs
--                    :: IM.IntMap (STransform "Quaternion" GLfloat (BoundingBox 3 GLfloat)))
--              Vector3 xl _ zl = lowBound bb3d
--              Vector3 xh _ zh = highBound bb3d



----------------------------------------------------------------------------------------------------
-- Edit city object set
----------------------------------------------------------------------------------------------------

--
---- | Add a list of new objects to a city
--addCityObjects :: [LocatedCityObject] -> City -> City
--addCityObjects xs city@City{objectsIn = objs} = city
--    { objectsIn = objs'
--    , ground    = rebuildGround bbox (ground city)
--    } where i = if IM.null objs then 1 else fst (IM.findMax objs) + 1
--            objs' = IM.union objs . IM.fromAscList $ zip [i..] xs
--            bbox = if IM.null objs'
--                   then boundingBox zeros zeros
--                   else boundMap3d2d objs'
--
--addCityStaticWires :: [[Vector3 GLfloat]] -> City -> City
--addCityStaticWires xs city = city{clutter = appendLineSet xs (clutter city)}
--
---- | Remove all geometry from city
--clearCity :: City -> City
--clearCity city = city
--    { activeObj = 0
--    , activeObjSnapshot = Nothing
--    , objectsIn = objs'
--    , ground = rebuildGround (boundingBox zeros zeros) (ground city)
--    , clutter = createLineSet (Vector4 0.8 0.4 0.4 1) []
--    } where objs' = IM.empty :: IM.IntMap LocatedCityObject

