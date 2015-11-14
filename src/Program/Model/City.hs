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

import GHCJS.Foreign.Callback (Callback (), releaseCallback)
import System.IO.Unsafe (unsafePerformIO)
import Data.Coerce (coerce, Coercible)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (Any)

--import qualified Control.Monad as M
--import qualified Data.IntMap.Strict as IM
import Control.Arrow ((***))
--import GHCJS.Foreign
import GHCJS.Types
import GHCJS.WebGL
import GHCJS.Marshal.Pure
import GHCJS.Useful
--import GHCJS.Concurrent

import Data.JSArray
import Data.Geometry
--import Data.Geometry.Transform
--import Geometry.Structure

import Program.Model.CityObject
--import Program.Model.CityGround
--import Program.Model.WiredGeometry


import Controllers.GUIEvents



instance LikeJS Scenario
instance LikeJSArray Scenario where
    type JSArrayElem Scenario = ScenarioObject
    {-# INLINE toJSArray #-}
    toJSArray = js_ScenarioToJSArray
    {-# INLINE fromJSArray #-}
    fromJSArray = js_JSArrayToScenario

{-# INLINE js_ScenarioToJSArray #-}
foreign import javascript unsafe "$1['features']"
    js_ScenarioToJSArray :: Scenario -> JSArray ScenarioObject
{-# INLINE js_JSArrayToScenario #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'FeatureCollection'; $r['features'] = $1"
    js_JSArrayToScenario :: JSArray ScenarioObject -> Scenario

-- | Basic entity in the program; Defines the logic of the interaction and visualization
newtype CityObjectCollection = CityObjectCollection JSVal
instance LikeJS CityObjectCollection
instance LikeJSArray CityObjectCollection where
    type JSArrayElem CityObjectCollection = CityObject


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
          -> Scenario -- ^ scenario to build city of
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
           -> Scenario -> City -> ([JSString], City)
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
-- City properties
----------------------------------------------------------------------------------------------------

--{-# INLINE activeObjId #-}
--foreign import javascript unsafe "$1['properties']['activeObjId']"
--    activeObjId :: City -> Int
--
--
--activeObjSnapshot :: City -> Maybe LocatedCityObject
--activeObjSnapshot city = pFromJSVal $ activeObjSnapshot' city
--{-# INLINE activeObjSnapshot' #-}
--foreign import javascript unsafe "$1['properties']['activeObjSnapshot']"
--    activeObjSnapshot' :: City -> JSVal

--foldCityObjects :: (Coercible a JSVal)
--                => (a -> LocatedCityObject -> a) -> a ->  CityObjectCollection -> a
--foldCityObjects f x0 objs = unsafePerformIO $ do
--        call <- syncCallbackUnsafe2 $ \x jsv -> case pFromJSVal jsv of
--                                            Nothing  -> return $ coerce x
--                                            Just obj -> return . coerce $ f (coerce x) obj
--        rez <- foldCityObjects' call (coerce x0) objs
--        rez `seq` releaseCallback call
--        return $ coerce rez



--{-# INLINE foldCityObjects' #-}
--foreign import javascript unsafe "$3.reduce($1, $2)"
--    foldCityObjects' :: (Callback (JSVal -> JSVal -> IO JSVal)) -> JSVal -> CityObjectCollection -> IO JSVal



--mapCityObjects :: (LocatedCityObject -> LocatedCityObject) -> City -> City
--mapCityObjects f city@City{objectsIn=objs} = unsafePerformIO $ do
--        call <- syncCallbackUnsafe1 $ \jsv -> case pFromJSVal jsv of
--                                            Nothing  -> return jsv
--                                            Just obj -> return . pToJSVal $ f obj
--        rez <- mapCityObjects' call objs
--        rez `seq` releaseCallback call
--        return city{objectsIn = rez}


--{-# INLINE mapCityObjects' #-}
--foreign import javascript unsafe "$2.map($1)"
--    mapCityObjects' :: (Callback (JSVal -> IO JSVal)) -> CityObjectCollection -> IO CityObjectCollection

processScenario :: GLfloat -- ^ default height in camera space
                -> GLfloat -- ^ scale objects before processing
                -> Vector2 GLfloat -- ^ shift objects before processing
                -> Scenario -> ([JSString],CityObjectCollection)
processScenario h sc sh = (toList *** fromJSArray)
        . jsmapEither (processScenarioObject h sc sh)


--mapScenarioObjects :: (ScenarioObject -> Either JSString CityObject)
--                   -> Scenario -> IO ([JSString],CityObjectCollection)
--mapScenarioObjects f arr = do
--    call <- syncCallbackUnsafe1 $ \jsx ->
--        case f (unsafeCoerce jsx) of
--            Right v  -> setRight (unsafeCoerce v)
--            Left str -> setLeft str
--    (sarr, city) <- mapScenarioObjects' call arr
--    sarr `seq` city `seq` releaseCallback call
--    return (unsafeCoerce sarr, city)

{-# INLINE setLeft #-}
foreign import javascript unsafe "[false, $1]"
    setLeft :: JSString -> IO JSVal
{-# INLINE setRight #-}
foreign import javascript unsafe "[true, $1]"
    setRight :: JSVal -> IO JSVal

--{-# INLINE mapScenarioObjects' #-}
--foreign import javascript unsafe "var rez = $2['features'].map($1);\
--                                 \$r1 = h$fromArray(rez.filter(function(e){return !e[0];}).map(function(e){ return e[1];}));\
--                                 \$r2 = rez.filter(function(e){return e[0];}).map(function(e){ return e[1];});"
--    mapScenarioObjects' :: (Callback (JSVal -> IO JSVal)) -> Scenario -> IO (Any, CityObjectCollection)

-- | Calculate scale and shift coefficients for scenario
--   dependent on desired diameter of the scene
scenarioViewScaling :: (Int->GLfloat) -- ^ desired diameter of a scenario based on number of objects
                    -> Scenario
                    -> (GLfloat, Vector2 GLfloat) -- ^ scale and shift coefficients
scenarioViewScaling diam scenario = ( diam n / normL2 (h-l) , (l + h) / 2)
    where (n,l,h) = js_boundScenario scenario


{-# INLINE js_boundScenario #-}
foreign import javascript unsafe "var r = gm$boundNestedArray($1['features'].map(function(co){return co['geometry']['coordinates'];}));\
                          \if(!r){ $r2 = [Infinity,Infinity];\
                          \        $r3 = [-Infinity,-Infinity];}\
                          \else { $r2 = r[0].slice(0,2); $r3 = r[1].slice(0,2); } $r1 = $1['features'].length;"
    js_boundScenario :: Scenario -> (Int, Vector2 x, Vector2 x)

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

