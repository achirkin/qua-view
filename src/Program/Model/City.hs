{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
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
    ( City (..)
    , buildCity
    , addCityObjects
    , clearCity
    , addCityStaticWires
    --, cityToJS
    ) where


--import qualified Control.Monad as M
import qualified Data.IntMap.Strict as IM

--import GHCJS.Foreign
--import GHCJS.Types
import GHCJS.WebGL

import Data.Geometry
import Data.Geometry.Transform
--import Geometry.Structure

import Program.Model.CityObject
import Program.Model.CityGround
import Program.Model.WiredGeometry




-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObj         :: !Int
    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !(IM.IntMap LocatedCityObject)
    , ground            :: !CityGround
    , clutter           :: !WiredGeometry
    --, drawTextures      :: !Bool
    }

-- | Helper for creation of the city from the list of city objects
buildCity :: [CityObject]
          -> [Vector3 GLfloat] -- ^ positions
          -> [GLfloat] -- ^ rotations (w.r.t. Y axis)
          -> [[Vector3 GLfloat]] -- ^ static wired geometry
          -> City
buildCity bs ps rs clut =  City
        { activeObj = 0
        , activeObjSnapshot = Nothing
        , objectsIn = objects
        , ground = buildGround bb
        , clutter = createLineSet (Vector4 0.8 0.4 0.4 1) clut
        --, drawTextures = False
        }
    where trans p r t = translate p t >>= rotateY r
          objects = IM.fromAscList . zip [1512,11923..] $ zipWith3 trans ps rs bs
          bb = if IM.null objects then boundingBox zeros zeros else boundMap3d2d objects


instance Boundable City 2 GLfloat where
    minBBox City{ objectsIn = objs } = if IM.null objs
                                       then boundingBox zeros zeros
                                       else boundMap3d2d objs

boundMap3d2d :: Boundable a 3 GLfloat
             => IM.IntMap (STransform "Quaternion" GLfloat a)
             -> BoundingBox 2 GLfloat
boundMap3d2d objs = boundingBox (Vector2 xl zl) (Vector2 xh zh)
        where bb3d = boundSet (fmap (fmap minBBox) objs
                    :: IM.IntMap (STransform "Quaternion" GLfloat (BoundingBox 3 GLfloat)))
              Vector3 xl _ zl = lowBound bb3d
              Vector3 xh _ zh = highBound bb3d


----------------------------------------------------------------------------------------------------
-- GeoJSON conversion
----------------------------------------------------------------------------------------------------

--
--type GeometryJson = JSVal
--data GeometryJson_
--
--cityToJS :: City -> IO GeometryJson
--cityToJS city = do
--    jcity <- newObj :: IO GeometryJson
--    --unsafeSetProp "type" "FeatureCollection" jcity
--    setJSString jcity (toJSString "type") (toJSString "FeatureCollection")
--
--    return jcity
--
--
----cityObjectToJSRef :: (Int, LocatedCityObject) -> JSVal
----cityObjectToJSRef (i, obj) = do
----    jsrMultiPolygon
--
--
--foreign import javascript safe "console.log({ \
--    \ 'type': 'Feature', properties: { Layer: 'buildings', 'SubClasses': $1, 'ExtendedEn': null, \
--    \ 'Linetype': null, 'EntityHand': '6F', 'Text': null }, 'geometry': { 'type': 'Polygon', 'coordinates': \
--    \[ [ [ 17093338.071588061749935, 39302064.126328192651272 ], [ 17093698.622214030474424, 39298130.289272576570511 ], [ 17083208.390065658837557, 39297168.820936642587185 ], [ 17082847.839439678937197, 39301102.657992236316204 ], [ 17093338.071588061749935, 39302064.126328192651272 ] ] ] } });"
--    jsrMultiPolygon :: Int -> IO ()
--
--foreign import javascript unsafe "$1[$2] = $3;"
--    setJSString :: JSVal -> JSString -> JSString -> IO ()

----------------------------------------------------------------------------------------------------
-- Edit city object set
----------------------------------------------------------------------------------------------------


-- | Add a list of new objects to a city
addCityObjects :: [LocatedCityObject] -> City -> City
addCityObjects xs city@City{objectsIn = objs} = city
    { objectsIn = objs'
    , ground    = rebuildGround bbox (ground city)
    } where i = if IM.null objs then 1 else fst (IM.findMax objs) + 1
            objs' = IM.union objs . IM.fromAscList $ zip [i..] xs
            bbox = if IM.null objs'
                   then boundingBox zeros zeros
                   else boundMap3d2d objs'

addCityStaticWires :: [[Vector3 GLfloat]] -> City -> City
addCityStaticWires xs city = city{clutter = appendLineSet xs (clutter city)}

-- | Remove all geometry from city
clearCity :: City -> City
clearCity city = city
    { activeObj = 0
    , activeObjSnapshot = Nothing
    , objectsIn = objs'
    , ground = rebuildGround (boundingBox zeros zeros) (ground city)
    , clutter = createLineSet (Vector4 0.8 0.4 0.4 1) []
    } where objs' = IM.empty :: IM.IntMap LocatedCityObject

