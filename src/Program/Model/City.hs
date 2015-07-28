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
    ) where


--import qualified Control.Monad as M
import qualified Data.IntMap.Strict as IM

--import GHCJS.Foreign (toJSString)
import GHCJS.WebGL

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Structure

import Program.Model.CityObject
import Program.Model.CityGround




-- | Map of all city objects (buildings, roads, etc).
data City = City
    { activeObj         :: !Int
    , activeObjSnapshot :: !(Maybe LocatedCityObject)
    , objectsIn         :: !(IM.IntMap LocatedCityObject)
    , ground            :: !CityGround
    --, drawTextures      :: !Bool
    }

-- | Helper for creation of the city from the list of city objects
buildCity :: [CityObject]
          -> [Vector3 GLfloat] -- ^ positions
          -> [GLfloat] -- ^ rotations (w.r.t. Y axis)
          -> City
buildCity bs ps rs =  City
        { activeObj = 0
        , activeObjSnapshot = Nothing
        , objectsIn = objects
        , ground = buildGround bb
        --, drawTextures = False
        }
    where trans p r t = translate p t >>= rotateY r
          objects = IM.fromAscList . zip [1512,11923..] $ zipWith3 trans ps rs bs
          bb = if IM.null objects then boundingBox zeros zeros else boundMap3d2d objects


instance Boundable City 2 GLfloat where
    minBBox City{ objectsIn = objs } = boundMap3d2d objs

boundMap3d2d :: Boundable a 3 GLfloat
             => IM.IntMap (STransform "Quaternion" GLfloat a)
             -> BoundingBox 2 GLfloat
boundMap3d2d objs = boundingBox (Vector2 xl zl) (Vector2 xh zh)
        where bb3d = boundSet (fmap (fmap minBBox) objs
                    :: IM.IntMap (STransform "Quaternion" GLfloat (BoundingBox 3 GLfloat)))
              Vector3 xl _ zl = lowBound bb3d
              Vector3 xh _ zh = highBound bb3d


----------------------------------------------------------------------------------------------------
-- Edit city object set
----------------------------------------------------------------------------------------------------


-- | Add a list of new objects to a city
addCityObjects :: [LocatedCityObject] -> City -> City
addCityObjects xs city@City{objectsIn = objs} = city
    { objectsIn = objs'
    , ground    = rebuildGround (boundMap3d2d objs') (ground city)
    } where i = if IM.null objs then 1 else fst (IM.findMax objs) + 1
            objs' = IM.union objs . IM.fromAscList $ zip [i..] xs

-- | Remove all geometry from city
clearCity :: City -> City
clearCity city = city
    { activeObj = 0
    , activeObjSnapshot = Nothing
    , objectsIn = objs'
    , ground = rebuildGround (boundingBox zeros zeros) (ground city)
    } where objs' = IM.empty :: IM.IntMap LocatedCityObject

