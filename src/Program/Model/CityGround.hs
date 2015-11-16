{-# LANGUAGE TypeFamilies, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.CityGround
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
--
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Program.Model.CityGround
    ( CityGround (..)
    , buildGround, emptyGround
--    , buildGround
--    , rebuildGround
--    , groundEvalGrid
--    , groundGridToTexArray
    ) where

import GHCJS.WebGL

import Data.JSArray
import Data.Geometry
import Data.Geometry.Transform
import Data.Geometry.Structure.LinearRing
import Data.Geometry.Structure.Polygon
import qualified Data.Geometry.Structure.PointSet as PS

import SmallGL.WritableVectors

import Program.Model.CityObject

dilateConstant :: GLfloat
dilateConstant = 1

newtype CityGround = CityGround { groundPoints :: PointData }

buildGround :: (LikeJSArray s,JSArrayElem s ~ LocatedCityObject)
            => s -> CityGround
buildGround s = CityGround $ packPointData vertices indices
    where points = fromJSArray . jsjoin $ jsmap (toJSArray . transform . fmap (PS.toPointArray . objPolygons)) s :: PS.PointArray 3 GLfloat
          hull' = resizeConvexHull2D dilateConstant $ convexPolygonHull2D points :: LinearRing 2 GLfloat
          hull = PS.toPointArray hull'
          (center, dirX, dirY) = PS.boundingRectangle2D hull
          indices = triangulate'' hull emptyJSArray
          vertices = packPoints (PS.enlargeVectors hull)
                                (PS.fillPointArray (jslength hull) $ vector3 0 0 127)
                                (fromJSArray $ jsmap gtx hull)
          gtx :: Vector2 GLfloat -> Vector2 GLushort
          gtx p = case p - ldcorner of q -> vector2 (round $ dot xmult q) (round $ dot ymult q)
          m = 65535 * 0.5 -- maximum of GLushort divided by 2
          ldcorner = center - dirX - dirY
          xmult = m * dirX / dirX .*. dirX
          ymult = m * dirY / dirY .*. dirY


emptyGround :: CityGround
emptyGround = CityGround emptyPointData


----import Data.Primitive.ByteArray
----import Data.Primitive (sizeOf)
--import Control.Monad.ST (runST)
--import GHCJS.WebGL
--import SmallGL.WritableVectors
--import Data.Geometry
--import Program.Model.CityObject
--
---- | This object is used to show service evaluation results on its surface
--data CityGround = CityGround
--    { groundBox    :: !(BoundingBox 2 GLfloat)
--    , groundHull   :: !(BoundingBox 2 GLfloat)
--    , groundPoints :: !(PointData Vertex20CNT GLushort)
--    }
--
--
---- | create new CityGround object
--buildGround :: BoundingBox 2 GLfloat -> CityGround
--buildGround bbox = CityGround
--    { groundBox    = bbox
--    , groundPoints = buildGroundPoints (lowBound bbox) (highBound bbox) pd
--    } where pd = PointData
--                { vertexArray       = runST $
--                    newByteArray (4 * sizeOf (undefined :: Vertex20CNT))
--                    >>= unsafeFreezeByteArray
--                , vertexArrayLength = 4
--                , indexArray        = runST $
--                    newByteArray (4 * sizeOf (undefined :: GLushort))
--                    >>= unsafeFreezeByteArray
--                , indexArrayLength  = 4
--                }
--
---- | change the size of the CityGround
--rebuildGround :: BoundingBox 2 GLfloat -> CityGround -> CityGround
--rebuildGround bbox cg = cg
--    { groundBox    = bbox
--    , groundPoints = buildGroundPoints (lowBound bbox) (highBound bbox) (groundPoints cg)
--    }
--
---- | fill bytearrays by ground corner points
--buildGroundPoints :: Vector2 GLfloat -- lower-left corner
--                  -> Vector2 GLfloat -- upper-right corner
--                  -> PointData Vertex20CNT GLushort
--                  -> PointData Vertex20CNT GLushort
--buildGroundPoints (Vector2 lx lz) (Vector2 hx hz) pd@PointData
--    { vertexArray = arr
--    , indexArray = iarr
--    } = runST $ do
--    marr <- unsafeThawByteArray arr
--    miarr <- unsafeThawByteArray iarr
--    -- write points
--    writeByteArray marr 0 (Vector3 lx 0 hz, npy, Vector2 0 0 :: Vector2 GLushort)
--    writeByteArray marr 1 (Vector3 hx 0 hz, npy, Vector2 m 0)
--    writeByteArray marr 2 (Vector3 hx 0 lz, npy, Vector2 m m)
--    writeByteArray marr 3 (Vector3 lx 0 lz, npy, Vector2 0 m)
--    -- write indices
--    writeByteArray miarr 0 (0 :: GLushort)
--    writeByteArray miarr 1 (1 :: GLushort)
--    writeByteArray miarr 2 (3 :: GLushort)
--    writeByteArray miarr 3 (2 :: GLushort)
--    arr' <- unsafeFreezeByteArray marr
--    iarr' <- unsafeFreezeByteArray miarr
--    return pd
--        { vertexArray = arr'
--        , indexArray = iarr'
--        }
--    where npy = Vector3 0 127 0 :: Vector3 GLbyte
--          m = 65535 :: GLushort
--
--
---- texture creation
--
--groundEvalGrid :: CityGround
--               -> GLfloat  -- ^ desired cell size
--               -> [Vector3 GLfloat] -- half size in 111 direction
--groundEvalGrid CityGround{groundBox = b} cellSize = zs >>= \z -> map (\x -> Vector3 x 0 z) xs
--    where Vector2 sx sz = highBound b .- lowBound b
--          Vector2 x0 _ = lowBound b
--          Vector2 _ z1 = highBound b
--          nx = max 1 . round $ sx / cellSize :: Int
--          nz = max 1 . round $ sz / cellSize :: Int
--          dx = sx / fromIntegral nx
--          dz = - sz / fromIntegral nz
--          xs = take nx $ iterate (+dx) (0.5*dx+x0)
--          zs = take nz $ iterate (+dz) (0.5*dz+z1)
--
--
--groundGridToTexArray :: CityGround
--                     -> GLfloat
--                     -> [Vector4 GLubyte]
--                     -> ([Vector4 GLubyte], Maybe (ByteArray, Vector2 GLsizei))
--groundGridToTexArray CityGround{groundBox = bb} cellSize colors = runST $ do
--    arr <- newByteArray (nx*nz * sizeOf (undefined :: Vector4 GLubyte))
--    let f i (x:xs) | i < nx*nz = writeByteArray arr i x >> f (i+1) xs
--                   | otherwise = return (i,x:xs)
--        f i [] = return (i, [])
--    (n, colors') <- f 0 colors
--    a <- unsafeFreezeByteArray arr
--    return (colors', if n >= nx*nz then Just (a, size) else Nothing)
--    where Vector2 sx sz = highBound bb .- lowBound bb
--          nx = max 1 . round $ sx / cellSize :: Int
--          nz = max 1 . round $ sz / cellSize :: Int
--          size = fromIntegral <$> Vector2 nx nz



