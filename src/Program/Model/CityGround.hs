{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.CityGround
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
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
--    , rebuildGround
    , groundEvalGrid
    , groundGridToTexArray
    , isEmptyGround
    ) where

import Unsafe.Coerce (unsafeCoerce)

import JsHs.WebGL

import JsHs.Array as JS
import Data.Geometry
import Data.Geometry.Transform
import Data.Geometry.Structure.LinearRing
import Data.Geometry.Structure.Polygon
import qualified Data.Geometry.Structure.PointSet as PS
import JsHs.TypedArray

import SmallGL.WritableVectors

import Program.Model.CityObject

data CityGround = CityGround
    { groundPoints :: !PointData
    , groundCorner :: !(Vector3 GLfloat)
    , groundX      :: !(Vector3 GLfloat)
    , groundY      :: !(Vector3 GLfloat)
    }

buildGround :: (LikeJSArray "Object" s, ArrayElem s ~ LocatedCityObject)
            => GLfloat -- ^ how much to dilate the ground area
            -> s -- ^ object collection to bound
            -> CityGround
buildGround dilateConstant s = CityGround
    { groundPoints = packPointData vertices indices
    , groundCorner = resizeVector ldcorner
    , groundX      = resizeVector $ dirX * 2
    , groundY      = resizeVector $ dirY * 2
    }
    where points = fromJSArray . JS.join $ JS.map (toJSArray . transform . fmap (PS.toPointArray . objPolygons)) s :: PS.PointArray 3 GLfloat
          hull' = resizeConvexHull2D dilateConstant $ convexPolygonHull2D points :: LinearRing 2 GLfloat
          hull = PS.toPointArray hull'
          (center, dirX, dirY) = PS.boundingRectangle2D hull
          indices = triangulate'' hull JS.emptyArray
          vertices = packPoints (PS.enlargeVectors hull)
                                (PS.fillPointArray (JS.length hull) $ vector3 0 0 127)
                                (fromJSArray $ JS.map gtx hull)
          gtx :: Vector2 GLfloat -> Vector2 GLushort
          gtx p = case p - ldcorner of q -> vector2 (roundclamp $ dot xmult q) (roundclamp $ dot ymult q)
          m = 65535*0.5 -- maximum of GLushort divided by 2
          ldcorner = center - dirX - dirY
          xmult = m * dirX / (dirX .*. dirX)
          ymult = m * dirY / (dirY .*. dirY)
          roundclamp = max 0 . min 65535 . round


emptyGround :: CityGround
emptyGround = CityGround
    { groundPoints = emptyPointData
    , groundCorner = 0
    , groundX      = vector3 1 0 0
    , groundY      = vector3 0 1 0
    }

isEmptyGround :: CityGround -> Bool
isEmptyGround gr = indexArrayLength (groundPoints gr) == 1


----import Data.Primitive.ByteArray
----import Data.Primitive (sizeOf)
--import Control.Monad.ST (runST)
--import JsHs.WebGL
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
-- texture creation

groundEvalGrid :: CityGround
               -> GLfloat  -- ^ desired cell size
               -> PS.PointArray 3 GLfloat -- half size in 111 direction
groundEvalGrid CityGround{..} cellSize = fromJSArray . JS.map f $ js_groundEvalGrid nx ny
    where nx = max 1 . round $ normL2 groundX / cellSize :: Int
          ny = max 1 . round $ normL2 groundY / cellSize :: Int
          dx = groundX / broadcastVector (fromIntegral nx)
          dy = groundY / broadcastVector (fromIntegral ny)
          f ij = case unpackV2 ij of
                   (i,j) -> groundCorner + broadcastVector i * dx
                                         + broadcastVector j * dy

foreign import javascript unsafe "$r = new Array($1*$2); for(i = 0; i < $1; i++){for(j = 0; j < $2; j++){$r[i+j*$1] = [i+0.5,j+0.5];}}"
    js_groundEvalGrid :: Int -> Int -> PS.PointArray 2 GLfloat


groundGridToTexArray :: CityGround
                     -> GLfloat
                     -> PS.PointArray 4 GLubyte
                     -> (PS.PointArray 4 GLubyte, Maybe (TypedArray GLubyte, (GLsizei, GLsizei)))
groundGridToTexArray CityGround{..} cellSize colors =
    if n' < nn
    then (fromJSArray JS.emptyArray, Nothing)
    else (JS.drop nn colors , Just (
        fromJSArrayToTypedArray . PS.flatten . JS.take nn $ colors
        , (nx, ny))
    )
    where nx = max 1 . round $ normL2 groundX / cellSize
          ny = max 1 . round $ normL2 groundY / cellSize
          nn = fromIntegral (nx*ny)
          n' = fromIntegral $ JS.length colors

fromJSArrayToTypedArray :: (TypedArrayOperations a) => JS.Array a -> TypedArray a
fromJSArrayToTypedArray = fromArray . unsafeFromJSArrayCoerce

unsafeFromJSArrayCoerce :: JS.Array a -> TypedArray a
unsafeFromJSArrayCoerce = unsafeCoerce

--foreign import javascript unsafe "$r = new Array($1*$2); for(i = 0; i < $1; i++){for(j = 0; j < $2; j++){$r[i+j*$1] = [i,j]}}"
--    js_groundEvalGrid :: Int -> Int -> TypedArray GL

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



