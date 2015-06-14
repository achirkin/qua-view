{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Model.Building
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Model.CityObject where

import qualified Control.Monad as M
--import qualified Data.IntMap.Strict as IM
import Data.List (zip4)
import Data.Primitive.ByteArray (writeByteArray)

import Geometry.Space
import Geometry.Space.Quaternion
import Geometry.Structure
--import Geometry.Space.Transform
import Geometry.Math

import Drawable.World
--import SmallGL.Helpers

--import Data.IORef
import GHCJS.WebGL

--import GHCJS.Types
--import GHCJS.Foreign
--import GHCJS.Marshal
--import Unsafe.Coerce

-- | Basic entity in the program; Defines the logic of the interaction and visualization
data CityObject =
    -- | Building object.
    --   Polygon of the building represents the roof shape;
    --   floor is assumed to be at zero height;
    --   walls are strictly vertical.
    --   All together this gives full info on the building shape - extruded polygon.
    --   Points of the polygon assumed to be centered around zero coords.
    Building
    { behavior   :: !ObjectBehavior
    , objPolygon :: !(Polygon 3 GLfloat)
    } |
    -- | Road object.
    --   RoadPoints form a line of the road
    Road
    { behavior   :: !ObjectBehavior
    , roadWidth  :: !GLfloat
    , roadPoints :: ![Point 2 GLfloat]
    } |
    -- | Rectangular primitive hut
    BoxHut
    { behavior   :: !ObjectBehavior
    , hutSize    :: !(Vector3 GLfloat)
    }

-- | Buffers for the object
data MeshData = MeshData
    !GLsizei -- size of the array in elements
    !ArrayBuffer -- host array
    !Buffer -- device array

-- | Object mesh - vertices and indices
data CityObjectMesh = CityObjectMesh
    { vertexData  :: !MeshData
    , surfIndices :: !MeshData
    , wireIndices :: !MeshData
    }

drawSurface :: Ctx -> CityObjectMesh -> IO ()
drawSurface gl CityObjectMesh
    { vertexData  = MeshData _ _ buf
    , surfIndices = MeshData n _ ibuf
    } = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl 0 3 gl_FLOAT False 20 0
    vertexAttribPointer gl 1 3 gl_BYTE True 20 12
    vertexAttribPointer gl 2 2 gl_UNSIGNED_SHORT True 20 16
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES n gl_UNSIGNED_SHORT 0
--    (typedArrayView idata :: IO (TypedArray GLushort)) >>= printRef
--    (typedArrayView mdata :: IO (TypedArray GLubyte)) >>= printRef
--    (typedArrayView mdata :: IO (TypedArray GLfloat)) >>= printRef

--foreign import javascript safe "console.log($1)"
--    printRef :: JSRef a -> IO ()

drawWires :: Ctx -> CityObjectMesh -> IO ()
drawWires gl CityObjectMesh
    { vertexData  = MeshData _ _ buf
    , wireIndices = MeshData n _ ibuf
    } = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl 0 3 gl_FLOAT False 20 0
    vertexAttribPointer gl 1 3 gl_BYTE True 20 12
    vertexAttribPointer gl 2 2 gl_UNSIGNED_SHORT True 20 16
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_LINES n gl_UNSIGNED_SHORT 0


-- | pack points, normals, and tex coords tightly in one buffer
packVertices :: [(Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort)]
             -> ArrayBuffer -> IO ()
packVertices xs p = M.forM_ (zip xs [0..])
        $ \((Vector3 x y z, Vector3 nx ny nz, Vector2 tx ty), i) -> do
    writeByteArray p (5*i) x
    writeByteArray p (5*i+1) y
    writeByteArray p (5*i+2) z
    writeByteArray p (20*i+12) nx
    writeByteArray p (20*i+13) ny
    writeByteArray p (20*i+14) nz
    writeByteArray p (20*i+15) (0 :: GLbyte)
    writeByteArray p (10*i+8) tx
    writeByteArray p (10*i+9) ty
--    writeByteArray p (8*i) x
--    writeByteArray p (8*i+1) y
--    writeByteArray p (8*i+2) z
--    writeByteArray p (8*i+3) (fromIntegral nx / 128 :: GLfloat)
--    writeByteArray p (8*i+4) (fromIntegral ny / 128 :: GLfloat)
--    writeByteArray p (8*i+5) (fromIntegral nz / 128 :: GLfloat)
--    writeByteArray p (8*i+6) (fromIntegral tx / 65535 :: GLfloat)
--    writeByteArray p (8*i+7) (fromIntegral ty / 65535 :: GLfloat)


-- | create an array and a buffer, and fill them
createPackedBuf :: Ctx
                -> [(Vector3 GLfloat,Vector3 GLbyte,Vector2 GLushort)]
                -> IO MeshData
createPackedBuf gl xs = do
    let size = length xs
    barr <- mallocArrayBuffer (size * 20)
    packVertices xs barr
    buf <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER barr gl_STATIC_DRAW
    return $ MeshData (fromIntegral size) barr buf

-- | create buffer with draw indices
createIndexBuf :: Ctx
               -> [GLushort]
               -> IO MeshData
createIndexBuf gl indices = do
    ibuf <- createBuffer gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    iarr <- newArrayBuffer indices
    bufferData gl gl_ELEMENT_ARRAY_BUFFER iarr gl_STATIC_DRAW
    return $ MeshData (fromIntegral $ length indices) iarr ibuf

--foreign import javascript safe "console.log($1)"
--    printRef :: JSRef a -> IO ()

-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq)


-- | I draw the roads on this height
roadLevel :: GLfloat
roadLevel = 0.01

-- | Precision for floating-point comparisons (e.g. check if lines are parallel)
precision :: GLfloat
precision = 0.001

-- | Supplementary function for city objects - creates a mesh for building.
--   If one adds another type of a city object, they need to add a function for it here
createObjectMesh :: World -> CityObject -> IO CityObjectMesh
createObjectMesh World{glctx = gl} (Building _ (SimpleConvexPolygon pts)) = do
    mbuf <- createPackedBuf gl (wpts ++ rpts)
    sibuf <- createIndexBuf gl (wis ++ triangulate' [nw..nw+nr-1])
    wibuf <- createIndexBuf gl (wiw ++ [nw+nr-1,nw] ++ ([nw..nw+nr-2] >>= \i -> [i,i+1]))
    return $ CityObjectMesh mbuf sibuf wibuf
    where n = fromIntegral $ length pts
          nw = fromIntegral $ length wpts
          nr = fromIntegral $ length rpts
          center = mean pts
          wpts = walls pts
          walls (p1:p2:xs) = buildWall p1 p2 center ++ walls (p2:xs)
          walls [pl] = buildWall pl (head pts) center
          walls [] = []
          wis = [0..n-1] >>= \i -> [4*i,4*i+1,4*i+2,4*i,4*i+2,4*i+3]
          wiw = [0..n-1] >>= \i -> [4*i,4*i+1,4*i+3,4*i+2,4*i,4*i+3]
          rpts = zip (ptl:pts) pts >>= \(p1,p2) -> let no = norm (p2 .- p1)
                                                  in [(p1,no, Vector2 0 0)]
          ptl = last pts
          norm :: Vector3 GLfloat -> Vector3 GLbyte
          norm v = fmap (round . max (-128) . min 127)
                . (*..127) . unit $ v `cross` (Vector3 0 1 0) `cross` v
createObjectMesh World{glctx = gl} (Road _ width pts) = do
    mbuf <- createPackedBuf gl $ zip3 ps (repeat . fmap round $ up *.. 127) (repeat $ Vector2 0 0)
    sibuf <- createIndexBuf gl $ zip4 [0,4..(n-1)*4] [1,5..(n-1)*4] [2,6..(n-1)*4] [3,7..(n-1)*4]
        >>= \(i1,i2,i3,i4) -> [i1,i2,i4,i1,i4,i3]
    wibuf <- createIndexBuf gl $ zip4 [0,4..(n-1)*4] [1,5..(n-1)*4] [2,6..(n-1)*4] [3,7..(n-1)*4]
        >>= \(i1,i2,i3,i4) -> [i1,i3,i2,i4]
    return $ CityObjectMesh mbuf sibuf wibuf
    where xpts@(x0:x1:_) = map (\(Vector2 x z) -> Vector3 x roadLevel z) pts
          n = fromIntegral $ length pts
          ps | left <- g x0 x1
                  , p1 <- x0 .- left
                  , p2 <- x0 .+ left = seg p1 p2 xpts
          seg p1 p2 (a:b:c:xs) | left <- f a b c
                               , p3 <- b .- left
                               , p4 <- b .+ left = p1:p2:p3:p4:seg p3 p4 (b:c:xs)
          seg p1 p2 (b:c:[]) | left <- g b c
                             , p3 <- c .- left
                             , p4 <- c .+ left = p1:p2:p3:p4:[]
          seg _ _ _ = []
          up = Vector3 0 1 0
          g a b | dir <- unit $ a .- b = (0.5*width ..*) up `cross` dir
          f a b c | dir1 <- unit $ c .- b
                  , dir0 <- unit $ b .- a = flip runApprox precision $
            areParallel dir1 dir0 >>= \parallel ->
            return . (0.5*width ..*) $ case parallel of
                True -> dir1 `cross` up
                False -> let q = sqrt $ getRotScale dir0 (neg dir1)
                    in rotScale q dir0 /.. (- up .*. imVec q)
createObjectMesh _ _ = undefined


----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

-- create list of points, normals, texture coords for a wall out of two points
buildWall :: Vector3 GLfloat -- first point
          -> Vector3 GLfloat -- second point
          -> Vector3 GLfloat -- arbitrary inside point
          -> [(Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort)]
buildWall p1@(Vector3 x1 y1 z1) p2@(Vector3 x2 y2 z2) c =
    [ (p1,norm,Vector2 m h1)
    , (p2,norm,Vector2 0 h2)
    , (Vector3 x2 0 z2,norm, Vector2 0 0)
    , (Vector3 x1 0 z1,norm, Vector2 m 0)]
    where (h1,h2) = if y1 > y2 then (m, round $ y2/y1 * fromIntegral m)
                               else (round $ y1/y2 * fromIntegral m, m)
          m = 65535
          nr' = Vector3 0 1 0 `cross` (p2.-p1)
          norm = fmap (round . max (-128) . min 127)
            $ (signum (nr' .*. (p1 .- c)) * 127 / normL2 nr') ..* nr'


triangulate' :: [a] -> [a]
triangulate' pts = f pts []
    where f [] [] = []
          f [_] [] = []
          f [_,_] [] = []
          f [] qs = f (reverse qs) []
          f [a] qs = f (reverse $ a:qs) []
          f [a,b] qs = f (reverse $ b:a:qs) []
          f (a:b:c:xs) qs = a:b:c: f (c:xs) (a:qs)
