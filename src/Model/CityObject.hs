{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
--import qualified Control.Arrow as A
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

import GHCJS.Types
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


clearMeshData :: Ctx -> MeshData -> IO ()
clearMeshData gl (MeshData _ _ buf) = deleteBuffer gl buf

deleteCityObjectMesh :: Ctx -> CityObjectMesh -> IO ()
deleteCityObjectMesh gl (CityObjectMesh a b c) = clearMeshData gl a
                                              >> clearMeshData gl b
                                              >> clearMeshData gl c

drawSurface :: Ctx -> (GLuint,Maybe (GLuint,GLuint) ) -> CityObjectMesh -> IO ()
drawSurface gl (ploc,olocs) CityObjectMesh
    { vertexData  = MeshData _ _ buf
    , surfIndices = MeshData n _ ibuf
    } = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    case olocs of
        Just (nloc,tloc) -> do
            vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
            vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
        Nothing -> return ()
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES n gl_UNSIGNED_SHORT 0
--    (typedArrayView idata :: IO (TypedArray GLushort)) >>= printRef
--    (typedArrayView mdata :: IO (TypedArray GLubyte)) >>= printRef
--    (typedArrayView mdata :: IO (TypedArray GLfloat)) >>= printRef

--foreign import javascript safe "console.log($1)"
--    printRef :: JSRef a -> IO ()

drawWires :: Ctx -> GLuint -> CityObjectMesh -> IO ()
drawWires gl ploc CityObjectMesh
    { vertexData  = MeshData _ _ buf
    , wireIndices = MeshData n _ ibuf
    } = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
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

-- | pack points, normals, and tex coords tightly in one buffer
packVertexCoords :: [Vector3 GLfloat]
             -> ArrayBuffer -> IO ()
packVertexCoords xs p = M.forM_ (zip xs [0..])
        $ \(Vector3 x y z, i) -> do
    writeByteArray p (5*i) x
    writeByteArray p (5*i+1) y
    writeByteArray p (5*i+2) z



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
createObjectMesh World{glctx = gl} (BoxHut _ (Vector3 x y z)) = do
    mbuf <- createPackedBuf gl $ w1 ++ w2 ++ w3 ++ w4 ++ ro
    sibuf <- createIndexBuf gl $ [0,4,8,12,16] >>= flip map [0,1,2,0,2,3] . (+)
    wibuf <- createIndexBuf gl $ [0,4,8,12,16] >>= flip map [0,1,1,2,2,3,3,0] . (+)
    return $ CityObjectMesh mbuf sibuf wibuf
    where uuu@(Vector3 x2 _ z2) = Vector3  (x/2) y (z/2)
          duu = Vector3 (-x2) ( y) ( z2)
          udu = Vector3 ( x2) ( 0) ( z2)
          ddu = Vector3 (-x2) ( 0) ( z2)
          uud = Vector3 ( x2) ( y) (-z2)
          dud = Vector3 (-x2) ( y) (-z2)
          udd = Vector3 ( x2) ( 0) (-z2)
          ddd = Vector3 (-x2) ( 0) (-z2)
          nnz = Vector3 ( 0) ( 0) (-127)
          npz = Vector3 ( 0) ( 0) ( 127)
          nnx = Vector3 (-127) ( 0) ( 0)
          npx = Vector3 ( 127) ( 0) ( 0)
          npy = Vector3 ( 0) ( 127) ( 0)
          txmax = x + z
          tymax = 2*y + z
          tx1 = round $ 65535 / txmax * x
          ty1 = round $ 65535 / tymax * y
          ty2 = round $ 65535 / tymax * 2 * y
          m = 65535
          w1 = [ (ddu, npz, Vector2 0   0  )
               , (udu, npz, Vector2 tx1 0  )
               , (uuu, npz, Vector2 tx1 ty1)
               , (duu, npz, Vector2 0   ty1)]
          w2 = [ (udu, npx, Vector2 tx1 0  )
               , (udd, npx, Vector2 m   0  )
               , (uud, npx, Vector2 m   ty1)
               , (uuu, npx, Vector2 tx1 ty1)]
          w3 = [ (udd, nnz, Vector2 0   ty1)
               , (ddd, nnz, Vector2 tx1 ty1)
               , (dud, nnz, Vector2 tx1 ty2)
               , (uud, nnz, Vector2 0   ty2)]
          w4 = [ (ddd, nnx, Vector2 tx1 ty1)
               , (ddu, nnx, Vector2 m   ty1)
               , (duu, nnx, Vector2 m   ty2)
               , (dud, nnx, Vector2 tx1 ty2)]
          ro = [ (duu, npy, Vector2 0   ty2)
               , (uuu, npy, Vector2 tx1 ty2)
               , (uud, npy, Vector2 tx1 m  )
               , (dud, npy, Vector2 0   m  )]
createObjectMesh _ _ = undefined



evaluationGrid :: CityObject -- ^ hut
               -> GLfloat  -- ^ desired cell size
               -> [Vector3 GLfloat]
evaluationGrid (BoxHut _ (Vector3 sx sy sz)) cellSize =
    w12 ++ w34 ++ ro
    where nx = max 1 . round $ sx / cellSize :: Int
          ny = max 1 . round $ sy / cellSize :: Int
          nz = max 1 . round $ sz / cellSize :: Int
          dx = sx / fromIntegral nx
          dy = sy / fromIntegral ny
          dz = sz / fromIntegral nz
          xs = take nx $ iterate (+dx) (0.5*(dx-sx))
          ys = take ny $ iterate (+dy) (0.5*dy)
          zs = take nz $ iterate (+dz) (0.5*(dz-sz))
          w1 = \y -> map (\x -> Vector3 x y (sz/2)) xs
          w2 = \y -> map (\z -> Vector3 (sx/2) y (-z)) zs
          w3 = \y -> map (\x -> Vector3 (-x) y (-sz/2)) xs
          w4 = \y -> map (\z -> Vector3 (-sx/2) y z) zs
          w12 = ys >>= \y -> w1 y ++ w2 y
          w34 = ys >>= \y -> w3 y ++ w4 y
          ro = zs >>= \z -> map (\x -> Vector3 x sy (-z)) xs
evaluationGrid _ _ = []

-- | Create a texture array from the color list.
--   Put remaining points as the second output
gridToTextureArray :: CityObject
                   -> GLfloat
                   -> [Vector4 GLubyte]
                   -> ([Vector4 GLubyte], IO (Maybe (TypedArray GLubyte, Vector2 GLsizei)))
gridToTextureArray (BoxHut _ (Vector3 sx sy sz)) cellSize colors =
    f (newTypedArray $ ntx*nty*4) 0 colors
    where nx = max 1 . round $ sx / cellSize :: Int
          ny = max 1 . round $ sy / cellSize :: Int
          nz = max 1 . round $ sz / cellSize :: Int
          ntx = nx+nz
          nty = 2*ny+nz
          rec = ntx*2*ny
          size = fmap fromIntegral $ Vector2 ntx nty
          writeVec i (Vector4 r g b a) arr = do
            setIdx arr (i) r
            setIdx arr (i+1) g
            setIdx arr (i+2) b
            setIdx arr (i+3) a
            return arr
          f :: IO (TypedArray GLubyte) -> Int -> [Vector4 GLubyte] -> ([Vector4 GLubyte], IO (Maybe (TypedArray GLubyte, Vector2 GLsizei)))
          f a i (x:xs) | i < rec                  = let a' = a >>= writeVec (4*i) x in a' `seq` f a' (i+1) xs -- first rectangular area
                       | (i - rec) `mod` ntx < nx && i < ntx*nty = let a' = a >>= writeVec (4*i) x in a' `seq` f a' (i+1) xs -- roof
                       | i < ntx*nty              = let a' = a >>= writeVec (4*i) (Vector4 0 0 0 0) in a' `seq` f a' (i+1) (x:xs) -- spare space
                       | otherwise                = (x:xs, a >>= (\arr -> return $ Just (arr, size))) -- finished, there are points left
          f a i [] | i >= ntx*nty = ([], a >>= (\arr -> return $ Just (arr, size))) -- finished, there is no points left
                   | (i - rec) `mod` ntx >= nx = let a' = a >>= writeVec (4*i) (Vector4 0 0 0 0) in a' `seq` f a' (i+1) [] -- finish spare space
                   | otherwise    = ([], return Nothing) -- there were not enough points!
gridToTextureArray _ _ colors =  (colors, return Nothing)


foreign import javascript safe "console.log($1)"
    printRe :: JSRef a -> IO ()


instance Boundable CityObject 2 GLfloat where
    minBBox Building{ objPolygon = poly} = boundingBox (Vector2 lx lz)
                                                       (Vector2 hx hz)
        where bound3 = minBBox poly
              Vector3 lx _ lz = lowBound bound3
              Vector3 hx _ hz = lowBound bound3
    minBBox Road{ roadPoints = poly, roadWidth = rw} = boundingBox lb hb
        where bound2 = boundSet poly
              lb = fmap (+(-r2)) $ lowBound bound2
              hb = fmap (+r2) $ highBound bound2
              r2 = rw / 2
    minBBox BoxHut { hutSize = hs } = boundingBox (Vector2 (-x) (-z))
                                                  (Vector2 x z)
        where Vector3 x _ z = (hs /.. 2)

instance Boundable CityObject 3 GLfloat where
    minBBox Building{ objPolygon = poly} = boundPair bb zbound
        where bb = minBBox poly
              Vector3 lx _ lz = lowBound bb
              Vector3 hx _ hz = highBound bb
              zbound = boundingBox (Vector3 lx 0 lz) (Vector3 hx 0 hz)
    minBBox Road{ roadPoints = poly, roadWidth = rw} = boundingBox (Vector3 lx 0 lz)
                                                                   (Vector3 hx roadLevel hz)
        where bound2 = boundSet poly
              Vector2 lx lz = fmap (+(-r2)) $ lowBound bound2
              Vector2 hx hz = fmap (+r2) $ highBound bound2
              r2 = rw / 2
    minBBox BoxHut { hutSize = Vector3 x y z } = boundingBox (Vector3 (-0.5*x) 0 (-0.5*z))
                                                             (Vector3 (0.5*x) y (0.5*z))


----------------------------------------------------------------------------------------------------
-- | Ground as a bounding rectangle
----------------------------------------------------------------------------------------------------

type Ground = BoundingBox 2 GLfloat

createGroundMesh :: World -> Ground -> IO CityObjectMesh
createGroundMesh World{glctx = gl} b = do
    mbuf <- createPackedBuf gl $ [ (Vector3 lx 0 hz, npy, Vector2 0 0)
                                 , (Vector3 hx 0 hz, npy, Vector2 m 0)
                                 , (Vector3 hx 0 lz, npy, Vector2 m m)
                                 , (Vector3 lx 0 lz, npy, Vector2 0 m)]
    sibuf <- createIndexBuf gl $ [0,1,3,2]
    wibuf <- createIndexBuf gl $ [0,1,1,2,2,3,3,0]
    return $ CityObjectMesh mbuf sibuf wibuf
    where Vector2 lx lz = lowBound b
          Vector2 hx hz = highBound b
          npy = Vector3 0 127 0
          m = 65535

updateGroundMesh :: World -> Ground -> CityObjectMesh -> IO ()
updateGroundMesh World{glctx = gl} b CityObjectMesh{vertexData = MeshData _ arr buf } = do
    packVertexCoords [ Vector3 lx 0 hz
                     , Vector3 hx 0 hz
                     , Vector3 hx 0 lz
                     , Vector3 lx 0 lz] arr
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
    where Vector2 lx lz = lowBound b
          Vector2 hx hz = highBound b


drawGround :: Ctx -> (GLuint,Maybe (GLuint,GLuint) ) -> CityObjectMesh -> IO ()
drawGround gl (ploc,olocs) CityObjectMesh
    { vertexData  = MeshData _ _ buf
    , surfIndices = MeshData n _ ibuf
    } = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    case olocs of
        Just (nloc,tloc) -> do
            vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
            vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
        Nothing -> return ()
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLE_STRIP n gl_UNSIGNED_SHORT 0

groundEvalGrid :: Ground
               -> GLfloat  -- ^ desired cell size
               -> [Vector3 GLfloat] -- half size in 111 direction
groundEvalGrid b cellSize = zs >>= \z -> map (\x -> Vector3 x 0 z) xs
    where Vector2 sx sz = highBound b .- lowBound b
          Vector2 x0 _ = lowBound b
          Vector2 _ z1 = highBound b
          nx = max 1 . round $ sx / cellSize :: Int
          nz = max 1 . round $ sz / cellSize :: Int
          dx = sx / fromIntegral nx
          dz = - sz / fromIntegral nz
          xs = take nx $ iterate (+dx) (0.5*dx+x0)
          zs = take nz $ iterate (+dz) (0.5*dz+z1)

groundGridToTexArray :: Ground
                     -> GLfloat
                     -> [Vector4 GLubyte]
                     -> ([Vector4 GLubyte], IO (Maybe (TypedArray GLubyte, Vector2 GLsizei)))
groundGridToTexArray bb cellSize colors =
    f (newTypedArray $ nx*nz*4) 0 colors
    where Vector2 sx sz = highBound bb .- lowBound bb
          nx = max 1 . round $ sx / cellSize :: Int
          nz = max 1 . round $ sz / cellSize :: Int
          size = fmap fromIntegral $ Vector2 nx nz
          writeVec i (Vector4 r g b a) arr = do
            setIdx arr (i) r
            setIdx arr (i+1) g
            setIdx arr (i+2) b
            setIdx arr (i+3) a
            return arr
          f :: IO (TypedArray GLubyte) -> Int -> [Vector4 GLubyte] -> ([Vector4 GLubyte], IO (Maybe (TypedArray GLubyte, Vector2 GLsizei)))
          f a i (x:xs) | i < nx*nz = let a' = a >>= writeVec (4*i) x in a' `seq` f a' (i+1) xs
                       | otherwise = (x:xs, a >>= (\arr -> return $ Just (arr, size))) -- finished, there are points left
          f a i [] | i >= nx*nz = ([], a >>= (\arr -> return $ Just (arr, size))) -- finished, there is no points left
                   | otherwise  = ([], return Nothing) -- there were not enough points!


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
