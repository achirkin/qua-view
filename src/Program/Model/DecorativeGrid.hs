-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.DecorativeGrid
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.DecorativeGrid where


import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Control.Monad.ST
import Control.Monad (foldM, foldM_, liftM)

import GHCJS.WebGL
import Geometry.Space

import SmallGL.WritableVectors ()


-- | Collection of lines in one entity.
--   Values: Color, Size (width and length), Vertices
data DecorativeGrid = DGrid !(Vector4 GLfloat) !GLsizei !ByteArray
--data Grid = forall s. Grid !(Vector4 GLfloat) !GLsizei !(MutableByteArray (PrimState (ST s)))

createDGrid :: GLfloat -- size of the grid (width and length)
            -> Int -- number of cells
            -> Vector4 GLfloat -- ^ color of the mesh
            -> DecorativeGrid
createDGrid size cells color = runST $ do
    arr <- newByteArray s
    i1 <- foldM (\i x -> do
            writeByteArray arr i     $ Vector3 x 0 (-size/2)
            writeByteArray arr (i+1) $ Vector3 x 0 ( size/2)
            return (i+2)
            ) 0 grid
    foldM_ (\i z -> do
            writeByteArray arr i     $ Vector3 (-size/2) 0 z
            writeByteArray arr (i+1) $ Vector3 ( size/2) 0 z
            return (i+2)
            ) i1 grid
    liftM (DGrid color n) $ unsafeFreezeByteArray arr
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          s = fromIntegral n * sizeOf (undefined::Vector3 GLfloat)
          n = fromIntegral $ 4*(cells-1) :: GLsizei
