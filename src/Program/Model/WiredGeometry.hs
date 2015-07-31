-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.WiredGeometry
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.WiredGeometry
    ( WiredGeometry (..)
    , createDecorativeGrid
    , createLineSet, appendLineSet
    ) where

import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Control.Monad.ST
import Control.Monad (foldM, foldM_, liftM)

import GHCJS.WebGL
import Geometry.Space

import SmallGL.WritableVectors ()

-- | Collection of lines in one entity.
--   Values: Color, Size (number of points), Vertices
data WiredGeometry = WiredGeometry !(Vector4 GLfloat) !GLsizei !ByteArray

createDecorativeGrid :: GLfloat -- size of the grid (width and length)
                     -> Int -- number of cells
                     -> Vector4 GLfloat -- ^ color of the mesh
                     -> WiredGeometry
createDecorativeGrid size cells color = runST $ do
    arr <- newByteArray s
    i1 <- foldM (\i x -> do
            writeByteArray arr i     $ Vector3 x h (-size/2)
            writeByteArray arr (i+1) $ Vector3 x h ( size/2)
            return (i+2)
            ) 0 grid
    foldM_ (\i z -> do
            writeByteArray arr i     $ Vector3 (-size/2) h z
            writeByteArray arr (i+1) $ Vector3 ( size/2) h z
            return (i+2)
            ) i1 grid
    liftM (WiredGeometry color n) $ unsafeFreezeByteArray arr
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          s = fromIntegral n * sizeOf (undefined::Vector3 GLfloat)
          n = fromIntegral $ 4*(cells-1) :: GLsizei
          h = 0

-- | Create a line set from multiple line loops
createLineSet :: Vector4 GLfloat
              -> [[Vector3 GLfloat]]
              -> WiredGeometry
createLineSet color xxs = runST $ do
    arr <- newByteArray s
    foldM_ (\i x -> writeByteArray arr i x >> return (i+1)) 0 points
    liftM (WiredGeometry color n) $ unsafeFreezeByteArray arr
    where points = concat $ map mkLineStrip xxs
          mkLineStrip (x:xs) = x:mkLineStrip' xs
          mkLineStrip _      = []
          mkLineStrip' (x:(xs@(_:_))) = x:x:mkLineStrip' xs
          mkLineStrip' [x] = [x]
          mkLineStrip' [] = []
          s = fromIntegral n * sizeOf (undefined::Vector3 GLfloat)
          n = fromIntegral $ length points :: GLsizei

-- | Append new points to existing line set
appendLineSet :: [[Vector3 GLfloat]]
              -> WiredGeometry
              -> WiredGeometry
appendLineSet xxs (WiredGeometry color n0 arr0) = runST $ do
    arr <- newByteArray s
    copyByteArray arr 0 arr0 0 s0
    foldM_ (\i x -> writeByteArray arr i x >> return (i+1)) (fromIntegral n0) points
    liftM (WiredGeometry color n) $ unsafeFreezeByteArray arr
    where points = concat $ map mkLineStrip xxs
          mkLineStrip (x:xs) = x:mkLineStrip' xs
          mkLineStrip _      = []
          mkLineStrip' (x:(xs@(_:_))) = x:x:mkLineStrip' xs
          mkLineStrip' [x] = [x]
          mkLineStrip' [] = []
          s = fromIntegral n * sizeOf (undefined::Vector3 GLfloat)
          n = n0 + (fromIntegral $ length points)
          s0 = fromIntegral n0 * sizeOf (undefined::Vector3 GLfloat)
