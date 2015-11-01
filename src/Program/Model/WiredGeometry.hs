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

import Control.Monad.ST
import JavaScript.TypedArray
import JavaScript.TypedArray.ST

import GHCJS.WebGL
import Data.Geometry

--import SmallGL.WritableVectors ()

-- | Collection of lines in one entity.
--   Values: Color, Size (number of points), Vertices
data WiredGeometry = WiredGeometry (Maybe Int) !(Vector4 GLfloat) !GLsizei !ArrayBuffer

createDecorativeGrid :: GLfloat -- size of the grid (width and length)
                     -> Int -- number of cells
                     -> Vector4 GLfloat -- ^ color of the mesh
                     -> WiredGeometry
createDecorativeGrid size cells color = WiredGeometry Nothing color n . arrayBuffer . fromList $
   (grid >>= \x -> [vector3 x h (-size/2), vector3 x h (size/2)]) ++
   (grid >>= \z -> [vector3 (-size/2) h z, vector3 ( size/2) h z])
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          n = fromIntegral $ 4*(cells-1) :: GLsizei
          h = 0

-- | Create a line set from multiple line loops
createLineSet :: Vector4 GLfloat
              -> [[Vector3 GLfloat]]
              -> WiredGeometry
createLineSet color xxs = WiredGeometry Nothing color n . arrayBuffer . fromList $ points
    where points = concatMap mkLineStrip xxs
          n = fromIntegral $ length points :: GLsizei

-- | Append new points to existing line set
appendLineSet :: [[Vector3 GLfloat]]
              -> WiredGeometry
              -> WiredGeometry
appendLineSet xxs (WiredGeometry _ color n0 buf0) = runST $ do
    arr <- newSTTypedArray (fromIntegral n)
    setArray 0 (arrayView buf0 :: TypedArray (Vector3 Float)) arr
    setList (fromIntegral n0) points arr
    WiredGeometry Nothing color n <$> unsafeFreeze (arrayBuffer arr)
    where points = concatMap mkLineStrip xxs
          n = n0 + fromIntegral (length points)


mkLineStrip :: [Vector3 GLfloat] -> [Vector3 GLfloat]
mkLineStrip (t:ts) = t:mkLineStrip' ts
    where mkLineStrip' (x:(xs@(_:_))) = x:x:mkLineStrip' xs
          mkLineStrip' [x] = [x]
          mkLineStrip' [] = []
mkLineStrip _      = []
