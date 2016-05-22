{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.WiredGeometry
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
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
    , createLineSet, appendLineSet, emptyLineSet
    ) where

import Unsafe.Coerce

import Control.Monad.ST
import JsHs.TypedArray
import JsHs.TypedArray.ST

import JsHs.WebGL
import Data.Geometry

import qualified Data.Geometry.Structure.LineString as LS
import qualified JsHs.Array as JS
--import SmallGL.WritableVectors ()

-- | Collection of lines in one entity.
--   Values: Color, Size (number of points), Vertices
data WiredGeometry = WiredGeometry (Maybe Int) !(Vector4 GLfloat) !GLsizei !ArrayBuffer

createDecorativeGrid :: GLfloat -- size of the grid (width and length)
                     -> Int -- number of cells
                     -> Vector4 GLfloat -- ^ color of the mesh
                     -> WiredGeometry
createDecorativeGrid size cells color = WiredGeometry Nothing color n . arrayBuffer . fromList $
   (grid >>= \x -> [vector3 x (-size/2) h, vector3 x (size/2) h]) ++
   (grid >>= \y -> [vector3 (-size/2) y h, vector3 ( size/2) y h])
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          n = fromIntegral $ 4*(cells-1) :: GLsizei
          h = 0


emptyLineSet :: Vector4 GLfloat
             -> (LS.MultiLineString 3 GLfloat, WiredGeometry)
emptyLineSet colors =
    ( LS.multiLineString []
    , WiredGeometry Nothing colors 0 (arrayBuffer (fromList [] :: TypedArray GLfloat))
    )

-- | Create a line set from multiple line loops
createLineSet :: Vector4 GLfloat
              -> LS.MultiLineString 3 GLfloat
              -> (LS.MultiLineString 3 GLfloat, WiredGeometry)
createLineSet color xxs =
    ( xxs
    , WiredGeometry Nothing color n . arrayBuffer . fromJSArrayToTypedArray $ points
    )
    where points = mkLineStrips xxs
          n = fromIntegral (JS.length points) `quot` 3 :: GLsizei

-- | Append new points to existing line set
appendLineSet :: LS.MultiLineString 3 GLfloat
              -> (LS.MultiLineString 3 GLfloat, WiredGeometry)
              -> (LS.MultiLineString 3 GLfloat, WiredGeometry)
appendLineSet xxs (xxso, WiredGeometry _ color n0 buf0) = (JS.fromList $ JS.toList xxso ++ JS.toList xxs, nwg)
    where points = mkLineStrips xxs
          n = n0 + fromIntegral (JS.length points) `quot` 3
          nwg = runST $ do
            let arrn = typedArray (fromIntegral n * 3) :: TypedArray GLfloat
            arr <- unsafeThaw arrn
            setArray 0 (arrayView buf0 :: TypedArray (Vector3 Float)) arr
            setArray (fromIntegral n0) (fromJSArrayToTypedArray points) arr
            WiredGeometry Nothing color n <$> unsafeFreeze (arrayBuffer arr)


foreign import javascript unsafe "[].concat.apply([],[].concat.apply([],\
                                 \  $1['coordinates'].map(function(r){\
                                 \        if(!r || r.length <= 1){return [];}\
                                 \        else {var s = new Array(r.length*2-2);s[0] = r[0];s[s.length-1] = r[r.length-1];\
                                 \              for(var i = 1; i < r.length-1; i++){s[i*2-1] = r[i]; s[i*2] = r[i];}\
                                 \              return s;}\
                                 \})))"
    mkLineStrips :: LS.MultiLineString n GLfloat -> JS.Array GLfloat

fromJSArrayToTypedArray :: (TypedArrayOperations a) => JS.Array a -> TypedArray a
fromJSArrayToTypedArray = fromArray . unsafeFromJSArrayCoerce

unsafeFromJSArrayCoerce :: JS.Array a -> TypedArray a
unsafeFromJSArrayCoerce = unsafeCoerce
