{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SmallGL.WritableVectors
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module SmallGL.WritableVectors
    ( Vertex20CNT
    , writePoint, writePoints, packPoints
    ) where

import GHC.ST (ST(..))
import GHC.Exts (State#)

import Data.Foldable (foldlM)

import Control.Monad (void)

import JsHs.TypedArray
import JsHs.TypedArray.ST
import Control.Monad.ST

import JsHs.WebGL.Types

import JsHs.Array as JS
import Data.Geometry
import Data.Geometry.Structure.PointSet


-- | Coordinate + normal + texture buffer pack
type Vertex20CNT = (Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort)

-- | write a point into ArrayBuffer
writePoint :: Int -> Vertex20CNT -> STArrayBuffer s -> ST s ()
writePoint offset ( unpackV3 -> (px,py,pz)
           , unpackV3 -> (nx,ny,nz)
           , unpackV2 -> (tx,ty)) buf = do
    unsafeWriteFloat 0 px dv
    unsafeWriteFloat 4 py dv
    unsafeWriteFloat 8 pz dv
    unsafeWriteInt8 12 nx dv
    unsafeWriteInt8 13 ny dv
    unsafeWriteInt8 14 nz dv
    unsafeWriteInt8 15 0 dv
    unsafeWriteWord16 16 tx dv
    unsafeWriteWord16 18 ty dv
    where dv = unsafeDataView' (offset*20) Nothing buf

-- | write a number of points into ArrayBuffer
writePoints :: Foldable f => Int -> f Vertex20CNT -> STArrayBuffer s -> ST s ()
writePoints start ps buf = void $ foldlM f start ps
    where floatView  = arrayView buf
          word16View = arrayView buf
          int8View   = arrayView buf
          f i ( unpackV3 -> (px,py,pz)
              , unpackV3 -> (nx,ny,nz)
              , unpackV2 -> (tx,ty) ) = do
            setIndex (5*i) px floatView
            setIndex (5*i + 1) py floatView
            setIndex (5*i + 2) pz floatView
            setIndex (20*i + 12) nx int8View
            setIndex (20*i + 13) ny int8View
            setIndex (20*i + 14) nz int8View
            setIndex (20*i + 15) 0 int8View
            setIndex (10*i + 8) tx word16View
            setIndex (10*i + 9) ty word16View
            return (i+1)

packPoints :: PointArray 3 GLfloat  -- ^ Points
           -> PointArray 3 GLbyte   -- ^ Normals
           -> PointArray 2 GLushort -- ^ Texture Coords
           -> ArrayBuffer
packPoints points normals texcoords = runST $ do
    buf <- newSTArrayBuffer (JS.length points * 20)
    setPoints 5  0  points    (arrayView buf)
    setPoints 20 12 normals   (arrayView buf)
    setPoints 10 8  texcoords (arrayView buf)
    unsafeFreeze buf


setPoints :: Int -> Int -> PointArray n x -> STTypedArray s x -> ST s ()
setPoints strip shift set arr = ST (setPoints' strip shift set arr)

foreign import javascript unsafe "$3.forEach(function(e,i){$4.set(e,$1*i + $2)})"
    setPoints' :: Int -> Int -> PointArray n x -> STTypedArray s x -> State# s -> (# State# s, () #)
