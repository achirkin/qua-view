{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.LinearRing
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.LinearRing
    ( LinearRing ()
    , linearRing, length, index, toList, toList'
    , convexPolygonHull
    ) where

import Prelude hiding (length)


import GHCJS.Foreign.Callback (Callback, releaseCallback)
import GHCJS.Useful
import System.IO.Unsafe (unsafePerformIO)
import Data.Coerce (coerce)

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits

import GHCJS.Types
import GHCJS.Marshal.Pure (PFromJSVal(..))

import Data.Geometry
import Data.Geometry.Structure.PointSet (PointSet, PointArray)
import qualified Data.Geometry.Structure.PointSet as PS

-- | GeoJSON LinearRing
newtype LinearRing (n::Nat) x = LinearRing JSVal
instance IsJSVal (LinearRing n x)
instance PFromJSVal (LinearRing n x) where
    pFromJSVal = LinearRing

instance PS.PointSet (LinearRing n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_LRtoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_LRtoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = js_PAtoLR
    {-# INLINE mean #-}
    mean = PS.mean . js_LRtoPA
    {-# INLINE var #-}
    var = PS.var . js_LRtoPA
    {-# NOINLINE mapSet #-}
    mapSet f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ return . coerce . f . coerce
        rez <- mapLinearRing' call arr
        releaseCallback call
        return rez
    {-# NOINLINE mapCallbackSet #-}
    mapCallbackSet = mapLinearRing''


-- | Create a LinearRing
linearRing :: Vector n x -- ^ First (and last) point of the LinearRing
           -> Vector n x -- ^ Second point
           -> Vector n x -- ^ Third point
           -> [Vector n x] -- ^ All remaining points (without duplicate of the first one)
           -> LinearRing n x
linearRing a b c xs = js_createLinearRing  . unsafeCoerce . seqList $ a:b:c:xs

-- | Get list of points from LinearRing (without repeatative last point)
toList :: LinearRing n x -> [Vector n x]
toList = unsafeCoerce . js_LRtoList

-- | Get list of points from LinearRing (with repeatative last point)
toList' :: LinearRing n x -> [Vector n x]
toList' = unsafeCoerce . js_LRtoList'


{-# INLINE length #-}
foreign import javascript unsafe "$1.length - 1"
    length :: LinearRing n x -> Int

{-# INLINE index #-}
foreign import javascript unsafe "$2[$1]"
    index :: Int -> LinearRing n x -> Vector n x


-- | create a convex polygon that bounds given point set (in projection of most variance plane)
convexPolygonHull :: (KnownNat n, PointSet s n x, Fractional x, JSNum x)
             => s -> LinearRing n x
convexPolygonHull s = js_RingFromIds set pointIds
    where set = PS.toPointArray s
          v = PS.pcaVectors set
          projset = PS.projectND v set
          pointIds = js_GrahamScanIds projset



{-# INLINE js_RingFromIds #-}
foreign import javascript unsafe "$r = new Array($2.length+1); $2.forEach(function(e,i){$r[i] = $1[e];}); $r[$2.length] = $r[0];"
    js_RingFromIds :: PointArray n x -> JSVal -> LinearRing n x

{-# INLINE js_GrahamScanIds #-}
foreign import javascript unsafe "gm$GrahamScanIds($1)"
    js_GrahamScanIds :: PointArray 2 x -> JSVal




{-# INLINE js_createLinearRing #-}
foreign import javascript unsafe "$r = h$listToArray($1); $r.push($r[0]);"
    js_createLinearRing :: Any -> LinearRing n x

{-# INLINE js_LRtoList #-}
foreign import javascript unsafe "h$toHsListJSVal($1.slice(0,$1.length-1))"
    js_LRtoList:: LinearRing n x -> Any

{-# INLINE js_LRtoList' #-}
foreign import javascript unsafe "h$toHsListJSVal($1)"
    js_LRtoList':: LinearRing n x -> Any

{-# INLINE js_LRtoPA #-}
foreign import javascript unsafe "$1.slice(0,$1.length-1)"
    js_LRtoPA :: LinearRing n x -> PointArray n x

{-# INLINE js_PAtoLR #-}
foreign import javascript unsafe "$r = Array.from($1); $r.push($1[0]);"
    js_PAtoLR :: PointArray n x -> LinearRing n x

seqList :: [a] -> [a]
seqList xs = foldr seq () xs `seq` xs

{-# INLINE mapLinearRing' #-}
foreign import javascript unsafe "$2.map(function(e){ return $1(e);})"
    mapLinearRing' :: (Callback (JSVal -> IO JSVal)) -> LinearRing n x -> IO (LinearRing n x)
{-# INLINE mapLinearRing'' #-}
foreign import javascript unsafe "$2.map(function(e){ return $1(e);})"
    mapLinearRing'' :: (Callback a) -> LinearRing n x -> LinearRing n x
