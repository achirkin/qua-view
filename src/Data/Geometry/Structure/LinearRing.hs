{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.LinearRing
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.LinearRing
    ( LinearRing ()
    , linearRing, lrlength
    , toList -- ^ Get list of points from LinearRing (without repeatative last point)
    , toList'
    , convexPolygonHull
    , convexPolygonHull2D, resizeConvexHull2D
    ) where

import Prelude hiding (length)


import GHCJS.Foreign.Callback (Callback) --, releaseCallback)
--import GHCJS.Useful
--import System.IO.Unsafe (unsafePerformIO)
--import Data.Coerce (coerce)

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits

import GHCJS.Types
import GHCJS.Marshal.Pure (PFromJSVal(..))

import Data.JSArray
import Data.Geometry
import Data.Geometry.Transform
import Data.Geometry.Structure.PointSet (PointSet, PointArray)
import qualified Data.Geometry.Structure.PointSet as PS



----------------------------------------------------------------------------------------------------
-- Base Types
----------------------------------------------------------------------------------------------------

-- | GeoJSON LinearRing
newtype LinearRing (n::Nat) x = LinearRing JSVal
instance IsJSVal (LinearRing n x)
instance PFromJSVal (LinearRing n x) where
    pFromJSVal = LinearRing
instance LikeJS (LinearRing n x)
instance LikeJSArray (LinearRing n x) where
    type JSArrayElem (LinearRing n x) = Vector n x
    {-# INLINE toJSArray #-}
    toJSArray = js_LRRtoPoints
    {-# INLINE fromJSArray #-}
    fromJSArray = js_PointsToLR

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
    {-# INLINE mapSet #-}
    mapSet = jsmapSame
    {-# INLINE mapCallbackSet #-}
    mapCallbackSet = js_mapLinearRing
    {-# INLINE foldSet #-}
    foldSet = jsfoldl
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldLinearRing f (asJSVal a)

instance Transformable (LinearRing 3 x) 3 x where
    transform sarr = jsmapSame (transform . flip wrap sarr) arr
        where arr = unwrap sarr

-- | Create a LinearRing
linearRing :: Vector n x -- ^ First (and last) point of the LinearRing
           -> Vector n x -- ^ Second point
           -> Vector n x -- ^ Third point
           -> [Vector n x] -- ^ All remaining points (without duplicate of the first one)
           -> LinearRing n x
linearRing a b c xs = js_createLinearRing  . unsafeCoerce . seqList $ a:b:c:xs


-- | Get list of points from LinearRing (with repeatative last point)
toList' :: LinearRing n x -> [Vector n x]
toList' = unsafeCoerce . js_LRtoList'


----------------------------------------------------------------------------------------------------
-- Custom Funcions
----------------------------------------------------------------------------------------------------


-- | create a convex polygon that bounds given point set (in projection of most variance plane)
convexPolygonHull :: (KnownNat n, PointSet s n x, Fractional x, JSNum x)
             => s -> LinearRing n x
convexPolygonHull s = js_RingFromIds set pointIds
    where set = PS.toPointArray s
          v = PS.pcaVectors set
          projset = PS.projectND v set
          pointIds = js_GrahamScanIds projset


-- | create a convex polygon that bounds given point set on 2D plane (first two coordinates)
convexPolygonHull2D :: (KnownNat n, PointSet s n x, Fractional x, JSNum x)
                    => s -> LinearRing 2 x
convexPolygonHull2D = js_PAtoLR . js_GrahamScan . PS.shrinkVectors . PS.toPointArray

-- | Dilate or shrink LinearRing. Worning: works correctly only in case of a convex ring
resizeConvexHull2D :: JSNum x => x -> LinearRing 2 x -> LinearRing 2 x
resizeConvexHull2D = js_resizeConvexHull2D . fromNum

foreign import javascript unsafe "$r = gm$resizeConvexHull($1,$2.slice(0,$2.length-1)); $r.push($r[0]);"
    js_resizeConvexHull2D :: JSVal -> LinearRing 2 x -> LinearRing 2 x












----------------------------------------------------------------------------------------------------
-- JS Callbacks
----------------------------------------------------------------------------------------------------



{-# INLINE lrlength #-}
foreign import javascript unsafe "$1.length - 1"
    lrlength :: LinearRing n x -> Int

{-# INLINE js_RingFromIds #-}
foreign import javascript unsafe "$r = new Array($2.length+1); $2.forEach(function(e,i){$r[i] = $1[e];}); $r.push($r[0]);"
    js_RingFromIds :: PointArray n x -> JSVal -> LinearRing n x

{-# INLINE js_GrahamScanIds #-}
foreign import javascript unsafe "gm$GrahamScanIds($1)"
    js_GrahamScanIds :: PointArray 2 x -> JSVal

{-# INLINE js_GrahamScan #-}
foreign import javascript unsafe "gm$GrahamScan($1)"
    js_GrahamScan :: PointArray 2 x -> PointArray 2 x

{-# INLINE js_createLinearRing #-}
foreign import javascript unsafe "$r = h$listToArray($1); $r.push($r[0]);"
    js_createLinearRing :: Any -> LinearRing n x

{-# INLINE js_LRtoList' #-}
foreign import javascript unsafe "h$toHsListJSVal($1)"
    js_LRtoList':: LinearRing n x -> Any

{-# INLINE js_LRtoPA #-}
foreign import javascript unsafe "$1.slice(0,$1.length-1)"
    js_LRtoPA :: LinearRing n x -> PointArray n x

{-# INLINE js_PAtoLR #-}
foreign import javascript unsafe "$r = $1.slice(); $r.push($1[0]);"
    js_PAtoLR :: PointArray n x -> LinearRing n x

{-# INLINE js_LRRtoPoints #-}
foreign import javascript unsafe "$1.slice(0,$1.length-1)"
    js_LRRtoPoints :: LinearRing n x -> JSArray (Vector n x)

{-# INLINE js_PointsToLR #-}
foreign import javascript unsafe "$r = $1.slice(); $r.push($1[0]);"
    js_PointsToLR :: JSArray (Vector n x) -> LinearRing n x

seqList :: [a] -> [a]
seqList xs = foldr seq () xs `seq` xs


{-# INLINE js_mapLinearRing #-}
foreign import javascript unsafe "$2.map($1)"
    js_mapLinearRing :: Callback (Vector n x -> Vector n x) -> LinearRing n x -> LinearRing n x

{-# INLINE js_foldLinearRing #-}
foreign import javascript unsafe "$3.reduce($1,$2)"
    js_foldLinearRing :: Callback (a -> Vector n x -> a) -> JSVal -> LinearRing n x -> JSVal

