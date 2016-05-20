{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, KindSignatures, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.PointSet
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.PointSet
    ( PointSet (..), boundSet
    , PointArray (), fillPointArray
    , pointArray, index, unflatten
    , pcaVectors, boundingRectangle, boundingRectangle2D
    , project1D, projectND
    , shrinkVectors, enlargeVectors
    ) where

--import Debug.Trace
import Prelude hiding (length)

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits
import Data.Typeable (Proxy (..))

import GHCJS.Types
import GHCJS.Marshal.Pure (PFromJSVal(..))
import GHCJS.Foreign.Callback (Callback) --, releaseCallback)
--import GHCJS.Useful
--import System.IO.Unsafe (unsafePerformIO)
--import Data.Coerce (coerce)

import Data.Geometry
import Data.Geometry.Transform
import Data.JSArray


-- | Array of points
newtype PointArray (n::Nat) x = PointArray JSVal
instance IsJSVal (PointArray n x)
instance PFromJSVal (PointArray n x) where
    pFromJSVal = PointArray
instance LikeJS (PointArray n x)
instance LikeJSArray (PointArray n x) where
    type JSArrayElem (PointArray n x) = Vector n x

-- | Create a PointArray
pointArray :: [Vector n x]
           -> PointArray n x
pointArray = fromJSArray . fromList


{-# INLINE index #-}
index :: Int -> PointArray n x -> Vector n x
index = flip (!)

{-# INLINE fillPointArray #-}
foreign import javascript unsafe "Array.apply(null, Array($1)).map(function(){return $2;})"
    fillPointArray :: Int -> Vector n x -> PointArray n x


-- | convert an array to the point set
{-# INLINE unflatten #-}
unflatten :: (KnownNat n) => JSArray x -> PointArray n x
unflatten val = arr
    where arr = js_unflatten (dim' arr Proxy) val

{-# INLINE dim' #-}
dim' :: (KnownNat n) => PointArray n x -> Proxy n -> Int
dim' _ = fromInteger . natVal


class PointSet s n x | s -> n, s -> x where
    -- | make a flat JSArrayout of the point set
    flatten :: s -> JSArray x
    -- | make a JSArray of coordinates out of the point set
    toPointArray :: s -> PointArray (n::Nat) x
    -- | convert an array to the point set
    fromPointArray :: PointArray (n::Nat) x -> s
    -- | mean of the set
    mean :: s -> Vector n x
    -- | variance of the set
    var :: s -> Matrix n x
    -- | map a function on points
    mapSet :: (Vector n x -> Vector n x) -> s -> s
    -- | map a JS callback on points
    mapCallbackSet :: (Callback (Vector n x -> Vector n x)) -> s -> s
    -- | fold a function on points
    foldSet :: LikeJS a => (a -> Vector n x -> a) -> a -> s -> a
    -- | fold a JS callback on points
    foldCallbackSet :: LikeJS a => (Callback (a -> Vector n x -> a)) -> a -> s -> a


instance PointSet (PointArray n x) n x where
    {-# INLINE flatten #-}
    flatten = js_flatten
    {-# INLINE toPointArray #-}
    toPointArray = id
    {-# INLINE fromPointArray #-}
    fromPointArray = id
    {-# INLINE mean #-}
    mean = js_mean
    {-# INLINE var #-}
    var = js_var
    {-# NOINLINE mapSet #-}
    mapSet = jsmapSame
    {-# NOINLINE mapCallbackSet #-}
    mapCallbackSet = js_mapPointArray
    {-# NOINLINE foldSet #-}
    foldSet = jsfoldl
    {-# NOINLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldPointArray f (asJSVal a)

instance Transformable (PointArray 3 x) 3 x where
    transform sarr = jsmapSame (transform . flip wrap sarr) arr
        where arr = unwrap sarr

boundSet :: ( PointSet s n x
            , KnownNat n)
         => s -> (Vector n x, Vector n x)
boundSet s = (l, u)
    where (l,u) = js_boundSet (unsafeCoerce s) (dim l)

foreign import javascript unsafe "var r = gm$boundNestedArray($1['coordinates'] ? $1['coordinates'] : $1);\
                          \if(!r){ $r1 = Array.apply(null, Array($2)).map(Number.prototype.valueOf,Infinity);\
                          \        $r2 = Array.apply(null, Array($2)).map(Number.prototype.valueOf,-Infinity);}\
                          \else { $r1 = r[0]; $r2 = r[1]; }"
    js_boundSet :: JSVal -> Int -> (Vector n x, Vector n x)

{-# INLINE js_flatten #-}
foreign import javascript unsafe "[].concat.apply([], $1)"
    js_flatten :: PointArray n x -> JSArray x


{-# INLINE js_unflatten #-}
foreign import javascript unsafe "$r = []; for (var i=0; i < $2.length; i+=$1){$r.push($2.slice(i,i+chunkSize));}"
    js_unflatten :: Int -> JSArray x -> PointArray n x

{-# INLINE js_mean #-}
foreign import javascript unsafe "gm$mean($1)"
    js_mean :: PointArray n x -> Vector n x

{-# INLINE js_var #-}
foreign import javascript unsafe "gm$cov($1)"
    js_var :: PointArray n x -> Matrix n x

enlargeVectors :: (KnownNat n, KnownNat m) => PointArray n x -> PointArray m x
enlargeVectors v = r
    where r = enlargeVectors' n m v
          n = dim' v Proxy
          m = dim' r Proxy

shrinkVectors :: (KnownNat n, KnownNat m) => PointArray n x -> PointArray m x
shrinkVectors v = r
    where r = shrinkVectors' m v
          m = dim' r Proxy

{-# INLINE enlargeVectors' #-}
foreign import javascript unsafe "var z = Array.apply(null, Array($2-$1)).map(Number.prototype.valueOf,0); $r = $3.map(function(e){return e.concat(z);});"
    enlargeVectors' :: Int -> Int -> PointArray n x -> PointArray m x

{-# INLINE shrinkVectors' #-}
foreign import javascript unsafe "$2.map(function(e){return e.slice(0,$1);})"
    shrinkVectors' :: Int -> PointArray n x -> PointArray m x


-- | Calculate principal components in order of reducing eigenvalues
pcaVectors :: (KnownNat n, PointSet s n x) => s -> [Vector n x]
pcaVectors s = unsafeCoerce $ js_eigs m (dim m)
    where m = var s

{-# INLINE js_eigs #-}
foreign import javascript unsafe "h$toHsListJSVal(gm$principalEigenvectors($1,$2))"
    js_eigs :: Matrix n x -> Int -> Any

-- | Get 1D javaScript array by projecting set on vector
project1D :: (KnownNat n, PointSet s n x, Fractional x, JSNum x) => Vector n x -> s -> JSVal
project1D v = js_project1D (v / (v .*. v) ) . toPointArray

{-# INLINE js_project1D #-}
foreign import javascript unsafe "$2.map(function (v){ return dotJSVec($1,v); })"
    js_project1D :: Vector n x -> PointArray n x -> JSVal


-- | Get ND javaScript array by projecting set on vector
projectND :: (KnownNat n, KnownNat m, PointSet s m x, Fractional x, JSNum x)
          => [Vector m x] -> s -> PointArray n x
projectND vs s = rez
    where rez = js_projectND (fromList $ map (\v -> v / (v .*. v)) vs ) (dim' rez Proxy) (toPointArray s)

{-# INLINE js_projectND #-}
foreign import javascript unsafe "var vs = $1.slice(0,$2); $r = $3.map(function (v){ return vs.map( function(e){ return dotJSVec(v,e); }); });"
    js_projectND :: JSArray (Vector m x) -> Int -> PointArray m x -> PointArray n x

-- | Get ND javaScript array by projecting set on vector
projectND' :: (KnownNat n, KnownNat m, PointSet s m x, Fractional x, JSNum x)
          => [Vector m x] -> s -> PointArray n x
projectND' vs s = rez
    where rez = js_projectND (fromList vs) (dim' rez Proxy) (toPointArray s)


-- | Find a center and axes of bounding rectangle (flat) for a point set
boundingRectangle :: (KnownNat n, PointSet s n x, Fractional x, JSNum x)
             => s -> (Vector n x, Vector n x, Vector n x)
boundingRectangle s = (unproj center, unproj x, unproj y)
    where set = toPointArray s
          v@(px:py:_) = pcaVectors set
          projset = projectND' (map unit v) set
          (_,_,center, x, y) = js_minRectAngle projset
          unproj p2d = case unpackV2 p2d of (i,j) -> broadcastVector i * px + broadcastVector j * py


-- | Find a center and axes of bounding rectangle (flat) for a point set
boundingRectangle2D :: (PointSet s 2 x, Fractional x, JSNum x)
                    => s -> (Vector 2 x, Vector 2 x, Vector 2 x)
boundingRectangle2D s = (center, x, y)
    where (_,_,center, x, y) = js_minRectAngle $ toPointArray s

{-# INLINE js_minRectAngle #-}
foreign import javascript unsafe "var rez = gm$minRectAngle(gm$GrahamScan($1)); $r1 = rez[0]; $r2 = rez[1]; $r3 = rez[2]; $r4 = rez[3]; $r5 = rez[4];"
    js_minRectAngle :: PointArray 2 x -> (Double,Double, Vector2 x, Vector2 x, Vector2 x)

{-# INLINE js_mapPointArray #-}
foreign import javascript unsafe "$2.map($1)"
    js_mapPointArray :: Callback (Vector n x -> Vector n x) -> PointArray n x -> PointArray n x

{-# INLINE js_foldPointArray #-}
foreign import javascript unsafe "$3.reduce($1,$2)"
    js_foldPointArray :: Callback (a -> Vector n x -> a) -> JSVal -> PointArray n x -> JSVal



