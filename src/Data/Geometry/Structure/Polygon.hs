{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, GHCForeignImportPrim #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.Polygon
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.Polygon
    ( Polygon ()
    , triangulate, triangulate', triangulatePolygon3D, triangulateMultiPolygon3D
    , MultiPolygon ()
    , toPolygon3D, toMultiPolygon3D
    ) where


import GHCJS.Foreign.Callback (Callback, releaseCallback)
import GHCJS.Useful
import System.IO.Unsafe (unsafePerformIO)
import Data.Coerce (coerce)

import Prelude hiding (length)
import Data.List (foldl')

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits

import GHCJS.Types
import GHCJS.Marshal.Pure (PFromJSVal(..))

import Data.JSArray
import Data.Geometry
import qualified Data.Geometry.Structure.PointSet as PS
import Data.Geometry.Structure.LinearRing (LinearRing)
--import qualified Data.Geometry.Structure.LinearRing as LRing

-- | GeoJSON Polygon
newtype Polygon (n::Nat) x = Polygon JSVal
instance IsJSVal (Polygon n x)
instance PFromJSVal (Polygon n x) where
    pFromJSVal = Polygon
instance LikeJS (Polygon n x)
instance LikeJSArray (Polygon n x) where
    type JSArrayElem (Polygon n x) = LinearRing n x
    {-# INLINE toJSArray #-}
    toJSArray = js_PolygonToRingArray
    {-# INLINE fromJSArray #-}
    fromJSArray = js_RingArrayToPolygon

instance PS.PointSet (Polygon n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_PtoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_PtoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = polygon .  (:[]) . PS.fromPointArray
    {-# INLINE mean #-}
    mean = PS.mean . js_PtoPA
    {-# INLINE var #-}
    var = PS.var . js_PtoPA
    {-# NOINLINE mapSet #-}
    mapSet f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ return . coerce . f . coerce
        rez <- mapPolygon' call arr
        releaseCallback call
        return rez
    {-# NOINLINE mapCallbackSet #-}
    mapCallbackSet = mapPolygon''


-- | Create a Polygon
polygon :: [LinearRing n x] -- ^ All remaining points (without duplicate of the first one)
           -> Polygon n x
polygon = js_createPolygon  . unsafeCoerce . seqList

{-# INLINE js_RingArrayToPolygon #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Polygon'; $r['coordinates'] = $1;"
    js_RingArrayToPolygon :: JSArray (LinearRing n x) -> Polygon n x

{-# INLINE js_PolygonToRingArray #-}
foreign import javascript unsafe "$1['coordinates']"
    js_PolygonToRingArray ::  Polygon n x -> JSArray (LinearRing n x)

-- | Calculate indices of triangulation and put them into Haskell list
triangulate :: (KnownNat n, Fractional x, JSNum x) => Polygon n x -> [Int]
triangulate p = unsafeCoerce $ js_indicesListPrim arr
    where (_,_,arr) = triangulate' p

-- | Calculate indices of triangulation and keep them in JS array
--   First return is array of points
--   Second return is normals to the polygon (remaining PCA vectors)
triangulate' :: (KnownNat n, Fractional x, JSNum x)
             => Polygon n x -> (PS.PointArray n x, [Vector n x], JSVal)
triangulate' poly = (set, vs, triangulate'' projset rinds)
    where set = PS.toPointArray poly
          rinds = js_ringIndices poly
          (v1:v2:vs) = PS.pcaVectors set
          projset = PS.projectND [v1,v2] set

triangulatePolygon3D :: (KnownNat n, Fractional x, JSNum x)
                        => Polygon n x -> (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)
triangulatePolygon3D poly = (points, PS.fillPointArray (jslength points) normal, indsx)
    where (points, normal, indsx) = f $ triangulate' poly
          f (p, [n], inds) = (coerce p           , coerce n      , inds)
          f (p, [] , inds) = (PS.enlargeVectors p, vector3 0 0 1 , inds)
          f (p, n:_, inds) = (PS.shrinkVectors  p, resizeVector n, inds)


triangulateMultiPolygon3D :: (KnownNat n, Fractional x, JSNum x)
                          => MultiPolygon n x -> (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)
triangulateMultiPolygon3D mpoly = triags
    where triags = foldl' concatTriags startTriags . map (f . triangulate') $ polygons mpoly
          f (p, [n], inds) = (coerce p           , coerce n      , inds)
          f (p, [] , inds) = (PS.enlargeVectors p, vector3 0 0 1 , inds)
          f (p, n:_, inds) = (PS.shrinkVectors  p, resizeVector n, inds)

{-# INLINE toMultiPolygon3D #-}
foreign import javascript unsafe "var nc = $2['coordinates'].map(function(p){\
                                                \   return p.map(function(r){return r.map(function(x){ return x.length === 2 ? x.concat([$1]) : x.slice(0,3);});});\
                                                \ });\
                                 \$r = {}; $r['type'] = 'MultiPolygon'; $r['coordinates'] = nc;"
    toMultiPolygon3D :: Float -> MultiPolygon n x -> MultiPolygon 3 x

{-# INLINE toPolygon3D #-}
foreign import javascript unsafe "var nc = $2['coordinates'].map(function(r){return r.map(function(x){ return x.length === 2 ? x.concat([$1]) : x.slice(0,3);});});\
                                 \$r = {}; $r['type'] = 'Polygon'; $r['coordinates'] = nc;"
    toPolygon3D :: Float -> Polygon n x -> Polygon 3 x


concatTriags :: (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)
             -> (PS.PointArray 3 x, Vector 3 x, JSVal)
             -> (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)
concatTriags (a,b,c) (d,e,f) = concatTriags' a b c d e f

{-# INLINE concatTriags' #-}
foreign import javascript unsafe "$r1 = $1.concat($4);\
                                 \$r2 = $2.concat(Array.apply(null, Array($4.length)).map(function(){return $5;}));\
                                 \$r3 = $3.concat($6.map(function(e){return e + $1.length;}));"
    concatTriags' :: PS.PointArray 3 x -> PS.PointArray 3 x -> JSVal
                  -> PS.PointArray 3 x -> Vector 3 x -> JSVal
                  -> (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)

{-# INLINE startTriags #-}
foreign import javascript unsafe "$r1 = []; $r2 = []; $r3 = [];"
    startTriags :: (PS.PointArray 3 x, PS.PointArray 3 x, JSVal)










-- | GeoJSON MultiPolygon
newtype MultiPolygon (n::Nat) x = MultiPolygon JSVal
instance IsJSVal (MultiPolygon n x)
instance PFromJSVal (MultiPolygon n x) where
    pFromJSVal = MultiPolygon
instance LikeJS (MultiPolygon n x)
instance LikeJSArray (MultiPolygon n x) where
    type JSArrayElem (MultiPolygon n x) = Polygon n x
    {-# INLINE toJSArray #-}
    toJSArray = js_MPToPArr
    {-# INLINE fromJSArray #-}
    fromJSArray = js_PArrToMP



instance PS.PointSet (MultiPolygon n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_MPtoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_MPtoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = multiPolygon .  (:[]) . PS.fromPointArray
    {-# INLINE mean #-}
    mean = PS.mean . js_MPtoPA
    {-# INLINE var #-}
    var = PS.var . js_MPtoPA
    {-# NOINLINE mapSet #-}
    mapSet f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ return . coerce . f . coerce
        rez <- mapMultiPolygon' call arr
        releaseCallback call
        return rez
    {-# NOINLINE mapCallbackSet #-}
    mapCallbackSet = mapMultiPolygon''


-- | Create a MultiPolygon
multiPolygon :: [Polygon n x] -- ^ All remaining points (without duplicate of the first one)
             -> MultiPolygon n x
multiPolygon = js_createMultiPolygon  . unsafeCoerce . seqList

-- | Get list of points from Polygon (without repeatative last point)
polygons :: MultiPolygon n x -> [Polygon n x]
polygons = unsafeCoerce . js_MPtoPList


{-# INLINE js_PArrToMP #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPolygon';\
                                 \$r['coordinates'] = $1.map(function(e){return e['coordinates'];});"
    js_PArrToMP :: JSArray (Polygon n x) -> MultiPolygon n x

{-# INLINE js_MPToPArr #-}
foreign import javascript unsafe "$1['coordinates'].map(function(e){\
                                    \ var r = {}; r['type'] = 'Polygon'; r['coordinates'] = e;\
                                    \ return r; })"
    js_MPToPArr ::  MultiPolygon n x -> JSArray (Polygon n x)



-- | takes a polygon with holes and ring indices
--   (it assumes that point array is a polygon)
triangulate'' :: PS.PointArray 2 x -> JSVal -> JSVal
triangulate'' set rinds = js_triangulate (PS.flatten set) rinds

{-# INLINE js_triangulate #-}
foreign import javascript unsafe "earcut($1,$2)"
    js_triangulate :: JSVal -> JSVal -> JSVal


{-# INLINE index #-}
foreign import javascript unsafe "$2['coordinates'][$1]"
    index :: Int -> Polygon n x -> LinearRing n x


{-# INLINE js_createPolygon #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Polygon'; $r['coordinates'] = h$listToArray($1);"
    js_createPolygon :: Any -> Polygon n x

{-# INLINE js_createMultiPolygon #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPolygon';\
                                 \$r['coordinates'] = h$listToArray($1).map(function(e){return e['coordinates'];});"
    js_createMultiPolygon :: Any -> MultiPolygon n x

{-# INLINE js_PtoLRList #-}
foreign import javascript unsafe "h$toHsListJSVal($1['coordinates'])"
    js_PtoLRList:: Polygon n x -> Any

{-# INLINE js_MPtoPList #-}
foreign import javascript unsafe "h$toHsListJSVal($1['coordinates'].map(function(e){\
                                    \ var r = {}; r['type'] = 'Polygon'; r['coordinates'] = e;\
                                    \ return r; }))"
    js_MPtoPList:: MultiPolygon n x -> Any

{-# INLINE js_MPtoPA #-}
foreign import javascript unsafe "$r = [].concat.apply([], [].concat.apply([], $1['coordinates'])\
                                  \.map(function(a){return a.slice(0,a.length-1);}));"
    js_MPtoPA :: MultiPolygon n x -> PS.PointArray n x

{-# INLINE js_PtoPA #-}
foreign import javascript unsafe "[].concat.apply([], $1['coordinates'].map(function(a){return a.slice(0,a.length-1);}))"
    js_PtoPA :: Polygon n x -> PS.PointArray n x

{-# INLINE js_ringIndices #-}
foreign import javascript unsafe "$1['coordinates'].slice(0,$1['coordinates'].length-1)\
                                        \.reduce(function(r,e){return r.concat([e.length-1]);},[])"
    js_ringIndices :: Polygon n x -> JSVal

{-# INLINE js_indicesListPrim #-}
foreign import javascript unsafe "h$fromArrayNoWrap($1)"
    js_indicesListPrim:: JSVal -> Any

seqList :: [a] -> [a]
seqList xs = foldr seq () xs `seq` xs



{-# INLINE mapPolygon' #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Polygon';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(r){return r.map($1);});"
    mapPolygon' :: (Callback (JSVal -> IO JSVal)) -> Polygon n x -> IO (Polygon n x)
{-# INLINE mapPolygon'' #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Polygon';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(r){return r.map($1);});"
    mapPolygon'' :: (Callback a) -> Polygon n x -> Polygon n x


{-# INLINE mapMultiPolygon' #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPolygon';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(p){return p.map(function(r){return r.map($1);});});"
    mapMultiPolygon' :: (Callback (JSVal -> IO JSVal)) -> MultiPolygon n x -> IO (MultiPolygon n x)
{-# INLINE mapMultiPolygon'' #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPolygon';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(p){return p.map(function(r){return r.map($1);});});"
    mapMultiPolygon'' :: (Callback a) -> MultiPolygon n x -> MultiPolygon n x
