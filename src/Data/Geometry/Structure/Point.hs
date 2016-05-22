{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE ExistentialQuantification, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.Point
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.Point
    ( Point (), pointCoordinate, point
    , MultiPoint (), multiPoint
    ) where


import JsHs.Callback (Callback)

import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeLits

import JsHs.Types
--import GHCJS.Marshal.Pure (PFromJSVal(..))

import JsHs.Array as JS
import Data.Geometry
import Data.Geometry.Transform
import Data.Geometry.Structure.PointSet (PointSet, PointArray)
import qualified Data.Geometry.Structure.PointSet as PS


----------------------------------------------------------------------------------------------------
-- Base Types
----------------------------------------------------------------------------------------------------

-- | GeoJSON Point
newtype Point (n::Nat) x = Point JSVal
instance IsJSVal (Point n x)
instance LikeJS "Array" (Point n x)

-- | GeoJSON MultiPoint
newtype MultiPoint (n::Nat) x = MultiPoint JSVal
instance IsJSVal (MultiPoint n x)
instance LikeJS "Array" (MultiPoint n x)

instance LikeJSArray "Array" (MultiPoint n x) where
    type ArrayElem (MultiPoint n x) = Vector n x
    {-# INLINE toJSArray #-}
    toJSArray = js_MPToVArr
    {-# INLINE fromJSArray #-}
    fromJSArray = js_VArrToMP


{-# INLINE pointCoordinate #-}
foreign import javascript unsafe "$1['coordinates'].slice()"
    pointCoordinate :: Point n x -> Vector n x

{-# INLINE point #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Point';\
                                 \$r['coordinates'] = $1.slice();"
    point :: Vector n x -> Point n x

{-# INLINE multiPoint #-}
multiPoint :: [Vector n x] -> MultiPoint n x
multiPoint = fromJSArray . fromList

----------------------------------------------------------------------------------------------------
-- Point as PointSet
----------------------------------------------------------------------------------------------------

instance PointSet (Point n x) n x where
    {-# INLINE flatten #-}
    flatten = unsafeCoerce . pointCoordinate
    {-# INLINE toPointArray #-}
    toPointArray = js_PointToArray
    {-# INLINE fromPointArray #-}
    fromPointArray = js_PointFromArray
    {-# INLINE mean #-}
    mean = pointCoordinate
    {-# INLINE var #-}
    var = js_toZero
    {-# INLINE mapSet #-}
    mapSet f = point . f . pointCoordinate
    {-# INLINE mapCallbackSet #-}
    mapCallbackSet = js_mapPoint
    {-# INLINE foldSet #-}
    foldSet f a = f a . pointCoordinate
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldPoint f (asJSVal a)

instance Transformable (Point 3 x) 3 x where
    transform sarr = point . transform . flip wrap sarr . pointCoordinate $ unwrap sarr

{-# INLINE js_toZero #-}
foreign import javascript unsafe "var n = $1['coordinates'].length; $r = Array.apply(null, Array(n*n)).map(Number.prototype.valueOf,0);"
    js_toZero :: Point n x -> Matrix n x

{-# INLINE js_PointToArray #-}
foreign import javascript unsafe "[$1['coordinates'].slice()]"
    js_PointToArray :: Point n x -> PointArray n x

{-# INLINE js_PointFromArray #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Point';\
                                 \$r['coordinates'] = $1[0].slice();"
    js_PointFromArray :: PointArray n x -> Point n x

{-# INLINE js_mapPoint #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'Point';\
                                 \$r['coordinates'] = $1($2['coordinates']);"
    js_mapPoint :: (Callback (Vector n x -> Vector n x)) -> Point n x -> Point n x


{-# INLINE js_foldPoint #-}
foreign import javascript unsafe "$1($2, $3['coordinates'])"
    js_foldPoint :: Callback (a -> Vector n x -> a) -> JSVal -> Point n x -> JSVal

---------------------------------------------------------------------------------------------------
-- MultiPoint as PointSet
----------------------------------------------------------------------------------------------------

instance PointSet (MultiPoint n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_MPtoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_MPtoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = js_PAtoMP
    {-# INLINE mean #-}
    mean = PS.mean . js_MPtoPA
    {-# INLINE var #-}
    var = PS.var . js_MPtoPA
    {-# INLINE mapSet #-}
    mapSet = JS.mapSame
    {-# INLINE mapCallbackSet #-}
    mapCallbackSet = js_mapMultiPoint
    {-# INLINE foldSet #-}
    foldSet = JS.foldl
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldMultiPoint f (asJSVal a)

instance Transformable (MultiPoint 3 x) 3 x where
    transform sarr = PS.mapSet (transform . flip wrap sarr) $ unwrap sarr

{-# INLINE js_MPtoPA #-}
js_MPtoPA :: MultiPoint n x -> PointArray n x
js_MPtoPA = fromJSArray . js_MPToVArr

{-# INLINE js_PAtoMP #-}
js_PAtoMP :: PointArray n x -> MultiPoint n x
js_PAtoMP = js_VArrToMP . toJSArray

{-# INLINE js_mapMultiPoint #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPoint'; $r['coordinates'] = $2['coordinates'].map($1);"
    js_mapMultiPoint :: (Callback (Vector n x -> Vector n x)) -> MultiPoint n x -> MultiPoint n x

{-# INLINE js_foldMultiPoint #-}
foreign import javascript unsafe "$3['coordinates'].reduce($1,$2)"
    js_foldMultiPoint :: Callback (a -> Vector n x -> a) -> JSVal -> MultiPoint n x -> JSVal

{-# INLINE js_VArrToMP #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiPoint'; $r['coordinates'] = $1.slice();"
    js_VArrToMP :: JS.Array (Vector n x) -> MultiPoint n x

{-# INLINE js_MPToVArr #-}
foreign import javascript unsafe "$1['coordinates'].slice()"
    js_MPToVArr ::  MultiPoint n x -> JS.Array (Vector n x)
