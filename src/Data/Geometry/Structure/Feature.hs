{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE ExistentialQuantification, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.Feature
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.Feature
    ( FeatureCollection (..)
    , Feature (..)
    , GeoJsonGeometryND (..), GeoJsonGeometry (..)
    , getGeoJSONGeometry
    , boundingBox2D
    ) where


import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
import GHCJS.Foreign (isTruthy)
import GHCJS.Types (JSVal)
import Data.Proxy (Proxy(..))
import Data.JSString (JSString, append)
import Data.JSArray
import Data.Geometry
import qualified Data.Geometry.Structure.PointSet as PS
import Data.Geometry.Structure.LineString (LineString (), MultiLineString ())
import Data.Geometry.Structure.Point (Point (), MultiPoint ())
import Data.Geometry.Structure.Polygon (Polygon (), MultiPolygon ())

----------------------------------------------------------------------------------------------------
-- Base Types
----------------------------------------------------------------------------------------------------

-- | GeoJSON Feature
newtype Feature = Feature JSVal
instance LikeJS Feature

-- | GeoJSON FeatureCollection
newtype FeatureCollection = FeatureCollection JSVal
instance LikeJS FeatureCollection

instance LikeJSArray FeatureCollection where
    type JSArrayElem FeatureCollection = Feature
    {-# INLINE toJSArray #-}
    toJSArray = js_FCToJSArray
    {-# INLINE fromJSArray #-}
    fromJSArray = js_JSArrayToFC

----------------------------------------------------------------------------------------------------
-- Some Functions
----------------------------------------------------------------------------------------------------

foreign import javascript unsafe "var r = gm$boundNestedArray(($1['geometry'] && $1['geometry']['coordinates']) ? $1['geometry']['coordinates'] : []);\
                          \if(!r){ $r1 = Array.apply(null, Array(2)).map(Number.prototype.valueOf,Infinity);\
                          \        $r2 = Array.apply(null, Array(2)).map(Number.prototype.valueOf,-Infinity);}\
                          \else { $r1 = r[0].slice(0,2); $r2 = r[1].slice(0,2); }"
    boundingBox2D :: Feature -> (Vector2 x, Vector2 x)



----------------------------------------------------------------------------------------------------
-- Getting Geometry
----------------------------------------------------------------------------------------------------

data GeoJsonGeometryND x = forall n . KnownNat n => ND (GeoJsonGeometry n x)

data GeoJsonGeometry n x = GeoPoint (Point n x)
                         | GeoMultiPoint (MultiPoint n x)
                         | GeoLineString (LineString n x)
                         | GeoMultiLineString (MultiLineString n x)
                         | GeoPolygon (Polygon n x)
                         | GeoMultiPolygon (MultiPolygon n x)


getGeoJSONGeometry :: Feature -> Either JSString (GeoJsonGeometryND x)
getGeoJSONGeometry fe = if not (isTruthy js)
    then Left "Cannot parse GeoJsonGeometryND: it is falsy!"
    else let mdims = someNatVal . toInteger $ getDimensionality js
         in case mdims of
             Nothing -> Left "Cannot parse GeoJsonGeometryND: failed to find dimensionality of the data"
             Just (SomeNat proxy) -> getGeoJSONGeometryN js >>= Right . ND . dimensionalize proxy
    where js = getGeoJSONGeometry' fe


getGeoJSONGeometryN :: JSVal -> Either JSString (GeoJsonGeometry n x)
getGeoJSONGeometryN js = if not (isTruthy js)
        then Left "Cannot parse GeoJsonGeometry: it is falsy!"
        else case getGeoJSONType js of
            "Point"           -> Right . GeoPoint $ asLikeJS js
            "MultiPoint"      -> Right . GeoMultiPoint $ asLikeJS js
            "LineString"      -> Right . GeoLineString $ asLikeJS js
            "MultiLineString" -> Right . GeoMultiLineString $ asLikeJS js
            "Polygon"         -> Right . GeoPolygon $ asLikeJS js
            "MultiPolygon"    -> Right . GeoMultiPolygon $ asLikeJS js
            t                 -> Left $ "Cannot parse GeoJsonGeometry: type " `append` t `append` " is not supported."


{-# INLINE dimensionalize #-}
dimensionalize :: KnownNat n => Proxy n -> GeoJsonGeometry n x -> GeoJsonGeometry n x
dimensionalize _ = id



{-# INLINE getGeoJSONType #-}
foreign import javascript unsafe "$1['type']"
    getGeoJSONType :: JSVal -> JSString
{-# INLINE getGeoJSONGeometry #-}
foreign import javascript unsafe "$1['geometry']"
    getGeoJSONGeometry' :: Feature -> JSVal



{-# INLINE getDimensionality #-}
-- | Get length of the coordinates in GeoJSON object.
--   Default length is 3.
foreign import javascript unsafe "var dims = $1['coordinates'] ? gm$GeometryDims($1['coordinates']) : 3; $r = dims === 0 ? 3 : dims;"
    getDimensionality :: JSVal -> Int



instance PS.PointSet (GeoJsonGeometry n x) n x where
    {-# INLINE flatten #-}
    flatten (GeoPoint x)           = PS.flatten x
    flatten (GeoMultiPoint x)      = PS.flatten x
    flatten (GeoLineString x)      = PS.flatten x
    flatten (GeoMultiLineString x) = PS.flatten x
    flatten (GeoPolygon x)         = PS.flatten x
    flatten (GeoMultiPolygon x)    = PS.flatten x
    {-# INLINE toPointArray #-}
    toPointArray (GeoPoint x)           = PS.toPointArray x
    toPointArray (GeoMultiPoint x)      = PS.toPointArray x
    toPointArray (GeoLineString x)      = PS.toPointArray x
    toPointArray (GeoMultiLineString x) = PS.toPointArray x
    toPointArray (GeoPolygon x)         = PS.toPointArray x
    toPointArray (GeoMultiPolygon x)    = PS.toPointArray x
    {-# INLINE fromPointArray #-}
    fromPointArray = GeoMultiPoint . PS.fromPointArray
    {-# INLINE mean #-}
    mean (GeoPoint x)           = PS.mean x
    mean (GeoMultiPoint x)      = PS.mean x
    mean (GeoLineString x)      = PS.mean x
    mean (GeoMultiLineString x) = PS.mean x
    mean (GeoPolygon x)         = PS.mean x
    mean (GeoMultiPolygon x)    = PS.mean x
    {-# INLINE var #-}
    var (GeoPoint x)           = PS.var x
    var (GeoMultiPoint x)      = PS.var x
    var (GeoLineString x)      = PS.var x
    var (GeoMultiLineString x) = PS.var x
    var (GeoPolygon x)         = PS.var x
    var (GeoMultiPolygon x)    = PS.var x
    {-# INLINE mapSet #-}
    mapSet f (GeoPoint x)           = GeoPoint           $ PS.mapSet f x
    mapSet f (GeoMultiPoint x)      = GeoMultiPoint      $ PS.mapSet f x
    mapSet f (GeoLineString x)      = GeoLineString      $ PS.mapSet f x
    mapSet f (GeoMultiLineString x) = GeoMultiLineString $ PS.mapSet f x
    mapSet f (GeoPolygon x)         = GeoPolygon         $ PS.mapSet f x
    mapSet f (GeoMultiPolygon x)    = GeoMultiPolygon    $ PS.mapSet f x
    {-# INLINE mapCallbackSet #-}
    mapCallbackSet f (GeoPoint x)           = GeoPoint           $ PS.mapCallbackSet f x
    mapCallbackSet f (GeoMultiPoint x)      = GeoMultiPoint      $ PS.mapCallbackSet f x
    mapCallbackSet f (GeoLineString x)      = GeoLineString      $ PS.mapCallbackSet f x
    mapCallbackSet f (GeoMultiLineString x) = GeoMultiLineString $ PS.mapCallbackSet f x
    mapCallbackSet f (GeoPolygon x)         = GeoPolygon         $ PS.mapCallbackSet f x
    mapCallbackSet f (GeoMultiPolygon x)    = GeoMultiPolygon    $ PS.mapCallbackSet f x
    {-# INLINE foldSet #-}
    foldSet f a (GeoPoint x)           = PS.foldSet f a x
    foldSet f a (GeoMultiPoint x)      = PS.foldSet f a x
    foldSet f a (GeoLineString x)      = PS.foldSet f a x
    foldSet f a (GeoMultiLineString x) = PS.foldSet f a x
    foldSet f a (GeoPolygon x)         = PS.foldSet f a x
    foldSet f a (GeoMultiPolygon x)    = PS.foldSet f a x
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a (GeoPoint x)           = PS.foldCallbackSet f a x
    foldCallbackSet f a (GeoMultiPoint x)      = PS.foldCallbackSet f a x
    foldCallbackSet f a (GeoLineString x)      = PS.foldCallbackSet f a x
    foldCallbackSet f a (GeoMultiLineString x) = PS.foldCallbackSet f a x
    foldCallbackSet f a (GeoPolygon x)         = PS.foldCallbackSet f a x
    foldCallbackSet f a (GeoMultiPolygon x)    = PS.foldCallbackSet f a x

----------------------------------------------------------------------------------------------------
-- FeatureCollection converters
----------------------------------------------------------------------------------------------------

{-# INLINE js_FCToJSArray #-}
foreign import javascript unsafe "$1['features']"
    js_FCToJSArray :: FeatureCollection -> JSArray Feature
{-# INLINE js_JSArrayToFC #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'FeatureCollection'; $r['features'] = $1"
    js_JSArrayToFC :: JSArray Feature -> FeatureCollection
