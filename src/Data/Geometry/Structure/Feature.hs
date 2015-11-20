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
    , Feature (..), feature, setFeature
    , GeoJsonGeometryND (..), GeoJsonGeometry (..)
    , FeatureGeometryType (..), featureGeometryType
    , getGeoJSONGeometry, getSizedGeoJSONGeometry
    , boundingBox2D, filterGeometryTypes
    ) where


import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
import GHCJS.Foreign (isTruthy)
import GHCJS.Marshal.Pure (PToJSVal (..))
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


{-# INLINE filterGeometryTypes #-}
filterGeometryTypes :: FeatureCollection -> (JSArray Feature, JSArray Feature, JSArray Feature)
filterGeometryTypes = js_filterGeometryTypes . toJSArray

{-# INLINE js_filterGeometryTypes #-}
foreign import javascript unsafe "var t = $1.filter(function(e){return e && e['geometry'] && e['geometry']['type'];});\
                                 \$r1 = t.filter(function(e){return e['geometry']['type'] === 'Point' || e['geometry']['type'] === 'MultiPoint';});\
                                 \$r2 = t.filter(function(e){return e['geometry']['type'] === 'LineString' || e['geometry']['type'] === 'MultiLineString';});\
                                 \$r3 = t.filter(function(e){return e['geometry']['type'] === 'Polygon' || e['geometry']['type'] === 'MultiPolygon';});"
    js_filterGeometryTypes :: JSArray Feature -> (JSArray Feature, JSArray Feature, JSArray Feature)

setFeature :: GeoJsonGeometry n x -> Feature -> Feature
setFeature geom = js_setFeature (pToJSVal geom)

feature :: GeoJsonGeometry n x -> Feature
feature = js_feature . pToJSVal


{-# INLINE js_setFeature #-}
foreign import javascript unsafe "$r = {}; $r['properties'] = $2['properties']; $r['type'] = 'Feature'; $r['geometry'] = $1;"
    js_setFeature :: JSVal -> Feature -> Feature

{-# INLINE js_feature #-}
foreign import javascript unsafe "$r = {}; $r['properties'] = {}; $r['type'] = 'Feature'; $r['geometry'] = $1;"
    js_feature :: JSVal -> Feature

----------------------------------------------------------------------------------------------------
-- Getting Geometry
----------------------------------------------------------------------------------------------------

data FeatureGeometryType = FeaturePoint
                         | FeatureMultiPoint
                         | FeatureLineString
                         | FeatureMultiLineString
                         | FeaturePolygon
                         | FeatureMultiPolygon


featureGeometryType :: Feature -> FeatureGeometryType
featureGeometryType = asLikeJS . js_featureGeometryType

foreign import javascript unsafe "$r = $1['geometry']['type'];"
    js_featureGeometryType :: Feature -> JSVal

instance LikeJS FeatureGeometryType where
    asJSVal FeaturePoint           = asJSVal ("Point" :: JSString)
    asJSVal FeatureMultiPoint      = asJSVal ("MultiPoint" :: JSString)
    asJSVal FeatureLineString      = asJSVal ("LineString" :: JSString)
    asJSVal FeatureMultiLineString = asJSVal ("MultiLineString" :: JSString)
    asJSVal FeaturePolygon         = asJSVal ("Polygon" :: JSString)
    asJSVal FeatureMultiPolygon    = asJSVal ("MultiPolygon" :: JSString)
    asLikeJS jsv = case asLikeJS jsv :: JSString of
                     "Point"           -> FeaturePoint
                     "MultiPoint"      -> FeatureMultiPoint
                     "LineString"      -> FeatureLineString
                     "MultiLineString" -> FeatureMultiLineString
                     "Polygon"         -> FeaturePolygon
                     "MultiPolygon"    -> FeatureMultiPolygon
                     _                 -> FeaturePoint


data GeoJsonGeometryND x = forall n . KnownNat n => ND (GeoJsonGeometry n x)

data GeoJsonGeometry n x = GeoPoint (Point n x)
                         | GeoMultiPoint (MultiPoint n x)
                         | GeoLineString (LineString n x)
                         | GeoMultiLineString (MultiLineString n x)
                         | GeoPolygon (Polygon n x)
                         | GeoMultiPolygon (MultiPolygon n x)

instance PToJSVal (GeoJsonGeometry n x) where
    pToJSVal (GeoPoint x)           = asJSVal x
    pToJSVal (GeoMultiPoint x)      = asJSVal x
    pToJSVal (GeoLineString x)      = asJSVal x
    pToJSVal (GeoMultiLineString x) = asJSVal x
    pToJSVal (GeoPolygon x)         = asJSVal x
    pToJSVal (GeoMultiPolygon x)    = asJSVal x

-- | Get Feature GeoJSON geometry, without knowledge of how many dimensions there are in geometry
getGeoJSONGeometry :: Feature -> Either JSString (GeoJsonGeometryND x)
getGeoJSONGeometry fe = if isTruthy js
    then let mdims = someNatVal . toInteger $ getDimensionality js
         in case mdims of
             Nothing -> Left "Cannot parse GeoJsonGeometryND: failed to find dimensionality of the data"
             Just (SomeNat proxy) -> getGeoJSONGeometryN js >>= Right . ND . dimensionalize' proxy
    else Left "Cannot parse GeoJsonGeometryND: it is falsy!"
    where js = getGeoJSONGeometry' fe

-- | Get geometry of certain dimensionality;
--   Does not check the real dimensionality of geoJSON geometry!
getGeoJSONGeometryN :: JSVal -> Either JSString (GeoJsonGeometry n x)
getGeoJSONGeometryN js = if isTruthy js
        then case getGeoJSONType js of
            "Point"           -> Right . GeoPoint $ asLikeJS js
            "MultiPoint"      -> Right . GeoMultiPoint $ asLikeJS js
            "LineString"      -> Right . GeoLineString $ asLikeJS js
            "MultiLineString" -> Right . GeoMultiLineString $ asLikeJS js
            "Polygon"         -> Right . GeoPolygon $ asLikeJS js
            "MultiPolygon"    -> Right . GeoMultiPolygon $ asLikeJS js
            t                 -> Left $ "Cannot parse GeoJsonGeometry: type " `append` t `append` " is not supported."
        else Left "Cannot parse GeoJsonGeometry: it is falsy!"

-- | Try to resize geometry inside feature to required dimensionality, and then return it
getSizedGeoJSONGeometry :: Vector n x -- ^ values to substitute into each coordinate if the vector dimensionality is larger than that of points
                        -> Feature -> Either JSString (GeoJsonGeometry n x)
getSizedGeoJSONGeometry v fe = if isTruthy js
    then getGeoJSONGeometryN $ js_getSizedGeoJSONGeometry v js
    else Left "Cannot parse GeoJsonGeometry: it is falsy!"
    where js = getGeoJSONGeometry' fe

foreign import javascript unsafe "var res = gm$resizeNestedArray($1,$2['coordinates']); $r = {}; $r['coordinates'] = res; $r['type'] = $2['type'];"
    js_getSizedGeoJSONGeometry :: Vector n x -> JSVal -> JSVal


{-# INLINE dimensionalize' #-}
dimensionalize' :: KnownNat n => Proxy n -> GeoJsonGeometry n x -> GeoJsonGeometry n x
dimensionalize' _ = id



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
