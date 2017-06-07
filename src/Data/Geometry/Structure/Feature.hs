{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE ExistentialQuantification, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.Feature
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.Feature
    ( GeometryInput (..)
    , FeatureCollection (..)
    , SomeJSONInput (..)
    , Feature (..), feature, setFeature
    , GeoJsonGeometryND (..), GeoJsonGeometry (..)
    , FeatureGeometryType (..), featureGeometryType
    , getGeoJSONGeometry, getSizedGeoJSONGeometry
    , boundingBox2D, filterGeometryTypes
    , ParsedFeatureCollection (..), smartProcessFeatureCollection
    , ParsedGeometryInput (..), smartProcessGeometryInput
    , js_FCToGI
    ) where


import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
---- import GHCJS.Foreign (isTruthy)
--import GHCJS.Marshal.Pure (PToJSVal (..))
import JsHs.Types (JSVal)
import Data.Proxy (Proxy(..))
import JsHs.JSString (JSString, append)
import JsHs.Array as JS
import JsHs.Types.Prim (jsIsNullOrUndef)
import Data.Geometry
import qualified Data.Geometry.Structure.PointSet as PS
import Data.Geometry.Structure.LineString (LineString (), MultiLineString ())
import Data.Geometry.Structure.Point (Point (), MultiPoint ())
import Data.Geometry.Structure.Polygon (Polygon (), MultiPolygon ())
import Data.Coerce

import Program.Settings

----------------------------------------------------------------------------------------------------
-- Base Types
----------------------------------------------------------------------------------------------------

-- | GeoJSON Feature
newtype Feature = Feature JSVal
instance LikeJS "Object" Feature where
  asJSVal = js_deleteTimestamp

foreign import javascript unsafe "delete $1['properties']['timestamp']; $r = $1;"
  js_deleteTimestamp :: Feature -> JSVal
foreign import javascript unsafe "$1['features'].forEach(function(e){delete e['properties']['timestamp'];}); $r = $1;"
  js_deleteFcTimestamp :: FeatureCollection -> JSVal
foreign import javascript unsafe "$1['geometry']['features'].forEach(function(e){delete e['properties']['timestamp'];}); $r = $1;"
  js_deleteGiTimestamp :: GeometryInput -> JSVal

-- | GeoJSON FeatureCollection
newtype FeatureCollection = FeatureCollection JSVal
instance LikeJS "Object" FeatureCollection where
  asJSVal = js_deleteFcTimestamp

instance LikeJSArray "Object" FeatureCollection where
    type ArrayElem FeatureCollection = Feature
    {-# INLINE toJSArray #-}
    toJSArray = js_FCToJSArray
    {-# INLINE fromJSArray #-}
    fromJSArray = js_JSArrayToFC

-- | JSON GeometryInput
newtype GeometryInput = GeometryInput JSVal
instance LikeJS "Object" GeometryInput where
  asJSVal = js_deleteGiTimestamp

instance LikeJSArray "Object" GeometryInput where
    type ArrayElem GeometryInput = Feature
    {-# INLINE toJSArray #-}
    toJSArray = js_GIToJSArray
    {-# INLINE fromJSArray #-}
    fromJSArray = js_JSArrayToGI

fromGItoFC :: GeometryInput -> FeatureCollection
fromGItoFC = coerce

data SomeJSONInput = SJIExtended GeometryInput | SJIGeoJSON FeatureCollection
instance LikeJS "Object" SomeJSONInput where
  asJSVal (SJIExtended gi) = asJSVal gi
  asJSVal (SJIGeoJSON fc) = asJSVal fc

  asLikeJS jsv = case (getProp "type" jsv :: Maybe String) of
    Just "FeatureCollection" -> SJIGeoJSON (coerce jsv :: FeatureCollection)
    _ ->  SJIExtended (coerce jsv :: GeometryInput)
  
----------------------------------------------------------------------------------------------------
-- Some Functions
----------------------------------------------------------------------------------------------------

data ParsedGeometryInput x = ParsedGeometryInput
  { pgiFeatureCollection :: FeatureCollection
  , pgiErrors            :: JS.Array JSString
  , pgiLatLonAlt         :: Maybe (Vector 3 x)
  , pgiSrid              :: Maybe Int
  }

data ParsedFeatureCollection n x = ParsedFeatureCollection
  { pfcPoints  :: JS.Array Feature
  , pfcLines   :: JS.Array Feature
  , pfcPolys   :: JS.Array Feature
  , pfcDeletes :: JS.Array Int
  , pfcErrors  :: JS.Array JSString
  , pfcMin     :: Vector n x
  , pfcMax     :: Vector n x
  , pfcDims    :: Int
  , pfcLonLat  :: Maybe (Vector 2 x)
  }

smartProcessGeometryInput :: Int -- ^ maximum geomId in current City
                          -> Vector n x -- ^ default vector to substitute
                          -> SomeJSONInput
                          -> (Maybe Int, Maybe (Vector 3 x), [JSString], ParsedFeatureCollection n x)
smartProcessGeometryInput n defVals input = case input of
  SJIGeoJSON fc -> (Nothing, Nothing, [], smartProcessFeatureCollection n defVals "Unknown" fc)
  SJIExtended gi -> (srid, originLatLonAlt, errors, smartProcessFeatureCollection n defVals cs fc)
            where
              parsedGeometryInput = smartProcessGItoFC defVals gi
              srid = pgiSrid parsedGeometryInput
              originLatLonAlt = pgiLatLonAlt parsedGeometryInput
              errors = JS.toList $ pgiErrors parsedGeometryInput
              fc = pgiFeatureCollection parsedGeometryInput
              cs = case (srid, originLatLonAlt) of
                    (Just 4326, _) -> "WGS84"
                    (Nothing, Nothing) -> "Unknown"
                    _ -> "Metric"

smartProcessGItoFC :: Vector n x -- ^ default vector to substitute
                   -> GeometryInput
                   -> ParsedGeometryInput x
smartProcessGItoFC defVals gi = ParsedGeometryInput fc errors (asLikeJS originLonLatAlt) (asLikeJS srid)
  where
    (fc, errors, originLonLatAlt, srid) = js_smartProcessGeometryInput gi defVals

foreign import javascript unsafe "var a = gm$smartProcessGeometryInput($1, $2);$r1=a[0];$r2=a[1];$r3=a[2];$r4=a[3]"
  js_smartProcessGeometryInput
    :: GeometryInput -> Vector n x
    -> (FeatureCollection, JS.Array JSString, JSVal, JSVal)

smartProcessFeatureCollection :: Int -- ^ maximum geomId in current City
                              -> Vector n x -- ^ default vector to substitute
                              -> JSString -- ^ determine conversion
                              -> FeatureCollection
                              -> ParsedFeatureCollection n x
smartProcessFeatureCollection n defVals cs fc = ParsedFeatureCollection points lins polys deletes errors cmin cmax cdims (asLikeJS mLonLat)
  where
    (points, lins, polys, deletes, errors, cmin, cmax, cdims, mLonLat) = js_smartProcessFeatureCollection fc cs defVals n


foreign import javascript unsafe "var a = gm$smartProcessFeatureCollection($1, $2, $3, $4);$r1=a[0];$r2=a[1];$r3=a[2];$r4=a[3];$r5=a[4];$r6=a[5];$r7=a[6];$r8=a[7];$r9=a[8];"
    js_smartProcessFeatureCollection
      :: FeatureCollection -> JSString -> Vector n x -> Int
      -> (JS.Array Feature, JS.Array Feature, JS.Array Feature, JS.Array Int, JS.Array JSString, Vector n x, Vector n x, Int, JSVal)


foreign import javascript unsafe "var r = gm$boundNestedArray(($1['geometry'] && $1['geometry']['coordinates']) ? $1['geometry']['coordinates'] : []);\
                          \if(!r){ $r1 = Array.apply(null, Array(2)).map(Number.prototype.valueOf,Infinity);\
                          \        $r2 = Array.apply(null, Array(2)).map(Number.prototype.valueOf,-Infinity);}\
                          \else { $r1 = r[0].slice(0,2); $r2 = r[1].slice(0,2); }"
    boundingBox2D :: Feature -> (Vector2 x, Vector2 x)


{-# INLINE filterGeometryTypes #-}
filterGeometryTypes :: FeatureCollection -> (JS.Array Feature, JS.Array Feature, JS.Array Feature, JS.Array Feature)
filterGeometryTypes = js_filterGeometryTypes . toJSArray

{-# INLINE js_filterGeometryTypes #-}
foreign import javascript unsafe "var t = $1.filter(function(e){return e && e['geometry'] && e['geometry']['type'] && e['geometry']['coordinates'];});\
                                 \$r1 = t.filter(function(e){return (e['geometry']['type'] === 'Point' || e['geometry']['type'] === 'MultiPoint') && e['geometry']['coordinates'][0] != null;});\
                                 \$r2 = t.filter(function(e){return (e['geometry']['type'] === 'LineString' || e['geometry']['type'] === 'MultiLineString') && e['geometry']['coordinates'][0] != null && e['geometry']['coordinates'][0][0] != null;});\
                                 \$r3 = t.filter(function(e){return (e['geometry']['type'] === 'Polygon' || e['geometry']['type'] === 'MultiPolygon') && e['geometry']['coordinates'][0] != null && e['geometry']['coordinates'][0][0] != null && e['geometry']['coordinates'][0][0][0] != null;});\
                                 \$r4 = [];"
    js_filterGeometryTypes :: JS.Array Feature -> (JS.Array Feature, JS.Array Feature, JS.Array Feature, JS.Array Feature)

--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'Point' && $1['geometry'][0] != null)"
--    checkPoint :: Feature -> Bool
--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'MultiPoint' && $1['geometry']['coordinates'][0] != null && $1['geometry']['coordinates'][0][0] != null)"
--    checkMultiPoint :: Feature -> Bool
--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'LineString' && $1['geometry']['coordinates'][0] != null && $1['geometry']['coordinates'][0][0] != null)"
--    checkLineString :: Feature -> Bool
--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'MultiLineString' && $1['geometry']['coordinates'][0] != null && $1['geometry']['coordinates'][0][0] != null && $1['geometry']['coordinates'][0][0][0] != null)"
--    checkMultiLineString :: Feature -> Bool
--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'Polygon' && $1['geometry']['coordinates'][0] != null && $1['geometry']['coordinates'][0][0] != null && $1['geometry']['coordinates'][0][0][0] != null)"
--    checkPolygon :: Feature -> Bool
--foreign import javascript unsafe "($1 != null && $1['geometry'] && $1['geometry']['coordinates'] && $1['geometry']['type'] == 'MultiPolygon' && $1['geometry']['coordinates'][0] != null && $1['geometry']['coordinates'][0][0] != null && $1['geometry']['coordinates'][0][0][0] != null && $1['geometry']['coordinates'][0][0][0][0] != null)"
--    checkMultiPolygon :: Feature -> Bool

setFeature :: GeoJsonGeometry n x -> Feature -> Feature
setFeature geom = js_setFeature (asJSVal geom)

feature :: GeoJsonGeometry n x -> Feature
feature = js_feature . asJSVal


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

instance LikeJS "String" FeatureGeometryType where
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

instance LikeJS "Array" (GeoJsonGeometry n x) where
    asJSVal (GeoPoint x)           = asJSVal x
    asJSVal (GeoMultiPoint x)      = asJSVal x
    asJSVal (GeoLineString x)      = asJSVal x
    asJSVal (GeoMultiLineString x) = asJSVal x
    asJSVal (GeoPolygon x)         = asJSVal x
    asJSVal (GeoMultiPolygon x)    = asJSVal x
    asLikeJS _ = undefined



-- | Get Feature GeoJSON geometry, without knowledge of how many dimensions there are in geometry
getGeoJSONGeometry :: Feature -> Either JSString (GeoJsonGeometryND x)
getGeoJSONGeometry fe = if not (jsIsNullOrUndef js)
    then let mdims = someNatVal . toInteger $ getDimensionality js
         in case mdims of
             Nothing -> Left "Cannot parse GeoJsonGeometryND: failed to find dimensionality of the data"
             Just (SomeNat proxy) -> getGeoJSONGeometryN js >>= Right . ND . dimensionalize' proxy
    else Left "Cannot parse GeoJsonGeometryND: it is falsy!"
    where js = getGeoJSONGeometry' fe

-- | Get geometry of certain dimensionality;
--   Does not check the real dimensionality of geoJSON geometry!
getGeoJSONGeometryN :: JSVal -> Either JSString (GeoJsonGeometry n x)
getGeoJSONGeometryN js = if not (jsIsNullOrUndef js)
        then case getGeoJSONType js of
--            "Point"           -> if checkPoint js then Right . GeoPoint $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            "MultiPoint"      -> if checkMultiPoint js then Right . GeoMultiPoint $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            "LineString"      -> if checkLineString js then Right . GeoLineString $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            "MultiLineString" -> if checkMultiLineString js then Right . GeoMultiLineString $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            "Polygon"         -> if checkPolygon js then Right . GeoPolygon $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            "MultiPolygon"    -> if checkMultiPolygon js then Right . GeoMultiPolygon $ asLikeJS js else Left "Not a proper GeoJSON Feature."
--            t                 -> Left $ "Cannot parse GeoJsonGeometry: type " `append` t `append` " is not supported."
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
getSizedGeoJSONGeometry v fe = if not (jsIsNullOrUndef js)
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
    js_FCToJSArray :: FeatureCollection -> JS.Array Feature
{-# INLINE js_JSArrayToFC #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'FeatureCollection'; $r['features'] = $1"
    js_JSArrayToFC :: JS.Array Feature -> FeatureCollection

----------------------------------------------------------------------------------------------------
-- GeometryInput converters
----------------------------------------------------------------------------------------------------

{-# INLINE js_GIToFC #-}
foreign import javascript unsafe "$1['geometry']"
    js_GIToFC :: GeometryInput -> FeatureCollection
{-# INLINE js_FCToGI #-}
foreign import javascript unsafe "$r = {}; $r['geometry'] = $1"
    js_FCToGI :: FeatureCollection -> GeometryInput

{-# INLINE js_GIToJSArray #-}
foreign import javascript unsafe "$1['geometry']['features']"
    js_GIToJSArray :: GeometryInput -> JS.Array Feature
{-# INLINE js_JSArrayToGI #-}
foreign import javascript unsafe "$r = {}; $r['geometry'] = {}; $r['geometry']['type'] = 'FeatureCollection'; $r['geometry']['features'] = $1;"
    js_JSArrayToGI :: JS.Array Feature -> GeometryInput
