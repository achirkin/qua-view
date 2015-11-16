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
    ) where


import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
import GHCJS.Foreign (isTruthy)
import GHCJS.Types (JSVal)
import Data.Proxy (Proxy(..))
import Data.JSString (JSString, append)
import Data.JSArray
import Data.Geometry.Structure.Polygon

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
--
----------------------------------------------------------------------------------------------------











----------------------------------------------------------------------------------------------------
-- Getting Geometry
----------------------------------------------------------------------------------------------------

data GeoJsonGeometryND x = forall n . KnownNat n => ND (GeoJsonGeometry n x)

data GeoJsonGeometry n x = GeoPoint JSVal -- coordinates (Vector n GLfloat)
                         | GeoMultiPoint JSVal -- coordinates (PointArray n GLfloat)
                         | GeoLineString JSVal -- coordinates LineString
                         | GeoMultiLineString JSVal -- coordinates MultiLineString
                         | GeoPolygon (Polygon n x)
                         | GeoMultiPolygon (MultiPolygon n x)


getGeoJSONGeometry :: Feature -> Either JSString (GeoJsonGeometryND x)
getGeoJSONGeometry fe = if not (isTruthy js)
    then Left "Could parse GeoJsonGeometryND: it is falsy!"
    else let mdims = someNatVal . toInteger $ getDimensionality js
         in case mdims of
             Nothing -> Left "Could parse GeoJsonGeometryND: failed to find dimensionality of the data"
             Just (SomeNat proxy) -> getGeoJSONGeometryN js >>= Right . ND . dimensionalize proxy
    where js = getGeoJSONGeometry' fe


getGeoJSONGeometryN :: JSVal -> Either JSString (GeoJsonGeometry n x)
getGeoJSONGeometryN js = if not (isTruthy js)
        then Left "Could parse GeoJsonGeometry: it is falsy!"
        else case getGeoJSONType js of
            "Polygon"      -> Right . GeoPolygon $ asLikeJS js
            "MultiPolygon" -> Right . GeoMultiPolygon $ asLikeJS js
            t              -> Left $ "Could parse GeoJsonGeometry: type " `append` t `append` " is not supported currently."


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


----------------------------------------------------------------------------------------------------
-- FeatureCollection converters
----------------------------------------------------------------------------------------------------

{-# INLINE js_FCToJSArray #-}
foreign import javascript unsafe "$1['features']"
    js_FCToJSArray :: FeatureCollection -> JSArray Feature
{-# INLINE js_JSArrayToFC #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'FeatureCollection'; $r['features'] = $1"
    js_JSArrayToFC :: JSArray Feature -> FeatureCollection
