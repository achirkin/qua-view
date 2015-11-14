{-# LANGUAGE TypeFamilies #-}
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
    ( FeatureCollection ()
    , Feature ()
    ) where

import GHCJS.Types (JSVal)
import Data.JSArray

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





















{-# INLINE js_FCToJSArray #-}
foreign import javascript unsafe "$1['features']"
    js_FCToJSArray :: FeatureCollection -> JSArray Feature
{-# INLINE js_JSArrayToFC #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'FeatureCollection'; $r['features'] = $1"
    js_JSArrayToFC :: JSArray Feature -> FeatureCollection
