{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE ExistentialQuantification, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Structure.LineString
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Structure.LineString
    ( LineString (), lineString
    , MultiLineString (), multiLineString
    ) where


import GHC.Exts (Any)
import JsHs.Callback (Callback, releaseCallback)
import System.IO.Unsafe (unsafePerformIO)

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

-- | GeoJSON LineString
newtype LineString (n::Nat) x = LineString JSVal
instance IsJSVal (LineString n x)
--instance PFromJSVal (LineString n x) where
--    pFromJSVal = LineString
instance LikeJS "Array" (LineString n x)

instance LikeJSArray "Array" (LineString n x) where
    type ArrayElem (LineString n x) = Vector n x
    {-# INLINE toJSArray #-}
    toJSArray = js_LSToVArr
    {-# INLINE fromJSArray #-}
    fromJSArray = js_VArrToLS

-- | GeoJSON LineString
newtype MultiLineString (n::Nat) x = MultiLineString JSVal
instance IsJSVal (MultiLineString n x)
--instance PFromJSVal (MultiLineString n x) where
--    pFromJSVal = MultiLineString
instance LikeJS "Array" (MultiLineString n x)

instance LikeJSArray "Array" (MultiLineString n x) where
    type ArrayElem (MultiLineString n x) = LineString n x
    {-# INLINE toJSArray #-}
    toJSArray = js_MLSToLSArr
    {-# INLINE fromJSArray #-}
    fromJSArray = js_LSArrToMLS


{-# INLINE lineString #-}
lineString :: Vector n x -> Vector n x -> [Vector n x] -> LineString n x
lineString x y zs = fromJSArray . fromList $ x:y:zs

{-# INLINE multiLineString #-}
multiLineString :: [LineString n x] -> MultiLineString n x
multiLineString = fromJSArray . fromList

----------------------------------------------------------------------------------------------------
-- LineString as PointSet
----------------------------------------------------------------------------------------------------

instance PointSet (LineString n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_LStoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_LStoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = js_PAtoLS
    {-# INLINE mean #-}
    mean = PS.mean . js_LStoPA
    {-# INLINE var #-}
    var = PS.var . js_LStoPA
    {-# INLINE mapSet #-}
    mapSet = JS.mapSame
    {-# INLINE mapCallbackSet #-}
    mapCallbackSet = js_mapLineString
    {-# INLINE foldSet #-}
    foldSet = JS.foldl
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldLineString f (asJSVal a)

instance Transformable (LineString 3 x) 3 x where
    transform sarr = PS.mapSet (transform . flip wrap sarr) $ unwrap sarr

{-# INLINE js_LStoPA #-}
js_LStoPA :: LineString n x -> PointArray n x
js_LStoPA = fromJSArray . js_LSToVArr

{-# INLINE js_PAtoLS #-}
js_PAtoLS :: PointArray n x -> LineString n x
js_PAtoLS = js_VArrToLS . toJSArray

{-# INLINE js_mapLineString #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'LineString'; $r['coordinates'] = $2['coordinates'].map($1);"
    js_mapLineString :: (Callback (Vector n x -> Vector n x)) -> LineString n x -> LineString n x

{-# INLINE js_foldLineString #-}
foreign import javascript unsafe "$3['coordinates'].reduce($1,$2)"
    js_foldLineString :: Callback (a -> Vector n x -> a) -> JSVal -> LineString n x -> JSVal

{-# INLINE js_VArrToLS #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'LineString'; $r['coordinates'] = $1.slice();"
    js_VArrToLS :: JS.Array (Vector n x) -> LineString n x

{-# INLINE js_LSToVArr #-}
foreign import javascript unsafe "$1['coordinates'].slice()"
    js_LSToVArr ::  LineString n x -> JS.Array (Vector n x)


----------------------------------------------------------------------------------------------------
-- MultiLineString as PointSet
----------------------------------------------------------------------------------------------------

instance PointSet (MultiLineString n x) n x where
    {-# INLINE flatten #-}
    flatten = PS.flatten . js_MLStoPA
    {-# INLINE toPointArray #-}
    toPointArray = js_MLStoPA
    {-# INLINE fromPointArray #-}
    fromPointArray = js_PAtoMLS
    {-# INLINE mean #-}
    mean = PS.mean . js_MLStoPA
    {-# INLINE var #-}
    var = PS.var . js_MLStoPA
    {-# NOINLINE mapSet #-}
    mapSet f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 f
        rez <- js_mapMultiLineStringIO call arr
        rez `seq` releaseCallback call
        return rez
    {-# NOINLINE mapCallbackSet #-}
    mapCallbackSet = js_mapMultiLineString
    {-# NOINLINE foldSet #-}
    foldSet f a arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS r) e)
        rez <- asLikeJS <$> js_foldMultiLineStringIO call (asJSVal a) arr
        rez `seq` releaseCallback call
        return rez
    {-# INLINE foldCallbackSet #-}
    foldCallbackSet f a = asLikeJS . js_foldMultiLineString f (asJSVal a)

instance Transformable (MultiLineString 3 x) 3 x where
    transform sarr = PS.mapSet (transform . flip wrap sarr) $ unwrap sarr

{-# INLINE js_LSArrToMLS #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiLineString';\
                                 \$r['coordinates'] = $1.map(function(e){return e['coordinates'];});"
    js_LSArrToMLS :: JS.Array (LineString n x) -> MultiLineString n x

{-# INLINE js_MLSToLSArr #-}
foreign import javascript unsafe "$1['coordinates'].map(function(e){\
                                    \ var r = {}; r['type'] = 'LineString'; r['coordinates'] = e;\
                                    \ return r; })"
    js_MLSToLSArr ::  MultiLineString n x -> JS.Array (LineString n x)

{-# INLINE js_MLStoPA #-}
foreign import javascript unsafe "[].concat.apply([], $1['coordinates'].map(function(a){return a;}))"
    js_MLStoPA :: MultiLineString n x -> PointArray n x

{-# INLINE js_PAtoMLS #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiLineString'; $r['coordinates'] = [$1];"
    js_PAtoMLS :: PointArray n x -> MultiLineString n x


{-# INLINE js_mapMultiLineString #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiLineString';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(r){return r.map($1);});"
    js_mapMultiLineString :: (Callback (Vector n x -> Vector n x)) -> MultiLineString n x -> MultiLineString n x

{-# INLINE js_foldMultiLineString #-}
foreign import javascript unsafe "$3['coordinates'].reduce(function(rr,r){return r.reduce($1,rr);},$2)"
    js_foldMultiLineString :: Callback (a -> Vector n x -> a) -> JSVal -> MultiLineString n x -> JSVal

{-# INLINE js_mapMultiLineStringIO #-}
foreign import javascript unsafe "$r = {}; $r['type'] = 'MultiLineString';\
                                 \$r['coordinates'] = $2['coordinates'].map(function(r){return r.map($1);});"
    js_mapMultiLineStringIO :: (Callback (Vector n x -> Vector n x)) -> MultiLineString n x -> IO (MultiLineString n x)

{-# INLINE js_foldMultiLineStringIO #-}
foreign import javascript unsafe "$3['coordinates'].reduce(function(rr,r){return r.reduce($1,rr);},$2)"
    js_foldMultiLineStringIO :: Callback (a -> Vector n x -> a) -> JSVal -> MultiLineString n x -> IO JSVal



----------------------------------------------------------------------------------------------------
-- JS Callbacks
----------------------------------------------------------------------------------------------------

{-# INLINE syncCallbackUnsafe1 #-}
syncCallbackUnsafe1 :: (a -> b) -> IO (Callback (a -> b))
syncCallbackUnsafe1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)

{-# INLINE syncCallbackUnsafe2 #-}
syncCallbackUnsafe2 :: (a -> b -> c) -> IO (Callback (a -> b -> c))
syncCallbackUnsafe2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)

{-# INLINE js_syncCallbackApplyReturnUnsafe #-}
foreign import javascript unsafe
  "h$makeCallbackApply($1, h$runSyncReturnUnsafe, [false], $2)"
  js_syncCallbackApplyReturnUnsafe :: Int -> Any -> IO (Callback f)
