{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.JSArray
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- JavaScript Arrays containing elements of a single type.
-- Also provides an interface to derive behavior of arrays (if you do newtype JSVal and assume it contains an array).
-- For array-like structures instanciate `LikeJSArray` class.
-- For elements of arrays instanciate `LikeJS` class.
--
-----------------------------------------------------------------------------

module Data.JSArray
    ( JSArray (), emptyJSArray
    , LikeJSArray (..), LikeJS (..)
    , jsjoin
    , jsmap, jsmapi, jsmapSame, jsmapIO, jsmapIO_, jsmapiIO, jsmapiIO_
    , jsfoldl, jsfoldl1, jsfoldr, jsfoldr1, jsfoldi, jsfoldi1
    , jsfoldIO, jsfoldIO_, jsfoldiIO, jsfoldiIO_
    , jszip, jszipi, jszipIO, jszipIO_, jszipiIO, jszipiIO_
    , jsunionZip, jsunionZipIO, jsunionZipIO_
    , fromList, toList
    , jslength, (!), jsslice, jstake, jsdrop, jsconcat
    , jsfilter, jsmapEither
    ) where

-- for instances of LikeJS
import Data.Int
import Data.Word
import Foreign.C.Types
import Data.Geometry.VectorMath --(Vector )

-- everything else that I use in this module
import Data.Foldable (foldr')
import Data.JSString (JSString, pack, unpack')
import Data.Coerce (Coercible (), coerce)
import GHC.Exts (Any)
import GHCJS.Foreign.Callback (Callback, OnBlocked (..), releaseCallback, syncCallback1, syncCallback2, syncCallback3)
import GHCJS.Types (JSVal, IsJSVal)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (void)

instance LikeJS (Vector n x)
instance LikeJS (Matrix n x)
instance (LikeJS a) => LikeJSArray (Vector n a) where
    type JSArrayElem (Vector n a) = a
instance (LikeJS a) => LikeJSArray (Matrix n a) where
    type JSArrayElem (Matrix n a) = a


-- | JavaScript array containing elements of single type
newtype (LikeJS a) => JSArray a = JSArray JSVal
instance IsJSVal (JSArray a)
instance LikeJS (JSArray a)
instance (LikeJS a) => LikeJSArray (JSArray a) where
    type JSArrayElem (JSArray a) = a
    {-# INLINE toJSArray #-}
    toJSArray = id
    {-# INLINE fromJSArray #-}
    fromJSArray = id

-- | This says that the data type is a JS Array
class (LikeJS (JSArrayElem a)) => LikeJSArray a where
    type JSArrayElem a
    toJSArray :: a -> JSArray (JSArrayElem a)
    fromJSArray :: JSArray (JSArrayElem a) -> a

    {-# INLINE toJSArray #-}
    default toJSArray :: Coercible a JSVal => a -> JSArray (JSArrayElem a)
    toJSArray = coerce
    {-# INLINE fromJSArray #-}
    default fromJSArray :: Coercible JSVal a => JSArray (JSArrayElem a) -> a
    fromJSArray = unsafeCoerce


-- | This type can be passed into JSArray.
--   Default implementation works for anything coercible to JSVal
class LikeJS a where
    asJSVal :: a -> JSVal
    asLikeJS :: JSVal -> a

    {-# INLINE asJSVal #-}
    default asJSVal :: Coercible a JSVal => a -> JSVal
    asJSVal = unsafeCoerce
    {-# INLINE asLikeJS #-}
    default asLikeJS :: Coercible JSVal a => JSVal -> a
    asLikeJS = unsafeCoerce


-- | Convert haskell list into JS array
fromList :: ( LikeJSArray a )
         => [JSArrayElem a] -> a
fromList = fromJSArray . js_ListToJSArray . unsafeCoerce . seqList . map asJSVal

-- | Convert JS array to haskell list
toList :: ( LikeJSArray a )
         => a -> [JSArrayElem a]
toList = map asLikeJS . unsafeCoerce . js_JSArrayToList . toJSArray


{-# NOINLINE jsmap #-}
jsmap :: ( LikeJSArray a
         , LikeJS y)
      => (JSArrayElem a -> y)
      -> a -> JSArray y
jsmap f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- js_mapJSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsmapSame #-}
jsmapSame :: ( LikeJSArray a
             , LikeJS x
             , x ~ JSArrayElem a)
          => (x -> x)
          -> a -> a
jsmapSame f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- fromJSArray <$> js_mapJSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsmapi #-}
jsmapi :: ( LikeJSArray a
         , LikeJS y)
      => (Int -> JSArrayElem a -> y)
      -> a -> JSArray y
jsmapi f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \e i -> asJSVal (f (asLikeJS i) (asLikeJS e))
        r <- js_mapJSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsmapIO #-}
jsmapIO :: ( LikeJSArray a
           , LikeJS y)
        => (JSArrayElem a -> IO y)
        -> a -> IO (JSArray y)
jsmapIO f arr = do
        call <- syncCallbackUnsafeIO1 $ fmap asJSVal . f . asLikeJS
        r <- js_mapJSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsmapIO_ #-}
jsmapIO_ :: LikeJSArray a => (JSArrayElem a -> IO ()) -> a -> IO ()
jsmapIO_ f arr = do
        call <- syncCallback1 ContinueAsync $ f . asLikeJS
        js_mapJSArray_ call (toJSArray arr)
        releaseCallback call

{-# INLINE jsmapiIO #-}
jsmapiIO :: ( LikeJSArray a, LikeJS y) => (Int -> JSArrayElem a -> IO y) -> a -> IO (JSArray y)
jsmapiIO f arr = do
        call <- syncCallbackUnsafeIO2 $ \e i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e))
        r <- js_mapJSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsmapiIO_ #-}
jsmapiIO_ :: LikeJSArray a => (Int -> JSArrayElem a -> IO ()) -> a -> IO ()
jsmapiIO_ f arr = do
        call <- syncCallback2 ContinueAsync $ \e i -> f (asLikeJS i) (asLikeJS e)
        js_mapJSArray_ call (toJSArray arr)
        releaseCallback call


{-# RULES
"jsmap/concat"     forall f g arr . jsmap     f (jsmap g arr)     = jsmap     (f . g) arr
"jsmapSame/concat" forall f g arr . jsmapSame f (jsmapSame g arr) = jsmapSame (f . g) arr
"jsmapSN/concat"   forall f g arr . jsmap     f (jsmapSame g arr) = jsmap     (f . g) arr
"jsmap/toSame"     forall f   arr . fromJSArray (jsmap f arr)     = jsmapSame f arr
    #-}


{-# NOINLINE jsfoldl #-}
jsfoldl :: ( LikeJSArray t
           , LikeJS a)
        => (a -> JSArrayElem t -> a)
        -> a -> t -> a
jsfoldl f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldlJSArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsfoldl1 #-}
jsfoldl1 :: ( LikeJSArray t
            , LikeJS a)
         => (a -> JSArrayElem t -> a)
         -> t -> a
jsfoldl1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldl1JSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsfoldr #-}
jsfoldr :: ( LikeJSArray t
           , LikeJS a)
        => (JSArrayElem t -> a -> a)
        -> a -> t -> a
jsfoldr f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldrJSArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsfoldr1 #-}
jsfoldr1 :: ( LikeJSArray t
            , LikeJS a)
         => (JSArrayElem t -> a -> a)
         -> t -> a
jsfoldr1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \r e -> asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldr1JSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsfoldi #-}
jsfoldi :: ( LikeJSArray t
           , LikeJS a)
        => (Int -> a -> JSArrayElem t -> a)
        -> a -> t -> a
jsfoldi f x0 arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \r e i -> asJSVal (f (asLikeJS i) (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldlJSArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsfoldi1 #-}
jsfoldi1 :: ( LikeJSArray t
            , LikeJS a)
         => (Int -> a -> JSArrayElem t -> a)
         -> t -> a
jsfoldi1 f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \r e i -> asJSVal (f (asLikeJS i) (asLikeJS r) (asLikeJS e))
        r <- asLikeJS <$> js_foldl1JSArray call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsfoldIO #-}
jsfoldIO :: ( LikeJSArray t
            , LikeJS y)
         => (y -> JSArrayElem t -> IO y)
         -> y -> t -> IO y
jsfoldIO f x0 arr = do
        call <- syncCallbackUnsafeIO2 $ \r e -> fmap asJSVal (f (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldlJSArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsfoldIO_ #-}
jsfoldIO_ :: ( LikeJSArray t
             , LikeJS y)
          => (y -> JSArrayElem t -> IO y)
          -> y -> t -> IO ()
jsfoldIO_ f x0 arr = void $ jsfoldIO f x0 arr

{-# INLINE jsfoldiIO #-}
jsfoldiIO :: ( LikeJSArray t
             , LikeJS y)
          => (Int -> y -> JSArrayElem t -> IO y)
          -> y -> t -> IO y
jsfoldiIO f x0 arr = do
        call <- syncCallbackUnsafeIO3 $ \r e i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e) (asLikeJS r))
        r <- asLikeJS <$> js_foldlJSArray call (asJSVal x0) (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# INLINE jsfoldiIO_ #-}
jsfoldiIO_ :: ( LikeJSArray t
              , LikeJS y)
           => (Int -> y -> JSArrayElem t -> IO y)
           -> y -> t -> IO ()
jsfoldiIO_ f x0 arr = void $ jsfoldiIO f x0 arr



{-# RULES
"jsfoldlmap/concat"      forall f g x0 arr . jsfoldl  f x0 (jsmap g arr)     = jsfoldl  (\r   -> f r . g  ) x0 arr
"jsfoldlmapSame/concat"  forall f g x0 arr . jsfoldl  f x0 (jsmapSame g arr) = jsfoldl  (\r   -> f r . g  ) x0 arr
"jsfoldl1map/concat"     forall f g arr    . jsfoldl1 f    (jsmap g arr)     = jsfoldl1 (\r   -> f r . g  )    arr
"jsfoldl1mapSame/concat" forall f g arr    . jsfoldl1 f    (jsmapSame g arr) = jsfoldl1 (\r   -> f r . g  )    arr
"jsfoldrmap/concat"      forall f g x0 arr . jsfoldr  f x0 (jsmap g arr)     = jsfoldr  (\e r -> f r (g e)) x0 arr
"jsfoldrmapSame/concat"  forall f g x0 arr . jsfoldr  f x0 (jsmapSame g arr) = jsfoldr  (\e r -> f r (g e)) x0 arr
"jsfoldr1map/concat"     forall f g arr    . jsfoldr1 f    (jsmap g arr)     = jsfoldr1 (\e r -> f r (g e))    arr
"jsfoldr1mapSame/concat" forall f g arr    . jsfoldr1 f    (jsmapSame g arr) = jsfoldr1 (\e r -> f r (g e))    arr
    #-}


-- zipping

{-# NOINLINE jszip #-}
jszip :: ( LikeJSArray a
         , LikeJSArray b
         , LikeJS y)
      => (JSArrayElem a -> JSArrayElem b -> y)
      -> a -> b -> JSArray y
jszip f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe2 $ \e1 e2 -> asJSVal (f (asLikeJS e1) (asLikeJS e2))
        r <- js_zipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jszipi #-}
jszipi :: ( LikeJSArray a
         , LikeJSArray b
         , LikeJS y)
      => (Int -> JSArrayElem a -> JSArrayElem b -> y)
      -> a -> b -> JSArray y
jszipi f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \e1 e2 i -> asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_zipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE jszipIO #-}
jszipIO :: ( LikeJSArray a
         , LikeJSArray b
         , LikeJS y)
      => (JSArrayElem a -> JSArrayElem b -> IO y)
      -> a -> b -> IO (JSArray y)
jszipIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO2 $ \e1 e2 -> fmap asJSVal (f (asLikeJS e1) (asLikeJS e2))
        r <- js_zipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE jszipiIO #-}
jszipiIO :: ( LikeJSArray a
         , LikeJSArray b
         , LikeJS y)
      => (Int -> JSArrayElem a -> JSArrayElem b -> IO y)
      -> a -> b -> IO (JSArray y)
jszipiIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO3 $ \e1 e2 i -> fmap asJSVal (f (asLikeJS i) (asLikeJS e1) (asLikeJS e2))
        r <- js_zipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r

{-# INLINE jszipIO_ #-}
jszipIO_ :: ( LikeJSArray a
            , LikeJSArray b )
         => (JSArrayElem a -> JSArrayElem b -> IO ())
         -> a -> b -> IO ()
jszipIO_ f arr1 arr2 = do
        call <- syncCallback2 ContinueAsync $ \e1 e2 -> f (asLikeJS e1) (asLikeJS e2)
        js_zipJSArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call

{-# INLINE jszipiIO_ #-}
jszipiIO_ :: ( LikeJSArray a
             , LikeJSArray b )
          => (Int -> JSArrayElem a -> JSArrayElem b -> IO ())
          -> a -> b -> IO ()
jszipiIO_ f arr1 arr2 = do
        call <- syncCallback3 ContinueAsync $ \e1 e2 i -> f (asLikeJS i) (asLikeJS e1) (asLikeJS e2)
        js_zipJSArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call

{-# NOINLINE jsunionZip #-}
jsunionZip :: ( LikeJSArray a
              , LikeJSArray b
              , LikeJS y )
           => (Int -> Maybe (JSArrayElem a) -> Maybe (JSArrayElem b) -> y)
           -> a -> b -> JSArray y
jsunionZip f arr1 arr2 = unsafePerformIO $ do
        call <- syncCallbackUnsafe3 $ \e1 e2 i -> asJSVal (f (asLikeJS i) (may e1) (may e2))
        r <- js_unionZipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r
        where may e = if js_isDefined e then Just (asLikeJS e) else Nothing


{-# INLINE jsunionZipIO #-}
jsunionZipIO :: ( LikeJSArray a
                , LikeJSArray b
                , LikeJS y )
             => (Int -> Maybe (JSArrayElem a) -> Maybe (JSArrayElem b) -> IO y)
             -> a -> b -> IO (JSArray y)
jsunionZipIO f arr1 arr2 = do
        call <- syncCallbackUnsafeIO3 $ \e1 e2 i -> fmap asJSVal (f (asLikeJS i) (may e1) (may e2))
        r <- js_unionZipJSArray call (toJSArray arr1) (toJSArray arr2)
        r `seq` releaseCallback call
        return r
        where may e = if js_isDefined e then Just (asLikeJS e) else Nothing


{-# INLINE jsunionZipIO_ #-}
jsunionZipIO_ :: ( LikeJSArray a
                 , LikeJSArray b )
              => (Int -> Maybe (JSArrayElem a) -> Maybe (JSArrayElem b) -> IO ())
              -> a -> b -> IO ()
jsunionZipIO_ f arr1 arr2 = do
        call <- syncCallback3 ContinueAsync $ \e1 e2 i -> f (asLikeJS i) (may e1) (may e2)
        js_unionZipJSArray_ call (toJSArray arr1) (toJSArray arr2)
        releaseCallback call
        where may e = if js_isDefined e then Just (asLikeJS e) else Nothing


instance (Show a, LikeJS a) => Show (JSArray a) where
    show = unpack' . js_show . jsmap (pack . show)

{-# RULES
"show/JSStringArray" show = unpack' . js_show
    #-}







instance (LikeJS a, LikeJS b) => LikeJS (Either a b) where
    {-# NOINLINE asJSVal #-}
    asJSVal (Left  x) = js_ToLeft $ asJSVal x
    asJSVal (Right x) = js_ToRight $ asJSVal x
    {-# NOINLINE asLikeJS #-}
    asLikeJS jsv = if js_IsEitherRight jsv
                   then Right . asLikeJS $ js_EitherVal jsv
                   else Left  . asLikeJS $ js_EitherVal jsv

{-# INLINE js_ToLeft #-}
foreign import javascript unsafe "[false,$1]" js_ToLeft :: JSVal -> JSVal
{-# INLINE js_ToRight #-}
foreign import javascript unsafe "[true,$1]" js_ToRight :: JSVal -> JSVal
{-# INLINE js_IsEitherRight #-}
foreign import javascript unsafe "$1[0]" js_IsEitherRight :: JSVal -> Bool
{-# INLINE js_EitherVal #-}
foreign import javascript unsafe "$1[1]" js_EitherVal :: JSVal -> JSVal

instance LikeJS a => LikeJS (Maybe a) where
    {-# INLINE asJSVal #-}
    asJSVal Nothing = js_ToNothing
    asJSVal (Just x) = js_ToJust $ asJSVal x
    {-# INLINE asLikeJS #-}
    asLikeJS jsv = if js_IsNothing jsv
                   then Nothing
                   else Just . asLikeJS $ js_FromJust jsv

{-# INLINE js_ToNothing #-}
foreign import javascript unsafe "[]" js_ToNothing :: JSVal
{-# INLINE js_IsNothing #-}
foreign import javascript unsafe "$r = ($1 != null && $1.length == 1) ? true : false"
    js_IsNothing :: JSVal -> Bool
{-# INLINE js_ToJust #-}
foreign import javascript unsafe "[$1]" js_ToJust :: JSVal -> JSVal
{-# INLINE js_FromJust #-}
foreign import javascript unsafe "$1[0]" js_FromJust :: JSVal -> JSVal

instance LikeJS a => LikeJS [a] where
    {-# INLINE [1] asJSVal #-}
    asJSVal = asJSVal . (fromList :: [a] -> JSArray a)
    {-# INLINE [1] asLikeJS #-}
    asLikeJS = (toList :: JSArray a -> [a]) . asLikeJS

{-# RULES
"asJSVal/String"   asJSVal = js_toJSString . unsafeCoerce . seqList :: String -> JSVal
"asLikeJS/String" asLikeJS = unsafeCoerce . js_fromJSString :: JSVal -> String
    #-}

-- convert to / from JSVal
#define LIKEJS(T) \
    foreign import javascript unsafe "$r = $1" \
        js_fromJSVal/**/T :: JSVal -> T;  {-# INLINE js_fromJSVal/**/T #-}; \
    foreign import javascript unsafe "$r = $1" \
        js_toJSVal/**/T :: T -> JSVal;  {-# INLINE js_toJSVal/**/T #-}; \
    instance LikeJS T where { \
        asJSVal = js_toJSVal/**/T; {-# INLINE asJSVal #-}; \
        asLikeJS = js_fromJSVal/**/T; {-# INLINE asLikeJS #-}; }

LIKEJS(JSString)
LIKEJS(Bool)
LIKEJS(Char)

LIKEJS(Int)
LIKEJS(Int32)
LIKEJS(Int16)
LIKEJS(Int8)
LIKEJS(Word)
LIKEJS(Word32)
LIKEJS(Word16)
LIKEJS(Word8)
LIKEJS(Float)
LIKEJS(Double)
LIKEJS(CChar)
LIKEJS(CSChar)
LIKEJS(CUChar)
LIKEJS(CShort)
LIKEJS(CUShort)
LIKEJS(CInt)
LIKEJS(CUInt)
LIKEJS(CLong)
LIKEJS(CULong)
LIKEJS(CFloat)
LIKEJS(CDouble)


-- mapping

{-# INLINE js_mapJSArray #-}
foreign import javascript unsafe "$2.map(h$retIfDef($1))"
    js_mapJSArray :: Callback f -> JSArray a -> IO (JSArray b)


{-# INLINE js_mapJSArray_ #-}
foreign import javascript unsafe "$2.forEach(h$doIfDef($1))"
    js_mapJSArray_ :: Callback f -> JSArray a -> IO ()


-- folding

{-# INLINE js_foldlJSArray #-}
foreign import javascript unsafe "$3.reduce(h$retIfDef2oa($1),$2)"
    js_foldlJSArray :: Callback f -> JSVal -> JSArray a -> IO JSVal

{-# INLINE js_foldl1JSArray #-}
foreign import javascript unsafe "$2.reduce(h$retIfDef2oa($1))"
    js_foldl1JSArray :: Callback f -> JSArray a -> IO JSVal

{-# INLINE js_foldrJSArray #-}
foreign import javascript unsafe "$3.reduceRight(h$retIfDef2oa($1),$2)"
    js_foldrJSArray :: Callback f -> JSVal -> JSArray a -> IO JSVal

{-# INLINE js_foldr1JSArray #-}
foreign import javascript unsafe "$2.reduceRight(h$retIfDef2oa($1))"
    js_foldr1JSArray :: Callback f -> JSArray a -> IO JSVal


-- zipping

{-# INLINE js_zipJSArray #-}
foreign import javascript unsafe "if($3){var f = h$retIfDef2($1); $r = $2.map(function(e,i){return f(e,$3[i],i);});}else{$r = [];}"
    js_zipJSArray :: Callback f -> JSArray a -> JSArray b -> IO (JSArray c)

{-# INLINE js_zipJSArray_ #-}
foreign import javascript unsafe "if($3){var f = h$retIfDef2($1); $2.forEach(function(e,i){f(e,$3[i],i);});}"
    js_zipJSArray_ :: Callback f -> JSArray a -> JSArray b -> IO ()

{-# INLINE js_unionZipJSArray #-}
foreign import javascript unsafe "var le = $2 || [], ri = $3 || []; var n = Math.max(le.length, ri.length); $r = new Array(n); for(var i = 0; i < n; i++){$r[i] = $1(le[i],ri[i],i);}"
    js_unionZipJSArray :: Callback f -> JSArray a -> JSArray b -> IO (JSArray c)

{-# INLINE js_unionZipJSArray_ #-}
foreign import javascript unsafe "var le = $2 || [], ri = $3 || []; var n = Math.max(le.length, ri.length); for(var i = 0; i < n; i++){$1(le[i],ri[i],i);}"
    js_unionZipJSArray_ :: Callback f -> JSArray a -> JSArray b -> IO ()

-- filtering

{-# NOINLINE jsfilter #-}
jsfilter :: ( LikeJSArray a )
         => (JSArrayElem a -> Bool)
         -> a -> a
jsfilter f arr = unsafePerformIO $ do
        call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
        r <- fromJSArray <$> js_filter call (toJSArray arr)
        r `seq` releaseCallback call
        return r

{-# NOINLINE jsmapEither #-}
jsmapEither :: ( LikeJSArray a
               , LikeJS x
               , LikeJS y)
            => (JSArrayElem a -> Either x y)
            -> a -> (JSArray x, JSArray y)
jsmapEither f arr = unsafePerformIO $ do
    call <- syncCallbackUnsafe1 $ asJSVal . f . asLikeJS
    r <- call `seq` js_mapEither call (toJSArray arr)
    r `seq` releaseCallback call
    return r

{-# INLINE js_filter #-}
foreign import javascript unsafe "$2.filter($1)"
    js_filter :: (Callback (a -> Bool)) -> JSArray a -> IO (JSArray a)

{-# INLINE js_mapEither #-}
foreign import javascript unsafe "var rez = $2.map($1); $r1 = rez.filter(function(e){return !e[0];}).map(function(e){return e[1];}); $r2 = rez.filter(function(e){return  e[0];}).map(function(e){return e[1];});"
    js_mapEither :: (Callback (a -> Either b c)) -> JSArray a -> IO (JSArray b, JSArray c)


-- callbacks

{-# INLINE syncCallbackUnsafe1 #-}
syncCallbackUnsafe1 :: (JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO1 #-}
syncCallbackUnsafeIO1 :: (JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)

{-# INLINE syncCallbackUnsafe2 #-}
syncCallbackUnsafe2 :: (JSVal -> JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO2 #-}
syncCallbackUnsafeIO2 :: (JSVal -> JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)

{-# INLINE syncCallbackUnsafe3 #-}
syncCallbackUnsafe3 :: (JSVal -> JSVal -> JSVal -> JSVal) -> IO (Callback f)
syncCallbackUnsafe3 x = js_syncCallbackApplyReturnUnsafe 3 (unsafeCoerce x)
{-# INLINE syncCallbackUnsafeIO3 #-}
syncCallbackUnsafeIO3 :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO (Callback f)
syncCallbackUnsafeIO3 x = js_syncCallbackApplyReturnUnsafe 3 (unsafeCoerce x)


{-# INLINE js_syncCallbackApplyReturnUnsafe #-}
foreign import javascript unsafe
  "h$makeCallbackApply($1, h$runSyncReturnUnsafe, [false], $2)"
  js_syncCallbackApplyReturnUnsafe :: Int -> Any -> IO (Callback f)

{-# INLINE js_show #-}
foreign import javascript unsafe "JSON.stringify($1)"
  js_show :: JSArray a -> JSString

{-# INLINE js_fromJSString #-}
foreign import javascript unsafe "h$toHsString($1)"
  js_fromJSString :: JSVal -> Any

{-# INLINE js_toJSString #-}
foreign import javascript unsafe "h$fromHsString($1)"
  js_toJSString :: Any -> JSVal

{-# INLINE js_JSArrayToList #-}
foreign import javascript unsafe "h$JSArrayToList($1)"
  js_JSArrayToList :: JSArray a -> Any

{-# INLINE js_ListToJSArray #-}
foreign import javascript unsafe "h$ListToJSArray($1)"
  js_ListToJSArray :: Any -> JSArray a

-- reduce the spine and all list elements to whnf
seqList :: [a] -> [a]
seqList xs = foldr' seq () xs `seq` xs

{-# INLINE js_isDefined #-}
foreign import javascript unsafe "h$isDefined($1)"
  js_isDefined :: JSVal -> Bool


----------------------------------------------------------------------------------------------------
-- custom functions
----------------------------------------------------------------------------------------------------

{-# INLINE jslength #-}
jslength :: LikeJSArray a => a -> Int
jslength = js_length . toJSArray

{-# INLINE js_length #-}
foreign import javascript unsafe "$1.length"
    js_length :: JSArray a -> Int

-- | index JS array
(!) :: LikeJSArray a => a -> Int -> JSArrayElem a
(!) arr = asLikeJS . js_index (toJSArray arr)

{-# INLINE js_index #-}
foreign import javascript unsafe "$1[$2]"
    js_index  :: JSArray a -> Int -> JSVal

{-# INLINE jsslice #-}
jsslice :: LikeJSArray a => Int -> Int -> a -> a
jsslice a b = fromJSArray . js_slice a b . toJSArray

{-# INLINE jstake #-}
jstake :: LikeJSArray a => Int -> a -> a
jstake n = fromJSArray . js_slice 0 n . toJSArray

{-# INLINE jsdrop #-}
jsdrop :: LikeJSArray a => Int -> a -> a
jsdrop n = fromJSArray . js_slice1 n . toJSArray

{-# INLINE js_slice #-}
foreign import javascript unsafe "$3.slice($1,$2)"
    js_slice :: Int -> Int -> JSArray a -> JSArray a

{-# INLINE js_slice1 #-}
foreign import javascript unsafe "$2.slice($1)"
    js_slice1 :: Int -> JSArray a -> JSArray a

-- | Concatenate two JS arrays
{-# INLINE jsconcat #-}
jsconcat :: LikeJSArray a => a -> a -> a
jsconcat a = fromJSArray . js_concat (toJSArray a) . toJSArray

{-# INLINE js_concat #-}
foreign import javascript unsafe "$1.concat($2)"
    js_concat :: JSArray a -> JSArray a -> JSArray a

-- | Concatenate array of arrays into single array
{-# INLINE jsjoin #-}
foreign import javascript unsafe "[].concat.apply([], $1)"
    jsjoin :: JSArray (JSArray a) -> JSArray a

{-# INLINE emptyJSArray #-}
foreign import javascript unsafe "[]"
    emptyJSArray ::JSArray a

