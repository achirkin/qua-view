{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Commons.NoReflex.EasyTensorJSFFI
    ( dataFrameToTransferable
    , transferableToDataFrame
    , Transferable
    , unsafeSubArrayFreeze, unsafeSubArray, unsafeArrayThaw
    , applyTransformDF
    ) where


import Commons.NoReflex.Import
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits
import GHCJS.DOM.Types (GObject (..), IsGObject)
import GHCJS.Types (jsval)
import Unsafe.Coerce (unsafeCoerce)
import JavaScript.Array (JSArray)
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import JavaScript.Object
import System.IO.Unsafe (unsafePerformIO)

instance ToJSVal    (IODataFrame t ns) where
    toJSVal = pure . coerce
instance FromJSVal  (IODataFrame t ns) where
    fromJSValUnchecked = pure . coerce
    fromJSVal = pure . fmap (coerce :: JSVal -> IODataFrame t ns) . nullableToMaybe . Nullable
instance PToJSVal   (IODataFrame t ns) where
    pToJSVal = coerce
instance PFromJSVal (IODataFrame t ns) where
    pFromJSVal = coerce

instance PToJSVal (DataFrame t (n':ns :: [Nat])) where
    pToJSVal = unsafeCoerce
instance PFromJSVal (DataFrame t (n':ns :: [Nat])) where
    pFromJSVal = unsafeCoerce

instance ToJSVal    (DataFrame t (n':ns :: [Nat])) where
    toJSVal = pure . pToJSVal
instance FromJSVal  (DataFrame t (n':ns :: [Nat])) where
    fromJSValUnchecked = pure . pFromJSVal
    fromJSVal = pure . fmap (pFromJSVal :: JSVal -> DataFrame t (n':ns :: [Nat]))
                     . nullableToMaybe . Nullable

instance ToJSVal (SomeIODataFrame t '[N n, XN k]) where
    toJSVal (SomeIODataFrame (df :: IODataFrame t ns))
      | (Evidence :: Evidence ([n,m] ~ ns)) <- unsafeCoerce (Evidence @(ns ~ ns)) = do
        o <- create
        unsafeSetProp "n"  (pToJSVal (dimVal' @m)) o
        unsafeSetProp "df" (coerce df) o
        return $ jsval o

instance (KnownDim n, ElemTypeInference t)
      => FromJSVal (SomeIODataFrame t '[N n, XN k]) where
    fromJSVal jsv = do
      let o = unsafeCoerce jsv
      maybeM <- someIntNatVal . pFromJSVal <$> unsafeGetProp "n" o
      case maybeM of
        Nothing -> return Nothing
        Just (SomeIntNat (_ :: Proxy m)) -> case inferNumericFrame @t @'[n,m] of
          Evidence -> do
            df <- coerce <$> unsafeGetProp "df" o :: IO (IODataFrame t '[n, m])
            return . Just $ SomeIODataFrame df

instance ToJSVal (SomeIODataFrame t '[N n1, N n2, XN k]) where
    toJSVal (SomeIODataFrame (df :: IODataFrame t ns))
      | (Evidence :: Evidence ([n1,n2,m] ~ ns)) <- unsafeCoerce (Evidence @(ns ~ ns)) = do
        o <- create
        unsafeSetProp "n"  (pToJSVal (dimVal' @m)) o
        unsafeSetProp "df" (coerce df) o
        return $ jsval o

instance (KnownDim n1, KnownDim n2, ElemTypeInference t)
      => FromJSVal (SomeIODataFrame t '[N n1, N n2, XN k]) where
    fromJSVal jsv = do
      let o = unsafeCoerce jsv
      maybeM <- someIntNatVal . pFromJSVal <$> unsafeGetProp "n" o
      case maybeM of
        Nothing -> return Nothing
        Just (SomeIntNat (_ :: Proxy m)) -> case inferNumericFrame @t @'[n1,n2,m] of
          Evidence -> do
            df <- coerce <$> unsafeGetProp "df" o :: IO (IODataFrame t '[n1,n2,m])
            return . Just $ SomeIODataFrame df



instance PToJSVal t => PToJSVal (DataFrame t ('[] :: [Nat])) where
    pToJSVal = pToJSVal . unScalar
instance PFromJSVal t => PFromJSVal (DataFrame t ('[] :: [Nat])) where
    pFromJSVal = scalar . pFromJSVal

instance ToJSVal t => ToJSVal    (DataFrame t ('[] :: [Nat])) where
    toJSVal = toJSVal . unScalar
instance FromJSVal t => FromJSVal (DataFrame t ('[] :: [Nat])) where
    fromJSValUnchecked = fmap scalar . fromJSValUnchecked
    fromJSVal = fmap (fmap scalar) . fromJSVal

instance ToJSON (Vector t n) where
    toJSON = js_vecToJSArray . pToJSVal
foreign import javascript unsafe "Array.prototype.slice.call($1)"
    js_vecToJSArray :: JSVal -> Value

instance FromJSON (Vector t n) where
    parseJSON = withArray "JS number array" (pure . pFromJSVal . js_wrapFloat32Array)
foreign import javascript unsafe "new Float32Array($1)"
    js_wrapFloat32Array :: JSArray -> JSVal


dataFrameToTransferable :: IODataFrame t ns -> IO Transferable
dataFrameToTransferable = unsafeCoerce <$> arrayBuffer

transferableToDataFrame :: Transferable -> IO MutableArrayBuffer
transferableToDataFrame = pure . unsafeCoerce

-- | This data type can be used to do zero-copy transfer to and from WebWorkers
newtype Transferable = Transferable GObject
    deriving (PFromJSVal, PToJSVal, FromJSVal, ToJSVal, IsGObject)



foreign import javascript unsafe "$1.subarray($2,$3)"
    js_unsafeSubArrayFreeze :: IODataFrame t asbs -> Int -> Int -> IO JSVal

-- | Index subarray allowing to take only a part of the first indexing dimension.
unsafeSubArrayFreeze :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat])
                   . ( ConcatList as (b :+ bs) asbs
                     , Dimensions (b :+ bs)
                     , Dimensions (as +: b')
                     , Dimensions as
                     )
                   => IODataFrame t asbs -> Idx (b :+ bs) -> IO (DataFrame t (as +: b'))
unsafeSubArrayFreeze df i
  | off <- fromEnum i * dimVal (dim @as)
  = unsafeCoerce <$> js_unsafeSubArrayFreeze df off (off + totalDim (Proxy @(as +: b')))

-- | Index subarray allowing to take only a part of the first indexing dimension.
unsafeSubArray :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat])
                . ( ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  , Dimensions (as +: b')
                  , Dimensions as
                  )
               => IODataFrame t asbs -> Idx (b :+ bs) -> IO (IODataFrame t (as +: b'))
unsafeSubArray df i
  | off <- fromEnum i * dimVal (dim @as)
  = unsafeCoerce <$> js_unsafeSubArrayFreeze df off (off + totalDim (Proxy @(as +: b')))



unsafeArrayThaw :: DataFrame t (a :+ as) -> IO (IODataFrame t (a :+ as))
unsafeArrayThaw df = pure (unsafeCoerce df)

-- | Apply matrix transform on dataframes.
applyTransformDF :: Mat44f -- ^ transform matrix
                 -> IODataFrame Float (4 :+ ns) -- ^ source matrix
                 -> IODataFrame Float (4 :+ ns') -- ^ destination matrix
                 -> IO ()
applyTransformDF m src dst = case unsafePerformIO $ unsafeArrayThaw m of
    m' -> m' `seq` src `seq` dst `seq` js_applyTransform m' src dst


-- I had to use very weird initializator i = -4 in order to workaround crazy js code builder.
-- the problem was that the code i +=4 is erazed from the code after $1.set command.
foreign import javascript unsafe
    "var n = $2.length, i = -4, v;\
    \ while(i < n - 4){\
    \   i = i + 4;\
    \   v = [0,0,0,0];\
    \   for(var j = 0; j < 4; j++){\
    \     for(var k = 0; k < 4; k++){\
    \       v[j] += $2[i+k] * $1[j + 4*k];\
    \     }\
    \   }\
    \   $3.set(v,i);\
    \ }"
    js_applyTransform :: IODataFrame Float '[4,4]     -- ^ transform matrix
                      -> IODataFrame Float (4 :+ ns)  -- ^ source matrix
                      -> IODataFrame Float (4 :+ ns')  -- ^ destination matrix
                      -> IO ()


