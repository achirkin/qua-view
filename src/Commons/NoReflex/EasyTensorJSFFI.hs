{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Commons.NoReflex.EasyTensorJSFFI
    ( dataFrameToTransferable
    , transferableToDataFrame
    , Transferable
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
      | (Evidence :: Evidence ([n,m] ~ ns)) <- unsafeCoerce (Evidence :: Evidence ()) = do
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
      | (Evidence :: Evidence ([n1,n2,m] ~ ns)) <- unsafeCoerce (Evidence :: Evidence ()) = do
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



