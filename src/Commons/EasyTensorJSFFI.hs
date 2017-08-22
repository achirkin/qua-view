{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Commons.EasyTensorJSFFI
    ( dataFrameToTransferable
    , transferableToDataFrame
    , Transferable
    ) where


import Commons.Import
import Numeric.DataFrame
import Numeric.DataFrame.IO
import GHCJS.DOM.Types
import Unsafe.Coerce (unsafeCoerce)
import JavaScript.Array (JSArray)
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances

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



