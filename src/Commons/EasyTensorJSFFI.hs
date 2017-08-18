{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Commons.EasyTensorJSFFI
    ( dataFrameToTransferable
    , transferableToDataFrame
    , Transferable
    ) where


import Commons.Import
import Numeric.DataFrame.IO
import GHCJS.DOM.Types
import Unsafe.Coerce (unsafeCoerce)

instance ToJSVal    (IODataFrame t ns) where
    toJSVal = pure . coerce
instance FromJSVal  (IODataFrame t ns) where
    fromJSValUnchecked = pure . coerce
    fromJSVal = pure . fmap (coerce :: JSVal -> IODataFrame t ns) . nullableToMaybe . Nullable
instance PToJSVal   (IODataFrame t ns) where
    pToJSVal = coerce
instance PFromJSVal (IODataFrame t ns) where
    pFromJSVal = coerce

dataFrameToTransferable :: IODataFrame t ns -> IO Transferable
dataFrameToTransferable = unsafeCoerce <$> arrayBuffer

transferableToDataFrame :: Transferable -> IO MutableArrayBuffer
transferableToDataFrame = pure . unsafeCoerce

-- | This data type can be used to do zero-copy transfer to and from WebWorkers
newtype Transferable = Transferable GObject
    deriving (PFromJSVal, PToJSVal, FromJSVal, ToJSVal, IsGObject)
