{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Workers
   ( JSFFI.DedicatedWorkerGlobalScope
   , create, JSFFI.postMessage, postMessage', postMsgWithBuf
   , onMessage, dfToArrayBuffer
#ifdef ISWORKER
   , getSelf
#endif
   ) where



import Commons
import Numeric.DataFrame.IO

import qualified GHCJS.DOM.Types as JSFFI
import Data.Time.Clock.POSIX
import qualified Data.JSString as JSString
import qualified GHCJS.DOM.EventM as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.DedicatedWorkerGlobalScope as JSFFI
import qualified JavaScript.Web.Worker as WebWorker
import Unsafe.Coerce (unsafeCoerce)


-- | Create a WebWorker
create :: MonadIO m => JSString -> m JSFFI.DedicatedWorkerGlobalScope
create name = liftIO $ do
    t <- getCurrentTime
    unsafeCoerce <$> WebWorker.create $ name `JSString.append` "?" `JSString.append` JSString.pack (show $ utcTimeToPOSIXSeconds t)


postMessage' :: (ToJSVal msg, MonadIO m) => JSFFI.DedicatedWorkerGlobalScope -> msg -> m ()
postMessage' w m = JSFFI.postMessage w m ([] :: [JSFFI.ArrayBuffer])

postMsgWithBuf :: MonadIO m => JSFFI.DedicatedWorkerGlobalScope -> JSString -> JSFFI.ArrayBuffer -> m ()
postMsgWithBuf w msg buf = do
    o <- liftIO $ js_msgWithBuf msg buf
    JSFFI.postMessage w o [buf]

onMessage :: (MonadIO m, MonadLogger m) => JSFFI.DedicatedWorkerGlobalScope -> (JSFFI.MessageEvent -> ReaderT LoggerFunc IO ()) -> m (IO ())
onMessage w msgCallback = do
    f <- askLogger
    liftIO $ JSFFI.on w JSFFI.message (ReaderT $ \msg -> runReaderT (msgCallback msg) f)




-- message :: EventName DedicatedWorkerGlobalScope MessageEvent
-- on :: (IsEventTarget t, IsEvent e) => t -> EventName t e -> EventM t e () -> IO (IO ())


dfToArrayBuffer :: IODataFrame t ds -> IO JSFFI.ArrayBuffer
dfToArrayBuffer = unsafeCoerce . arrayBuffer


#ifdef ISWORKER
-- | Get context of a current worker
getSelf :: MonadIO m => m JSFFI.DedicatedWorkerGlobalScope
getSelf = liftIO js_getSelf

foreign import javascript unsafe "$r = self;" js_getSelf :: IO JSFFI.DedicatedWorkerGlobalScope
#endif

foreign import javascript unsafe "[$1,$2]" js_msgWithBuf :: JSString -> JSFFI.ArrayBuffer -> IO JSVal

