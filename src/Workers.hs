{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Workers
   ( JSFFI.DedicatedWorkerGlobalScope
   , WorkerDef (..), workerLS
   , create, postMessage, postMessage'
   , onMessage
#ifdef ISWORKER
   , getSelf, execWorkerConduit
#else
   , runWorker
#endif
   ) where



import Commons

import qualified GHCJS.DOM.Types as JSFFI
import Data.Time.Clock.POSIX
import qualified Data.JSString as JSString
import qualified GHCJS.DOM.EventM as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.DedicatedWorkerGlobalScope as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.MessageEvent as JSFFI
import qualified JavaScript.Web.Worker as WebWorker
import Unsafe.Coerce (unsafeCoerce)
#ifdef ISWORKER
import Control.Concurrent.Chan
import Data.Conduit
#else
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class
#endif


-- | Create a WebWorker
create :: MonadIO m => JSString -> m JSFFI.DedicatedWorkerGlobalScope
create name = liftIO $ do
    t <- getCurrentTime
    unsafeCoerce <$> WebWorker.create $ name
                       `JSString.append` "?"
                       `JSString.append` JSString.pack (show $ utcTimeToPOSIXSeconds t)

-- | Send a message without attachments
postMessage :: (ToJSVal msg, MonadIO m) => JSFFI.DedicatedWorkerGlobalScope -> msg -> m ()
postMessage w m = JSFFI.postMessage w m ([] :: [Transferable])

-- | Send a message with transferrable attachments.
--   The message must refer to content of the attachments to get them back in an onMessage event.
postMessage' :: (ToJSVal msg, MonadIO m)
             => JSFFI.DedicatedWorkerGlobalScope -> msg -> [Transferable] -> m ()
postMessage' = JSFFI.postMessage

-- | Primitive message handler.
--   Returns a release callback (to be called after the worker finishes its work).
onMessage :: (FromJSVal msg, MonadIO m, MonadLogger m)
          => JSFFI.DedicatedWorkerGlobalScope -> (msg -> ReaderT LoggerFunc IO ()) -> m (IO ())
onMessage w msgCallback = do
    f <- askLogger
    liftIO $ JSFFI.on w JSFFI.message
        ( ReaderT $ \msg -> runReaderT (JSFFI.getData msg
                                         >>= liftIO . fromJSValUnchecked
                                         >>= msgCallback) f
        )


#ifdef ISWORKER
-- | Get context of a current worker
getSelf :: MonadIO m => m JSFFI.DedicatedWorkerGlobalScope
getSelf = liftIO js_getSelf

foreign import javascript unsafe "$r = self;" js_getSelf :: IO JSFFI.DedicatedWorkerGlobalScope
#endif

-- | Worker definition
data WorkerDef = WorkerDef
  { workerName :: JSString
  , workerUrl  :: JSString
  }

-- | Write logs from a worker
workerLS :: WorkerDef -> LogSource
workerLS = LogSource . workerName


#ifdef ISWORKER
-- | Execute worker process in current WebWorker thread
execWorkerConduit :: (FromJSVal inMsg, ToJSVal outMsg, MonadIO m, MonadLogger m)
                  => WorkerDef
                  -> ConduitM inMsg (outMsg, [Transferable]) m a
                  -> m a
execWorkerConduit wd pipe = do
    inCh <- liftIO newChan
    w <- getSelf
    release <- liftIO . JSFFI.on w JSFFI.message . ReaderT $ \msg ->
            JSFFI.getData msg >>= liftIO . fromJSValUnchecked >>= liftIO . writeChan inCh
    let msgSource = yieldM (liftIO (readChan inCh)) >> msgSource
        msgSink = await >>= \mmsg -> case mmsg of
            Nothing -> return ()
            Just (m, bs)  -> lift (postMessage' w m bs) >> msgSink
    logDebug (workerLS wd) ("I am initialized" :: JSString)
    r <- runConduit $ msgSource =$= pipe `fuseUpstream` msgSink
    logDebug (workerLS wd) ("I have finished" :: JSString)
    liftIO release
    return r
#else
-- | Run worker in a dedicated WebWorker thread
runWorker :: ( ToJSVal inMsg, FromJSVal outMsg
             , MonadIO m, Reflex t
             , TriggerEvent t m
             , PerformEvent t m
             , MonadIO (Performable m)
             )
          => WorkerDef
          -> Event t (inMsg, [Transferable])
          -> m (Event t outMsg)
runWorker wd inEvs = do
    w <- create $ workerUrl wd
    performEvent_ $ uncurry (postMessage' w) <$> inEvs
    newEventWithLazyTriggerWithOnComplete $ \performEv -> do
        releaseRef <- JSFFI.on w JSFFI.message . ReaderT $ \msg ->
          JSFFI.getData msg >>= fromJSValUnchecked >>= flip performEv (pure ())
        return $ releaseRef >> WebWorker.terminate (unsafeCoerce w)
#endif


