{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
#ifndef ISWORKER
{-# LANGUAGE RecursiveDo     #-}
#endif
module Workers
   ( JSFFI.DedicatedWorkerGlobalScope
   , WebWorker (terminate), WorkerDef (..), workerLS
   , create, postMessage, postMessage', onMessage
#ifdef ISWORKER
   , getSelf, execWorkerConduit
   , module Workers.Commons
#else
   , runWorkerDyn, runWorker, withAbsoluteUrl
#endif
   ) where


import Data.Time.Clock.POSIX
import Data.Char (isAlphaNum)
import Data.JSString as JSString
import Control.Exception.Base (catch)
import Control.Monad ((>=>))
import Control.Monad.Trans.Except
import GHCJS.Foreign.Callback (Callback, releaseCallback, asyncCallback1)
import GHCJS.Prim (JSException (..))
import qualified GHCJS.DOM.Types as JSFFI
import qualified GHCJS.DOM.EventM as JSFFI (on)
import qualified GHCJS.DOM.JSFFI.Generated.DedicatedWorkerGlobalScope as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.MessageEvent as JSFFI
import Language.Haskell.TH
#ifdef ISWORKER
import Commons.NoReflex
import Workers.Commons
import Control.Concurrent.Chan
#else
import Commons
import qualified GHCJS.DOM.JSFFI.Generated.ErrorEvent as JSFFI
import qualified QuaTypes
import System.FilePath ((</>))
import Reflex
#endif

-- | Web Worker handle together with a function to release its resources
data WebWorker = WebWorker
  { handle    :: !JSFFI.DedicatedWorkerGlobalScope
  , terminate :: !(IO ())
    -- ^ Release webworker callbacks and terminate it
  }

-- | Worker definition
data WorkerDef = WorkerDef
  { workerName :: JSString
  , workerUrl  :: JSString
  }

-- | Write logs from a worker
workerLS :: WorkerDef -> LogSource
workerLS = LogSource . ("Workers." <>) . workerName


-- | Create a WebWorker
create :: (MonadIO m, FromJSVal msg)
       => JSString
       -> (msg -> IO ())
          -- ^ onmessage callback provides 'data' field of message
       -> (JSFFI.ErrorEvent -> IO ())
          -- ^ onerror callback provides error event
       -> ExceptT JSException m WebWorker
create name onmsg onerr= ExceptT . liftIO $ do
    onmsgCbk <- asyncCallback1 $ JSFFI.getData . coerce >=> fromJSValUnchecked >=> onmsg
    onerrCbk <- asyncCallback1 $ onerr . coerce
    let releaseCbks = releaseCallback onmsgCbk >> releaseCallback onerrCbk
    eW <- (Right <$> create' onmsgCbk onerrCbk) `catch` (pure . Left)
    case eW of
      Left err -> Left err <$ releaseCbks
      Right w -> pure . Right . WebWorker w $ releaseCbks >> js_terminate w
  where
    create' = js_create $ $(do
      t <- round . utcTimeToPOSIXSeconds <$> runIO getCurrentTime :: Q Int
      let ts = pure . LitE . StringL $ '?' : toB64 (show t)
          toB64 [] = []
          toB64 [c]  = [c]
          toB64 (a:b:cs) = toB (read [a,b] :: Int) ++ toB64 cs
          toB n | isAlphaNum c = [c]
                | otherwise = show n
            where
              c = toEnum $ 48 + mod n 74
      [e| flip JSString.append $(ts) |]
      ) name


-- | Send a message without attachments
postMessage :: (ToJSVal msg, MonadIO m) => WebWorker -> msg -> m ()
postMessage w m = JSFFI.postMessage (handle w) m ([] :: [Transferable])

-- | Send a message with transferrable attachments.
--   The message must refer to content of the attachments to get them back in an onMessage event.
postMessage' :: (ToJSVal msg, MonadIO m)
             => WebWorker -> msg -> [Transferable] -> m ()
postMessage' = JSFFI.postMessage . handle

-- | Primitive message handler.
--   Returns a release callback (to be called after the worker finishes its work).
onMessage :: (FromJSVal msg, MonadIO m, MonadLogger m)
          => WebWorker -> (msg -> ReaderT LoggerFunc IO ()) -> m (IO ())
onMessage w msgCallback = do
    f <- askLogger
    liftIO $ JSFFI.on (handle w) JSFFI.message
        ( ReaderT $
            liftIO . ( JSFFI.getData >=> fromJSValUnchecked >=> flip runReaderT f . msgCallback )
        )


#ifdef ISWORKER
-- | Get context of a current worker
getSelf :: MonadIO m => m WebWorker
getSelf = (\h -> WebWorker h (pure ())) <$> liftIO js_getSelf

foreign import javascript unsafe "$r = self;" js_getSelf :: IO JSFFI.DedicatedWorkerGlobalScope

-- | Execute worker process in current WebWorker thread
execWorkerConduit :: (FromJSVal inMsg, ToJSVal outMsg, MonadIO m, MonadLogger m)
                  => WorkerDef
                  -> ConduitM inMsg (outMsg, [Transferable]) m a
                  -> m a
execWorkerConduit wd pipe = do
    inCh <- liftIO newChan
    w <- getSelf
    release <- liftIO . JSFFI.on (handle w) JSFFI.message . ReaderT $
            liftIO . (JSFFI.getData >=> fromJSValUnchecked >=> writeChan inCh)
    let msgSource = yieldM (liftIO (readChan inCh)) >> msgSource
        msgSink = await >>= \mmsg -> case mmsg of
            Nothing -> return ()
            Just (m, bs)  -> lift (postMessage' w m bs) >> msgSink
    logDebug @JSString (workerLS wd) "The worker is initialized."
    r <- runConduit $ msgSource =$= pipe `fuseUpstream` msgSink
    logDebug @JSString (workerLS wd) "The worker has succesfully finished its job."
    liftIO release
    return r
#else

-- | Run worker in a dedicated WebWorker thread
runWorker :: ( ToJSVal inMsg, FromJSVal outMsg
             , Reflex t
             , TriggerEvent t (QuaViewT isWriting t m)
             , PerformEvent t (QuaViewT isWriting t m)
             , MonadSample t (QuaViewT isWriting t m)
             , MonadHold t (QuaViewT isWriting t m)
             , MonadIO (QuaViewT isWriting t m)
             , MonadIO (Performable m)
             , MonadFix (QuaViewT isWriting t m)
             , MonadLogger  (QuaViewT isWriting t m)
             , MonadLogger  (Performable (QuaViewT isWriting t m))
             , Applicative m
             , QuaViewTrans isWriting
             )
          => WorkerDef
          -> Event t (inMsg, [Transferable])
          -> QuaViewT isWriting t m (Event t outMsg)
runWorker wd inEvs = withAbsoluteUrl wd >>= \dwd -> runWorkerDyn dwd inEvs


-- | Re-create a worker and associated callbacks every time settings updated
runWorkerDyn ::
             ( ToJSVal inMsg, FromJSVal outMsg
             , Reflex t
             , TriggerEvent t (QuaViewT isWriting t m)
             , PerformEvent t (QuaViewT isWriting t m)
             , MonadSample t (QuaViewT isWriting t m)
             , MonadHold t (QuaViewT isWriting t m)
             , MonadIO (QuaViewT isWriting t m)
             , MonadIO (Performable m)
             , MonadFix (QuaViewT isWriting t m)
             , MonadLogger  (QuaViewT isWriting t m)
             , MonadLogger  (Performable (QuaViewT isWriting t m))
             )
          => Dynamic t WorkerDef
          -> Event t (inMsg, [Transferable])
          -> QuaViewT isWriting t m (Event t outMsg)
runWorkerDyn wdDyn inEvs = do
    -- receive messages here
    (outMsgE, outMsgCallback) <- newTriggerEvent
    -- and also react to errors
    (errE, errCallback) <- newTriggerEvent
    -- create workers
    worker1 <- sample (current wdDyn) >>= create' outMsgCallback errCallback
    rec workersE <- performEvent $ cleanAndCreate outMsgCallback errCallback
                                <$> workersB <@> updated wdDyn
        workersB <- hold worker1 workersE
    -- send messages
    performEvent_ $ postMessage'' <$> current wdDyn <*> workersB <@> inEvs
    -- show errors messages
    performEvent_ $ postErrorMsg <$> errE
    -- messages from worker
    return outMsgE
  where
    create' evCbk errCbk wd = do
      wE <-runExceptT $ create (workerUrl wd) evCbk (\e -> errCbk (wd,e))
      case wE of
        Left  (JSException jsv msg) -> Nothing <$ logWarn' (workerLS wd) msg jsv
        Right w -> return $ Just w
    cleanAndCreate evCbk errCbk Nothing wd = create' evCbk errCbk wd
    cleanAndCreate evCbk errCbk (Just oldW) wd
        = liftIO (terminate oldW) >> create' evCbk errCbk wd
    postMessage'' wd Nothing _
      = logWarn @JSString (workerLS wd) "The worker does not exist at this moment."
    postMessage'' _ (Just w) (msg, atts)
      = postMessage' w msg atts
    postErrorMsg (wd,e) = do
      msg <- JSFFI.getMessage e
      fname <- JSFFI.getFilename e
      lno <- JSFFI.getLineno e
      cno <- JSFFI.getColno e
      if msg == ""
      then logWarn @JSString (workerLS wd) $
           "Got an error without message, likely to be a url resolving error (url = "
           <> workerUrl wd <> ")."
      else logWarn @String (workerLS wd) $
           "Got an error (" <> fname <> (':':show lno) <> (':':show cno) <> "): " <> msg

-- | Update worker definition dynamically in case if js root url changes.
withAbsoluteUrl :: ( MonadIO (QuaViewT isWriting t m), Reflex t
                   , PerformEvent t (QuaViewT isWriting t m)
                   , MonadHold t (QuaViewT isWriting t m)
                   , MonadIO (Performable (QuaViewT isWriting t m))
                   , QuaViewTrans isWriting
                   , Applicative m
                   )
                => WorkerDef
                -> QuaViewT isWriting t m (Dynamic t WorkerDef)
withAbsoluteUrl wd = fmap f <$> (quaSettings >>= onlyChanges . fmap QuaTypes.jsRootUrl)
  where
    f url = wd{ workerUrl = pack $ unpack url </> unpack (workerUrl wd) }

#endif


foreign import javascript safe
  "$r = new Worker($1); $r.addEventListener(\"message\", $2); $r.addEventListener(\"error\", $3);"
     js_create :: JSString
               -> Callback (JSVal -> IO ()) -- ^ onmessage events
               -> Callback (JSVal -> IO ()) -- ^ onerror events
               -> IO JSFFI.DedicatedWorkerGlobalScope

foreign import javascript unsafe
  "$1.terminate()" js_terminate :: JSFFI.DedicatedWorkerGlobalScope -> IO ()

