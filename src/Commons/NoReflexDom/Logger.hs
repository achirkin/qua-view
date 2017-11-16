{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Commons.NoReflexDom.Logger
    ( module Commons.NoReflex.Logger
    , logMsgEvents', logMsgEvents
    , logDebugEvents  , logInfoEvents  , logWarnEvents  , logErrorEvents
    , logDebugEvents' , logInfoEvents' , logWarnEvents' , logErrorEvents'
    ) where



import Reflex.PerformEvent.Class
import Commons.NoReflexDom.Import
import Commons.NoReflex.Logger

-- | Log a message and show an attached JS value
logMsgEvents' :: ( ToJSString msg
                 , ToJSVal attachment
                 , MonadLogger m
                 , PerformEvent t m
                 , MonadIO (Performable m)
                 )
              => LogLevel -> LogSource -> Event t (msg, Maybe attachment) -> m ()
logMsgEvents' ll ls ea = do
    f <- askLogger
    performEvent_ $ (\(msg,ma) -> liftIO $ mapM toJSVal ma >>= f ll ls (toJSString msg)) <$> ea


-- | Log a message
logMsgEvents :: ( ToJSString msg
                , MonadLogger m
                , PerformEvent t m
                , MonadIO (Performable m)
                )
             =>  LogLevel -> LogSource -> Event t msg -> m ()
logMsgEvents ll ls ea = do
    f <- askLogger
    performEvent_ $ (\msg -> liftIO $ f ll ls (toJSString msg) Nothing) <$> ea


logDebugEvents' :: ( ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m
                   , MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 3
logDebugEvents' _ _ = pure ()
#else
logDebugEvents' = logMsgEvents' LevelDebug
#endif
{-# INLINE logDebugEvents' #-}

logDebugEvents :: ( ToJSString msg, MonadLogger m, PerformEvent t m
                  , MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 3
logDebugEvents _ _ = pure ()
#else
logDebugEvents = logMsgEvents LevelDebug
#endif
{-# INLINE logDebugEvents #-}

logInfoEvents' :: ( ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m
                  , MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 2
logInfoEvents' _ _ = pure ()
#else
logInfoEvents' = logMsgEvents' LevelInfo
#endif
{-# INLINE logInfoEvents' #-}

logInfoEvents :: ( ToJSString msg, MonadLogger m, PerformEvent t m
                 , MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 2
logInfoEvents _ _ = pure ()
#else
logInfoEvents = logMsgEvents LevelInfo
#endif
{-# INLINE logInfoEvents #-}

logWarnEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m
                  , MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 1
logWarnEvents' _ _ = pure ()
#else
logWarnEvents' = logMsgEvents' LevelWarn
#endif
{-# INLINE logWarnEvents' #-}

logWarnEvents :: ( ToJSString msg, MonadLogger m, PerformEvent t m
                 , MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 1
logWarnEvents _ _ = pure ()
#else
logWarnEvents = logMsgEvents LevelWarn
#endif
{-# INLINE logWarnEvents #-}

logErrorEvents' :: ( ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m
                   , MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 0
logErrorEvents' _ _ = pure ()
#else
logErrorEvents' = logMsgEvents' LevelError
#endif
{-# INLINE logErrorEvents' #-}

logErrorEvents :: ( ToJSString msg, MonadLogger m, PerformEvent t m
                  , MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 0
logErrorEvents _ _ = pure ()
#else
logErrorEvents = logMsgEvents LevelError
#endif
{-# INLINE logErrorEvents #-}

