{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Commons.Logger
    ( LogLevel(..), LogOutput (..), LogSource (..), LoggerFunc
    , MonadLogger (..), logMsg', logMsg
    , logDebug  , logInfo  , logWarn  , logError , logUser
    , logDebug' , logInfo' , logWarn' , logError', logUser'
#ifndef ISWORKER
    , WidgetWithLogs, logMsgEvents', logMsgEvents
    , logDebugEvents  , logInfoEvents  , logWarnEvents  , logErrorEvents , logUserEvents
    , logDebugEvents' , logInfoEvents' , logWarnEvents' , logErrorEvents', logUserEvents'
    , makeWidgetLogger
#endif
    , stdOutLogger
    ) where


import Control.Monad.Trans.Reader
import Data.String (IsString (..))
#ifdef ISWORKER
import Data.Conduit
#else
import Reflex.PerformEvent.Class
#endif

import Commons.Import

-- | Logging levels from 0 to 3.
--   One can use CPP flag '-DLOGLEVEL=n' to set up verbosity of console logging at compile time.
--   LevelDebug is the most verbose level and corresponds to 0;
--   LevelError is for printing only errors and corresponds to level 3.
--
--   Setting CPP flag to a verbosity level 4 or above turns the console logging completely off.
--   Default log level is LevelWarn (2).
data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Eq, Ord, Enum, Show, Read)


#ifndef LOGLEVEL
#define LOGLEVEL 2
#endif


-- | Output channel for the logs.
--   Console output is meant for developers and uses compile-time flags to set logging level.
--   Widget output is meant for users and normally writes all messages independent of logging level.
data LogOutput
  = OutConsole
  | OutWidget
  deriving (Eq, Show, Read)


newtype LogSource = LogSource JSString
  deriving (PToJSVal, ToJSVal, ToJSString, IsString, Show, Eq)

-- | A callback to send logging messages to a browser console or some widget.
type LoggerFunc = LogOutput -> LogLevel -> LogSource -> JSString -> Maybe JSVal -> IO ()

#ifndef ISWORKER
-- | A widget wrapped into reader transformer that can log stuff.
type WidgetWithLogs x = ReaderT LoggerFunc (Widget x)
#endif

class MonadIO m => MonadLogger m where
    askLogger :: m LoggerFunc

-- | Log a message and show an attached JS value
logMsg' :: (ToJSString msg, ToJSVal attachment, MonadLogger m)
        => LogOutput -> LogLevel -> LogSource -> msg -> attachment -> m ()
logMsg' lo ll ls msg a = do
    f <- askLogger
    liftIO $ toJSVal a >>= f lo ll ls (toJSString msg) . Just

-- | Log a message
logMsg :: (ToJSString msg,  MonadLogger m)
       => LogOutput -> LogLevel -> LogSource -> msg -> m ()
logMsg lo ll ls msg = do
    f <- askLogger
    liftIO $ f lo ll ls (toJSString msg) Nothing

instance MonadIO m => MonadLogger (ReaderT LoggerFunc m) where
    askLogger = ask

#ifdef ISWORKER
instance MonadLogger m => MonadLogger (ConduitM i o m) where
    askLogger = lift askLogger
#endif


logDebug' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL >= 1
logDebug' _ _ _ = pure ()
#else
logDebug' = logMsg' OutConsole LevelDebug
#endif
{-# INLINE logDebug' #-}

logDebug :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL >= 1
logDebug _ _ = pure ()
#else
logDebug = logMsg OutConsole LevelDebug
#endif
{-# INLINE logDebug #-}

logInfo' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL >= 2
logInfo' _ _ _ = pure ()
#else
logInfo' = logMsg' OutConsole LevelInfo
#endif
{-# INLINE logInfo' #-}

logInfo :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL >= 2
logInfo _ _ = pure ()
#else
logInfo = logMsg OutConsole LevelInfo
#endif
{-# INLINE logInfo #-}

logWarn' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL >= 3
logWarn' _ _ _ = pure ()
#else
logWarn' = logMsg' OutConsole LevelWarn
#endif
{-# INLINE logWarn' #-}

logWarn :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL >= 3
logWarn _ _ = pure ()
#else
logWarn = logMsg OutConsole LevelWarn
#endif
{-# INLINE logWarn #-}

logError' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL >= 4
logError' _ _ _ = pure ()
#else
logError' = logMsg' OutConsole LevelError
#endif
{-# INLINE logError' #-}

logError :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL >= 4
logError _ _ = pure ()
#else
logError = logMsg OutConsole LevelError
#endif
{-# INLINE logError #-}

logUser' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
         => msg -> attachment -> m ()
logUser' = logMsg' OutWidget LevelInfo (LogSource "User message")
{-# INLINE logUser' #-}


logUser :: (ToJSString msg, MonadLogger m)
        => msg  -> m ()
logUser = logMsg OutWidget LevelInfo (LogSource "User message")
{-# INLINE logUser #-}



#ifndef ISWORKER
-- | Log a message and show an attached JS value
logMsgEvents' :: ( ToJSString msg
                 , ToJSVal attachment
                 , MonadLogger m
                 , PerformEvent t m
                 , MonadIO (Performable m)
                 )
              => LogOutput -> LogLevel -> LogSource -> Event t (msg, Maybe attachment) -> m ()
logMsgEvents' lo ll ls ea = do
    f <- askLogger
    performEvent_ $ (\(msg,ma) -> liftIO $ mapM toJSVal ma >>= f lo ll ls (toJSString msg)) <$> ea


-- | Log a message
logMsgEvents :: ( ToJSString msg
                , MonadLogger m
                , PerformEvent t m
                , MonadIO (Performable m)
                )
             => LogOutput -> LogLevel -> LogSource -> Event t msg -> m ()
logMsgEvents lo ll ls ea = do
    f <- askLogger
    performEvent_ $ (\msg -> liftIO $ f lo ll ls (toJSString msg) Nothing) <$> ea


logDebugEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL >= 1
logDebugEvents' _ _ = pure ()
#else
logDebugEvents' = logMsgEvents' OutConsole LevelDebug
#endif
{-# INLINE logDebugEvents' #-}

logDebugEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL >= 1
logDebugEvents _ _ = pure ()
#else
logDebugEvents = logMsgEvents OutConsole LevelDebug
#endif
{-# INLINE logDebugEvents #-}

logInfoEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL >= 2
logInfoEvents' _ _ = pure ()
#else
logInfoEvents' = logMsgEvents' OutConsole LevelInfo
#endif
{-# INLINE logInfoEvents' #-}

logInfoEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL >= 2
logInfoEvents _ _ = pure ()
#else
logInfoEvents = logMsgEvents OutConsole LevelInfo
#endif
{-# INLINE logInfoEvents #-}

logWarnEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL >= 3
logWarnEvents' _ _ = pure ()
#else
logWarnEvents' = logMsgEvents' OutConsole LevelWarn
#endif
{-# INLINE logWarnEvents' #-}

logWarnEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL >= 3
logWarnEvents _ _ = pure ()
#else
logWarnEvents = logMsgEvents OutConsole LevelWarn
#endif
{-# INLINE logWarnEvents #-}

logErrorEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL >= 4
logErrorEvents' _ _ = pure ()
#else
logErrorEvents' = logMsgEvents' OutConsole LevelError
#endif
{-# INLINE logErrorEvents' #-}

logErrorEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL >= 4
logErrorEvents _ _ = pure ()
#else
logErrorEvents = logMsgEvents OutConsole LevelError
#endif
{-# INLINE logErrorEvents #-}

logUserEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => Event t (msg, Maybe attachment) -> m ()
logUserEvents' = logMsgEvents' OutWidget LevelInfo (LogSource "User message")
{-# INLINE logUserEvents' #-}

logUserEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
              => Event t msg -> m ()
logUserEvents = logMsgEvents OutWidget LevelInfo (LogSource "User message")
{-# INLINE logUserEvents #-}
#endif













#ifndef ISWORKER
-- | Supply a customized widget logging callback to get a proper `LoggerFunc`.
makeWidgetLogger :: (LogLevel -> JSString -> Maybe JSVal -> IO ()) -> LoggerFunc
makeWidgetLogger _ OutConsole ll ls msg mv = logToConsole ll ls msg mv
#if LOGLEVEL >= 1
makeWidgetLogger f OutWidget  ll _ msg mv = f ll msg mv
#else
makeWidgetLogger f OutWidget  ll ls msg mv = logToConsole ll ls msg mv >> f ll msg mv
#endif
#endif

-- | Default logger function that sends all messages to console.
stdOutLogger :: LoggerFunc
stdOutLogger _ = logToConsole


-- | A wrapper on top of pure JavaScript functions console.log,
--   printing a message into a browser console.
logToConsole :: LogLevel -> LogSource -> JSString -> Maybe JSVal -> IO ()
#if LOGLEVEL < 4
logToConsole loglevel (LogSource source) msg Nothing
    | fromEnum loglevel >= LOGLEVEL
    = js_logToConsole (logLevelToJSString loglevel) source msg
logToConsole loglevel (LogSource source) msg (Just val)
    | fromEnum loglevel >= LOGLEVEL
    = js_logToConsoleJSVal (logLevelToJSString loglevel) source msg val
#endif
logToConsole _ _ _ _ = pure ()

#if LOGLEVEL < 4
logLevelToJSString :: LogLevel -> JSString
logLevelToJSString LevelDebug = "Debug"
logLevelToJSString LevelInfo  = "Info"
logLevelToJSString LevelWarn  = "Warn"
logLevelToJSString LevelError = "Error"

foreign import javascript unsafe "console.log((new Date()).toTimeString().substr(0,8) + ' [' + $1 + '](' + $2 + '): ' + $3);"
    js_logToConsole :: JSString -- ^ log level
                    -> JSString -- ^ component name
                    -> JSString -- ^ message
                    -> IO ()

foreign import javascript unsafe "console.log((new Date()).toTimeString().substr(0,8) + ' [' + $1 + '](' + $2 + '): ' + $3, $4);"
    js_logToConsoleJSVal :: JSString -- ^ log level
                         -> JSString -- ^ component name
                         -> JSString -- ^ message
                         -> JSVal    -- ^ Attached object
                         -> IO ()
#endif

