{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}

module Commons.Logger
    ( -- * User-level logging
      --   Show some messages to a client.
      UserProgressCallback (..), UserMessage (..), UserMessageCallback (..)
      -- * Application-level logging
    , LogLevel(LevelError,LevelWarn,LevelInfo,LevelDebug)
    , LogSource (..), LoggerFunc
    , MonadLogger (..), logMsg', logMsg
    , logDebug  , logInfo  , logWarn  , logError --, logUser
    , logDebug' , logInfo' , logWarn' , logError' --, logUser'
#ifndef ISWORKER
    , logMsgEvents', logMsgEvents
    , logDebugEvents  , logInfoEvents  , logWarnEvents  , logErrorEvents
    , logDebugEvents' , logInfoEvents' , logWarnEvents' , logErrorEvents'
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

newtype UserMessageCallback
  = UserMessageCallback { getUMsgCallback :: forall t . UserMessage t -> IO t }

-- | Show some message to a user in a form of a log.
--   It appears in the lower left corner of qua-view, at the bottom of the control panel.
--
--   Use `ProgressMsg` if you want to inform a user about some process state,
--   or `SingleMsg` to just show some text only once.
data UserMessage t where
  SingleMsg :: !JSString -> UserMessage ()
  ProgressMsg :: !JSString -> UserMessage UserProgressCallback

instance IsString (UserMessage ()) where
  fromString = SingleMsg . toJSString

-- I disable this instance to provide better type inference in simple cases
-- when we want to show a message without progress
--instance IsString (UserMessage UserProgressCallback) where
--  fromString = ProgressMsg . toJSString


-- | A pair of callbacks returned from the `ProgressMsgCallback`,
--   i.e. when an initial message is shown.
data UserProgressCallback
  = UserProgressCallback
  { uMsgProgress :: !(JSString -> IO ())
    -- ^ Update content of the user message to that some progress is made.
    --   This callback does update the content only if `uMsgComplete` has not been called yet.
    --   You can call this function as many times as you want, but only until `uMsgComplete` is fired.
  , uMsgComplete :: !(JSString -> IO ())
    -- ^ Update content of the user message to show that the process has finished its execution.
    --   You should call this function only once.
  }


-- | Logging levels from 0 to 4.
--   One can use CPP flag '-DLOGLEVEL=n' to set up verbosity of console logging at compile time.
--   LevelNone turns off any logging and corresponds to level 0.
--   LevelError is for printing only errors and corresponds to level 1.
--   LevelDebug is the most verbose level and corresponds to 4;
--
--   Setting CPP flag to a verbosity level 0 turns the console logging completely off.
--   Default log level is LevelWarn (2).
--   Levels greater than 4 make no sense.
data LogLevel
  = LevelNone
  | LevelError
  | LevelWarn
  | LevelInfo
  | LevelDebug
  deriving (Eq, Ord, Enum, Show, Read)


#ifndef LOGLEVEL
#define LOGLEVEL 2
#endif

newtype LogSource = LogSource JSString
  deriving (PToJSVal, ToJSVal, ToJSString, IsString, Show, Eq)

-- | A callback to send logging messages to a browser console or some widget.
type LoggerFunc = LogLevel -> LogSource -> JSString -> Maybe JSVal -> IO ()


class MonadIO m => MonadLogger m where
    askLogger :: m LoggerFunc

-- | Log a message and show an attached JS value
logMsg' :: (ToJSString msg, ToJSVal attachment, MonadLogger m)
        => LogLevel -> LogSource -> msg -> attachment -> m ()
logMsg' ll ls msg a = do
    f <- askLogger
    liftIO $ toJSVal a >>= f ll ls (toJSString msg) . Just

-- | Log a message
logMsg :: (ToJSString msg,  MonadLogger m)
       => LogLevel -> LogSource -> msg -> m ()
logMsg ll ls msg = do
    f <- askLogger
    liftIO $ f ll ls (toJSString msg) Nothing

instance MonadIO m => MonadLogger (ReaderT LoggerFunc m) where
    askLogger = ask

#ifdef ISWORKER
instance MonadLogger m => MonadLogger (ConduitM i o m) where
    askLogger = lift askLogger
#endif


logDebug' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL <= 3
logDebug' _ _ _ = pure ()
#else
logDebug' = logMsg' LevelDebug
#endif
{-# INLINE logDebug' #-}

logDebug :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL <= 3
logDebug _ _ = pure ()
#else
logDebug = logMsg LevelDebug
#endif
{-# INLINE logDebug #-}

logInfo' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL <= 2
logInfo' _ _ _ = pure ()
#else
logInfo' = logMsg' LevelInfo
#endif
{-# INLINE logInfo' #-}

logInfo :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL <= 2
logInfo _ _ = pure ()
#else
logInfo = logMsg LevelInfo
#endif
{-# INLINE logInfo #-}

logWarn' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL <= 1
logWarn' _ _ _ = pure ()
#else
logWarn' = logMsg' LevelWarn
#endif
{-# INLINE logWarn' #-}

logWarn :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL <= 1
logWarn _ _ = pure ()
#else
logWarn = logMsg LevelWarn
#endif
{-# INLINE logWarn #-}

logError' :: (ToJSString msg,  ToJSVal attachment, MonadLogger m)
          => LogSource -> msg -> attachment -> m ()
#if LOGLEVEL <= 0
logError' _ _ _ = pure ()
#else
logError' = logMsg' LevelError
#endif
{-# INLINE logError' #-}

logError :: (ToJSString msg, MonadLogger m)
          => LogSource -> msg  -> m ()
#if LOGLEVEL <= 0
logError _ _ = pure ()
#else
logError = logMsg LevelError
#endif
{-# INLINE logError #-}


#ifndef ISWORKER
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


logDebugEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 3
logDebugEvents' _ _ = pure ()
#else
logDebugEvents' = logMsgEvents' LevelDebug
#endif
{-# INLINE logDebugEvents' #-}

logDebugEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 3
logDebugEvents _ _ = pure ()
#else
logDebugEvents = logMsgEvents LevelDebug
#endif
{-# INLINE logDebugEvents #-}

logInfoEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 2
logInfoEvents' _ _ = pure ()
#else
logInfoEvents' = logMsgEvents' LevelInfo
#endif
{-# INLINE logInfoEvents' #-}

logInfoEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 2
logInfoEvents _ _ = pure ()
#else
logInfoEvents = logMsgEvents LevelInfo
#endif
{-# INLINE logInfoEvents #-}

logWarnEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 1
logWarnEvents' _ _ = pure ()
#else
logWarnEvents' = logMsgEvents' LevelWarn
#endif
{-# INLINE logWarnEvents' #-}

logWarnEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
              => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 1
logWarnEvents _ _ = pure ()
#else
logWarnEvents = logMsgEvents LevelWarn
#endif
{-# INLINE logWarnEvents #-}

logErrorEvents' :: (ToJSString msg, ToJSVal attachment, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
                => LogSource -> Event t (msg, Maybe attachment) -> m ()
#if LOGLEVEL <= 0
logErrorEvents' _ _ = pure ()
#else
logErrorEvents' = logMsgEvents' LevelError
#endif
{-# INLINE logErrorEvents' #-}

logErrorEvents :: (ToJSString msg, MonadLogger m, PerformEvent t m, MonadIO (Performable m))
               => LogSource -> Event t msg -> m ()
#if LOGLEVEL <= 0
logErrorEvents _ _ = pure ()
#else
logErrorEvents = logMsgEvents LevelError
#endif
{-# INLINE logErrorEvents #-}

#endif










-- | A wrapper on top of pure JavaScript functions console.log,
--   printing a message into a browser console.
stdOutLogger :: LoggerFunc
#if LOGLEVEL >= 1
stdOutLogger loglevel (LogSource source) msg Nothing
    | fromEnum loglevel <= LOGLEVEL
    = js_logToConsole (logLevelToJSString loglevel) source msg
stdOutLogger loglevel (LogSource source) msg (Just val)
    | fromEnum loglevel <= LOGLEVEL
    = js_logToConsoleJSVal (logLevelToJSString loglevel) source msg val
#endif
stdOutLogger _ _ _ _ = return ()

#if LOGLEVEL >= 1
logLevelToJSString :: LogLevel -> JSString
logLevelToJSString LevelDebug = "Debug"
logLevelToJSString LevelInfo  = "Info"
logLevelToJSString LevelWarn  = "Warn"
logLevelToJSString LevelError = "Error"
logLevelToJSString LevelNone  = "None"

foreign import javascript unsafe
    "console.log((new Date()).toTimeString().substr(0,8) + ' [' + $1 + '](' + $2 + '): ' + $3);"
    js_logToConsole :: JSString -- ^ log level
                    -> JSString -- ^ component name
                    -> JSString -- ^ message
                    -> IO ()

foreign import javascript unsafe
    "console.log((new Date()).toTimeString().substr(0,8) + ' [' + $1 + '](' + $2 + '): ' + $3, $4);"
    js_logToConsoleJSVal :: JSString -- ^ log level
                         -> JSString -- ^ component name
                         -> JSString -- ^ message
                         -> JSVal    -- ^ Attached object
                         -> IO ()
#endif

