{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commons.QuaViewMonad
    ( QuaViewTrans (..), QuaWidget, QuaViewM
    , Writing, NoWriting, IsWritingEvents (..)
    , runQuaWidget, quaSettings
    , showUserMessage, showUserPanic
    , registerEvent, askEvent
    , replaceUserMessageCallback, replaceUserPanicCallback
    ) where


import           Control.Concurrent.MVar
import           Control.Monad.Trans.Writer.Lazy
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control
import           Data.IORef
import           GHCJS.DOM.JSFFI.Generated.ParentNode (querySelector)
import           GHCJS.DOM.JSFFI.Generated.Element    (getAttribute)
import           GHCJS.DOM (currentDocument)
import           JavaScript.JSON.Types.Instances (toJSON)
import           Reflex
import           Reflex.Dom
import           System.IO.Unsafe (unsafeInterleaveIO)

import           Commons.Import
import           Commons.Http
import           Commons.NoReflex
import           Commons.NoReflexDom.EventMap
import           Commons.NoReflexDom.QuaViewMonad


-- | Alias to QuaView widget
type QuaWidget t x = QuaViewT Writing t (Widget x)


-- | Try to fetch settings, initialize context and run qua-view inside
runQuaWidget :: QuaWidget (SpiderTimeline Global) x () -> Widget x ()
runQuaWidget m = mdo
  ctx <- initQuaViewContext qEvs
  ((), qEvs) <- runWithCtx m ctx
  return ()


-- | Try to fetch settings and initialize context
initQuaViewContext :: ( MonadIO m, MonadFix m, HasJSContext m
                      , MonadHold (SpiderTimeline Global) m
                      , TriggerEvent (SpiderTimeline Global) m
                      , PerformEvent (SpiderTimeline Global) m
                      , MonadIO (Performable m)
                      , Reflex t
                      ) => QuaViewEvents t -> m (QuaViewContext t)
initQuaViewContext quaViewEvents = do
    -- This is when qua-view starts, so we don't have any loggers and etc. set up.
    -- Following functions are used to log things happening at this initialization step.
    let initLogErr m  = stdOutLogger LevelWarn "initQuaViewContext" m Nothing
        initLogInfo m = stdOutLogger LevelInfo "initQuaViewContext" m
                        . Just . pToJSVal . toJSON
        quaViewLoggerFunc = stdOutLogger

    guessedSettings <- liftIO $ unsafeInterleaveIO guessQuaSettings
    -- try to get settings url from meta tag in an html page
    --   <meta property="qua-view:settingsUrl" content="{settingsUrl}">
    mRequestedSettingsE <- runMaybeT $ do
        doc <- MaybeT currentDocument
        settingsEl  <- MaybeT $ querySelector doc ("meta[property='qua-view:settingsUrl']" :: JSString)
        url <- MaybeT $ getAttribute settingsEl ("content" :: JSString)
        setsE <- liftIO newEmptyMVar
        lift . httpGetNow' url $ putMVar setsE
        MaybeT . liftIO . unsafeInterleaveIO $ takeMVar setsE >>= \e -> case e of
            Left (JSError err) -> Nothing <$ initLogErr ("Error in httpGet: " <> err)
            Right sets -> pure $ Just sets

    -- initial settings: either use the one from settingsUrl, or try to guess some.
    resolvedSettings <- liftIO $ case mRequestedSettingsE of
        Nothing -> guessedSettings <$ initLogInfo "Using guessed settings: " guessedSettings
        Just sets -> sets <$ initLogInfo "Using requested settings: " sets

    let settingAccumulator oldSettings (Right newSettings)
          = pure $ newSettings <> oldSettings
        settingAccumulator oldSettings (Left (JSError errMsg))
          = (oldSettings <$) . liftIO $ initLogErr ("Error in httpGet: " <> errMsg)
    -- for now, we never fire the event of getting new settings,
    --  but I keep quaViewSettings being a dynamic in case we want to change it in future,
    --  because reloadable settings are cool!
    let quaViewSettingsE = never
    quaViewSettings <- accumM settingAccumulator resolvedSettings quaViewSettingsE

    -- log settings updates
    performEvent_ . ffor (updated quaViewSettings)
      $ liftIO . initLogInfo "Updated settings"

    quaViewUserMessageHandlers <- liftIO . newIORef $ UserMessageCallback defaultMsgFun
    quaViewPanicMsgHandler     <- liftIO . newIORef $ defaultMsgFun . SingleMsg

    return QuaViewContext {..}


defaultMsgFun :: UserMessage t -> IO t
defaultMsgFun (SingleMsg msg) = stdOutLogger
    LevelWarn "initQuaViewContext"
    ("Got this message before initialized msg widget: " <> msg)
    Nothing
defaultMsgFun (ProgressMsg msg) = do
  stdOutLogger
    LevelWarn "initQuaViewContext"
    ("Got this message before initialized msg widget: [start] " <> msg)
    Nothing
  return $ let f pref m = stdOutLogger LevelWarn "initQuaViewContext"
                                ("Got this message before initialized msg widget:" <> pref <> m)
                                Nothing
           in UserProgressCallback (f " [progress] ") (f " [finish] ")




instance DomBuilder t m => DomBuilder t (QuaViewT NoWriting t m) where
  type DomBuilderSpace (QuaViewT NoWriting t m) = DomBuilderSpace m
instance (Reflex t, DomBuilder t m) => DomBuilder t (QuaViewT Writing t m) where
  type DomBuilderSpace (QuaViewT Writing t m) = DomBuilderSpace m
  element t cfg child = do
    (relem, (a,evs)) <- liftWith $ \runInner -> element t cfg $ runInner child
    QuaViewT . lift $ tell evs
    return (relem, a)
  selectElement cfg child = do
    (relem, (a,evs)) <- liftWith $ \runInner -> selectElement cfg $ runInner child
    QuaViewT . lift $ tell evs
    return (relem, a)



instance HasJSContext m => HasJSContext (QuaViewT NoWriting t m) where
  type JSContextPhantom (QuaViewT NoWriting t m) = JSContextPhantom m
  askJSContext = QuaViewT' askJSContext
instance (Reflex t, HasJSContext m) => HasJSContext (QuaViewT Writing t m) where
  type JSContextPhantom (QuaViewT Writing t m) = JSContextPhantom m
  askJSContext = quaViewT askJSContext


instance HasDocument m => HasDocument (QuaViewT NoWriting t m)
instance (Reflex t, HasDocument m) => HasDocument (QuaViewT Writing t m)
