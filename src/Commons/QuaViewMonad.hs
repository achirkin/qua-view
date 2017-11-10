{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Commons.QuaViewMonad
    ( QuaViewT (), QuaViewM, QuaWidget
    , runQuaWidget, quaSettings
    , showUserMessage, showUserPanic
    , replaceUserMessageCallback, replaceUserPanicCallback
    , hoistQuaView
    ) where


import           Control.Concurrent.MVar
import           Control.Monad ((>=>))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           GHCJS.DOM.JSFFI.Generated.ParentNode (querySelector)
import           GHCJS.DOM.JSFFI.Generated.Element    (getAttribute)
import           GHCJS.DOM (currentDocument)
import           JavaScript.JSON.Types.Instances (toJSON)
import qualified QuaTypes
import           Reflex
import           Reflex.Dom
import           System.IO.Unsafe (unsafeInterleaveIO)

import           Commons.Import
import           Commons.Local
import           Commons.Logger
import           Commons.Http

-- | Qua-view monad
--
--   Provides facilities for:
--
--     * Logging
--     * Showing informational messages to a user
--     * Providing qua-view environment settings
newtype QuaViewT t m a = QuaViewT { unQuaViewT :: ReaderT (QuaViewContext t) m a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO, HasDocument
           , TriggerEvent t, MonadSample t, MonadHold t)


-- | Alias to QuaView transformer monad on top of IO
type QuaViewM t = QuaViewT t IO

-- | Alias to QuaView widget
type QuaWidget t x = QuaViewT t (Widget x)

data QuaViewContext t = QuaViewContext
  { quaViewLoggerFunc          :: LoggerFunc
  , quaViewUserMessageHandlers :: IORef UserMessageCallback
  , quaViewPanicMsgHandler     :: IORef (JSString -> IO ())
  , quaViewSettings            :: Dynamic t QuaTypes.Settings
  }


-- | Try to fetch settings, initialize context and run qua-view inside
runQuaWidget :: QuaWidget (SpiderTimeline Global) x () -> Widget x ()
runQuaWidget m = initQuaViewContext >>= runReaderT (unQuaViewT m)

-- | Get settings behavior
quaSettings :: Monad m => QuaViewT t m (Dynamic t QuaTypes.Settings)
quaSettings = QuaViewT $ quaViewSettings <$> ask

-- | Replace a monad inside QuaView transformer
hoistQuaView :: (m a -> n b) -> QuaViewT t m a -> QuaViewT t n b
hoistQuaView h q = QuaViewT . ReaderT $ h . runReaderT (unQuaViewT q)

-- | Try to fetch settings and initialize context
initQuaViewContext :: ( MonadIO m, MonadFix m, HasJSContext m
                      , MonadHold (SpiderTimeline Global) m
                      , TriggerEvent (SpiderTimeline Global) m
                      , PerformEvent (SpiderTimeline Global) m
                      , MonadIO (Performable m)
                      , Reflex t
                      ) => m (QuaViewContext t)
initQuaViewContext = do
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


-- | Display a message for a user
showUserMessage :: forall msgRetType t m
                 . MonadIO m
                => UserMessage msgRetType -> QuaViewT t m msgRetType
showUserMessage msg = QuaViewT $ fmap quaViewUserMessageHandlers ask
                               >>= liftIO . (readIORef >=> ($ msg) . getUMsgCallback)

-- | Display a fatal error explanation to a user.
--   The program is not supposed to work after this message is shown.
showUserPanic :: MonadIO m
              => JSString -> QuaViewT t m ()
showUserPanic msg = QuaViewT $ fmap quaViewPanicMsgHandler ask
                             >>= liftIO . (readIORef >=> ($ msg))


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT a = fmap (fromMaybe a) . runMaybeT

-- | Normally, we should call this function only once: to set up user message widget
replaceUserMessageCallback :: MonadIO m
                           => UserMessageCallback -> QuaViewT t m ()
replaceUserMessageCallback cb = QuaViewT $ fmap quaViewUserMessageHandlers ask
                              >>= liftIO . flip writeIORef cb


-- | Normally, we should call this function only once: to set up a crash alert
replaceUserPanicCallback :: MonadIO m
                         => (JSString -> IO ()) -> QuaViewT t m ()
replaceUserPanicCallback cb = QuaViewT $ fmap quaViewPanicMsgHandler ask
                              >>= liftIO . flip writeIORef cb



instance MonadIO m => MonadLogger (QuaViewT t m) where
  askLogger = QuaViewT $ quaViewLoggerFunc <$> ask

instance MonadTrans (QuaViewT t) where
  lift = QuaViewT . lift

instance MonadTransControl (QuaViewT t) where
  type StT (QuaViewT t) a = a
  liftWith f = QuaViewT . ReaderT $ \r -> f $ \t -> runReaderT (unQuaViewT t) r
  restoreT = QuaViewT . restoreT

instance MonadAdjust t m => MonadAdjust t (QuaViewT t m) where
  runWithReplace a0 = QuaViewT . runWithReplace (unQuaViewT a0) . fmap unQuaViewT
  traverseDMapWithKeyWithAdjust f dm0 =
    QuaViewT . traverseDMapWithKeyWithAdjust (\a -> unQuaViewT . f a) dm0
  traverseDMapWithKeyWithAdjustWithMove f dm0 =
    QuaViewT . traverseDMapWithKeyWithAdjustWithMove (\a -> unQuaViewT . f a) dm0


instance DomBuilder t m => DomBuilder t (QuaViewT t m) where
  type DomBuilderSpace (QuaViewT t m) = DomBuilderSpace m

instance PostBuild t m => PostBuild t (QuaViewT t m) where
  getPostBuild = lift getPostBuild

instance PerformEvent t m => PerformEvent t (QuaViewT t m) where
  type Performable (QuaViewT t m) = QuaViewT t (Performable m)
  performEvent_ e = QuaViewT $ do
    r <- ask
    lift $ performEvent_ $ flip runReaderT r . unQuaViewT <$> e
  performEvent e = QuaViewT $ do
    r <- ask
    lift $ performEvent $ flip runReaderT r . unQuaViewT <$> e

instance HasJSContext m => HasJSContext (QuaViewT t m) where
  type JSContextPhantom (QuaViewT t m) = JSContextPhantom m
  askJSContext = QuaViewT askJSContext


-- | Try our best to guess default settings for qua-view operation.
--
--   * Get window.location.href as the viewUrl
--   * Get all optionals to be Nothing
--   * Try to get js root as a folder containing qua-view.js
--
guessQuaSettings :: IO QuaTypes.Settings
guessQuaSettings = do
    viewUrl <- js_getViewUrl
    jsRootUrl <- fromMaybeT viewUrl $ do
        doc <- MaybeT currentDocument
        settingsEl  <- MaybeT $ querySelector doc ("script[src*='qua-view.js']" :: JSString)
        qvSrc <- MaybeT $ getAttribute settingsEl ("src" :: JSString)
        return $ js_splitFst qvSrc "qua-view.js"
    return QuaTypes.Settings {..}
  where
    loggingUrl = Nothing
    luciUrl = Nothing
    getSubmissionGeometryUrl = Nothing
    postSubmissionUrl = Nothing
    reviewSettingsUrl = Nothing

foreign import javascript unsafe
    "window['location']['href']['split']('?')[0]['split']('#')[0]"
    js_getViewUrl :: IO JSString

foreign import javascript unsafe
    "$1['split']($2)[0]"
    js_splitFst :: JSString -> JSString -> JSString

