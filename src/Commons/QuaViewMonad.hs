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
module Commons.QuaViewMonad
    ( QuaViewTrans (..), QuaWidget, QuaViewM
    , Writing, NoWriting, IsWritingEvents
    , runQuaWidget, quaSettings
    , showUserMessage, showUserPanic
    , registerEvent, askEvent
    , replaceUserMessageCallback, replaceUserPanicCallback
    ) where


import           Control.Concurrent.MVar
import           Control.Monad ((>=>))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.Lazy
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control
import qualified Data.Dependent.Map as DMap (singleton, findWithDefault)
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
import           Commons.Events


data Writing
data NoWriting

data IsWritingEvents isWriting where
  WritingEvents :: IsWritingEvents Writing
  NoWritingEvents :: IsWritingEvents NoWriting

class QuaViewTrans isWriting where
    -- | Qua-view monad
    --
    --   Provides facilities for:
    --
    --     * Logging
    --     * Showing informational messages to a user
    --     * Providing qua-view environment settings
    data QuaViewT isWriting t (m :: * -> *) a
    -- | `lift` operation of Monad.Trans
    quaViewT :: Functor m => m a -> QuaViewT isWriting t m a
    -- | synonim for `ask` operation of Monad.Trans.Reader
    askContext :: Applicative m => QuaViewT isWriting t m (QuaViewContext t)
    -- | Replace a monad inside QuaView transformer
    hoistQuaView :: (m (StT (QuaViewT isWriting t) a) -> n (StT (QuaViewT isWriting t) b))
                -> QuaViewT isWriting t m a -> QuaViewT isWriting t n b
    -- | Check if this is a writing events transformer at runtime
    isWritingEvents :: IsWritingEvents isWriting



instance QuaViewTrans Writing where
  newtype QuaViewT Writing t m a = QuaViewT
    { unQuaViewT :: ReaderT (QuaViewContext t) (WriterT (QuaViewEvents t) m) a }
  quaViewT = QuaViewT . ReaderT . const . WriterT . fmap (flip (,) noEvents)
  {-# INLINE quaViewT #-}
  askContext = QuaViewT . ReaderT $ \c -> WriterT (pure (c, noEvents))
  {-# INLINE askContext #-}
  hoistQuaView h q = QuaViewT . ReaderT $ \r -> WriterT . h . runWriterT $ runReaderT (unQuaViewT q) r
  {-# INLINE hoistQuaView #-}
  isWritingEvents = WritingEvents
  {-# INLINE isWritingEvents #-}

runWithCtx :: QuaViewT Writing t m a -> QuaViewContext t -> m (a, QuaViewEvents t)
runWithCtx m = runWriterT . runReaderT (unQuaViewT m)

instance QuaViewTrans NoWriting where
  newtype QuaViewT NoWriting t m a = QuaViewT'
    { unQuaViewT' :: ReaderT (QuaViewContext t) m a }
  quaViewT = QuaViewT' . ReaderT . const
  {-# INLINE quaViewT #-}
  askContext = QuaViewT' $ ReaderT pure
  {-# INLINE askContext #-}
  hoistQuaView h q = QuaViewT' . ReaderT $ h . runReaderT (unQuaViewT' q)
  {-# INLINE hoistQuaView #-}
  isWritingEvents = NoWritingEvents
  {-# INLINE isWritingEvents #-}


-- | Alias to QuaView transformer monad on top of IO and basic Reflex
type QuaViewM t = QuaViewT Writing t (PerformEventT t (SpiderHost Global))

-- | Alias to QuaView widget
type QuaWidget t x = QuaViewT Writing t (Widget x)

data QuaViewContext t = QuaViewContext
  { quaViewLoggerFunc          :: LoggerFunc
  , quaViewUserMessageHandlers :: IORef UserMessageCallback
  , quaViewPanicMsgHandler     :: IORef (JSString -> IO ())
  , quaViewSettings            :: Dynamic t QuaTypes.Settings
  , quaViewEvents              :: QuaViewEvents t
  }



-- | Try to fetch settings, initialize context and run qua-view inside
runQuaWidget :: QuaWidget (SpiderTimeline Global) x () -> Widget x ()
runQuaWidget m = mdo
  ctx <- initQuaViewContext qEvs
  ((), qEvs) <- runWithCtx m ctx
  return ()

-- | Get settings behavior
quaSettings :: (QuaViewTrans isWriting, Applicative m, Functor (QuaViewT isWriting t m))
            => QuaViewT isWriting t m (Dynamic t QuaTypes.Settings)
quaSettings = quaViewSettings <$> askContext


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


-- | Display a message for a user
showUserMessage :: forall msgRetType isWriting t m
                 . ( QuaViewTrans isWriting
                   , MonadIO (QuaViewT isWriting t m)
                   , Applicative m)
                => UserMessage msgRetType -> QuaViewT isWriting t m msgRetType
showUserMessage msg = fmap quaViewUserMessageHandlers askContext
                   >>= liftIO . (readIORef >=> ($ msg) . getUMsgCallback)

-- | Display a fatal error explanation to a user.
--   The program is not supposed to work after this message is shown.
showUserPanic :: ( QuaViewTrans isWriting
                 , MonadIO (QuaViewT isWriting t m)
                 , Applicative m)
              => JSString -> QuaViewT isWriting t m ()
showUserPanic msg = fmap quaViewPanicMsgHandler askContext
                  >>= liftIO . (readIORef >=> ($ msg))


-- | Put an event into a global environment.
--   This will be available in all widgets via `askEvent` function.
registerEvent :: (Reflex t, Applicative m)
              => QEventType a
              -> Event t a
              -> QuaViewT Writing t m ()
registerEvent key = QuaViewT . ReaderT . const
                  . WriterT . pure . (,) () . QuaViewEvents . DMap.singleton key

-- | Get an event from an environment.
--   If the event has not ever been registered in qua-view, this function returns `never`,
--   i.e. the returned event never fires.
askEvent :: (Reflex t, Applicative m)
         => QEventType a
         -> QuaViewT Writing t m (Event t a)
askEvent key = DMap.findWithDefault never key . unQuaViewEvents . quaViewEvents <$> askContext


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT a = fmap (fromMaybe a) . runMaybeT

-- | Normally, we should call this function only once: to set up user message widget
replaceUserMessageCallback :: ( QuaViewTrans isWriting
                              , MonadIO (QuaViewT isWriting t m)
                              , Applicative m)
                           => UserMessageCallback -> QuaViewT isWriting t m ()
replaceUserMessageCallback cb = fmap quaViewUserMessageHandlers askContext
                              >>= liftIO . flip writeIORef cb


-- | Normally, we should call this function only once: to set up a crash alert
replaceUserPanicCallback :: ( QuaViewTrans isWriting
                            , MonadIO (QuaViewT isWriting t m)
                            , Applicative m)
                         => (JSString -> IO ()) -> QuaViewT isWriting t m ()
replaceUserPanicCallback cb = fmap quaViewPanicMsgHandler askContext
                            >>= liftIO . flip writeIORef cb


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



instance {-# OVERLAPPING #-} MonadIO m
      => MonadLogger (QuaViewT NoWriting t m) where
  askLogger = quaViewLoggerFunc <$> askContext

instance {-# OVERLAPPING #-} (MonadIO m, Reflex t)
      => MonadLogger (QuaViewT Writing t m) where
  askLogger = quaViewLoggerFunc <$> askContext

instance {-# OVERLAPPING #-} QuaViewTrans isWriting => MonadTrans (QuaViewT isWriting t) where
  lift = quaViewT

instance {-# OVERLAPPING #-} MonadTransControl (QuaViewT NoWriting t) where
  type StT (QuaViewT NoWriting t) a = a
  liftWith f = QuaViewT' . ReaderT $ \r -> f $ \t -> runReaderT (unQuaViewT' t) r
  restoreT = QuaViewT' . restoreT
instance {-# OVERLAPPING #-} MonadTransControl (QuaViewT Writing t) where
  type StT (QuaViewT Writing t) a = (a, QuaViewEvents t)
  liftWith f = QuaViewT . ReaderT $
        \r -> WriterT $ fmap (flip (,) noEvents)
                             (f $ \t -> runWriterT $ runReaderT (unQuaViewT t) r)
  restoreT = QuaViewT . ReaderT . const . WriterT


instance {-# OVERLAPPING #-} MonadAdjust t m => MonadAdjust t (QuaViewT NoWriting t m) where
  runWithReplace a0 = QuaViewT' . runWithReplace (unQuaViewT' a0) . fmap unQuaViewT'
  traverseDMapWithKeyWithAdjust f dm0 =
    QuaViewT' . traverseDMapWithKeyWithAdjust (\a -> unQuaViewT' . f a) dm0
  traverseDMapWithKeyWithAdjustWithMove f dm0 =
    QuaViewT' . traverseDMapWithKeyWithAdjustWithMove (\a -> unQuaViewT' . f a) dm0


-- TODO: atm we discard @Event t (QuaViewEvents t)@,
-- whereas a correct behavior would be to transform this using something like switchPromptly.
-- this means we may omit some events if they update via MonadAdjust functions!
instance {-# OVERLAPPING #-} (Reflex t, MonadAdjust t m) => MonadAdjust t (QuaViewT Writing t m) where
  runWithReplace a0 a' = do
    ctx <- askContext
    ((a,ie), evs) <- quaViewT $ runWithReplace (runWithCtx a0 ctx)
                              $ flip runWithCtx ctx <$> a'
    QuaViewT . lift $ tell ie
    return (a, fst <$> evs)
  traverseDMapWithKeyWithAdjust f dm0 dm' =  do
    ctx <- askContext
    quaViewT $ traverseDMapWithKeyWithAdjust (\k v -> fst <$> runWithCtx (f k v) ctx) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    ctx <- askContext
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fst <$> runWithCtx (f k v) ctx) dm0 dm'



instance {-# OVERLAPPING #-} DomBuilder t m => DomBuilder t (QuaViewT NoWriting t m) where
  type DomBuilderSpace (QuaViewT NoWriting t m) = DomBuilderSpace m
instance {-# OVERLAPPING #-} (Reflex t, DomBuilder t m) => DomBuilder t (QuaViewT Writing t m) where
  type DomBuilderSpace (QuaViewT Writing t m) = DomBuilderSpace m
  element t cfg child = do
    (relem, (a,evs)) <- liftWith $ \runInner -> element t cfg $ runInner child
    QuaViewT . lift $ tell evs
    return (relem, a)
  selectElement cfg child = do
    (relem, (a,evs)) <- liftWith $ \runInner -> selectElement cfg $ runInner child
    QuaViewT . lift $ tell evs
    return (relem, a)


instance {-# OVERLAPPING #-} PostBuild t m => PostBuild t (QuaViewT NoWriting t m) where
  getPostBuild = quaViewT getPostBuild
instance {-# OVERLAPPING #-} PostBuild t m => PostBuild t (QuaViewT Writing t m) where
  getPostBuild = quaViewT getPostBuild

instance {-# OVERLAPPING #-} PerformEvent t m => PerformEvent t (QuaViewT NoWriting t m) where
  type Performable (QuaViewT NoWriting t m) = QuaViewT NoWriting t (Performable m)
  performEvent_ e = QuaViewT' . ReaderT $ \r ->
    performEvent_ $ flip runReaderT r . unQuaViewT' <$> e
  performEvent  e = QuaViewT' . ReaderT $ \r ->
    performEvent  $ flip runReaderT r . unQuaViewT' <$> e

instance {-# OVERLAPPING #-} PerformEvent t m => PerformEvent t (QuaViewT Writing t m) where
  type Performable (QuaViewT Writing t m) = QuaViewT NoWriting t (Performable m)
  performEvent_ e = QuaViewT . ReaderT $ \r ->
    lift . performEvent_ $ flip runReaderT r . unQuaViewT' <$> e
  performEvent  e = QuaViewT . ReaderT $ \r ->
    lift . performEvent  $ flip runReaderT r . unQuaViewT' <$> e


instance {-# OVERLAPPING #-} HasJSContext m => HasJSContext (QuaViewT NoWriting t m) where
  type JSContextPhantom (QuaViewT NoWriting t m) = JSContextPhantom m
  askJSContext = QuaViewT' askJSContext
instance {-# OVERLAPPING #-} (Reflex t, HasJSContext m) => HasJSContext (QuaViewT Writing t m) where
  type JSContextPhantom (QuaViewT Writing t m) = JSContextPhantom m
  askJSContext = quaViewT askJSContext


deriving instance {-# OVERLAPPING #-} Functor m        => Functor (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} Applicative m    => Applicative (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} Monad m          => Monad (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} MonadFix m       => MonadFix (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} MonadIO m        => MonadIO (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} MonadSample t m  => MonadSample t (QuaViewT NoWriting t m)
deriving instance {-# OVERLAPPING #-} MonadHold t m    => MonadHold t (QuaViewT NoWriting t m)


deriving instance {-# OVERLAPPING #-} Functor m => Functor (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, Applicative m)    => Applicative (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, Monad m)          => Monad (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, MonadFix m)       => MonadFix (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, MonadIO m)        => MonadIO (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, MonadSample t m)  => MonadSample t (QuaViewT Writing t m)
deriving instance {-# OVERLAPPING #-} (Reflex t, MonadHold t m)    => MonadHold t (QuaViewT Writing t m)


instance {-# OVERLAPPING #-} HasDocument m => HasDocument (QuaViewT NoWriting t m)
instance {-# OVERLAPPING #-} (Reflex t, HasDocument m) => HasDocument (QuaViewT Writing t m)

deriving instance {-# OVERLAPPING #-} TriggerEvent t m
      => TriggerEvent t (QuaViewT NoWriting t m)
instance {-# OVERLAPPING #-} (Reflex t, TriggerEvent t m)
      => TriggerEvent t (QuaViewT Writing t m) where
    newTriggerEvent = quaViewT newTriggerEvent
    newTriggerEventWithOnComplete = quaViewT newTriggerEventWithOnComplete
    newEventWithLazyTriggerWithOnComplete = quaViewT . newEventWithLazyTriggerWithOnComplete

--
--instance {-# OVERLAPPABLE #-}
--         (QuaViewTrans isWriting, Functor m)
--      => Functor (QuaViewT isWriting t m) where
--    fmap = case isWritingEvents @isWriting of
--      WritingEvents -> fmap
--      NoWritingEvents -> fmap
--instance {-# OVERLAPPABLE #-}
--         (Reflex t, QuaViewTrans isWriting, Applicative m)
--      => Applicative (QuaViewT isWriting t m) where
--    pure = case isWritingEvents @isWriting of
--      WritingEvents -> pure
--      NoWritingEvents -> pure
--    (<*>) = case isWritingEvents @isWriting of
--      WritingEvents -> (<*>)
--      NoWritingEvents -> (<*>)
--instance {-# OVERLAPPABLE #-}
--         (Reflex t, QuaViewTrans isWriting, Monad m)
--      => Monad (QuaViewT isWriting t m) where
--    return = case isWritingEvents @isWriting of
--      WritingEvents -> return
--      NoWritingEvents -> return
--    (>>=) = case isWritingEvents @isWriting of
--      WritingEvents -> (>>=)
--      NoWritingEvents -> (>>=)
--instance {-# OVERLAPPABLE #-}
--         (Reflex t, QuaViewTrans isWriting, MonadFix m)
--      => MonadFix (QuaViewT isWriting t m) where
--    mfix = case isWritingEvents @isWriting of
--      WritingEvents -> mfix
--      NoWritingEvents -> mfix
--instance {-# OVERLAPPABLE #-}
--         (Reflex t, QuaViewTrans isWriting, MonadIO m)
--      => MonadIO (QuaViewT isWriting t m) where
--    liftIO = case isWritingEvents @isWriting of
--      WritingEvents -> liftIO
--      NoWritingEvents -> liftIO

