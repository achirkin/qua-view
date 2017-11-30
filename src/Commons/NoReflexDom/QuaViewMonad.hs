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
module Commons.NoReflexDom.QuaViewMonad
    ( QuaViewTrans (..), QuaViewT (..), QuaViewContext (..), QuaViewM
    , Writing, NoWriting, IsWritingEvents (..)
    , quaSettings, guessQuaSettings, runWithCtx
    , showUserMessage, showUserPanic
    , registerEvent, askEvent
    , replaceUserMessageCallback, replaceUserPanicCallback
    ) where


import           Control.Monad ((>=>), join)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.Lazy
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Control
import qualified Data.Dependent.Map as DMap
import           Data.Functor.Misc
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           GHCJS.DOM.JSFFI.Generated.ParentNode (querySelector)
import           GHCJS.DOM.JSFFI.Generated.Element    (getAttribute)
import           GHCJS.DOM (currentDocument)
import qualified QuaTypes
import           Reflex
import           Reflex.Patch.DMapWithMove

import           Commons.NoReflex
import           Commons.NoReflexDom.EventMap


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
    { unQuaViewT :: ReaderT (QuaViewContext t)
                            (WriterT (DynQuaViewEventList t) m)
                            a }
  quaViewT = QuaViewT . ReaderT . const . WriterT . fmap (flip (,) mempty)
  {-# INLINE quaViewT #-}
  askContext = QuaViewT . ReaderT $ \c -> WriterT (pure (c, mempty))
  {-# INLINE askContext #-}
  hoistQuaView h q = QuaViewT . ReaderT $ \r -> WriterT . h . runWriterT $ runReaderT (unQuaViewT q) r
  {-# INLINE hoistQuaView #-}
  isWritingEvents = WritingEvents
  {-# INLINE isWritingEvents #-}

runWithCtx :: QuaViewT Writing t m a
           -> QuaViewContext t
           -> m (a, DynQuaViewEventList t)
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


data QuaViewContext t = QuaViewContext
  { quaViewLoggerFunc          :: LoggerFunc
  , quaViewUserMessageHandlers :: IORef UserMessageCallback
  , quaViewPanicMsgHandler     :: IORef (JSString -> IO ())
  , quaViewSettings            :: Dynamic t QuaTypes.Settings
  , quaViewEvents              :: QuaViewEvents t
  }



-- | Get settings behavior
quaSettings :: (QuaViewTrans isWriting, Applicative m, Functor (QuaViewT isWriting t m))
            => QuaViewT isWriting t m (Dynamic t QuaTypes.Settings)
quaSettings = quaViewSettings <$> askContext



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
registerEvent :: Applicative m
              => QEventType a
              -> Event t a
              -> QuaViewT Writing t m ()
registerEvent key = QuaViewT . ReaderT . const
                  . WriterT . pure . (,) () . singDynEvList key

-- | Get an event from an environment.
--   If the event has not ever been registered in qua-view, this function returns `never`,
--   i.e. the returned event never fires.
askEvent :: ( Reflex t
            , Applicative m
            , QuaViewTrans isWriting
            , Functor (QuaViewT isWriting t m)
            )
         => QEventType a
         -> QuaViewT isWriting t m (Event t a)
askEvent key = lookupEvent key . quaViewEvents <$> askContext


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
    putSubmissionUrl = Nothing
    reviewSettingsUrl = Nothing

foreign import javascript unsafe
    "window['location']['href']['split']('?')[0]['split']('#')[0]"
    js_getViewUrl :: IO JSString

foreign import javascript unsafe
    "$1['split']($2)[0]"
    js_splitFst :: JSString -> JSString -> JSString



instance MonadIO m => MonadLogger (QuaViewT NoWriting t m) where
  askLogger = quaViewLoggerFunc <$> askContext

instance MonadIO m => MonadLogger (QuaViewT Writing t m) where
  askLogger = quaViewLoggerFunc <$> askContext

instance QuaViewTrans isWriting => MonadTrans (QuaViewT isWriting t) where
  lift = quaViewT

instance MonadTransControl (QuaViewT NoWriting t) where
  type StT (QuaViewT NoWriting t) a = a
  liftWith f = QuaViewT' . ReaderT $ \r -> f $ \t -> runReaderT (unQuaViewT' t) r
  restoreT = QuaViewT' . restoreT
instance MonadTransControl (QuaViewT Writing t) where
  type StT (QuaViewT Writing t) a = (a, DynQuaViewEventList t)
  liftWith f = QuaViewT . ReaderT $
        \r -> WriterT $ fmap (flip (,) mempty)
                             (f $ \t -> runWriterT $ runReaderT (unQuaViewT t) r)
  restoreT = QuaViewT . ReaderT . const . WriterT


instance MonadAdjust t m => MonadAdjust t (QuaViewT NoWriting t m) where
  runWithReplace a0 = QuaViewT' . runWithReplace (unQuaViewT' a0) . fmap unQuaViewT'
  traverseDMapWithKeyWithAdjust f dm0 =
    QuaViewT' . traverseDMapWithKeyWithAdjust (\a -> unQuaViewT' . f a) dm0
  traverseDMapWithKeyWithAdjustWithMove f dm0 =
    QuaViewT' . traverseDMapWithKeyWithAdjustWithMove (\a -> unQuaViewT' . f a) dm0


instance (MonadHold t m, MonadAdjust t m)
      => MonadAdjust t (QuaViewT Writing t m) where
  runWithReplace a0 a' = do
    ctx <- askContext
    ((a,wI), evs) <- quaViewT $ runWithReplace (runWithCtx a0 ctx)
                              $ flip runWithCtx ctx <$> a'
    let (rE, wE) = splitE evs
    qevsI <- assembleQuaViewEvents wI
    let qevsE = pushAlways assembleQuaViewEvents wE
    qevs <- QuaViewEvents . join . fmap unQuaViewEvents <$> hold qevsI qevsE
    QuaViewT . lift . tell $ DynQuaViewEventsAssembled qevs
    return (a, rE)
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    ctx <- askContext
    (rmap', revs') <- quaViewT $ traverseDMapWithKeyWithAdjust
       (\k v -> WrapedRes <$> runWithCtx (f k v) ctx) dm0 dm'
    let (wI, rmap) = DMap.mapAccumLWithKey (\xs _ (WrapedRes (y,x)) -> (x:xs, y)) [] rmap'
        (wE, revs) = splitE
                   $ DMap.mapAccumLWithKey mapPatch [] . unPatchDMap <$> revs'
        mapPatch xs _ (ComposeMaybe Nothing) = (xs, ComposeMaybe Nothing)
        mapPatch xs _ (ComposeMaybe (Just (WrapedRes (y,x)))) = (x:xs, ComposeMaybe (Just y))
    qevsI <- assembleQuaViewEvents $ mconcat wI
    let qevsE = pushAlways assembleQuaViewEvents $ mconcat <$> wE
    qevs <- QuaViewEvents . join . fmap unQuaViewEvents <$> hold qevsI qevsE
    QuaViewT . lift . tell $ DynQuaViewEventsAssembled qevs
    return $ (rmap, PatchDMap <$> revs)
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    ctx <- askContext
    (rmap', revs') <- quaViewT $ traverseDMapWithKeyWithAdjustWithMove
       (\k v -> WrapedRes <$> runWithCtx (f k v) ctx) dm0 dm'
    let (wI, rmap) = DMap.mapAccumLWithKey (\xs _ (WrapedRes (y,x)) -> (x:xs, y)) [] rmap'
        (wE, revs) = splitE
                   $ DMap.mapAccumLWithKey mapPatch [] . unPatchDMapWithMove <$> revs'
        mapPatch xs _ (NodeInfo From_Delete to)
            = (xs, NodeInfo From_Delete to)
        mapPatch xs _ (NodeInfo (From_Insert (WrapedRes (y,x))) to)
            = (x:xs, NodeInfo  (From_Insert y) to)
        mapPatch xs _ (NodeInfo (From_Move y) to)
            = (xs, NodeInfo (From_Move y) to)
    qevsI <- assembleQuaViewEvents $ mconcat wI
    let qevsE = pushAlways assembleQuaViewEvents $ mconcat <$> wE
    qevs <- QuaViewEvents . join . fmap unQuaViewEvents <$> hold qevsI qevsE
    QuaViewT . lift . tell $ DynQuaViewEventsAssembled qevs
    return $ (rmap, PatchDMapWithMove <$> revs)

newtype WrapedRes t v a = WrapedRes (v a, DynQuaViewEventList t)


instance PostBuild t m => PostBuild t (QuaViewT NoWriting t m) where
  getPostBuild = quaViewT getPostBuild
instance PostBuild t m => PostBuild t (QuaViewT Writing t m) where
  getPostBuild = quaViewT getPostBuild

instance PerformEvent t m => PerformEvent t (QuaViewT NoWriting t m) where
  type Performable (QuaViewT NoWriting t m) = QuaViewT NoWriting t (Performable m)
  performEvent_ e = QuaViewT' . ReaderT $ \r ->
    performEvent_ $ flip runReaderT r . unQuaViewT' <$> e
  performEvent  e = QuaViewT' . ReaderT $ \r ->
    performEvent  $ flip runReaderT r . unQuaViewT' <$> e

instance PerformEvent t m => PerformEvent t (QuaViewT Writing t m) where
  type Performable (QuaViewT Writing t m) = QuaViewT NoWriting t (Performable m)
  performEvent_ e = QuaViewT . ReaderT $ \r ->
    lift . performEvent_ $ flip runReaderT r . unQuaViewT' <$> e
  performEvent  e = QuaViewT . ReaderT $ \r ->
    lift . performEvent  $ flip runReaderT r . unQuaViewT' <$> e


deriving instance Functor m        => Functor (QuaViewT NoWriting t m)
deriving instance Applicative m    => Applicative (QuaViewT NoWriting t m)
deriving instance Monad m          => Monad (QuaViewT NoWriting t m)
deriving instance MonadFix m       => MonadFix (QuaViewT NoWriting t m)
deriving instance MonadIO m        => MonadIO (QuaViewT NoWriting t m)
deriving instance MonadSample t m  => MonadSample t (QuaViewT NoWriting t m)
deriving instance MonadHold t m    => MonadHold t (QuaViewT NoWriting t m)


deriving instance Functor m        => Functor (QuaViewT Writing t m)
deriving instance Applicative m    => Applicative (QuaViewT Writing t m)
deriving instance Monad m          => Monad (QuaViewT Writing t m)
deriving instance MonadFix m       => MonadFix (QuaViewT Writing t m)
deriving instance MonadIO m        => MonadIO (QuaViewT Writing t m)
deriving instance MonadSample t m  => MonadSample t (QuaViewT Writing t m)
deriving instance MonadHold t m    => MonadHold t (QuaViewT Writing t m)

deriving instance TriggerEvent t m
      => TriggerEvent t (QuaViewT NoWriting t m)
instance TriggerEvent t m
      => TriggerEvent t (QuaViewT Writing t m) where
    newTriggerEvent = quaViewT newTriggerEvent
    newTriggerEventWithOnComplete = quaViewT newTriggerEventWithOnComplete
    newEventWithLazyTriggerWithOnComplete = quaViewT . newEventWithLazyTriggerWithOnComplete
