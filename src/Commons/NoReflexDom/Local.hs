{-# LANGUAGE FlexibleContexts #-}
module Commons.NoReflexDom.Local
    ( onlyChanges
    , nowAndOnUpdates
    ) where

import Commons.NoReflex
import Commons.NoReflexDom.Import
import Reflex

-- | Get a dynamic that has update events only when its value actually changes.
--   It uses `Eq` instance to check if the value changed.
onlyChanges :: ( Reflex t, Eq a, Monad m
               , MonadSample t m
               , MonadHold t m
               )
             => Dynamic t a
             -> m (Dynamic t a)
onlyChanges d = do
    initial <- sample (current d)
    holdDyn initial changesE
  where
    changesE = attachWithMaybe (\old new -> if old == new then Nothing else Just new)
                               (current d)
                               (updated d)

-- | Run an action on initial state of the dynamic and on each update.
nowAndOnUpdates :: ( Reflex t
                   , Monad m, PerformEvent t m, MonadSample t m
                   , MonadIO m, MonadIO (Performable m)
                   )
                => (a -> IO ())
                -> Dynamic t a
                -> m ()
nowAndOnUpdates k d = do
    sample (current d) >>= liftIO . k
    performEvent_ $ liftIO . k <$> updated d
