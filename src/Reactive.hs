{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Reactive
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- This module provides minimalistic primitive reactive framework.
-- Adding an event to an environment means implementing Reaction class.
--
-----------------------------------------------------------------------------

module Reactive
    ( module Reactive.Core
    , reactiveCycle
    , EventBox (..)
    , EventHole (..)
    ) where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar,putMVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (liftM, void)

--import GHC.TypeLits

import Reactive.Core
--import Unsafe.Coerce (unsafeCoerce)

data EventBox program = forall event . EventSense program event => EBox event

data EventHole program = EventHole
    { reqEvent :: EventBox program -> IO ()
    , optEvent :: EventBox program -> IO Bool
    }


reactiveCycle :: program -- ^ init
              -> (program -> IO ()) -- ^ view
              -> IO (EventHole program)
reactiveCycle iprog view = do
    senseRef <- newEmptyMVar
    void . forkIO $ pIteration senseRef iprog view
    return $ EventHole (putMVar senseRef) (tryPutMVar senseRef) -- (PE $ putMVar senseRef . EBox) --



pIteration :: MVar (EventBox program)
           -> program
           -> (program -> IO ())
           -> IO ()
pIteration senseRef prog view = do
    np <- liftM (\(EBox e) -> processAllReactions e prog) $ takeMVar senseRef
    view np
    pIteration senseRef np view
