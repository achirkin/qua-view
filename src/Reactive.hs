{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
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
-- Adding an event processing to an environment is done by implementing `Reaction` class.
-- @ $(createEventSense) @ aggregates all `Reaction` implementations (reactions-respones).
-- To run the machine use `reactiveCycle`.
-- To fire events, throw them into EventHole.
--
-----------------------------------------------------------------------------

module Reactive
    ( R (..)
    , Reaction (..)
    , EventBox (..)
    , EventHole (..)
    , EventSense (..)
    , createEventSense
    , reactiveCycle
    ) where

import Language.Haskell.TH
import GHC.TypeLits

import Data.List (groupBy)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar,putMVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (void,liftM,foldM)


-- | Wrap any type of event into this EventBox before throwing it into the `EventHole`.
data EventBox program view = forall event . EventSense program view event => EBox event

-- | This data type is returned by the `reactiveCycle`:
--   append one of these functions after each event you want to process.
data EventHole program view = EventHole
    { reqEvent :: EventBox program view -> IO () -- ^ Event that must be processed.
    , optEvent :: EventBox program view -> IO Bool -- ^ Event that is processed optionally (if MVar is empty).
    }

-- | Empty data type declaring the type of a reaction to use.
--   We only need this for eliminating ambiguity when implementing `Reaction` class.
data R (name :: Symbol) = R

-- | Implement this to have a reaction on an event. The `react` transforms a program.
--   Implement `response` to do some IO action on a view after the event is processed.
class Reaction program view event (name :: Symbol) | program event name -> view where
    -- | Transform the program given an event.
    --   By default transform is `id`.
    react :: R name -> event -> program -> program
    react _ _ = id
    -- | Response IO action: gets an event, a program changed after `react`, and a view to modify.
    --   By default does nothing.
    response :: R name -> event -> program -> view -> IO view
    response _ _ _ = return

-- | Main cycle of our reactive programming engine.
--   Supply here the init states of the program and its view,
--   and it immediately returns `EventHole` - set of functions to supply events into the engine.
reactiveCycle :: program -- ^ init
              -> view -- ^ init view representation
              -> IO (EventHole program view)
reactiveCycle iprog irep = do
    senseRef <- newEmptyMVar
    void . forkIO $ pIteration senseRef (iprog,irep)
    return $ EventHole (putMVar senseRef) (tryPutMVar senseRef)


-- | Create an `EventSense` instance declarations for all `Reaction` instances available in the scope.
--   Run TH declaration $(createEventSense) in a place wich imports all modules that define `Reaction` instances.
createEventSense :: Q [Dec]
createEventSense = do
    ClassI _ instances <- reify ''Reaction
    let expr = map ( -- create SRs from instances
              \xs@(InstanceD _ (AppT (AppT (AppT (AppT _ pt) vt) et) _) _ : _) ->
                ( ListE
                  $ map (\(InstanceD _ (AppT _ reactionName) _)
                  -> ConE 'SR `AppE` SigE (ConE 'R) (AppT (ConT ''R) reactionName)) xs
                , InstanceD [] (ConT ''EventSense `AppT` pt `AppT` vt `AppT` et) []
                )
            )
            . groupBy ( -- by event and program types
                \(InstanceD _ (AppT (AppT (AppT (AppT _ pt1) vt1) et1) _) _)
                 (InstanceD _ (AppT (AppT (AppT (AppT _ pt2) vt2) et2) _) _)
                -> pt1 == pt2 && vt1 == vt2 && et1 == et2)
            $ instances
        decs = flip map expr $ \(srs, InstanceD _ t _)
            -> InstanceD [] t [FunD 'processAllReactions [
                    Clause [] (NormalB $ AppE (VarE 'prList) srs) []
                ]]
    return decs

-- | Process all available reactions - instances should be generated by TH using `createEventSense`
class EventSense program view event where
    -- | Process all reactions on a given event - do not call it explicitly
    processAllReactions :: event -> (program, view) -> IO (program, view)


----------------------------------------------------------------------------------------------------
--- Helpers ----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


-- | Hide name of the reaction - existential data type
data SR program view event = forall name . Reaction program view event name => SR (R name)


-- Helper function to process event by reaction list
prList :: [ SR program view event ]
       -> event
       -> (program, view) -> IO (program, view)
prList rs ev progview = foldM (\pv (SR r) -> prReaction ev pv r) progview rs

-- Process single event by single reaction
prReaction :: Reaction program view event name
           => event
           -> (program, view)
           -> R name -> IO (program, view)
prReaction ev (prog, view) r = np `seq` liftM ((,) np) $ response r ev np view
    where np = react r ev prog

-- Do one iteration of of processing
pIteration :: MVar (EventBox program view)
           -> (program, view)
           -> IO ()
pIteration senseRef pv = takeMVar senseRef
    >>= \(EBox e) -> processAllReactions e pv
    >>= pIteration senseRef
