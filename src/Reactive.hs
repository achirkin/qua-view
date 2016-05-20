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
-- License     :  MIT
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
    , createAllEventSenses
    , reactiveCycle
    , createEventSense
    ) where

--import GHCJS.Useful
--import GHCJS.Marshal

import Language.Haskell.TH
import GHC.TypeLits

import Data.List (foldl', groupBy, sortBy)
import Data.Ord (comparing)

import Control.Concurrent (forkIO)

import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad (when,void,foldM)

-- | Wrap any type of event into this EventBox before throwing it into the `EventHole`.
data EventBox program view = forall event . EventSense program view event => EBox !event
-- EventSense program view event =>

--data EventBox_ program view = forall event . EventSense program view event => EBox_ !event

-- | This data type is returned by the `reactiveCycle`:
--   append one of these functions after each event you want to process.
data EventHole program view = EventHole
    { reqEvent :: !(EventBox program view -> IO ()) -- ^ Event that must be processed.
    , optEvent :: !(EventBox program view -> IO Bool) -- ^ Event that is processed optionally (if MVar is empty).
    }

-- | Empty data type declaring the type of a reaction to use.
--   We only need this for eliminating ambiguity when implementing `Reaction` class.
data R (name :: Symbol) (priority :: Nat) = R

-- | Implement this to have a reaction on an event. The `react` transforms a program.
--   Implement `response` to do some IO action on a view after the event is processed.
--   Order of executions of reactions and responses is given by the `priority` parameter.
--   All responses go after all reactions.
class Reaction program view event (name :: Symbol) (priority :: Nat)
       | program event name priority -> view where
    -- | Transform the program given an event.
    --   By default transform is `id`.
    react :: R name priority -> event -> program -> program
    react _ _ = id
    -- | Response IO action: gets an event, a program changed after `react`, and a view to modify.
    --   By default does nothing.
    response :: R name priority -> EventHole program view -> event -> program -> view -> IO view
    response _ _ _ _ = return

-- | Main cycle of our reactive programming engine.
--   Supply here the init states of the program and its view,
--   and it immediately returns `EventHole` - set of functions to supply events into the engine.
reactiveCycle :: program -- ^ init
              -> view -- ^ init view representation
              -> IO (EventHole program view)
reactiveCycle iprog irep = iprog `seq` irep `seq` do
    senseRef <- newTChanIO
    let ehole = EventHole (atomically . writeTChan senseRef) (atomically . tryWriteTChan senseRef)
    void . forkIO $ pIteration ehole senseRef (iprog,irep)
    return ehole
    where tryWriteTChan :: TChan a -> a -> STM Bool
          tryWriteTChan ref val = do
            em <- isEmptyTChan ref
            when em $ writeTChan ref val
            return em

-- | Create an `EventSense` instance declarations for all `Reaction` instances available in the scope.
--   Run TH declaration $(createAllEventSenses) in a place wich imports all modules that define `Reaction` instances.
createAllEventSenses :: Q [Dec]
createAllEventSenses = do
    ClassI _ instances <- reify ''Reaction
    ClassI _ notInstances <- reify ''EventSense
    return
        . map ( -- create instances of EventSense
                (\(srs, t) ->
                    InstanceD [] t
                    [ FunD 'processAllReactions
                        [ Clause [] (NormalB $ AppE (VarE 'reactList) srs) [] ]
                    , FunD 'processAllResponses
                        [ Clause [] (NormalB $ AppE (VarE 'responseList) srs) [] ]
                    ])
            .   -- create SRs from instances
                (\xs@(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt) vt) et) _) _) _ : _) ->
                    ( ListE $ map (
                            \(InstanceD _ (AppT (AppT _ reactionName) priority) _)
                            -> ConE 'SR `AppE` SigE (ConE 'R) (ConT ''R `AppT` reactionName `AppT` priority)
                        ) xs
                    , ConT ''EventSense `AppT` pt `AppT` vt `AppT` et
                    ))
            )
        . groupBy ( -- by event and program types
                \(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt1) vt1) et1) _) _) _)
                 (InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt2) vt2) et2) _) _) _)
                -> pt1 == pt2 && vt1 == vt2 && et1 == et2)
        . filter (\(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt) vt) et) _) _) _)
                    -> not $ any (\(InstanceD _ (AppT (AppT (AppT _ rpt) rvt) ret) _)
                                   -> pt == rpt && vt == rvt && et == ret
                                 )
                    notInstances
                 )
        . sortBy ( -- first need to sort, overwise groupBy will not work
                \(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt1) vt1) et1) _) (LitT (NumTyLit pr1))) _)
                 (InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt2) vt2) et2) _) (LitT (NumTyLit pr2))) _)
                -> comparing show pt1 pt2
                   `mappend` comparing show vt1 vt2
                   `mappend` comparing show et1 et2
                   `mappend` compare pr1 pr2)
        $ instances

-- | Create an `EventSense` instance declarations for all `Reaction` instances available in the scope
--   for given event name.
--   Run TH declaration $(createEventSense name).
createEventSense :: Name -> Q [Dec]
createEventSense eventName = do
    ClassI _ instances <- reify ''Reaction
    return
        . map ( -- create instances of EventSense
                (\(srs, t) ->
                    InstanceD [] t
                    [ FunD 'processAllReactions
                        [ Clause [] (NormalB $ AppE (VarE 'reactList) srs) [] ]
                    , FunD 'processAllResponses
                        [ Clause [] (NormalB $ AppE (VarE 'responseList) srs) [] ]
                    ])
            .   -- create SRs from instances
                (\xs@(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt) vt) et) _) _) _ : _) ->
                    ( ListE $ map (
                            \(InstanceD _ (AppT (AppT _ reactionName) priority) _)
                            -> ConE 'SR `AppE` SigE (ConE 'R) (ConT ''R `AppT` reactionName `AppT` priority)
                        ) xs
                    , ConT ''EventSense `AppT` pt `AppT` vt `AppT` et
                    ))
            )
        . groupBy ( -- by event and program types
                \(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt1) vt1) et1) _) _) _)
                 (InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt2) vt2) et2) _) _) _)
                -> pt1 == pt2 && vt1 == vt2 && et1 == et2)
        . filter (\(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ _) _) et) _) _) _)
                    -> et == ConT eventName
                 )
        . sortBy ( -- first need to sort, overwise groupBy will not work
                \(InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt1) vt1) et1) _) (LitT (NumTyLit pr1))) _)
                 (InstanceD _ (AppT (AppT (AppT (AppT (AppT _ pt2) vt2) et2) _) (LitT (NumTyLit pr2))) _)
                -> comparing show pt1 pt2
                   `mappend` comparing show vt1 vt2
                   `mappend` comparing show et1 et2
                   `mappend` compare pr1 pr2)
        $ instances


-- | Process all available reactions - instances should be generated by TH using `createEventSense`
class EventSense program view event | program -> view, view -> program where
    -- | Process all reactions on a given event; do not call it explicitly
    processAllReactions :: event -> program -> program
    processAllReactions _ = id
    -- | Process all responses on a given event; do not call it explicitly
    processAllResponses :: EventHole program view -> event -> program -> view -> IO view
    processAllResponses _ _ _ = return


----------------------------------------------------------------------------------------------------
--- Helpers ----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------


-- | Hide name of the reaction - existential data type
data SR program view event = forall name priority
    . Reaction program view event name priority => SR !(R name priority)


reactList :: [ SR program view event ]
          -> event
          -> program -> program
reactList rs ev program =  ev `seq` program `seq` foldl' (\p (SR r) -> react r ev p) program rs

responseList :: [ SR program view event ]
             -> EventHole program view
             -> event -> program -> view -> IO view
responseList rs hole ev programNew view = ev `seq` programNew `seq` view `seq`
    foldM (\v (SR r) -> response r hole ev programNew v) view rs


-- Do one iteration of processing
pIteration :: EventHole program view
           -> TChan (EventBox program view)
           -> (program, view)
           -> IO ()
pIteration hole senseRef pv = pv `seq` do
    EBox event <- atomically $ readTChan senseRef
    (nprog,nview) <- pProcessIteration hole event pv
    pIteration hole senseRef (nprog, nview)

--pIteration hole senseRef pv = pv `seq` do
--    (nprog,nview,nevs) <- pProcessIteration event pv
--    pIteration senseRef (oevs ++ reverse nevs) (nprog, nview)

pProcessIteration :: (EventSense program view event)
                  => EventHole program view
                  -> event -> (program, view) -> IO (program, view)
pProcessIteration hole event (program, view) = event `seq` program `seq` view `seq`
    case processAllReactions event program of
        nprog -> (\nview -> (nprog, nview))
            <$> processAllResponses hole event nprog view
