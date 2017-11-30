{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Keep all global events to be registered in qua-view
module Commons.NoReflexDom.EventMap
    ( -- * Global event map
      QuaViewEvents (..), lookupEvent
      -- * Not yet assembled event map
    , QuaViewEventList (..), emptyEvList, singletonEvList
    , assembleQuaViewEvents
    , DynQuaViewEventList (..), singDynEvList
    ) where

import Control.Monad (foldM)
import qualified Data.Dependent.Map as DMap
import Reflex.Class

import Commons.NoReflex

-- | A map of qua-view events with suitable monoid instances.
--   When two maps merge and have same event name (key), the events are merged using
--   `lefmost`.
--   Since there is not symantic way to represent which event occurrence is preferred when
--   an event you declare coincide with another one, you must be very careful to avoid such situations.
newtype QuaViewEvents t = QuaViewEvents
  { unQuaViewEvents :: Behavior t (DMap QEventType (Event t))}

-- | Lookup an event in the qua-view events map
lookupEvent :: Reflex t => QEventType a -> QuaViewEvents t -> Event t a
lookupEvent key = switch . fmap (DMap.findWithDefault never key) . unQuaViewEvents

instance Reflex t => Semigroup (QuaViewEvents t) where
  a <> b = QuaViewEvents
         $ DMap.unionWithKey (\_ x y -> leftmost [x,y])
        <$> unQuaViewEvents a <*> unQuaViewEvents b

instance Reflex t => Monoid (QuaViewEvents t) where
  mempty = QuaViewEvents $ constant mempty
  mappend = (<>)


newtype QuaViewEventList t = QuaViewEventList
  { unQuaViewEventList :: DMap QEventType (EventList t)}

newtype EventList t e = EventList { unEvents :: [Event t e] }


-- | Make an event list with a single event (useful for WriterT)
singletonEvList :: QEventType e -> Event t e -> QuaViewEventList t
singletonEvList k e = QuaViewEventList $ DMap.singleton k (EventList [e])

emptyEvList :: QuaViewEventList t
emptyEvList = QuaViewEventList DMap.empty


instance Semigroup (QuaViewEventList t) where
  a <> b = QuaViewEventList
         $ DMap.unionWithKey (\_ x y -> EventList (unEvents y ++ unEvents x))
                             (unQuaViewEventList a)
                             (unQuaViewEventList b)

instance Monoid (QuaViewEventList t) where
  mempty = emptyEvList
  mappend = (<>)


data DynQuaViewEventList t
  = DynQuaViewEventList ![(QuaViewEventList t, Maybe (Event t (QuaViewEventList t)))]
  | DynQuaViewEventsAssembled !(QuaViewEvents t)
  | DynQuaViewEventsMixed ![(QuaViewEventList t, Maybe (Event t (QuaViewEventList t)))] ![QuaViewEvents t]

instance Semigroup (DynQuaViewEventList t) where
  DynQuaViewEventList as <> DynQuaViewEventList bs
    = DynQuaViewEventList (as <> bs)
  DynQuaViewEventList [] <> DynQuaViewEventsAssembled b
    = DynQuaViewEventsAssembled b
  DynQuaViewEventList as <> DynQuaViewEventsAssembled b
    = DynQuaViewEventsMixed as [b]
  DynQuaViewEventList as <> DynQuaViewEventsMixed bs es
    = DynQuaViewEventsMixed (as <> bs) es
  DynQuaViewEventsAssembled a <> DynQuaViewEventList []
    = DynQuaViewEventsAssembled a
  DynQuaViewEventsAssembled a <> DynQuaViewEventList as
    = DynQuaViewEventsMixed as [a]
  DynQuaViewEventsAssembled a <> DynQuaViewEventsAssembled b
    = DynQuaViewEventsMixed mempty [a,b]
  DynQuaViewEventsAssembled a <> DynQuaViewEventsMixed bs es
    = DynQuaViewEventsMixed bs (a : es)
  DynQuaViewEventsMixed as ds <> DynQuaViewEventList bs
    = DynQuaViewEventsMixed (as <> bs) ds
  DynQuaViewEventsMixed as ds <> DynQuaViewEventsAssembled b
    = DynQuaViewEventsMixed as (ds ++ [b])
  DynQuaViewEventsMixed as ds <> DynQuaViewEventsMixed bs es
    = DynQuaViewEventsMixed (as <> bs) (ds <> es)

instance Monoid (DynQuaViewEventList t) where
  mempty = DynQuaViewEventList mempty
  mappend = (<>)

singDynEvList :: QEventType e -> Event t e -> DynQuaViewEventList t
singDynEvList k e = DynQuaViewEventList [(singletonEvList k e, Nothing)]


-- | Use leftmost to aggregate all events of each type into a single event
assembleQuaViewEvents :: (Reflex t, MonadHold t m)
                      => DynQuaViewEventList t
                      -> m (QuaViewEvents t)
assembleQuaViewEvents (DynQuaViewEventList x) =
    fmap (QuaViewEvents . fmap mergeEvs) $ foldM mergeBE (constant mempty) x
  where
    mergeBE b (e,Nothing) = pure $ mappend b (constant e)
    mergeBE b (e,Just es) = mappend b <$> hold e es
    mergeEvs = DMap.map (leftmost . unEvents) . unQuaViewEventList
assembleQuaViewEvents (DynQuaViewEventsAssembled x) = pure x
assembleQuaViewEvents (DynQuaViewEventsMixed as bs) = do
    b <- assembleQuaViewEvents (DynQuaViewEventList as)
    return $ foldMap id (b:bs)
