{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

-- | Keep all global events to be registered in qua-view
module Commons.NoReflexDom.EventMap
    ( -- * Global event map
      QuaViewEvents (..), noEvents
      -- * Not yet assembled event map
    , QuaViewEventList (..), emptyEvList, singletonEvList
    , assembleQuaViewEvents
    ) where


import Data.Dependent.Map (unionWithKey, empty, singleton)
import qualified Data.Dependent.Map as DMap
import Reflex.Class (leftmost)

import Commons.NoReflex
import Commons.NoReflexDom.Import

-- | A map of qua-view events with suitable monoid instances.
--   When two maps merge and have same event name (key), the events are merged using
--   `lefmost`.
--   Since there is not symantic way to represent which event occurrence is preferred when
--   an event you declare coincide with another one, you must be very careful to avoid such situations.
newtype QuaViewEvents t = QuaViewEvents
  { unQuaViewEvents :: DMap QEventType (Event t)}

noEvents :: QuaViewEvents t
noEvents = QuaViewEvents empty

instance Reflex t => Semigroup (QuaViewEvents t) where
  a <> b = QuaViewEvents
         $ unionWithKey (\_ x y -> leftmost [x,y])
                        (unQuaViewEvents a)
                        (unQuaViewEvents b)

instance Reflex t => Monoid (QuaViewEvents t) where
  mempty = noEvents
  mappend = (<>)

-- | Make an event list with a single event (useful for WriterT)
singletonEvList :: QEventType e -> Event t e -> QuaViewEventList t
singletonEvList k e = QuaViewEventList $ singleton k (EventList [e])

emptyEvList :: QuaViewEventList t
emptyEvList = QuaViewEventList empty

newtype EventList t e = EventList { unEvents :: [Event t e] }

newtype QuaViewEventList t = QuaViewEventList
  { unQuaViewEventList :: DMap QEventType (EventList t)}

instance Semigroup (QuaViewEventList t) where
  a <> b = QuaViewEventList
         $ unionWithKey (\_ x y -> EventList (unEvents y ++ unEvents x))
                        (unQuaViewEventList a)
                        (unQuaViewEventList b)

instance Monoid (QuaViewEventList t) where
  mempty = emptyEvList
  mappend = (<>)

-- | Use leftmost to aggregate all events of each type into a single event
assembleQuaViewEvents :: Reflex t => QuaViewEventList t -> QuaViewEvents t
assembleQuaViewEvents = QuaViewEvents . DMap.map (leftmost . unEvents) . unQuaViewEventList
