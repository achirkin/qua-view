{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
-- | Keep all global events to be registered in qua-view
module Commons.NoReflexDom.EventMap
    ( -- * Global event map
      QuaViewEvents (..), noEvents
    ) where


import Data.Dependent.Map (unionWithKey, empty)
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


