{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
-- | Keep all global events to be registered in qua-view
module Commons.Events
    ( QuaViewEvents (..), noEvents
    , QEventType (..)
    , UserRequest (..), ScId
    ) where


import Data.Dependent.Map (unionWithKey, empty)
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Reflex.Class (leftmost)

import Commons.Import
import Commons.Local

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


-- | All possible global events in qua-view
--    (those, which are passed between components only).
data QEventType evArg where
    UserRequest    :: UserRequest evArg -> QEventType evArg
    -- ^ Some type of user action - interaction with UI
    GeometryLoaded :: QEventType LoadedTextContent
    -- ^ When we got geometetry from file


-- | TODO this datatype should be in luci module;
--   represents a scenario id
newtype ScId = ScId Int
  deriving (Eq, Show, Ord)

-- | Event types fired by user actions
data UserRequest evArg where
    AskSaveScenario   :: UserRequest Text
    -- ^ User wants to save scenario with this name
    AskSelectScenario :: UserRequest ScId
    -- ^ User selects a scenario in the scenario list.
    AskClearGeometry  :: UserRequest ()
    -- ^ User wants to clear all geometry
    AskResetCamera    :: UserRequest ()
    -- ^ User wants to reset camera to its default position
    AskSubmitProposal :: UserRequest Text
    -- ^ User wants to submit exercise. TODO: Need to add image and geometry?


deriveGEq ''UserRequest
deriveGCompare ''UserRequest
deriveGEq ''QEventType
deriveGCompare ''QEventType

instance Eq (UserRequest a) where
  (==) = defaultEq
  (/=) = defaultNeq

instance Eq (QEventType a) where
  (==) = defaultEq
  (/=) = defaultNeq
