{-# LANGUAGE GADTs #-}
-- | Keep all global events to be registered in qua-view
module Commons.Events
    ( QEventType (..)
    , UserRequest (..), ScId
    ) where


import Commons.Import
import Commons.Local


-- | All possible global events in qua-view
--    (those, which are passed between components only).
data QEventType evArg where
    UserRequest    :: UserRequest evArg -> QEventType evArg
    -- ^ Some type of user action - interaction with UI
    GeometryLoaded :: QEventType LoadedTextContent
    -- ^ When we got geometetry from file


-- | TODO this datatype should be in luci module;
--   represents a scenario id
data ScId

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
