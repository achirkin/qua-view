{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Program.UserAction
    ( QEventTag (..)
    , ScId (..)
    ) where


import Commons.NoReflex
import Model.Scenario.Properties (PropName)

-- | Represents a scenario id.
--   TODO: this datatype should be in luci module;
newtype ScId = ScId Int
  deriving (Eq, Show, Ord)

-- | Event types fired by user actions
data instance QEventTag UserAction evArg where
    -- | User wants to save scenario with this name.
    AskSaveScenario   :: QEventTag UserAction Text
    -- | User selects a scenario in the scenario list.
    AskSelectScenario :: QEventTag UserAction ScId
    -- | User wants to clear all geometry.
    AskClearGeometry  :: QEventTag UserAction ()
    -- | User wants to reset camera to its default position.
    AskResetCamera    :: QEventTag UserAction ()
    -- | User selected a property so that we can colorize all objects according to prop value
    PropertyClicked   :: QEventTag UserAction PropName



deriveEvent ''UserAction
