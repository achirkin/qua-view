{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.BrowseScenarios
    ( popupBrowseScenarios
    , UserSelectedScenario (..)
    ) where

import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.CommonWidget
import Widgets.Modal


-- | TODO this datatype should be in luci module;
--  it should contain info required to visualize a single row in a scenario list.
data ScenarioDesc

-- | TODO this datatype should be in luci module;
--   represents a scenario id
data ScId

-- | Event happening when user selects a scenario in the scenario list.
newtype UserSelectedScenario = UserSelectedScenario { selectedScenario :: ScId }
--  deriving (Eq, Show)


-- | Modal window allowing user to select a scenario to load it from luci.
popupBrowseScenarios :: Reflex t => Event t (ElementClick s) -> Widget x (Event t UserSelectedScenario)
popupBrowseScenarios browsePopupE
    = snd <$> createModalWithClicks browsePopupE Inactive browseScenariosContent



browseScenariosContent :: Reflex t => Widget x (Event t (ElementClick "Close browse scenarios modal"), Event t UserSelectedScenario)
browseScenariosContent = flip (,) never <$> do
  elAttr "div" (("class" =: "modal-heading") <> ("style" =: "max-height: 10%")) $
    elClass "p" "modal-title" $ text "Select scenario"
  elAttr "div" (("class" =: "modal-inner") <> ("style" =: "max-height: 80%")) $
    text "Scenario List Table will be here." -- TODO: Scenario list table
  elAttr "div" (("class" =: "modal-footer") <> ("style" =: "max-height: 10%")) $
    elClass "p" "text-right" $
      flatButton "Cancel"
