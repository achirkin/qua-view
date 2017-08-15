{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.SaveScenario
    ( popupSaveScenario
    , UserAsksSaveScenario (..)
    ) where

import Reflex.Dom

import Commons
import Widgets.Commons
import Widgets.Modal


-- | Event happening when user clicks on save scenario button.
--   TODO: probably, the type of scenario name should be different and provided by luci.
newtype UserAsksSaveScenario = UserAsksSaveScenario JSString
  deriving (Eq, Show)

popupSaveScenario :: Reflex t => Event t (ElementClick saveScenarioButton) -> Widget x (Event t UserAsksSaveScenario)
popupSaveScenario savePopupE = snd <$> createModalWithClicks savePopupE Inactive saveScenarioContent

saveScenarioContent :: Reflex t => Widget x (Event t (ElementClick "cancel save scenario"), Event t UserAsksSaveScenario)
saveScenarioContent = do
  elClass "div" "modal-heading" $
    elClass "p" "modal-title" $ text "Enter a name for a new scenario to save it on a server"
  _scNameE <- elClass "div" "modal-inner" $ -- TODO: transform this into behavior and then apply on save event
    elClass "div" "form-group form-group-label" $ do
      elAttr "label" (("class" =: "floating-label") <> ("for" =: "save-scenario-name-input")) $ text "Scenario name"
      textInput $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "save-scenario-name-input"))
  elClass "div" "modal-footer" $
    elClass "p" "text-right" $ do
      ce <- buttonFlat "Cancel" def
      se <- buttonFlat "Save" def -- TODO: Save scenario
      return (ce, UserAsksSaveScenario undefined <$ se)
