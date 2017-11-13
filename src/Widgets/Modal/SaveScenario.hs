{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.SaveScenario
    ( popupSaveScenario
    ) where

import Reflex.Dom

import Commons
import Widgets.Commons
import Widgets.Modal


popupSaveScenario :: Reflex t
                  => Event t (ElementClick saveScenarioButton)
                  -> Widget x (Event t Text)
popupSaveScenario savePopupE = snd <$> createModalWithClicks savePopupE Inactive saveScenarioContent

-- TODO: set input value to Nothing on save to reset the field.
saveScenarioContent :: Reflex t
                    => Widget x ( Event t (ElementClick "cancel save scenario")
                                , Event t Text)
saveScenarioContent = do
  elClass "div" "modal-heading" $
    elClass "p" "modal-title" $ text "Enter a name for a new scenario to save it on a server"
  scNameI <- elClass "div" "modal-inner" $
    elClass "div" "form-group form-group-label" $ do
      elAttr "label" (("class" =: "floating-label") <> ("for" =: "save-scenario-name-input")) $ text "Scenario name"
      textInput $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "save-scenario-name-input"))
  elClass "div" "modal-footer" $
    elClass "p" "text-right" $ do
      ce <- buttonFlat "Cancel" def
      se <- buttonFlat "Save" def
      return (leftmost [ce, se], current (_textInput_value scNameI) <@ se)
