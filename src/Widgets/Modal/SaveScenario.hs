{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Modal.SaveScenario
    ( saveScenarioPane
    ) where

import Data.Semigroup
import Reflex.Dom

import Widgets.CommonWidget
import Widgets.Modal

saveScenarioPane :: Reflex t => Event t () -> Widget x (Event t (), Event t ())
saveScenarioPane savePopupE = createModal savePopupE fst saveScenarioContent

saveScenarioContent :: Reflex t => Widget x (Event t (), Event t ())
saveScenarioContent = do
  elClass "div" "modal-heading" $ do
    elClass "p" "modal-title" $ text "Enter a name for a new scenario to save it on a server"
  elClass "div" "modal-inner" $ do
    elClass "div" "form-group form-group-label" $ do
      elAttr "label" (("class" =: "floating-label") <> ("for" =: "save-scenario-name-input")) $ text "Scenario name"
      textInput $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "save-scenario-name-input"))
  elClass "div" "modal-footer" $ do
    elClass "p" "text-right" $ do
      ce <- flatButton' "Cancel"
      se <- flatButton' "Save" -- TODO: Save scenario
      return (ce, se)
