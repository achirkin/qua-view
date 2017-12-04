{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.BrowseScenarios
    ( popupBrowseScenarios
    ) where

import Reflex.Dom

import Commons
import Program.UserAction
import Widgets.Commons
import Widgets.Modal



-- | Modal window allowing user to select a scenario to load it from luci.
popupBrowseScenarios :: Reflex t => Event t (ElementClick s) -> Widget x (Event t ScId)
popupBrowseScenarios browsePopupE
    = snd <$> createModalWithClicks browsePopupE Inactive browseScenariosContent



browseScenariosContent :: Reflex t
                       => Widget x ( Event t (ElementClick "Close browse scenarios modal")
                                   , Event t ScId)
browseScenariosContent = flip (,) never <$> do
  elAttr "div" (("class" =: "modal-heading") <> ("style" =: "max-height: 10%")) $
    elClass "p" "modal-title" $ text "Select scenario"
  elAttr "div" (("class" =: "modal-inner") <> ("style" =: "max-height: 80%")) $
    text "Scenario List Table will be here." -- TODO: Scenario list table
  elAttr "div" (("class" =: "modal-footer") <> ("style" =: "max-height: 10%")) $
    elClass "p" "text-right" $
      buttonFlat "Cancel" def -- ("data-dismiss" =: "modal")
