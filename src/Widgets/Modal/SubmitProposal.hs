{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.SubmitProposal
    ( popupSubmitProposal
    ) where

import Reflex.Dom

import Commons
import Widgets.Commons
import Widgets.Modal



popupSubmitProposal :: Reflex t
                    => Event t (ElementClick submitProposalButton)
                    -> QuaWidget t x ()
popupSubmitProposal savePopupE
  = void $ createSmallModalWithClicks' savePopupE Inactive saveScenarioContent


saveScenarioContent :: Reflex t
                    => QuaWidget t x (Event t (ElementClick "cancel submit proposal"))
saveScenarioContent = do
  elClass "div" "modal-heading" $
    elClass "p" "modal-title" $ text "Submit your design"
  propDescrEl <- elClass "div" "modal-inner" $  do
         -- TODO: transform this into behavior and then apply on submit event
    el "div" blank -- TODO: Image preview
    elClass "div" "form-group form-group-label" $ do
      -- TODO: Add more things here
      elAttr "label" (("class" =: "floating-label") <> ("for" =: "submit-proposal-name-input")) $ text "Share your ideas"
      textArea $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "submit-proposal-name-input"))
  elClass "div" "modal-footer" $
    elClass "p" "text-right" $ do
      ce <- buttonFlat "Cancel" def
      se <- buttonFlat "Save" def -- TODO: Save scenario
      registerEvent (UserRequest AskSubmitProposal) $ current (_textArea_value propDescrEl) <@ se
      return ce
