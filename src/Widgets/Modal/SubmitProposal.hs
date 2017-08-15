{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.SubmitProposal
    ( popupSubmitProposal,
      UserAsksSubmitProposal (..)
    ) where

import Reflex.Dom

import Commons
import Widgets.Commons
import Widgets.Modal

newtype UserAsksSubmitProposal = UserAsksSubmitProposal JSString -- Should add image and geometry?
  deriving (Eq, Show)

popupSubmitProposal :: Reflex t => Event t (ElementClick submitProposalButton) -> Widget x (Event t UserAsksSubmitProposal)
popupSubmitProposal savePopupE = snd <$> createSmallModalWithClicks savePopupE Inactive saveScenarioContent

saveScenarioContent :: Reflex t => Widget x (Event t (ElementClick "cancel submit proposal"), Event t UserAsksSubmitProposal)
saveScenarioContent = do
  elClass "div" "modal-heading" $
    elClass "p" "modal-title" $ text "Submit your design"
  _proposalNameE <- elClass "div" "modal-inner" $  do -- TODO: transform this into behavior and then apply on submit event
    el "div" blank -- TODO: Image preview
    elClass "div" "form-group form-group-label" $ do
      -- TODO: Add more things here
      elAttr "label" (("class" =: "floating-label") <> ("for" =: "submit-proposal-name-input")) $ text "Share your ideas"
      textArea $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "submit-proposal-name-input"))
  elClass "div" "modal-footer" $
    elClass "p" "text-right" $ do
      ce <- buttonFlat "Cancel" def
      se <- buttonFlat "Save" def -- TODO: Save scenario
      return (ce, UserAsksSubmitProposal undefined <$ se)
