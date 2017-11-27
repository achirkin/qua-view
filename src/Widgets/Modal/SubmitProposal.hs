{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Widgets.Modal.SubmitProposal
    ( popupSubmitProposal
    ) where

import Reflex.Dom

import Commons
import SmallGL
import SmallGL.Types (ProjMatrix (..), ViewMatrix (..))
import Model.Camera (initCamera, defaultCState, projMatrix, viewMatrix)
import Model.Scenario (Scenario)
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal




popupSubmitProposal :: Reflex t
                    => RenderingApi
                    -> Behavior t Scenario
                    -> Event t (ElementClick submitProposalButton)
                    -> QuaWidget t x ()
popupSubmitProposal rApi scenarioB savePopupE
  = void $ createSmallModalWithClicks' savePopupE Inactive
         $ saveScenarioContent rApi scenarioB savePopupE


saveScenarioContent :: Reflex t
                    => RenderingApi
                    -> Behavior t Scenario
                    -> Event t (ElementClick submitProposalButton)
                    -> QuaWidget t x (Event t (ElementClick "cancel submit proposal"))
saveScenarioContent renderingApi _scenarioB savePopupE = do
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Submit your design"
    propDescrEl <- elClass "div" "modal-inner" $  do
      _ <- el "div" $ do
        imgw <- performEvent . (<$ savePopupE) $ do
          url <- liftIO $ SmallGL.renderToImage
                           renderingApi
                           (imgWidth, imgHeight)
                           projMat viewMat
          pure . void $ makeElementFromHtml def
            [jsstring|<img src="#{url}" style="width: 100%"></img>|]
        widgetHold blank imgw
      elClass "div" "form-group form-group-label" $ do
        -- TODO: Add more things here
        elAttr "label" ( ("class" =: "floating-label")
                       <> ("for" =: "submit-proposal-name-input"))
                     $ text "Share your ideas"
        textArea $ def & attributes .~ constDyn ( ("class" =: "form-control")
                                                <> ("id" =: "submit-proposal-name-input"))
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $ do
        ce <- buttonFlat "Cancel" def
        se <- buttonFlat "Save" def -- TODO: Save scenario
        registerEvent (UserRequest AskSubmitProposal) $ current (_textArea_value propDescrEl) <@ se
        return ce
  where
    imgWidth = 800
    imgHeight = 800
    defCam = initCamera (fromIntegral imgWidth)
                        (fromIntegral imgHeight)
                        defaultCState
    projMat = ProjM $ projMatrix defCam
    viewMat = ViewM $ viewMatrix defCam
