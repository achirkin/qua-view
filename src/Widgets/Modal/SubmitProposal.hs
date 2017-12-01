{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Widgets.Modal.SubmitProposal
    ( popupSubmitProposal
    ) where

import Control.Lens
import Reflex.Dom
import JavaScript.JSON.Types.Instances (toJSON)
import QuaTypes.Submission

import Commons
import SmallGL
import SmallGL.Types (ProjMatrix (..), ViewMatrix (..))
import Model.Camera (initCamera, lookAtState, projMatrix, viewMatrix)
import Model.Scenario
import Model.GeoJSON.Scenario () -- toJSON instance for Scenario
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal




popupSubmitProposal :: Reflex t
                    => RenderingApi
                    -> Behavior t Scenario
                    -> Event t JSString -- ^ where to submit url
                    -> QuaWidget t x ()
popupSubmitProposal rApi scenarioB submitPopupUrlE
  = void $ createSmallModalWithClicks' (ElementClick <$ submitPopupUrlE) Inactive
         $ saveScenarioContent rApi scenarioB submitPopupUrlE


saveScenarioContent :: Reflex t
                    => RenderingApi
                    -> Behavior t Scenario
                    -> Event t JSString -- ^  where to submit url
                    -> QuaWidget t x (Event t (ElementClick "cancel submit proposal"))
saveScenarioContent renderingApi scenarioB submitPopupUrlE = do
    -- prepare scenario preview url
    imgurlE <- performEvent . ffor (scenarioB <@ submitPopupUrlE) $ \scenario ->
      let imgWidth = 800
          imgHeight = 800
          defCam = initCamera (fromIntegral imgWidth)
                              (fromIntegral imgHeight)
                              (scenario^.viewState.clippingDist)
                              (lookAtState $ scenario^.viewState.cameraPos)
          projMat = ProjM $ projMatrix defCam
          viewMat = ViewM $ viewMatrix defCam
      in  liftIO $ SmallGL.renderToImage renderingApi (imgWidth, imgHeight) projMat viewMat
    -- prepare scenario content
    scContentE <- performEvent . ffor (scenarioB <@ submitPopupUrlE) $
      liftIO . jsonStringify . toJSON
    submitUrlB <- hold "" submitPopupUrlE
    imgurlB <- hold "" imgurlE
    scContentB <- hold "" scContentE

    -- widget header
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $
        text "Submit your design"
    -- widget body: image and description
    propDescrEl <-
      elClass "div" "modal-inner" $ do
        el "div" $ do
          void . widgetHold blank . ffor imgurlE $ \url -> do
            void $ makeElementFromHtml def
              [jsstring|<img src="#{url}" style="width: 100%"></img>|]
        elClass "div" "form-group form-group-label" $ do
          elAttr "label" ( "class" =: "floating-label"<> "for" =: submitProposalDescrId) $
            text "Share your ideas"
          textArea $ def & attributes .~ constDyn (  "class" =: "form-control"
                                                  <> "id" =: submitProposalDescrId)
    (cancelE, submitWithDescrE) <-
      elClass "div" "modal-footer" $
        elClass "p" "text-right" $ do
          ce <- buttonFlat "Cancel" def
          se <- buttonFlat "Save" def
          return (ce, textToJSString <$> current (value propDescrEl) <@ se)

    -- submit proposal
    responseE <- httpPut @_ @_ @JSVal
               $ makePut <$> submitUrlB <*> imgurlB <*> scContentB <@> submitWithDescrE

    performEvent_ . ffor responseE $ \eresponse -> case eresponse of
      Left (JSError err) -> showUserMessage . SingleMsg $
              "An error happened when submitting the design.\n"
           <> "Please, save your design to your computer and contact administrators.\n"
           <> "Error: " <> err
      Right _  -> showUserMessage $ SingleMsg $
              "Your design is submitted succesfully!\n"
           <> "You can continue working on it and submit a new version, or just close the window."
           <> "You can update the submission later via \"Work on a design link\"."

    return $ leftmost [cancelE, ElementClick <$ submitWithDescrE]
  where
    submitProposalDescrId = $( newVar >>= returnVars . (:[]))
    makePut submitUrl imgurl scContent description
      = ( submitUrl
        , SubmissionPost
          { subPostDescription  = description
          , subPostGeometry     = scContent
          , subPostPreviewImage = imgurl
          }
        )
