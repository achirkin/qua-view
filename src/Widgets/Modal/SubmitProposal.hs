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
import Program.UserAction
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
    -- unselet an active object to avoid red blocks on a screenshot
    -- TODO: what is earlier -- renderToImage or unselect??
    registerEvent (UserAction AskSelectObject) $ Nothing <$ submitPopupUrlE
    -- prepare scenario preview url
    imgurlE <- performEvent . ffor (scenarioB <@ submitPopupUrlE) $ \scenario ->
      let imgWidth = 800
          imgHeight = 800
          defCam = initCamera (fromIntegral imgWidth)
                              (fromIntegral imgHeight)
                              (scenario^.viewState.zoomLimits)
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
        el "div" $
          void . widgetHold blank . ffor imgurlE $ \url ->
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
    responseE <- httpPut
               $ makePut <$> submitUrlB <*> imgurlB <*> scContentB <@> submitWithDescrE

    msgE <- performEvent . ffor responseE $ \eresponse -> case eresponse of
      Left (JSError err) -> (Nothing <$) . showUserMessage . SingleMsg $
              "An error happened when submitting the design.\n"
           <> "Please, save your design to your computer and contact administrators.\n"
           <> "Error: " <> err
      Right (SubmitResponse msg) -> return $ Just msg
    popupOnSubmit $ fmapMaybe id msgE

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




popupOnSubmit :: Reflex t
              => Event t JSString -- ^ html message to show to a user
              -> QuaWidget t x ()
popupOnSubmit submittedE
  = void $ createSmallModalWithClicks' (ElementClick <$ submittedE) Inactive $ popupOnSubmitContent submittedE


popupOnSubmitContent :: Reflex t
                     => Event t JSString -- ^ msg
                     -> QuaWidget t x (Event t (ElementClick "close onsubmit popup"))
popupOnSubmitContent msg = do
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Submission complete"
    (e, ()) <- elClass' "div" "modal-inner" blank
    performEvent_ $ setInnerHTML e <$> msg
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $
        buttonFlat "Ok" def
