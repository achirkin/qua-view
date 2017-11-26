{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Widgets.Modal.DownloadScenario
    ( popupDownloadScenario
    ) where

import Reflex.Dom
import Text.Julius (julius)

import Commons
import Model.Scenario (Scenario) -- definition of scenario
import Model.GeoJSON.Scenario () -- toJSON instance for Scenario
import JavaScript.JSON.Types.Instances (toJSON)
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal



popupDownloadScenario :: Reflex t
                      => Behavior t Scenario
                      -> Event t (ElementClick downloadScenarioButton)
                      -> QuaWidget t x ()
popupDownloadScenario scB scContentPopupE
  = void $ createSmallModalWithClicks' scContentPopupE Inactive $ downloadScenarioContent scB scContentPopupE


downloadScenarioContent :: Reflex t
                        => Behavior t Scenario
                        -> Event t (ElementClick downloadScenarioButton)
                        -> QuaWidget t x (Event t (ElementClick "cancel submit proposal"))
downloadScenarioContent scB scContentPopupE = do
    runCode
    -- save scenario into globally defined url every time this event happens
    performEvent_ $ liftIO . (>>= js_makeScenarioContentUrl) . jsonStringify . toJSON
                 <$> scB <@ scContentPopupE

    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Download scenario geometry"
    fileNameEl <- elClass "div" "modal-inner" $
      elClass "div" "form-group form-group-label" $ do
        -- TODO: Add more things here
        elAttr "label" (("class" =: "floating-label") <> ("for" =: "download-scgeom-name-input"))
          $ text "File name"
        textInput
          $ def & attributes .~ constDyn (("class" =: "form-control") <> ("id" =: "download-scgeom-name-input"))
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $ do
        ce <- buttonFlat "Close" def
        se <- buttonFlat "Download" ("id" =: "q$ScenarioContentLink")
        return ce
  where
    runCode = $(qjs
        [julius|
            var q$ScenarioContentUrl = null;
            document.q$makeScenarioContentUrl = function(txt) {
                if (q$ScenarioContentUrl !== null) {
                  window['URL']['revokeObjectURL'](q$ScenarioContentUrl);
                }
                q$ScenarioContentUrl
                   = window['URL']['createObjectURL'](new Blob([txt], {type: 'application/json'}));
                document['getElementById']('q$ScenarioContentLink').href = q$ScenarioContentUrl;
            };
        |])


foreign import javascript "document.q$makeScenarioContentUrl($1);"
  js_makeScenarioContentUrl :: JSString -> IO ()
