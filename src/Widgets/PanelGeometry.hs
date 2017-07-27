{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelGeometry
    ( panelGeometry,
      popupScenario
    ) where

import Control.Monad (void)
import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.CommonWidget
import Widgets.Generation

panelGeometry :: Reflex t => Dynamic t PanelState -> Widget x (Event t (), Event t ())
panelGeometry pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $ do
      fileUploadGeometry
      luciScenarios
  where
    toPanelClass PanelGeometry = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"
-- File Upload Geometry

fileUploadGeometry :: Reflex t => Widget x ()
fileUploadGeometry = do
  text "Read GeoJSON from file"
  el "div" $ do
    clearGeometryBtn <- redButton "clear" -- TODO: Clear Geometry
    filesBtn <- redButton "files" -- TODO: Connect this with jsonFileInput for file uploading
    fileNameIndicator $ constDyn "placeholder.geojson" -- TODO: Dynamic text correspond with the uploaded file name
    jsonFileInput -- TODO: file uploadings

redButton :: Reflex t => Text -> Widget x ()
redButton = elClass "a" "btn btn-red waves-attach waves-light waves-effect" . text

fileNameIndicator :: Reflex t => Dynamic t Text -> Widget x ()
fileNameIndicator = elAttr "p" attrs . dynText
  where
    attrs = ("style" =: "display:inline") <> ("font-size" =: "0.9em")

jsonFileInput :: Reflex t => Widget x ()
jsonFileInput = elAttr "input" attrs blank
  where
    attrs = ("style" =: "display:none") <> ("type" =: "file")

-- Luci Scenario

data LuciState = Connected | Disconnected
  deriving Eq

luciScenarios :: Reflex t => Widget x (Event t (), Event t ())
luciScenarios = do
    let luciState = constDyn Connected -- Placeholder
    elDynAttr "p" (attrs1 <$> luciState) $ text "Luci scenarios are not available"
    elDynAttr "div" (attrs2 <$> luciState) luciScenarioPane
  where
    attrs1 state = ("style" =: ("display: " <> display1 state))
    attrs2 state = ("style" =: ("display: " <> display2 state))
    display1 Connected = "none"
    display1 Disconnected = "inline"
    display2 Connected = "inline"
    display2 Disconnected = "none"

luciScenarioPane :: Reflex t => Widget x (Event t (), Event t ())
luciScenarioPane = do
  el "div" $ text "Remote (Luci scenarios)"
  el "div" $ do
    browsePopupE <- browseScenarioWidget
    savePopupE <- saveScenarioWidget $ constDyn True -- TODO: Hide this button when there is no active scenario
    fileNameIndicator $ constDyn "placeholder" -- TODO: Dynamic text correspond with saved file name
    return (browsePopupE, savePopupE)

browseScenarioWidget :: Reflex t => Widget x (Event t ())
browseScenarioWidget = do
  (e, _) <- elClass' "a" "btn btn-red waves-attach waves-light waves-effect" $ text "Scenarios"
  return $ domEvent Click e

saveScenarioWidget :: Reflex t => Dynamic t Bool -> Widget x (Event t ())
saveScenarioWidget scenarioActive = do
  (e, _) <- elDynAttr' "a" (attrs <$> scenarioActive) $ text "Save"
  return $ domEvent Click e
  where
    attrs active = ("class" =: "btn btn-red waves-attach waves-light waves-effect")
                <> ("style" =: ("display: " <> display active))
    display True  = "inline"
    display False = "none"

-- Pop up Scenario

popupScenario :: Reflex t => Event t () -> Event t ()-> Widget x ()
popupScenario browsePopupE savePopupE = do
  browseScenarioPane browsePopupE
  saveScenarioPane savePopupE

browseScenarioPane :: Reflex t => Event t () -> Widget x ()
browseScenarioPane browsePopupE = mdo
    paneActive <- holdDyn False $ leftmost [False <$ cancelE, True <$ browsePopupE]
    cancelE <- elDynAttr "div" (attrs <$> paneActive) $ do
      elAttr "div" (("class" =: "modal-dialog") <> ("style" =: "max-height: 100%")) $ do
        elAttr "div" (("class" =: "modal-content") <> ("style" =: "max-height: 100%")) $ do
          elAttr "div" (("class" =: "modal-heading") <> ("style" =: "max-height: 10%")) $ do
            elClass "p" "modal-title" $ text "Select scenario"
          elAttr "div" (("class" =: "modal-inner") <> ("style" =: "max-height: 80%")) $ do
            text "Scenario List Table will be here." -- TODO: Scenario list table
          elAttr "div" (("class" =: "modal-footer") <> ("style" =: "max-height: 10%")) $ do
            elClass "p" "text-right" $ do
              flatButton' "Cancel"
    blank
  where
    attrs active = ("class" =: ("modal modal-va-middle fade" <> displayClass active))
                <> ("role" =: "dialog")
                <> ("tabindex" =: "-1")
                <> ("style" =: ("display: " <> displayStyle active))
    displayClass True  = " modal-va-middle-show in"
    displayClass False = ""
    displayStyle True  = "flex"
    displayStyle False = "none"

saveScenarioPane :: Reflex t => Event t () -> Widget x ()
saveScenarioPane savePopupE = mdo
    paneActive <- holdDyn False $ leftmost [False <$ cancelE, True <$ savePopupE]
    (cancelE, saveE) <- elDynAttr "div" (attrs <$> paneActive) $ do
      elClass "div" "modal-dialog" $ do
        elClass "div" "modal-content" $ do
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
    blank
  where
    attrs active = ("class" =: ("modal modal-va-middle fade" <> displayClass active))
                <> ("role" =: "dialog")
                <> ("tabindex" =: "-1")
                <> ("style" =: ("display: " <> displayStyle active))
    displayClass True  = " modal-va-middle-show in"
    displayClass False = ""
    displayStyle True  = "flex"
    displayStyle False = "none"
