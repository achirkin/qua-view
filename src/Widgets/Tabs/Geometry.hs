{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.Tabs.Geometry
    ( panelGeometry
    , GeometryTabOutE (..)
    , UserSelectedScenario (..)
    , UserAsksSaveScenario (..)
    ) where

import Reflex.Dom

import Commons
import Widgets.Modal.BrowseScenarios
import Widgets.Modal.SaveScenario
import Widgets.Tabs.Navigator (PanelState (..))



-- | GADT that helps to group all outgoing events from geometry tab GUI.
data GeometryTabOutE e where
  GeomOutUserSelectedScenario :: GeometryTabOutE UserSelectedScenario
  GeomOutUserAsksSaveScenario :: GeometryTabOutE UserAsksSaveScenario
  -- add here more output events

-- TODO do the same trick for input events if necessary


panelGeometry :: Reflex t => Dynamic t PanelState -> Widget x (EventSelector t GeometryTabOutE)
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
    _clearGeometryBtn <- redButton "clear" -- TODO: Clear Geometry
    _filesBtn <- redButton "files" -- TODO: Connect this with jsonFileInput for file uploading
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

luciScenarios :: Reflex t => Widget x (EventSelector t GeometryTabOutE)
luciScenarios = do
    let luciState = constDyn Connected -- Placeholder
    elDynAttr "p" (attrs1 <$> luciState) $ text "Luci scenarios are not available"
    elDynAttr "div" (attrs2 <$> luciState) luciScenarioPane
  where
    attrs1 state = "style" =: ("display: " <> display1 state)
    attrs2 state = "style" =: ("display: " <> display2 state)
    display1 Connected = "none"
    display1 Disconnected = "inline"
    display2 Connected = "inline"
    display2 Disconnected = "none"

luciScenarioPane :: forall t x . Reflex t => Widget x (EventSelector t GeometryTabOutE)
luciScenarioPane = do
  el "div" $ text "Remote (Luci scenarios)"
  el "div" $ do
    userSelectedScenarioE <- browseScenariosWidget
    userWantsToSaveScenarioE <- saveScenarioWidget $ constDyn True -- TODO: Hide this button when there is no active scenario
    fileNameIndicator $ constDyn "placeholder" -- TODO: Dynamic text correspond with saved file name
    let selectorFun :: forall e . GeometryTabOutE e -> Event t e
        selectorFun GeomOutUserSelectedScenario = userSelectedScenarioE
        selectorFun GeomOutUserAsksSaveScenario = userWantsToSaveScenarioE
    return $ EventSelector selectorFun


browseScenariosWidget :: Reflex t => Widget x (Event t UserSelectedScenario)
browseScenariosWidget = do
  (e, _) <- elClass' "a" "btn btn-red waves-attach waves-light waves-effect" $ text "Scenarios"
  popupBrowseScenarios $ ElementClick <$ domEvent Click e

saveScenarioWidget :: Reflex t => Dynamic t Bool -> Widget x (Event t UserAsksSaveScenario)
saveScenarioWidget scenarioActive = do
  (e, _) <- elDynAttr' "a" (attrs <$> scenarioActive) $ text "Save"
  popupSaveScenario $ ElementClick <$ domEvent Click e
  where
    attrs active = ("class" =: "btn btn-red waves-attach waves-light waves-effect")
                <> ("style" =: ("display: " <> displayClass active))
    displayClass True  = "inline"
    displayClass False = "none"
