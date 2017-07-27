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
import Widgets.Generation

panelGeometry :: Reflex t => Dynamic t PanelState -> Widget x (Event t ())
panelGeometry pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $ do
      fileUploadGeometry
      luciScenarios
  where
    toPanelClass PanelGeometry = openPanelState
    toPanelClass _ = closedPanelState
    -- Styles for the panel are generated statically.
    -- newVar guarantees that the class name is unique.
    (openPanelState, closedPanelState) = $(do
        placeholder <- newVar
        let ostate = placeholder <> "-open"
            cstate = placeholder <> "-closed"
        qcss
          [cassius|
            .#{placeholder}
                position: absolute

            .#{ostate}
                visibility: visible

            .#{cstate}
                visibility: hidden

          |]
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ fmap ((placeholder <> " ") <>) [ostate, cstate]
      )

-- File Upload Geometry

fileUploadGeometry :: Reflex t => Widget x ()
fileUploadGeometry = do
  text "Read GeoJSON from file"
  el "div" $ do
    clearGeometryBtn <- redButton "clear"
    filesBtn <- redButton "files"
    fileNameIndicator $ constDyn "placeholder.geojson"
    jsonFileInput

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

luciScenarios :: Reflex t => Widget x (Event t ())
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

luciScenarioPane :: Reflex t => Widget x (Event t ())
luciScenarioPane = do
  el "div" $ text "Remote (Luci scenarios)"
  el "div" $ do
    browseE <- browseScenarioWidget
    saveScenarioBtn <- saveScenarioWidget $ constDyn False -- Placeholder dynamic Bool
    fileNameIndicator $ constDyn "placeholder"
    return browseE

browseScenarioWidget :: Reflex t => Widget x (Event t ())
browseScenarioWidget = do
  (e, _) <- elClass' "a" "btn btn-red waves-attach waves-light waves-effect" $ text "Scenarios"
  return $ domEvent Click e

saveScenarioWidget :: Reflex t => Dynamic t Bool -> Widget x ()
saveScenarioWidget scenarioActive = elDynAttr "a" (attrs <$> scenarioActive) $ text "Save"
  where
    attrs active = ("class" =: "btn btn-red waves-attach waves-light waves-effect")
                <> ("style" =: ("display: " <> display active))
    display True  = "inline"
    display False = "none"

-- Pop up Scenario

popupScenario :: Reflex t => Event t () -> Widget x ()
popupScenario browseE = do
  browseScenarioPane browseE

browseScenarioPane :: Reflex t => Event t () -> Widget x ()
browseScenarioPane browseE = mdo
    paneActive <- holdDyn True $ leftmost [False <$ cancelE, True <$ browseE]
    cancelE <- elDynAttr "div" (attrs <$> paneActive) $ do
      elAttr "div" (("class" =: "modal-dialog") <> ("style" =: "max-height: 100%")) $ do
        elAttr "div" (("class" =: "modal-content") <> ("style" =: "max-height: 100%")) $ do
          elAttr "div" (("class" =: "modal-heading") <> ("style" =: "max-height: 10%")) $ do
            elClass "p" "modal-title" $ text "Select scenario"
          elAttr "div" (("class" =: "modal-inner") <> ("style" =: "max-height: 80%")) $ do
            blank -- for Scenario List Table
          elAttr "div" (("class" =: "modal-footer") <> ("style" =: "max-height: 10%")) $ do
            elClass "p" "text-right" $ do
              cancelButtonWidget
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

cancelButtonWidget :: Reflex t => Widget x (Event t ())
cancelButtonWidget = do
    (e, _) <- elAttr' "a" attrs $ text "Cancel"
    return $ domEvent Click e
  where
    attrs = ("class" =: "btn btn-flat btn-brand-accent waves-attach waves-effect")
         <> ("data-dismiss" =: "modal")



