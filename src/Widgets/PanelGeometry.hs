{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelGeometry
    ( panelGeometry
    ) where

import Control.Monad (void)
import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

panelGeometry :: Reflex t => Dynamic t PanelState -> Widget x ()
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

fileUploadGeometry :: Reflex t => Widget x ()
fileUploadGeometry = do
  text "Read GeoJSON from file"
  el "div" $ do
    clearGeometryBtn
    filesBtn
    void $ makeElementFromHtml def $(qhtml
      [hamlet|
        <div style="display:inline; font-size: 0.9em;">
        <input>
      |])

luciScenarios :: Reflex t => Widget x ()
luciScenarios = text "Luci"

clearGeometryBtn :: Reflex t => Widget x ()
clearGeometryBtn = elClass "a" "btn btn-red waves-attach waves-light waves-effect" $ text "clear"

filesBtn :: Reflex t => Widget x ()
filesBtn = elClass "a" "btn btn-red waves-attach waves-light waves-effect" $ text "files"

-- fileNameInput :: Reflex t => Widget x ()
-- fileNameInput 