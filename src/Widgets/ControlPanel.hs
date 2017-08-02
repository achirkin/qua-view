{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
module Widgets.ControlPanel
    ( controlPanel, ControlPanelState (..)
    ) where

import qualified Reflex.Dom as Dom
import Data.Semigroup

import CommonTypes
import Widgets.Generation
import Widgets.ControlButtons
import Widgets.Modal.BrowseScenario
import Widgets.Modal.EdxGuide
import Widgets.Modal.Help
import Widgets.Modal.SaveScenario
import Widgets.Tabs

-- | Control panel widget is a place for all controls in qua-view!
controlPanel :: Reflex t => Widget x (Dynamic t ControlPanelState)
controlPanel = mdo
    stateD <- Dom.elDynClass "div" (toClass <$> stateD) $ do
      (browsePopupE, savePopupE) <- Dom.elClass "div" tabContent panelTabs
      browseScenarioPane browsePopupE
      saveScenarioPane savePopupE
      -- GUI control buttons
      (stateD', popupHelpE) <- controlButtonGroup
      popupHelp popupHelpE
      return stateD'
    return stateD
  where
    toClass ControlPanelOpen   = openState
    toClass ControlPanelClosed = closedState
    -- Styles for the panel are generated statically.
    -- newVar guarantees that the class name is unique.
    (tabContent, openState, closedState) = $(do
        baseclass <- newVar
        tabContentClass <- newVar
        let ostate = baseclass <> "-open"
            cstate = baseclass <> "-closed"
        qcss
          [cassius|
            .#{baseclass}
                position: fixed
                opacity: 0.95
                top: 0
                padding: 0
                margin: 0
                z-index: 3
                overflow: visible
                max-width: 95%
                width: 400px
                height: 100%
                background-color: #FFFFFF
                -webkit-transition: right 300ms ease-in-out,min-width 300ms ease-in-out
                -moz-transition: right 300ms ease-in-out,min-width 300ms ease-in-out
                -o-transition: right 300ms ease-in-out,min-width 300ms ease-in-out
                transition: right 300ms ease-in-out,min-width 300ms ease-in-out

            .#{ostate}
                box-shadow: 15px 15px 15px 15px #999999
                min-width: 20%
                right: 0px

            .#{cstate}
                min-width: 0%
                box-shadow: 0

            @media (max-width: 400px)
                .#{ostate}
                    right: -95%

            @media (min-width: 401px)
                .#{cstate}
                    box-shadow: 0
                    right: -400px

            .#{tabContentClass}
                padding-left: 20px;
                padding-right: 20px;
          |] -- TODO padding properties in tabContentClass lead to incorrect layout of the tab pane. consider removing it.
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ tabContentClass : fmap ((baseclass <> " ") <>) [ostate, cstate]
      )

