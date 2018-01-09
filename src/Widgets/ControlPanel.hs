{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Widgets.ControlPanel
    ( controlPanel
    ) where

import qualified Reflex.Dom as Dom
import qualified QuaTypes

import Commons
import Model.Camera (Camera)
import Model.Scenario (Scenario)
import SmallGL (RenderingApi)
import Widgets.Generation
import Widgets.ControlButtons
import Widgets.UserMessages
import Widgets.Tabs
import Widgets.Tabs.Geometry
import Widgets.Tabs.Info
import Widgets.Tabs.Reviews
--import Widgets.Tabs.Services -- TODO: wait when luci services are implemented
import Model.Scenario.Object (ObjectId (..))


-- | Control panel widget is a place for all controls in qua-view!
controlPanel :: Reflex t
             => RenderingApi
             -> Behavior t Scenario
             -> Dynamic t (Maybe ObjectId)
             -> Dynamic t Camera
             -> QuaWidget t x (Dynamic t (ComponentState "ControlPanel"))
controlPanel renderingApi scenarioB selectedObjIdD cameraD = mdo
    settingsD <- quaSettings
    let permsD   = QuaTypes.permissions <$> settingsD
        showUcD  = QuaTypes.canEditProperties    <$> permsD
        showAdD  = QuaTypes.canAddDeleteGeometry <$> permsD
        showGeoD = (&&) <$> showUcD <*> showAdD

    stateD <- Dom.elDynClass "div" (toClass <$> stateD) $ mdo
      -- tab pane
      (_selTabD, ())
        <- Dom.elAttr "div" ("style" =: "overflow-y: auto; overflow-x: hidden; height: 100%;") $ do
          Dom.elAttr "div" ("style" =: "margin: 0; padding: 0; height: 56px;") Dom.blank
          runTabWidget $ do
            addTab "Geometry" $ panelGeometry showUcD showAdD
              renderingApi scenarioB selectedObjIdD cameraD
            addTab "Info" $ panelInfo scenarioB selectedObjIdD
            addTab "Reviews" panelReviews
            -- addTab "Services" panelServices

      -- view user message widget and register its handlers in qua-view monad
      userMessageWidget >>= replaceUserMessageCallback

      -- GUI control buttons
      controlButtonGroup renderingApi scenarioB

    return stateD
  where
    toClass Active   = openState
    toClass Inactive = closedState
    -- Styles for the panel are generated statically.
    -- newVar guarantees that the class name is unique.
    (openState, closedState) = $(do
        baseclass <- newVar
        let ostate = baseclass <> "-open"
            cstate = baseclass <> "-closed"
        qcss
          [cassius|
            .#{baseclass}
                display: -webkit-flex;
                display: flex
                -webkit-flex-direction: column;
                flex-direction: column
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
          |] -- TODO padding properties in tabContentClass lead to incorrect layout of the tab pane. consider removing it.
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ fmap ((baseclass <> " ") <>) [ostate, cstate]
      )
