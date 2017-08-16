{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Widgets.ControlPanel
    ( controlPanel
    ) where

import qualified Reflex.Dom as Dom

import Commons
import Widgets.Generation
import Widgets.ControlButtons
import Widgets.Logger
import Widgets.Tabs
import Widgets.Tabs.Geometry
import Widgets.Tabs.Info
import Widgets.Tabs.Services


-- | Control panel widget is a place for all controls in qua-view!
controlPanel :: Reflex t => EventSelector t CompState ->  Widget x (Event t (ElementClick "Reset Camera"), Dynamic t (ComponentState "ControlPanel"))
controlPanel compStates = mdo
    (resetCameraE, stateD) <- Dom.elDynClass "div" (toClass <$> stateD) $ mdo

      -- tab pane
      _outputEvs <-
        Dom.elAttr "div" ("style" =: "overflow-y: auto; overflow-x: hidden; height: 100%;") $ do
          Dom.elAttr "div" ("style" =: "margin: 0; padding: 0; height: 56px;") Dom.blank
          runTabWidget $ do
            r <- addTab "Geometry" (flip runReaderT loggerFunc $ panelGeometry compStates)
            addTab "Info" panelInfo
            addTab "Services" panelServices
            return r

      loggerFunc <- loggerWidget
      flip runReaderT loggerFunc $ do
        logUser @JSString "Hey ho!"
        logUser @Text "He asdfsdf "
        logUser @String "Hehehehe!"
        logUser @JSString "This is only visible in console"
        logUser @JSString "1"
        logUser @JSString "H2"
        logUser @JSString "3!"
        logUser @JSString "777777777"
        logUser @JSString "88 88 88888"
        logUser @JSString "8899999999998"
        logUser @JSString "Wow! Tenth message!"
        logUser @JSString "The first message should go away by now."
        logDebug @JSString "control panel" "Secret message!"
        logInfo  @JSString "control panel" "Secret message!"
        logWarn  @JSString "control panel" "Secret message!"
        logError @JSString "control panel" "Secret message!"

      -- GUI control buttons
      controlButtonGroup

    return (resetCameraE, stateD)
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


