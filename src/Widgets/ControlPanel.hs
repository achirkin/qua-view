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

-- | Control panel widget is a place for all controls in qua-view!
controlPanel :: Reflex t => Widget x (Dynamic t ControlPanelState)
controlPanel = mdo
    stateD <- Dom.elDynClass "div" (toClass <$> stateD) $ do
      -- GUI control buttons
      controlButtonGroup
    return stateD
  where
    toClass ControlPanelOpen   = openState
    toClass ControlPanelClosed = closedState
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
          |]
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ fmap ((baseclass <> " ") <>) [ostate, cstate]
      )

