{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE TypeApplications #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Control.Lens.Getter
import Control.Lens.Indexed (itraverse_)
import Data.Text (pack)
import Reflex.Dom
import JavaScript.JSON.Types.Instances()
import qualified JavaScript.JSON.Types.Internal as GHCJS

import Commons
import Data.JSString.Text (textFromJSString)
import qualified Model.Scenario as Scenario
import Model.Scenario.Properties
import Program.Scenario
import Widgets.Generation

panelInfo :: Reflex t => (Behavior t Scenario.Scenario) -> QuaWidget t x ()
panelInfo scenarioB = do
  scenarioPropUpdated <- askEvent $ ScenarioUpdate ScenarioPropertyUpdated
  scenarioUpdated     <- askEvent $ ScenarioUpdate ScenarioUpdated
  delayedE <- delay 0 $ leftmost --delay so we sample the behavior after its updated
    [ () <$ scenarioPropUpdated
    , () <$ scenarioUpdated
    ]
  let propE = view Scenario.properties <$> scenarioB <@ delayedE
  void $ widgetHold (return ()) (renderInfo <$> propE)

renderInfo :: Reflex t => Properties -> QuaWidget t x ()
renderInfo props = do
  elClass "table" tableClass $ itraverse_ renderProp props
  where
    renderProp (PropName key) = propValue $ \mvval -> do
      let render v' = do
            el "td" $ text $ textFromJSString key
            el "td" $ text v'
      case mvval of
        Just val -> el "tr" $ do
          case val of
            GHCJS.String v -> render $ textFromJSString v
            GHCJS.Number v -> render $ pack $ show v
            GHCJS.Bool v   -> render $ pack $ show v
            _ -> return ()
        Nothing -> return ()
      return (GHCJS.Null :: GHCJS.Value')
    tableClass = $(do
        tableCls <- newVar
        qcss
          [cassius|
            .#{tableCls}
              width: 100%
              tr:nth-child(even)
                background: white
              tr:hover
                background: #FF5722
                color: white
              td
                padding: 5px
              td:first-child
                text-align: right
          |]
        returnVars [tableCls]
      )
