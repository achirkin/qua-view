{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Control.Lens
import Control.Lens.Indexed (itraverse_)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack)
import Reflex.Dom
import JavaScript.JSON.Types.Instances()
import qualified JavaScript.JSON.Types.Internal as GHCJS

import Commons
import Data.JSString.Text (textFromJSString)
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import Model.Scenario.Object (ObjectId (..))
import Model.Scenario.Properties
import Program.Scenario
import Widgets.Generation

data InfoData = InfoData (Maybe ObjectId) Properties

panelInfo :: Reflex t
          => Behavior t Scenario.Scenario
          -> Dynamic t (Maybe ObjectId)
          -> QuaWidget t x ()
panelInfo scenarioB selectedObjIdD = do
  scenarioPropUpdatedE <- askEvent $ ScenarioUpdate ScenarioPropertyUpdated
  scenarioUpdatedE     <- askEvent $ ScenarioUpdate ScenarioUpdated
  delayedE <- delay 0 $ leftmost --delay so we sample the behavior after its updated
    [ () <$ scenarioPropUpdatedE
    , () <$ scenarioUpdatedE
    ]
  let getScProps = view Scenario.properties
  let getData (s, mObjId) = InfoData mObjId props
        where
          props = case mObjId of
            Just objId -> fromMaybe (getScProps s) $ s ^?
                          Scenario.objects.(at objId)._Just.Object.properties
            Nothing    -> getScProps s
  let scLoadedE    = Nothing <$ delayedE
      objSelectedE = updated selectedObjIdD
      propE = getData <$> attach scenarioB (leftmost [objSelectedE, scLoadedE])
  void $ widgetHold (return ()) (renderInfo <$> propE)

renderInfo :: Reflex t => InfoData -> QuaWidget t x ()
renderInfo (InfoData mObjId props) = do
  when (isJust mObjId) $
    el "p" $ text "Object selected"
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
