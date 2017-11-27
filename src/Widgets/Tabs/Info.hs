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
import Data.Map (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack)
import Data.Text.Read (double)
import Reflex.Dom
import JavaScript.JSON.Types.Instances()
import qualified JavaScript.JSON.Types.Internal as GHCJS

import Commons
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
  propUpdatedED <- widgetHold (return never) (renderInfo <$> propE)
  let propUpdatedE = switchPromptlyDyn propUpdatedED
      (updateScE, updateObjE) = fanEither $ f <$> current selectedObjIdD <@> propUpdatedE
      f Nothing prop              = Left prop
      f (Just oId) (propN, propV) = Right (oId, propN, propV)
  registerEvent (ScenarioUpdate ObjectPropertyUpdated)   updateObjE
  registerEvent (ScenarioUpdate ScenarioPropertyUpdated) updateScE

renderInfo :: Reflex t => InfoData -> QuaWidget t x (Event t (PropName, Maybe PropValue))
renderInfo (InfoData mObjId props) = do
  when (isJust mObjId)
    $ el "p" $ text "Object selected"
  fmap leftmost $ elClass "table" tableClass $ traverse renderProp $ toList props
  where
    renderProp (PropName key, pval)
        | mvval <- fromPropValue pval :: Maybe GHCJS.Value' = do
      updatedE <- case mvval of
        Just val -> if isJust (valToMTxt val)
                    then el "tr" $ do
                      el "td" $ text $ textFromJSString key
                      el "td" $ renderPropVal val
                    else return never
        Nothing -> return never
      return $ ((,) (PropName key)) <$> updatedE
    tableClass = $(do
        tableCls <- newVar
        qcss
          [cassius|
            .#{tableCls}
              tr:nth-child(even)
                background: white
              tr:hover
                background: #FF5722
                color: white
              td
                padding: 5px 5px 5px 15px
              td:first-child
                text-align: right
              td>.icon
                margin-left: 10px
                cursor: pointer
              td input
                width: 180px
                color: #BF360C
                background-color: white
                ~ .icon
                  display: none
          |]
        returnVars [tableCls]
      )

renderPropVal :: Reflex t
              => GHCJS.Value'
              -> QuaWidget t x (Event t (Maybe PropValue))
renderPropVal val = mdo
  valB <- hold val newValE
  let mvalB = valToMTxt <$> valB
  mTxtInputD <- let renderV editable = do
                      mval <- sample mvalB
                      case mval of
                        Just v -> case editable of
                          False -> text v >> return Nothing
                          True  -> do
                            t <- textInput $ def
                                   & textInputConfig_initialValue .~ v
                                   & attributes .~ constDyn ("class" =: "form-control")
                            return $ Just t
                        Nothing -> return Nothing
                in  widgetHold (renderV False) (renderV <$> editableE)
  saveE <- let getEnter (Just t) = tagPromptlyDyn (t^.textInput_value) --TODO: filter out empty input
                                     $ keypress Enter t
               getEnter Nothing  = never
           in  switchPromptOnly never $ getEnter <$> updated mTxtInputD
  (editBtn, _) <- elClass' "span" "icon" $ text "edit"
  let editableE = leftmost [
                   True  <$ domEvent Click editBtn
                 , False <$ saveE
                 ]
  let newValE = parseValue <$> saveE
  return $ Just <$> toPropValue <$> newValE

parseValue :: Text -> GHCJS.Value'
parseValue v
    | v `elem` ["True", "true"]   = GHCJS.Bool True
    | v `elem` ["False", "false"] = GHCJS.Bool False
    | Right n <- double v         = GHCJS.Number $ fst n
    | otherwise                   = GHCJS.String $ textToJSString v

valToMTxt :: GHCJS.Value' -> Maybe Text
valToMTxt (GHCJS.String v) = Just $ textFromJSString v
valToMTxt (GHCJS.Number v) = Just $ pack $ show v
valToMTxt (GHCJS.Bool v)   = Just $ pack $ show v
valToMTxt _                = Nothing
