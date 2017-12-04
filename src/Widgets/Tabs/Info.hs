{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Control.Lens
import Data.Map (toList)
import Data.Maybe (fromMaybe)
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

panelInfo :: Reflex t
          => Behavior t Scenario.Scenario
          -> Dynamic t (Maybe ObjectId)
          -> QuaWidget t x ()
panelInfo scenarioB selectedObjIdD = do
  (delayedE, updateCB) <- newTriggerEvent
  askEvent (ScenarioUpdate ObjectPropertyUpdated)   >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioPropertyUpdated) >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioUpdated)         >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioCleared)         >>= triggerDelayed updateCB

  let propsGivenE = getProps <$> scenarioB
                             <@> leftmost [ current selectedObjIdD <@ delayedE
                                          , updated selectedObjIdD]
      getProps s mid = fromMaybe (s^.Scenario.properties)
                     ( mid >>= \i -> s^?Scenario.objects.at i._Just.Object.properties )

  propUpdatedED <- widgetHold (pure never) (renderInfo <$> propsGivenE)
  let propUpdatedE = switchPromptlyDyn propUpdatedED
      (updateScE, updateObjE) = fanEither $ f <$> current selectedObjIdD <@> propUpdatedE
      f Nothing prop              = Left prop
      f (Just oId) (propN, propV) = Right (oId, propN, propV)
  registerEvent (ScenarioUpdate ObjectPropertyUpdated)   updateObjE
  registerEvent (ScenarioUpdate ScenarioPropertyUpdated) updateScE

renderInfo :: Reflex t => Properties -> QuaWidget t x (Event t (PropName, Maybe PropValue))
renderInfo props = do
  -- draw an image above the info table if it is available
  forM_ (props^.previewImgUrl) $
    \imgUrl -> elAttr "img" ( "src" =: textFromJSString imgUrl
                            <> "style" =: "width: 100%" ) blank
  -- draw the info table
  fmap leftmost $ elClass "table" tableClass $ traverse renderProp $ toList props
  where
    renderProp (PropName key, pval)
        | Just val <- fromPropValue pval >>= valToMTxt
        = do updatedE <- el "tr" $ do
               el "td" $ text $ textFromJSString key
               el "td" $ renderPropVal val
             return $ ((,) (PropName key)) <$> updatedE
        | otherwise = return never
    tableClass = $(do
        tableCls <- newVar
        qcss
          [cassius|
            .#{tableCls}
              width: 100%
              font-size: 10pt
              line-height: 15pt
              tr:
                width: 100%
              tr:nth-child(even)
                background: white
              tr:nth-child(odd)
                background: #FFF5EE
              tr:hover
                background: #FF5722
                color: white
              td
                padding: 3px 5px 3px 5px
                overflow: hidden
                word-wrap: break-word
                word-break: break-word
              td:first-child
                text-align: right
                width: 50%
              td>.icon
                margin-left: 10px
                cursor: pointer
              td input
                color: #BF360C
                background-color: white
                ~ .icon
                  display: none
          |]
        returnVars [tableCls]
      )

-- | Assume value is constant, because we redraw the whole list on every update anyway.
renderPropVal :: Reflex t
              => Text
              -> QuaWidget t x (Event t (Maybe PropValue))
renderPropVal val = mdo
  mTxtInputD <- let renderV False = text val >> return Nothing
                    renderV True
                      = fmap Just
                      . textInput $ def
                                  & textInputConfig_initialValue .~ val
                                  & attributes .~ constDyn ("class" =: "form-control")
                in  widgetHold (renderV False) (renderV <$> editableE)
  saveE <- let getEnter (Just t) = tagPromptlyDyn (t^.textInput_value) $ keypress Enter t
               getEnter Nothing  = never
           in  switchPromptOnly never $ getEnter <$> updated mTxtInputD
  (editBtn, _) <- elClass' "span" "icon" $ text "edit"
  let editableE = leftmost [
                   True  <$ domEvent Click editBtn
                 , False <$ saveE
                 ]
  return $ Just <$> toPropValue . parseValue <$> saveE

triggerDelayed :: Reflex t => (() -> IO ()) -> Event t a -> QuaWidget t x ()
triggerDelayed updateCB e = performEvent_ $ liftIO (updateCB ()) <$ e

parseValue :: Text -> GHCJS.Value'
parseValue v
    | v `elem` ["True", "true"]   = GHCJS.Bool True
    | v `elem` ["False", "false"] = GHCJS.Bool False
    | Right n <- double v         = GHCJS.Number $ fst n
    | otherwise                   = GHCJS.String $ textToJSString v

valToMTxt :: GHCJS.Value' -> Maybe Text
valToMTxt (GHCJS.String "") = Nothing
valToMTxt (GHCJS.String v)  = Just $ textFromJSString v
valToMTxt (GHCJS.Number v) | i <- round v :: Int
                           , fromIntegral i == v = Just $ pack $ show i
                           | otherwise           = Just $ pack $ show v
valToMTxt (GHCJS.Bool v)    = Just $ pack $ show v
valToMTxt _                 = Nothing
