{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Control.Lens
import Data.Map (fromList, toList)
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

  propChangeD <- widgetHold (pure (never, never)) (renderInfo <$> propsGivenE)
  let propUpdatedE = switchPromptlyDyn $ fst <$> propChangeD
      propClickE   = switchPromptlyDyn $ snd <$> propChangeD
      (updateScE, updateObjE) = fanEither $ f <$> current selectedObjIdD <@> propUpdatedE
      f Nothing prop              = Left prop
      f (Just oId) (propN, propV) = Right (oId, propN, propV)
  registerEvent (ScenarioUpdate ObjectPropertyUpdated)   updateObjE
  registerEvent (ScenarioUpdate ScenarioPropertyUpdated) updateScE
  registerEvent (UserAction PropertyClicked) propClickE

-- | Returns tuple (property-updated-event, property-selected-event)
renderInfo :: Reflex t
           => Properties
           -> QuaWidget t x ( Event t (PropName, Maybe PropValue)
                            , Event t PropName )
renderInfo props = do
  -- draw an image above the info table if it is available
  forM_ (props^.previewImgUrl) $
    \imgUrl -> elAttr "img" ( "src" =: textFromJSString imgUrl
                            <> "style" =: "width: 100%" ) blank
  -- draw the info table
  let leftm (es1, es2) = (leftmost es1, leftmost es2)
  propsE <- fmap (leftm . unzip) $
              elClass "table" tableClass $ traverse renderProp $ toList props

  newPropE <- elClass "div" newPropClass renderAddProp
  return $ (leftmost [fst propsE, newPropE], snd propsE)
  where
    renderProp (pName, pVal)
        | Just val <- fromPropValue pVal >>= valToMTxt
        = do
            (kEl, updatedE) <- el "tr" $ do
              let PropName key = pName
              (kEl, _) <- el' "td" $ text $ textFromJSString key
              upE      <- el  "td" $ renderPropVal val
              return (kEl, upE)
            let propUpdatedE = ((,) pName) <$> updatedE
                propClickE   = pName <$ domEvent Click kEl
            return (propUpdatedE, propClickE)
        | otherwise = return (never, never)
    (tableClass, newPropClass) = $(do
        tableCls <- newVar
        newPropCls <- newVar
        qcss
          [cassius|
            .#{tableCls}
              width: 100%
              font-size: 10pt
              line-height: 15pt
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
                width: 40%
                cursor: pointer
              td>.icon
                margin-left: 10px
                cursor: pointer
              td input
                color: #BF360C
                background-color: white
                ~ .icon
                  display: none
            .#{newPropCls}
              margin-left: 50px
              margin-bottom: 50px
              >input
                width: 30%
                float: left
                margin-right: 10px
              .icon
                font-weight: bold
                font-size: 20px
                cursor: pointer
              .btn ~ .icon
                display: none
          |]
        returnVars [tableCls, newPropCls]
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
  let getTxt eFn = let f (Just t) = tagPromptlyDyn (t^.textInput_value) $ eFn t
                       f Nothing  = never
                   in  switchPromptOnly never $ f <$> updated mTxtInputD
  enterE     <- getTxt $ keypress Enter
  loseFocusE <- getTxt (\t -> () <$ ffilter not (updated $ t^.textInput_hasFocus))
  let saveE = leftmost [enterE, loseFocusE]
  (editBtn, _) <- elClass' "span" "icon" $ text "edit"
  let editableE = leftmost [
                   True  <$ domEvent Click editBtn
                 , False <$ saveE
                 ]
  return $ Just <$> toPropValue . parseValue <$> saveE

renderAddProp :: Reflex t
              => QuaWidget t x (Event t (PropName, Maybe PropValue))
renderAddProp = mdo
  let makeInput placeholder = do
        input <- textInput $ def & attributes .~ constDyn (fromList
                   [("class", "form-control"), ("placeholder", placeholder)])
        return $ current $ input^.textInput_value
  saveD <- let render False = blank >> return never
               render True  = do
                 ktB <- makeInput "Name"
                 vtB <- makeInput "Value"
                 saveE <- do
                   let cls = "btn btn-red waves-attach waves-light waves-effect"
                   (e, _) <- elClass' "button" cls $ text "Add"
                   return $ ElementClick <$ domEvent Click e
                 return $ (,) <$> ktB <*> vtB <@ saveE
           in  widgetHold (render False) (render <$> editableE)
  (addBtn, _) <- elClass' "span" "icon" $ text "add"
  let editableE = True <$ domEvent Click addBtn
  let toProp (k, v) = (PropName $ textToJSString k, Just $ toPropValue $ parseValue v)
  return $ toProp <$> switchPromptlyDyn saveD


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
