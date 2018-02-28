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
import Data.Map (fromList, toList, filterWithKey)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Text.Read (double)
import Data.Time.Calendar
import Data.Time.Clock
import Reflex.Dom
import GHCJS.DOM.JSFFI.Generated.HTMLElement (focus)
import JavaScript.JSON.Types.Instances()
import qualified JavaScript.JSON.Types.Internal as GHCJS
import qualified QuaTypes
import QuaTypes.Submission

import Commons
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import Model.Scenario.Object (ObjectId (..), ObjectBehavior (..))
import Model.Scenario.Properties
import Program.Scenario
import Widgets.Generation

panelInfo :: Reflex t
          => Behavior t Scenario.Scenario
          -> Dynamic t (Maybe ObjectId)
          -> QuaWidget t x ()
panelInfo scenarioB selectedObjIdD = do

  -- Show user-provided description of their submission
  renderSubmissionDescription

  (delayedE, updateCB) <- newTriggerEvent
  askEvent (ScenarioUpdate ObjectPropertyUpdated)   >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioPropertyUpdated) >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioUpdated)         >>= triggerDelayed updateCB
  askEvent (ScenarioUpdate ScenarioCleared)         >>= triggerDelayed updateCB

  settingsD <- quaSettings
  let permsB = QuaTypes.permissions <$> current settingsD
      showHiddenB = QuaTypes.showHiddenProperties <$> permsB
      canEditB    = canEditF <$>  scenarioB <*> current selectedObjIdD <*> permsB
      canEditF _   Nothing   perms = canEditBasicF perms
      canEditF sc (Just oid) perms = canEditBasicF perms && canEditObjF sc oid perms
      canEditBasicF p = QuaTypes.canEditProperties p && not (QuaTypes.isViewerOnly p)
      canEditObjF s oid perms = case beh of
          Static -> QuaTypes.canModifyStaticObjects perms
          Dynamic -> True
        where
          beh = fromMaybe Dynamic $ s^?Scenario.objects.at oid._Just.Object.objectBehavior
      propsGivenE = getVisibleProps <$> showHiddenB <*> scenarioB <@> propsAllE
      propsAllE = getAllProps <$> scenarioB
                              <@> leftmost [ current selectedObjIdD <@ delayedE
                                           , updated selectedObjIdD]
      getVisibleProps True _ = id
      getVisibleProps False s = filterWithKey f
        where
          f (PropName k) _ = Set.notMember k $ Set.fromList (s^.Scenario.hiddenProperties)
                             -- use withoutKeys when we have containers >= 0.5.8
      getAllProps s mid = fromMaybe (s^.Scenario.properties)
                           ( mid >>= \i -> s^?Scenario.objects.at i._Just.Object.properties )


  -- draw an image above the info table if it is available
  _ <- widgetHold blank $ ffor propsAllE $ \props ->
    forM_ (props^.previewImgUrl) $
        \imgUrl -> elAttr "img" ( "src" =: textFromJSString imgUrl
                                <> "style" =: "width: 100%" ) blank


  propChangeD <- widgetHold (pure (never, never)) (renderInfo <$> canEditB <@> propsGivenE)
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
           => Bool -- ^ Can edit properties
           -> Properties
           -> QuaWidget t x ( Event t (PropName, Maybe PropValue)
                            , Event t PropName )
renderInfo canEdit props = do

  -- draw the info table
  let leftm (es1, es2) = (leftmost es1, leftmost es2)
  propsE <- fmap (leftm . unzip) $
              elClass "table" tableClass $
              traverse renderProp $ toList props

  newPropE <- if canEdit
              then elClass "div" newPropClass renderAddProp
              else return never
  return $ (leftmost [fst propsE, newPropE], snd propsE)
  where
    renderProp (pName, pVal)
        | Just val <- fromPropValue pVal >>= valToMTxt
        = do
            (kEl, updatedE) <- el "tr" $ do
              let PropName key = pName
              (kEl, _) <- el' "td" $ text $ textFromJSString key
              upE      <- el  "td" $ if canEdit
                                     then renderPropVal val
                                     else text val >> return never
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
                .icon
                  visibility: visible
              .icon
                visibility: hidden
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
                    renderV True  = do
                      t <- textInput $ def
                                  & textInputConfig_initialValue .~ val
                                  & attributes .~ constDyn ("class" =: "form-control")
                      builtE  <- getPostBuild
                      builtE' <- delay 0.1 builtE
                      performEvent_ $ (liftIO . focus $ _textInput_element t) <$ builtE'
                      return $ Just t
                in  widgetHold (renderV False) (renderV <$> editableE)
  let getTxt eFn = let f (Just t) = tagPromptlyDyn (t^.textInput_value) $ eFn t
                       f Nothing  = never
                   in  switchPromptOnly never $ f <$> updated mTxtInputD
  enterE     <- getTxt $ keypress Enter
  loseFocusE <- getTxt (\t -> () <$ ffilter not (updated $ t^.textInput_hasFocus))
  let saveE = leftmost [enterE, loseFocusE]
  (editBtn, _) <- elClass' "span" "icon" $ text "edit"
  let editBtnClickE = domEvent Click editBtn

  let editableE = leftmost [
                   True  <$ editBtnClickE
                 , False <$ saveE
                 ]
  return $ fmap toPropValue . parseValue <$> saveE

renderAddProp :: Reflex t
              => QuaWidget t x (Event t (PropName, Maybe PropValue))
renderAddProp = mdo
  let makeInput placeholder = do
        input <- textInput $ def & attributes .~ constDyn (fromList
                   [("class", "form-control"), ("placeholder", placeholder)])
        return (current $ input^.textInput_value, input)
  saveD <- let render False = blank >> return never
               render True  = do
                 (ktB, kt) <- makeInput "Name"
                 (vtB, vt) <- makeInput "Value"
                 performEvent_ $ (liftIO $ focus $ _textInput_element vt) <$
                   keypress Enter kt
                 btnE <- do
                   let cls = "btn btn-red waves-attach waves-light waves-effect"
                   (e, _) <- elClass' "button" cls $ text "Add"
                   return $ () <$ domEvent Click e
                 let saveE = leftmost [btnE, keypress Enter vt]
                 return $ (,) <$> ktB <*> vtB <@ saveE
           in  widgetHold (render False) (render <$> editableE)
  (addBtn, _) <- elClass' "span" "icon" $ text "add"
  let editableE = True <$ domEvent Click addBtn
  let toProp (k, v) = (PropName $ textToJSString k, toPropValue <$> parseValue v)
  return $ toProp <$> switchPromptlyDyn saveD


triggerDelayed :: Reflex t => (() -> IO ()) -> Event t a -> QuaWidget t x ()
triggerDelayed updateCB e = performEvent_ $ liftIO (updateCB ()) <$ e

parseValue :: Text -> Maybe (GHCJS.Value')
parseValue "" = Nothing
parseValue v
    | v `elem` ["True", "true"]   = Just $ GHCJS.Bool True
    | v `elem` ["False", "false"] = Just $ GHCJS.Bool False
    | Right n <- double v         = Just $ GHCJS.Number $ fst n
    | otherwise                   = Just $ GHCJS.String $ textToJSString v

valToMTxt :: GHCJS.Value' -> Maybe Text
valToMTxt (GHCJS.String "") = Nothing
valToMTxt (GHCJS.String v)  = Just $ textFromJSString v
valToMTxt (GHCJS.Number v) | i <- round v :: Int
                           , fromIntegral i == v = Just $ pack $ show i
                           | otherwise           = Just $ pack $ show v
valToMTxt (GHCJS.Bool v)    = Just $ pack $ show v
valToMTxt _                 = Nothing


-- | Show the scenario description provided by a user (student).
--   Contains a text that students enter on "Share your ideas" question when they submit their work.
renderSubmissionDescription :: Reflex t => QuaWidget t x ()
renderSubmissionDescription = do
    qsets <- quaSettings
    let infoUrl = QuaTypes.getSubmissionInfoUrl <$> qsets
    info <- httpGetNowOrOnUpdate infoUrl
    void $ widgetHold (pure ()) $ runWidget <$> info
  where
    runWidget (Left err) = logWarn "Sub-Description widget" err
    runWidget (Right si)
      = elAttr "div" ( "class" =: "card" <> "style" =: "padding: 0;" )
        $ elAttr "div" ( "class" =: "card-main" <> "style" =: "padding: 0; margin: 0;" )
          $ elAttr "div" ( "class" =: "card-inner"
                        <> "style" =: "padding: 2px; margin: 0; min-height: 40px;" )
            $ do
              elAttr "p" ( "style" =: "margin: 6px; color: #b71c1c; float: left;" )
                $ text $ textFromJSString $ subInfoUserName si
              elAttr "p" ( "style" =: "margin: 6px; color: #b71c1c; float: right;" )
                $ text $ pack . showGregorian . utctDay $ subInfoTime si
              (e, ()) <- elAttr' "p" ( "style" =: "margin: 8px;" ) blank
              setInnerHTML e "&nbsp;"
              elAttr "p" ( "style" =: "white-space: pre-line; margin: 2px;" )
                $ text $ textFromJSString $ subInfoDescription si


