{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Geometry
    ( panelGeometry
    , GeometryTabOutE (..)
    , UserSelectedScenario (..)
    , UserAsksSaveScenario (..)
    ) where

import Reflex.Dom
import Control.Lens ((%~), (^.))
import Control.Monad.Trans.Reader
import qualified GHCJS.DOM.JSFFI.Generated.File as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.FileReader as JSFFI
import qualified GHCJS.DOM.EventM as JSFFI
import qualified GHCJS.DOM.Types as JSFFI

import Commons
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal.BrowseScenarios
import Widgets.Modal.SaveScenario


type ErrorStr = JSString

-- | GADT that helps to group all outgoing events from geometry tab GUI.
data GeometryTabOutE e where
  GeomOutUserSelectedScenario  :: GeometryTabOutE UserSelectedScenario
  GeomOutUserAsksSaveScenario  :: GeometryTabOutE UserAsksSaveScenario
  GeomOutUserAsksClearGeometry :: GeometryTabOutE (ElementClick "ClearGeometry")
  GeomOutUserLoadsGeomFile     :: GeometryTabOutE JSString
  -- add here more output events


-- TODO do the same trick for input events if necessary


panelGeometry :: forall t x . Reflex t => EventSelector t CompState -> Widget x (EventSelector t GeometryTabOutE)
panelGeometry compStateEvs = do

    el "div" $ text "Read GeoJSON from file"
    clearGeometryClicked <-
      el "div" $ do
        -- clear geometry button
        cgClicked <- buttonRed @"ClearGeometry" "Clear" def
        -- File upload button and its dynamic label
        (errE, rezE) <- fileUpload cgClicked
        performEvent_ $ liftIO . print <$> errE
        performEvent_ $ liftIO . print <$> rezE

        return cgClicked

    luciScenarios

    -- combine all outgoing events together
    let outEvsSel :: forall a . GeometryTabOutE a -> Event t a
        outEvsSel GeomOutUserAsksClearGeometry = clearGeometryClicked

    return $ EventSelector outEvsSel












-- | Widget gets an event of clearing geometry to erase current file name label.
--   Returns two events: file loading errors and content events.
fileUpload :: Reflex t => Event t (ElementClick "ClearGeometry") -> Widget x (Event t ErrorStr, Event t JSString)
fileUpload clearGeomEv = mdo
    _ <- buttonRed "Files" ("onclick" =: ("document.getElementById('" <> finputId <> "').click()"))
    elAttr "div" ("style" =: "display:inline; font-size: 0.9em;" <> "class" =: smallMarginClass) $ dynText fileNameD
    fInput <- fileInput $ def & fileInputConfig_attributes %~ (fmap . mappend $ "style" =: "display:none" <> "id" =: finputId)
    let fileD = headMaybe <$> (fInput ^. fileInput_value)
    fileNameE <- performEvent $ maybe (pure "") JSFFI.getName <$> updated fileD
    fileNameD <- holdDyn "" $ leftmost [fileNameE, "" <$ clearGeomEv]
    -- create JavaSript file reader
    freader <- JSFFI.newFileReader
    -- create a reflex event trigger to put file load events there
    (onLoadEndE, loadEndCallback) <- newTriggerEvent
    (onErrorE  , errorCallback)   <- newTriggerEvent
    -- register reflex callback on file reader loading event
    void . liftIO $ JSFFI.on freader JSFFI.loadEnd $ ReaderT loadEndCallback
    void . liftIO $ JSFFI.on freader JSFFI.error $ ReaderT errorCallback
    void . liftIO $ JSFFI.on freader JSFFI.abortEvent $ ReaderT errorCallback
    performEvent_ $ flip (JSFFI.readAsText freader) (Nothing :: Maybe JSString) <$> updated fileD
    -- get events of completion or errors
    (errE, rezE) <-
       fmap (fanEither . fmap maybeNotRead) . performEvent $ JSFFI.getResult freader <$ onLoadEndE
    return ( leftmost
              [ errE
              , "Error happened when executing FileReader.readAsText." <$ onErrorE
              ]
           , rezE)
  where
    finputId = $(newVar >>= returnVars . (:[]))
    headMaybe (x:_) = Just x
    headMaybe [] = Nothing
    maybeNotRead Nothing = Left "Could not read file (FileReader.readAsText returned Nothing)."
    maybeNotRead (Just r) = case JSFFI.nullableToMaybe (hopeForString r) of
        Nothing -> Left "Scenario file must be a text file, but got binary file."
        Just s  -> Right s



foreign import javascript unsafe "((typeof $1 === 'string') ? $1 : null)"
    hopeForString :: JSFFI.StringOrArrayBuffer -> JSFFI.Nullable JSString







fileUploadGeometry :: Reflex t => Widget x (Event t (ElementClick "ClearGeometry"), Event t JSString)
fileUploadGeometry = do
  el "div" $ text "Read GeoJSON from file"
  el "div" $ do
    clearGeometryBtn <- buttonRed "Clear" def -- TODO: Clear Geometry

    fInput <- fileInput def
    _filesBtn <- buttonRed "Files" def -- TODO: Connect this with jsonFileInput for file uploading
    fileNameIndicator $ constDyn "" -- TODO: Dynamic text correspond with the uploaded file name
    jsonFileInput -- TODO: file uploadings

    return (clearGeometryBtn, never)



fileNameIndicator :: Reflex t => Dynamic t Text -> Widget x ()
fileNameIndicator = elAttr "div" attrs . dynText
  where
    attrs = "style" =: "display: inline; font-size: 0.9em;"

jsonFileInput :: Reflex t => Widget x ()
jsonFileInput = elAttr "input" attrs blank
  where
    attrs = ("style" =: "display:none") <> ("type" =: "file")

-- Luci Scenario

data LuciState = Connected | Disconnected
  deriving Eq

luciScenarios :: Reflex t => Widget x (EventSelector t GeometryTabOutE)
luciScenarios = do
    let luciState = constDyn Connected -- Placeholder
    elDynAttr "div" (attrs1 <$> luciState) $
      el "div" $ text "Luci scenarios are not available"
    elDynAttr "div" (attrs2 <$> luciState) luciScenarioPane
  where
    attrs1 state = "style" =: ("display: " <> display1 state)
    attrs2 state = "style" =: ("display: " <> display2 state)
    display1 Connected = "none"
    display1 Disconnected = "block"
    display2 Connected = "block"
    display2 Disconnected = "none"

luciScenarioPane :: forall t x . Reflex t => Widget x (EventSelector t GeometryTabOutE)
luciScenarioPane = do
  el "div" $ text "Remote (Luci scenarios)"
  el "div" $ do
    userSelectedScenarioE <- browseScenariosWidget
    userWantsToSaveScenarioE <- saveScenarioWidget $ constDyn True -- TODO: Hide this button when there is no active scenario
    fileNameIndicator $ constDyn "placeholder" -- TODO: Dynamic text correspond with saved file name
    let selectorFun :: forall e . GeometryTabOutE e -> Event t e
        selectorFun GeomOutUserSelectedScenario = userSelectedScenarioE
        selectorFun GeomOutUserAsksSaveScenario = userWantsToSaveScenarioE
    return $ EventSelector selectorFun


browseScenariosWidget :: Reflex t => Widget x (Event t UserSelectedScenario)
browseScenariosWidget = buttonRed "Scenarios" def >>= popupBrowseScenarios

saveScenarioWidget :: Reflex t => Dynamic t Bool -> Widget x (Event t UserAsksSaveScenario)
saveScenarioWidget scenarioActive = do
  (e, _) <- elDynAttr' "a" (attrs <$> scenarioActive) $ text "Save"
  popupSaveScenario $ ElementClick <$ domEvent Click e
  where
    attrs active = ("class" =: "btn btn-red waves-attach waves-light waves-effect")
                <> ("style" =: ("margin: 2px; display: " <> displayClass active))
    displayClass True  = "inline"
    displayClass False = "none"
