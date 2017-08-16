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
import Control.Monad.Trans.Class (lift)
import qualified GHCJS.DOM.JSFFI.Generated.HTMLInputElement as JSFFI (setValue)
import qualified GHCJS.DOM.JSFFI.Generated.File as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.FileReader as JSFFI
import qualified GHCJS.DOM.EventM as JSFFI

import Commons
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal.BrowseScenarios
import Widgets.Modal.SaveScenario


-- | GADT that helps to group all outgoing events from geometry tab GUI.
data GeometryTabOutE e where
  GeomOutUserSelectedScenario  :: GeometryTabOutE UserSelectedScenario
  GeomOutUserAsksSaveScenario  :: GeometryTabOutE UserAsksSaveScenario
  GeomOutUserAsksClearGeometry :: GeometryTabOutE (ElementClick "ClearGeometry")
  GeomOutUserLoadsGeomFile     :: GeometryTabOutE LoadedTextContent
  -- add here more output events


-- TODO do the same trick for input events if necessary


panelGeometry :: forall t x . Reflex t
              => EventSelector t CompState
              -> WidgetWithLogs x (EventSelector t GeometryTabOutE)
panelGeometry _compStateEvs = do

    el "div" $ text "Read GeoJSON from file"
    clearGeometryClickedE <-
      el "div" $ do
        -- clear geometry button
        cgClicked <- lift $ buttonRed @"ClearGeometry" "Clear" def
        -- File upload button and its dynamic label
        rezE <- fileUpload cgClicked
        -- Log file content on debug level
        logDebugEvents "LoadingFileGeometry" $ getTextContent <$> rezE

        return cgClicked

    (asksSaveScenarioE, asksSelectScenarioE) <- lift luciScenarioPane

    -- combine all outgoing events together
    let outEvsSel :: forall a . GeometryTabOutE a -> Event t a
        outEvsSel GeomOutUserAsksClearGeometry = clearGeometryClickedE
        outEvsSel GeomOutUserAsksSaveScenario  = asksSaveScenarioE
        outEvsSel GeomOutUserSelectedScenario  = asksSelectScenarioE

    return $ EventSelector outEvsSel









-- | Widget gets an event of clearing geometry to erase current file name label.
--   Returns two events: file loading errors and content events.
fileUpload :: Reflex t
           => Event t (ElementClick "ClearGeometry")
           -> WidgetWithLogs x (Event t LoadedTextContent)
fileUpload clearGeomEv = mdo
    _ <- lift $ buttonRed "Files" ("onclick" =: ("document.getElementById('" <> finputId <> "').click()"))
    elAttr "div" ("style" =: "display:inline; font-size: 0.9em;"
               <> "class" =: smallMarginClass)
                 $ dynText fileNameD
    fInput <- fileInput $ def & fileInputConfig_attributes
                              %~ (fmap . mappend $ "style" =: "display:none" <> "id" =: finputId)
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
       fmap (fanEither . fmap maybeNotRead) . performEvent $
               -- erase content of file input after we got the file
               (JSFFI.setValue (_fileInput_element fInput) ("" :: JSString)
                -- get file content
            >> JSFFI.getResult freader)
            <$ onLoadEndE
    logUserEvents $ leftmost [ getJSError <$> errE
                             , "Error happened when executing FileReader.readAsText." <$ onErrorE
                             ]
    return rezE
  where
    finputId = $(newVar >>= returnVars . (:[]))
    headMaybe (x:_) = Just x
    headMaybe [] = Nothing
    maybeNotRead Nothing = Left "Could not read file (FileReader.readAsText returned Nothing)."
    maybeNotRead (Just r) = case castToJSString r of
        Nothing -> Left "Scenario file must be a text file, but got binary file."
        Just s  -> Right $ LoadedTextContent s



-- | User indicates that they want to save scanerio with a specified name (via pop-up)
buttonSaveScenario :: Reflex t => Widget x (Event t UserAsksSaveScenario)
buttonSaveScenario = buttonRed "Save" def >>= popupSaveScenario

-- | User selects
buttonBrowseScenarios :: Reflex t => Widget x (Event t UserSelectedScenario)
buttonBrowseScenarios = buttonRed "Scenarios" def >>= popupBrowseScenarios


luciScenarioPane :: Reflex t
                 => Widget x ( Event t UserAsksSaveScenario
                             , Event t UserSelectedScenario )
luciScenarioPane = do
  el "div" $ text "Remote (Luci scenarios)"
  el "div" $ do
    userSelectedScenarioE    <- buttonBrowseScenarios
    userWantsToSaveScenarioE <- buttonSaveScenario
    performEvent_ $ (\(UserAsksSaveScenario r) -> liftIO $ print r) <$> userWantsToSaveScenarioE
    -- TODO: fileNameIndicator $ constDyn "placeholder" -- current scenario name
    return (userWantsToSaveScenarioE, userSelectedScenarioE)



