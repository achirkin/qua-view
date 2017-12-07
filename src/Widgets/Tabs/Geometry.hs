{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Widgets.Tabs.Geometry
    ( panelGeometry
      -- TODO: I have put it here just to keep a warning away. Should be inside the panel.
    , luciScenarioPane
    ) where

import Commons
import Reflex.Dom
import Control.Lens
import Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set
import qualified GHCJS.DOM.JSFFI.Generated.HTMLInputElement as JSFFI (setValue)
import qualified GHCJS.DOM.JSFFI.Generated.File as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.FileReader as JSFFI
import qualified GHCJS.DOM.EventM as JSFFI



import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import           Model.Scenario.Properties
import           Model.Camera
import           Program.UserAction
import           Program.Scenario

import Widgets.Commons
import Widgets.Generation
import Widgets.Modal.BrowseScenarios
import Widgets.Modal.SaveScenario

import qualified SmallGL


panelGeometry :: Reflex t
              => SmallGL.RenderingApi
              -> Behavior t Scenario.Scenario
              -> Dynamic t (Maybe Object.ObjectId)
              -> Dynamic t Camera
              -> QuaWidget t x ()
panelGeometry renderingApi scenarioB selectedObjD cameraD = do
    ucPaneSD <- uploadNclearPane
    whenActive ucPaneSD hr

    doPaneSD <- deleteObjectPane selectedObjD
    whenActive doPaneSD hr

    cloneObjectPane renderingApi scenarioB cameraD



----------------------------------------------------------------------------------------------------
-- * Modify geometry
----------------------------------------------------------------------------------------------------


deleteObjectPane :: Reflex t
                 => Dynamic t (Maybe Object.ObjectId)
                 -> QuaWidget t x (Dynamic t (ComponentState "deleteObjectPane"))
deleteObjectPane selectedObjD = do
    selectedObjI <- sample $ current selectedObjD
    clicksDE <- widgetHold (deleteWidget selectedObjI) (deleteWidget <$> updated selectedObjD)
    registerEvent (ScenarioUpdate ObjectDeleted) (switchPromptlyDyn clicksDE)
    return cstateD
  where
    cstateD = cstateF <$> selectedObjD
    cstateF (Just _) = Active
    cstateF Nothing  = Inactive
    deleteWidget (Just i) = (i <$) <$> buttonRed "Delete selected object" def
    deleteWidget Nothing  = return never


cloneObjectPane :: Reflex t
                 => SmallGL.RenderingApi
                 -> Behavior t Scenario.Scenario
                 -> Dynamic t Camera
                 -> QuaWidget t x ()
cloneObjectPane renderingApi scenarioB cameraD = do
    scsUpdateE <- askEvent (ScenarioUpdate ScenarioStateUpdatedOut)
    let camCenterB = viewPoint . newState <$> current cameraD
        imgWidth = 160
        imgHeight = 160
    cloneDE <- widgetHold (pure never) $ ffor (scenarioB <@ scsUpdateE) $ \scenario -> do
      let getTemplateObjIds (Left objId) = [objId]
          getTemplateObjIds (Right gId)  = scenario^..Scenario.viewState
                                                     .Scenario.objectGroups
                                                     .at gId._Just.traverse
          objIds :: [[Object.ObjectId]]
          objIds = scenario^..Scenario.viewState
                             .Scenario.templates
                             .to Set.toList
                             .traverse
                             .to getTemplateObjIds
          objs = flip map objIds $ \oids ->
              oids >>= \i -> (,) i <$> (scenario^..Scenario.objects.at i._Just)

          objsDatas = flip map objs . map $ \(i,o) ->
              ( (i, o^.Object.properties.property "name")
              , ( o^.Object.renderingId
                , Scenario.resolvedObjectColorIgnoreVisible scenario o ^. colorVeci)
              )

      -- render previews
      urlsIds <- liftIO $ forM objsDatas $ \objsData -> do
        url <- SmallGL.renderObjToImg renderingApi (imgWidth, imgHeight) $ map snd objsData
        return (url, map fst objsData)

      -- show widget
      unless (null urlsIds) $
        el "div" $ text "Add objects from a palette"
      cloneEs <- forM urlsIds $ \(imgUrl, obs) -> do
        (e, ()) <- elClass' "div" cloneObjectDivClass $ do
          elAttr "img" ("src" =: textFromJSString imgUrl) blank
          el "span" $ text $ (obs^?traverse._2._Just)^.non "Unnamed object"
        return $ (,) (map fst obs) <$> camCenterB <@ domEvent Click e
      return $ leftmost cloneEs

    registerEvent (ScenarioUpdate ObjectCloned) $ switchPromptlyDyn cloneDE

cloneObjectDivClass :: Text
cloneObjectDivClass = $(do
    cloneDiv <- newVar
    qcss
      [cassius|
        .#{cloneDiv}
          margin: 5px auto 5px auto
          padding: 0px 20px 0px 20px
          box-shadow: 0 -1px 0 #e5e5e5, 0 0 2px rgba(0,0,0,.12), 0 2px 4px rgba(0,0,0,.24)
          border-radius: 2px
          box-sizing: border-box
          overflow: hidden
          cursor: pointer
        .#{cloneDiv}:hover
          background: #FF5722
        .#{cloneDiv} span
          margin: 10px
          color: #ff6f00
          vertical-align: middle
          display: inline-block
          text-transform: uppercase
          user-select: none
          font-size: 16px
        .#{cloneDiv}:hover span
          color: white
        .#{cloneDiv} img
          width: 80px
          display: inline-block
          vertical-align: middle
          margin: -5px 0 -5px 0;
      |]
    returnVars [cloneDiv]
  )


----------------------------------------------------------------------------------------------------
-- | Clear whole geometry or upload geometry from file
----------------------------------------------------------------------------------------------------


uploadNclearPane :: Reflex t => QuaWidget t x (Dynamic t (ComponentState "uploadNclearPane"))
uploadNclearPane = do
    el "div" $ text "Read GeoJSON from file"
    (clearGeometryClickedE, geometryLoadedE) <-
      el "div" $ do
        -- clear geometry button
        cgClicked <- buttonRed @"ClearGeometry" "Clear" def
        -- File upload button and its dynamic label
        rezE <- fileUpload cgClicked

        return ( () <$ cgClicked, rezE)
    registerEvent (UserAction AskClearGeometry) clearGeometryClickedE
    registerEvent GeometryLoaded geometryLoadedE
    return $ constDyn Active



-- | Widget gets an event of clearing geometry to erase current file name label.
--   Returns two events: file loading errors and content events.
fileUpload :: Reflex t
           => Event t (ElementClick "ClearGeometry")
           -> QuaWidget t x (Event t LoadedTextContent)
fileUpload clearGeomEv = mdo
    _ <- elAttr "label" ("for" =: finputId) $ buttonRed "Files" def
    elAttr "div" ("style" =: "display:inline; font-size: 0.9em;"
               <> "class" =: smallMarginClass)
                 $ dynText fileNameD
    fInput <- fileInput $ def & fileInputConfig_attributes
                              %~ (fmap . mappend $ "style" =: hideWorkaroundStile
                                                <> "id" =: finputId)
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
    logErrorEvents "fileUpload" $ leftmost
                             [ getJSError <$> errE
                             , "Error happened when executing FileReader.readAsText." <$ onErrorE
                             ]
    return rezE
  where
    finputId = $(newVar >>= returnVars . (:[]))
    headMaybe (x:_) = Just x
    headMaybe [] = Nothing
    hideWorkaroundStile = "position:fixed;top:-10000px;left:-10000px;"
     -- "visibility:hidden;width:0;height:0;padding:0;margin:0;border:0;min-height:0;"
    maybeNotRead Nothing = Left "Could not read file (FileReader.readAsText returned Nothing)."
    maybeNotRead (Just r) = case castToJSString r of
        Nothing -> Left "Scenario file must be a text file, but got binary file."
        Just s  -> Right $ LoadedTextContent s



----------------------------------------------------------------------------------------------------
-- | Luci Scenario
----------------------------------------------------------------------------------------------------

-- TODO: this is only a stub at the moment
luciScenarioPane :: Reflex t => QuaWidget t x ()
luciScenarioPane = do
    el "div" $ text "Remote (Luci scenarios)"
    el "div" $ do
      buttonBrowseScenarios >>= registerEvent (UserAction AskSelectScenario)
      buttonSaveScenario >>= registerEvent (UserAction AskSaveScenario)


-- | User indicates that they want to save scanerio with a specified name (via pop-up)
buttonSaveScenario :: Reflex t => QuaWidget t x (Event t Text)
buttonSaveScenario = buttonRed "Save" def >>= lift . popupSaveScenario

-- | User selects
buttonBrowseScenarios :: Reflex t => QuaWidget t x (Event t ScId)
buttonBrowseScenarios = buttonRed "Scenarios" def >>= lift . popupBrowseScenarios





