{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where


import JsHs.LikeJS.Class (asJSVal)
import JsHs.Useful
--import JsHs.Nullable (Nullable(..))
import Control.Monad (when)
import Data.Geometry
import Data.Coerce
import qualified Data.Geometry.Transform as T


-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs

import Program
import Program.Model.Camera
import Program.Model.City
import Program.Model.CityObject
import Program.Model.LuciConnection
import Program.View
import Program.Settings
import Program.Types

-- Events
import qualified Program.Controllers.GeoJSONFileImport as JFI
import qualified Program.Controllers.GUI as GUI
import qualified Program.Controllers.Logging as Logging

main :: IO ()
main = do
    -- get program settings
    lsettings <- loadSettings
    putStrLn "Getting program input settings:"

    print lsettings
    -- whole document
--    body <- documentBody

    -- drawing area
    canvas <- getElementById "glcanvas"
    -- "import geometry" button converts GeoJSON into internal representation
--    importButton <- getElementById "jsonfileinput"
    -- geoJSON updates
    (clearGeomHandler, clearFire) <- newAddHandler
    (geoJSONImportsHandler, geoJSONImportFire) <- newAddHandler
--    JFI.registerButton geoJSONImportsHandler importButton





    -- ground draws and updates
    (groundUpdateRequestH, groundUpdateRequestFire) <- newAddHandler
    when (profile lsettings == Full) $ do
      JFI.registerClearGeometry clearFire
      JFI.registerJSONFileImports (geoJSONImportFire . Left)
      GUI.registerServiceClear (const $ groundUpdateRequestFire GroundClearRequest)
      GUI.registerServiceRun (const $ groundUpdateRequestFire GroundUpdateRequest)


    canv <- getCanvasById "glcanvas"
--    view <- initView program canv

    -- reactive-banana event network
    heh <- elementHandler $ coerce canvas
    network <- compile $ mdo

      -- initial state of various params
      isize <- viewPortSize heh >>= valueB
      let icamera = initCamera (realToFrac $ coordX isize)
                               (realToFrac $ coordY isize)
                               defaultCameraState

      -- GeoJSON updates
      geoJSONImportE <- fromAddHandler geoJSONImportsHandler
      clearGeometryE <- fmap (const ClearingGeometry) <$> fromAddHandler clearGeomHandler
      let cityChangeE = unionWith (const id) (CityUpdate . anyway <$> geoJSONImportE) (CityErase <$ clearGeometryE)
          anyway (Left a) = a
          anyway (Right a) = a

      -- canvas events
      pointerE <- pointerEvents heh
      wheelE   <- wheelEvents heh
      resizeE  <- resizeEvents heh
      curPointersB <- curPointers heh
      oldPointersB <- downPointers heh
      buttonsB' <- buttons heh
      ctrlKeyB <- ctrlKey heh
      shiftKeyB <- shiftKey heh
      let modButtons True True 1 = 4
          modButtons True False 1 = 2
          modButtons False True 1 = 2
          modButtons _ _ b = b
          buttonsB = modButtons <$> shiftKeyB <*> ctrlKeyB <*> buttonsB'
          coordsB = combinePointers <$> oldPointersB <*> curPointersB

      -----------------------
      -- program components
      -----------------------

      -- selection must go first for some reason (otherwise blocked by MVar)
      (heldObjIdB, heldObjIdE) <- heldObjectIdBehavior pointerE cameraB (context <$> viewB)
      (selObjIdB, selObjIdE)  <- selectedObjectIdBehavior pointerE cameraB (context <$> viewB)
      let allowCameraMoveB = f <$> selObjIdB <*> heldObjIdB
            where
              f _ Nothing = True
              f Nothing _ = True
              f (Just i) (Just j) | j /= i    = True
                                  | otherwise = False



      -- conrol camera
      (resetCamE, resetCamFire) <- newEvent
      liftIO $ GUI.registerResetCamera resetCamFire
      cameraB <- cameraBehavior icamera
                                geoJSONImportE
                                pointerE
                                wheelE
                                resizeE
                                resetCamE
                                buttonsB
                                coordsB
                                allowCameraMoveB

      -- object transform applies to any active object
      let objectTransformE :: Event (ObjectTransform T.QFTransform CityObject)
          objectTransformE = objectTransformEvents pointerE
                                                   buttonsB
                                                   coordsB
                                                   cameraB



      settingsB <- stepper lsettings (lsettings{objectScale = Nothing} <$ clearGeometryE)

      groundUpdateRequestE <- fromAddHandler groundUpdateRequestH
      (colorizePropertyE, colorizePropertyFire) <- newEvent
      liftIO $ GUI.registerColorizeProperty colorizePropertyFire
      reactimate $ (\mx -> case mx of
                            Nothing -> colorizePropertyFire Nothing
                            _ -> return ()
                   ) <$> selObjIdE
      -- city
      (vsResultsE', vsResultsFire') <- newEvent
      (cityChanges, cityB, errsE, motionRecordsE, groundUpdatedE) <- cityBehavior settingsB
                                           selObjIdB
                                           colorizePropertyE
                                           heldObjIdE
                                           objectTransformE
                                           cityChangeE
                                           groundUpdateRequestE
                                           vsResultsE'

      -- clear ground when city is updated
      reactimate $ groundUpdateRequestFire GroundClearRequest <$ cityChangeE

      -- when in full mode, we have a grid. Reset it on object motion!
      when (profile lsettings == Full) $
        reactimate $ groundUpdateRequestFire GroundClearRequest <$ motionRecordsE

      -- show building info on selection
      let showInfoA _  Nothing  = GUI.showInfo newObj
          showInfoA ci (Just i) = GUI.showInfo . maybe newObj (allProps . T.unwrap) $ getObject i ci
      reactimate $ showInfoA <$> cityB <@> selObjIdE

      -- a little bit of logging
      reactimate $ mapM_ logText' <$> errsE
--      reactimate $ print <$> motionRecordsE


      let programB = initProgram settingsB cameraB cityB

      -- render scene
      updateE <- updateEvents heh
      (wantPictureE, wantPictureFire) <- newEvent
      (viewB, pictureE) <- viewBehavior canv wantPictureE resizeE cityChanges updateE vsResultsE programB

      -- use luci only in full profile
      vsResultsE <- case profile lsettings of
        Full -> luciBehavior lsettings geoJSONImportFire cityB groundUpdatedE geoJSONImportE clearGeometryE motionRecordsE
        _ -> return never
      reactimate $ vsResultsFire' <$> vsResultsE

      -- log all actions if there is a place to log to
      when (profile lsettings /= ExternalViewer) $
       case loggingUrl lsettings of
        Nothing -> return ()
        Just url -> let onlyNew _ (CityNew _) = Just ()
                        onlyNew ci (CityUpdate _) = if isEmptyCity ci then Just () else Nothing
                        onlyNew _ _ = Nothing
                        onlyUpd ci (CityUpdate fc) = if isEmptyCity ci then Nothing else Just fc
                        onlyUpd _ _ = Nothing
                        getNew ci () = (storeCityAsIs ci, fst $ cityTransform ci)
                    in do
          cityNews <- mapEventIO (const $ return ()) $ filterJust $ onlyNew <$> cityB <@> cityChangeE
          Logging.logActions url motionRecordsE (getNew <$> cityB <@> cityNews) (filterJust $ onlyUpd <$> cityB <@> cityChangeE)


      -- save submission if in edit mode
      when (profile lsettings == ExternalEditor) $ do
          (submissionE, submissionFire) <- newEvent
          liftIO $ GUI.registerSubmit submissionFire
          waitForPictureE <- mapEventIO (\f -> wantPictureFire WantPicture >> return f) submissionE
          waitForPictureB <- stepper (return (return ())) waitForPictureE
          reactimate $ (\s c f p -> let construct Nothing = return ()
                                        construct (Just url) = f (url, storeCityAsIs c, asJSVal p)
                                    in construct $ submitUrl s
                       ) <$> settingsB <*> cityB <*> waitForPictureB <@> pictureE


      return ()

    actuate network
    play heh
    putStrLn "Program started."
    programIdle

    -- load geometry from a link if needed
    case scenarioUrl lsettings of
      Nothing  -> return ()
      Just url -> JFI.loadGeoJSONFromLink url (geoJSONImportFire . Left)

