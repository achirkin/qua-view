{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where


import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Text as Text
import qualified Data.Map.Lazy as Map

import Reflex.Dom
import Reflex.Dom.Widget.Animation as Animation
import JavaScript.Web.Canvas


import Types

import Widgets.LoadingSplash

main :: IO ()
main = do
    mainWidget $ do
      -- Change the state of the program
      (isProgramBusy, setIsProgramBusy) <- newTriggerEvent
      loadingSplash isProgramBusy

      liftIO $ setIsProgramBusy Busy
      canvas <- el "div" $ do
          canvas' <- fst <$> elAttr' "canvas"
                            ( Map.fromList [("width", "600")
                                           ,("height", "250")
                                           ,("style"
                                            , Text.unwords
                                                  [ "margin: 22px;"
                                                  , "padding: 10px 20px 30px 25px;"
                                                  , "border-color: darkred;"
                                                  , "border-style: dashed;"
                                                  , "background-color: white;"
                                                  , "width: 65%;"
                                                  , "height: 400px;"]
                                            )]
                            ) (pure ())

          el "p" $ text "Reflex is:"
          el "ul" $ do
            el "li" $ text "Efficient"
            el "li" $ text "Higher-order"
            el "li" $ text "Glitch-free"
          return canvas'

      ctx <- liftIO $ getElementContext canvas

      -- initialize animation handler (and all pointer events).
      aHandler <- Animation.registerHandler (_element_raw canvas) Nothing -- (Just print)

      -- draw a square every time I click on canvas
      performEvent_ . ffor (select (pointerEvents aHandler) PClickEvent) $ \ev -> liftIO $ do
        fillStyle 0x00 0xAA 0x66 0.4 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-4) (y-4) 9 9 ctx

      -- another square on pointer up event
      performEvent_ . ffor (select (pointerEvents aHandler) PUpEvent) $ \ev -> liftIO $ do
        fillStyle 0x00 0xFF 0xFF 0.9 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-2) (y-2) 5 5 ctx

      -- Combine event with a behavior "is shift button pressed when pointer down event fires?".
      -- Conditionally on shift button pressed I draw rectangle in different styles.
      performEvent_ . ffor ((,) <$> shiftKeyB aHandler <@> select (pointerEvents aHandler) PDownEvent) $ \( shift, ev) -> liftIO $ do
        if shift then fillStyle 0x00 0x11 0x00 1.0 ctx
                 else fillStyle 0xFF 0xFF 0xAA 1.0 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-7) (y-7) 15 15 ctx

      -- Conditionally on ctrl button pressed, draw rectangle in different styles on mouse move events
      performEvent_ . ffor ((,) <$> ctrlKeyB aHandler <@> select (pointerEvents aHandler) PMoveEvent)
                    $ \(ctrl, ev) -> liftIO $ do
        if ctrl then fillStyle 0x55 0xFF 0x99 1.0 ctx
                else fillStyle 0x00 0x33 0xAA 0.3 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-3) (y-3) 7 7 ctx

      -- Pointer cancel event usually happens when mouse gets out of the element area
      performEvent_ . ffor (select (pointerEvents aHandler) PCancelEvent) $ \ev -> liftIO $ do
        fillStyle 0xFF 0x00 0x00 1.0 ctx
        forM_ (pointers ev) $ \(x,y) -> fillRect (x-12) (y-12) 25 25 ctx

      -- Clear screen based on mouse wheel event.
      -- Use current viewPortSize to find out dimensions of the element.
      performEvent_ . ffor ((,) <$> current (viewPortSizeD aHandler) <@> wheelEvents aHandler) $ \((w,h), ev) -> liftIO $ do
        case ev of
          WheelUp   -> fillStyle 0xFF 0xCC 0xCC 1.0 ctx
          WheelDown -> fillStyle 0xCC 0xCC 0xFF 1.0 ctx
        fillRect 0 0 w h ctx


      liftIO $ setIsProgramBusy Idle



getElementContext :: Element r s t -> IO Context
getElementContext = getContext . unsafeCoerce . _element_raw

--    -- get program settings
--    lsettings <- loadSettings
--    putStrLn "Getting program input settings:"
--
--    print lsettings
--    -- whole document
----    body <- documentBody
--
--    -- drawing area
--    canvas <- getElementById "glcanvas"
--    -- "import geometry" button converts GeoJSON into internal representation
----    importButton <- getElementById "jsonfileinput"
--    -- geoJSON updates
--    (clearGeomHandler, clearFire) <- newAddHandler
--    (geoJSONImportsHandler, geoJSONImportFire) <- newAddHandler
----    JFI.registerButton geoJSONImportsHandler importButton
--
--
--
--
--
--    -- ground draws and updates
--    (groundUpdateRequestH, groundUpdateRequestFire) <- newAddHandler
--    when (profile lsettings == Full) $ do
--      JFI.registerClearGeometry clearFire
--      JFI.registerJSONFileImports (geoJSONImportFire . Left)
--      GUI.registerServiceClear (const $ groundUpdateRequestFire GroundClearRequest)
--      GUI.registerServiceRun (const $ groundUpdateRequestFire GroundUpdateRequest)
--
--
--    canv <- getCanvasById "glcanvas"
----    view <- initView program canv
--
--    -- reactive-banana event network
--    heh <- elementHandler $ coerce canvas
--    network <- compile $ mdo
--
--      -- initial state of various params
--      isize <- viewPortSize heh >>= valueB
--      let icamera = initCamera (realToFrac $ coordX isize)
--                               (realToFrac $ coordY isize)
--                               CState { viewPoint  = vector3 (-17.5) (-17) 0
--                                      , viewAngles = (0.345, 0.825)
--                                      , viewDist   = 138 }
--
--      -- GeoJSON updates
--      geoJSONImportE <- fromAddHandler geoJSONImportsHandler
--      clearGeometryE <- fmap (const ClearingGeometry) <$> fromAddHandler clearGeomHandler
--      let cityChangeE = unionWith (const id) (CityUpdate . fun <$> geoJSONImportE) (CityErase <$ clearGeometryE)
--          fun (Left a) = a
--          fun (Right a) = a
--
--      -- canvas events
--      pointerE <- pointerEvents heh
--      wheelE   <- wheelEvents heh
--      resizeE  <- resizeEvents heh
--      curPointersB <- curPointers heh
--      oldPointersB <- downPointers heh
--      buttonsB' <- buttons heh
--      ctrlKeyB <- ctrlKey heh
--      shiftKeyB <- shiftKey heh
--      let modButtons True True 1 = 4
--          modButtons True False 1 = 2
--          modButtons False True 1 = 2
--          modButtons _ _ b = b
--          buttonsB = modButtons <$> shiftKeyB <*> ctrlKeyB <*> buttonsB'
--          coordsB = combinePointers <$> oldPointersB <*> curPointersB
--
--      -----------------------
--      -- program components
--      -----------------------
--
--      -- selection must go first for some reason (otherwise blocked by MVar)
--      (heldObjIdB, heldObjIdE) <- heldObjectIdBehavior pointerE cameraB (context <$> viewB)
--      (selObjIdB, selObjIdE)  <- selectedObjectIdBehavior pointerE cameraB (context <$> viewB)
--      let allowCameraMoveB = f <$> selObjIdB <*> heldObjIdB
--            where
--              f _ Nothing = True
--              f Nothing _ = True
--              f (Just i) (Just j) | j /= i    = True
--                                  | otherwise = False
--
--
--
--      -- conrol camera
--      (resetCamE, resetCamFire) <- newEvent
--      liftIO $ GUI.registerResetCamera resetCamFire
--      cameraB <- cameraBehavior icamera
--                                pointerE
--                                wheelE
--                                resizeE
--                                resetCamE
--                                buttonsB
--                                coordsB
--                                allowCameraMoveB
--
--      -- object transform applies to any active object
--      let objectTransformE :: Event (ObjectTransform T.QFTransform CityObject)
--          objectTransformE = objectTransformEvents pointerE
--                                                   buttonsB
--                                                   coordsB
--                                                   cameraB
--
--
--
--      settingsB <- stepper lsettings (lsettings{objectScale = Nothing} <$ clearGeometryE)
--
--      groundUpdateRequestE <- fromAddHandler groundUpdateRequestH
--      (colorizePropertyE, colorizePropertyFire) <- newEvent
--      liftIO $ GUI.registerColorizeProperty colorizePropertyFire
--      reactimate $ (\mx -> case mx of
--                            Nothing -> colorizePropertyFire Nothing
--                            _ -> return ()
--                   ) <$> selObjIdE
--      -- city
--      (vsResultsE', vsResultsFire') <- newEvent
--      (cityChanges, cityB, errsE, motionRecordsE, groundUpdatedE) <- cityBehavior settingsB
--                                           selObjIdB
--                                           colorizePropertyE
--                                           heldObjIdE
--                                           objectTransformE
--                                           cityChangeE
--                                           groundUpdateRequestE
--                                           vsResultsE'
--
--      -- clear ground when city is updated
--      reactimate $ groundUpdateRequestFire GroundClearRequest <$ cityChangeE
--
--      -- when in full mode, we have a grid. Reset it on object motion!
--      when (profile lsettings == Full) $
--        reactimate $ groundUpdateRequestFire GroundClearRequest <$ motionRecordsE
--
--      -- show building info on selection
--      let showInfoA _  Nothing  = GUI.showInfo newObj
--          showInfoA ci (Just i) = GUI.showInfo . maybe newObj (allProps . T.unwrap) $ getObject i ci
--      reactimate $ showInfoA <$> cityB <@> selObjIdE
--
--      -- a little bit of logging
--      reactimate $ mapM_ logText' <$> errsE
----      reactimate $ print <$> motionRecordsE
--
--
--      let programB = initProgram settingsB cameraB cityB
--
--      -- render scene
--      updateE <- updateEvents heh
--      (wantPictureE, wantPictureFire) <- newEvent
--      (viewB, pictureE) <- viewBehavior canv wantPictureE resizeE cityChanges updateE vsResultsE programB
--
--      -- use luci only in full profile
--      vsResultsE <- case profile lsettings of
--        Full -> luciBehavior lsettings geoJSONImportFire cityB groundUpdatedE geoJSONImportE clearGeometryE motionRecordsE
--        _ -> return never
--      reactimate $ vsResultsFire' <$> vsResultsE
--
--      -- log all actions if there is a place to log to
--      when (profile lsettings /= ExternalViewer) $
--       case loggingUrl lsettings of
--        Nothing -> return ()
--        Just url -> let onlyNew _ (CityNew _) = Just ()
--                        onlyNew ci (CityUpdate _) = if isEmptyCity ci then Just () else Nothing
--                        onlyNew _ _ = Nothing
--                        onlyUpd ci (CityUpdate fc) = if isEmptyCity ci then Nothing else Just fc
--                        onlyUpd _ _ = Nothing
--                        getNew ci () = (storeCityAsIs ci, fst $ cityTransform ci)
--                    in do
--          cityNews <- mapEventIO (const $ return ()) $ filterJust $ onlyNew <$> cityB <@> cityChangeE
--          Logging.logActions url motionRecordsE (getNew <$> cityB <@> cityNews) (filterJust $ onlyUpd <$> cityB <@> cityChangeE)
--
--
--      -- save submission if in edit mode
--      when (profile lsettings == ExternalEditor) $ do
--          (submissionE, submissionFire) <- newEvent
--          liftIO $ GUI.registerSubmit submissionFire
--          waitForPictureE <- mapEventIO (\f -> wantPictureFire WantPicture >> return f) submissionE
--          waitForPictureB <- stepper (return (return ())) waitForPictureE
--          reactimate $ (\s c f p -> let construct Nothing = return ()
--                                        construct (Just url) = f (url, storeCityAsIs c, asJSVal p)
--                                    in construct $ submitUrl s
--                       ) <$> settingsB <*> cityB <*> waitForPictureB <@> pictureE
--
--
--      return ()
--
--    actuate network
--    play heh
--    putStrLn "Program started."
--    programIdle
--
--    -- load geometry from a link if needed
--    case scenarioUrl lsettings of
--      Nothing  -> return ()
--      Just url -> JFI.loadGeoJSONFromLink url (geoJSONImportFire . Left)

