{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where


--import Control.Concurrent

-- Various thins I use
--import Control.Arrow (second)
import Data.Geometry
--import JsHs
import JsHs.JSString (pack) -- JSString, append
--import Control.Monad (void, when)
import JsHs.Useful
--import Text.Read (readMaybe)
import Data.Coerce
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
--import JsHs.WebGL.Types (GLfloat)

import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
-- Program Logic
--import Reactive
import Program
--import Program.Model.Camera (CState(..))

-- Events
import qualified Program.Controllers.GeoJSONFileImport as JFI
import Program.Controllers.LuciClient
import qualified Program.Controllers.GUI as GUI


import qualified Data.Geometry.Transform as T
import Data.Geometry.Structure.Feature (FeatureCollection)
import qualified Data.Geometry.Structure.PointSet as PS
import Program.Model.CityObject

-- Get EventSense instances so that we can pass events into processing cycle
--import Program.Reactions ()

-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs
import Program.Model.Camera
import Program.Model.City
import Program.View
import Program.Settings
import Program.Types
import Program.VisualService
import Program.Model.CityGround

--import JsHs.Debug
import Data.Maybe (fromMaybe)
----import Control.Arrow (Arrow(..))

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
    JFI.registerClearGeometry clearFire
    (geoJSONImportsHandler, geoJSONImportFire) <- newAddHandler
    JFI.registerJSONFileImports (geoJSONImportFire . Left)
--    JFI.registerButton geoJSONImportsHandler importButton

    -- register user clicking on "get scenario" button
    (onAskForScenarioH, onAskForScenarioFire) <- newAddHandler
    registerAskLuciForScenario (curry onAskForScenarioFire)
    (getScListHandler, getScListFire) <- newAddHandler
    registerGetScenarioList getScListFire
    (onAskSaveScenarioH, onAskSaveScenarioFire) <- newAddHandler
    GUI.registerSaveScenario onAskSaveScenarioFire
    (onScenarioGetH, onScenarioGetFire) <- newAddHandler
    -- further luci+services stuff
    (serviceFinishH, serviceFinishFire) <- newAddHandler



    -- ground draws and updates
    (groundUpdateRequestH, groundUpdateRequestFire) <- newAddHandler
    GUI.registerServiceClear (const $ groundUpdateRequestFire GroundClearRequest >> GUI.toggleServiceClear False)
    GUI.registerServiceRun (const $ groundUpdateRequestFire GroundUpdateRequest >> GUI.toggleServiceClear True)

    -- get request processing
--    let userProfile = case getHtmlArg "role" of
--                    "edit" -> ExternalEditor
--                    "view" -> ExternalViewer
--                    _      -> Full
    let userProfile = Full


    canv <- getCanvasById "glcanvas"
--    view <- initView program canv

    -- reactive-banana event network
    heh <- elementHandler $ coerce canvas
    network <- compile $ mdo

      -- initial state of various params
      isize <- viewPortSize heh >>= valueB
      let icamera = initCamera (realToFrac $ coordX isize)
                               (realToFrac $ coordY isize)
                               CState { viewPoint  = vector3 3 0 0
                                      , viewAngles = (-pi/5, pi/12)
                                      , viewDist   = 30 }

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
      selObjIdB  <- selectedObjectIdBehavior pointerE cameraB (context <$> viewB)
      let allowCameraMoveB = f <$> selObjIdB <*> heldObjIdB
            where
              f _ Nothing = True
              f Nothing _ = True
              f (Just i) (Just j) | j /= i    = True
                                  | otherwise = False

      -- conrol camera
      cameraB <- cameraBehavior icamera
                                pointerE
                                wheelE
                                resizeE
                                buttonsB
                                coordsB
                                allowCameraMoveB

      -- object transform applies to any active object
      let objectTransformE :: Event (ObjectTransform T.QFTransform CityObject)
          objectTransformE = objectTransformEvents pointerE
                                                   buttonsB
                                                   coordsB
                                                   cameraB



      let settingsB = pure lsettings

      groundUpdateRequestE <- fromAddHandler groundUpdateRequestH
      -- city
      (cityChanges, cityB, errsE, motionRecordsE, groundUpdatedE) <- cityBehavior settingsB
                                           selObjIdB
                                           heldObjIdE
                                           objectTransformE
                                           cityChangeE
                                           groundUpdateRequestE

      -- a little bit of logging
      reactimate $ mapM_ logText' <$> errsE
      reactimate $ print <$> motionRecordsE


      let programB = initProgram userProfile settingsB cameraB cityB

      -- render scene
      updateE <- updateEvents heh
      viewB <- viewBehavior canv resizeE cityChanges updateE vsResultsE programB

      -- Luci Client testing
      (luciClientB, luciClientE, luciMessageE) <- luciHandler (fromMaybe "" $ luciRoute lsettings)
      let doLuciAction LuciClientOpening = logText' "Opening connection."
          doLuciAction LuciClientClosed = logText' "LuciClient WebSocket connection closed."
          doLuciAction (LuciClientError err) = logText' $ "LuciClient error: " <> err
          doLuciAction _luciClient = logText' "Luci is ready"
--            sendMessage luciClient $ runScenarioCreate "Our first scenario" (storeCityAsIs ci)
--            sendMessage luciClient runServiceList
--            sendMessage luciClient $ runTestFibonacci 10

      -- general response to luci messages
      reactimate $ (\msg -> parseLuciMessages (geoJSONImportFire . Right) onScenarioGetFire serviceFinishFire
                           ( msgHeaderValue msg
                           , attachments msg
                           )) <$>  luciMessageE


      -- actions to do when luci state changes
      reactimate $ doLuciAction <$> luciClientE

      -- asking luci for a scenario list on button click
      getScListE <- fromAddHandler getScListHandler
      reactimate $ flip sendMessage runScenarioList <$> luciClientB <@  getScListE

      -- asking luci to save a scenario on button click
      askSaveScenarioE <- fromAddHandler onAskSaveScenarioH
      scenarioSyncE_create <- mapEventIO id
           $ (\lc ci s -> do
              sendMessage lc $ runScenarioCreate s (storeCityAsIs ci)
              GUI.toggleSaveScenarioButton False s
              return $ SSPendingCreate s
            )
          <$> luciClientB <*> cityB <@> askSaveScenarioE

      -- Asking luci for a scenario on button click
      askForScenarioE <- fromAddHandler onAskForScenarioH
      reactimate $ (\lc (i,_) -> sendMessage lc (runScenarioGet i)) <$> luciClientB <@> askForScenarioE
      let getSync (SSSynced sId' name _) (sId, t) | sId == sId' && sId /= 0 = SSSynced sId name t
                                                  | otherwise = SSNotBound
          getSync SSEmpty (0, _) = SSEmpty
          getSync SSEmpty (sId, t) = SSSynced sId "Unknown scenario" t
          getSync SSNotBound (0, _) = SSNotBound
          getSync SSNotBound (sId, t) = SSSynced sId "Unknown scenario NB" t
          getSync (SSPendingCreate _) (0, _) = SSNotBound
          getSync (SSPendingCreate s) (sId, t) = SSSynced sId s t
          getSync (SSPendingGet sId' s) (sId, t) | sId == sId' && sId /= 0 = SSSynced sId s t
                                                 | otherwise = SSNotBound
      scenarioSyncE_obtained <- apply (getSync <$> scenarioSyncB) <$> fromAddHandler onScenarioGetH
      let scenarioSyncE_clearGeom = SSEmpty <$ clearGeometryE
          scenarioSyncE_extraUpdate = SSNotBound <$ fst (split geoJSONImportE)
          scenarioSyncE_get = uncurry SSPendingGet <$> askForScenarioE
          scenarioSyncE = unionsStepper [ scenarioSyncE_create
                                        , scenarioSyncE_get
                                        , scenarioSyncE_clearGeom
                                        , scenarioSyncE_extraUpdate
                                        , scenarioSyncE_obtained
                                        ]
      scenarioSyncB <- stepper SSEmpty scenarioSyncE


      -- Trying to keep scenario name
      let toggleSaveScenarioA SSEmpty = GUI.toggleSaveScenarioButton False ""
          toggleSaveScenarioA SSNotBound = GUI.toggleSaveScenarioButton True ""
          toggleSaveScenarioA (SSPendingCreate s) = GUI.toggleSaveScenarioButton False s
          toggleSaveScenarioA (SSPendingGet _ s) = GUI.toggleSaveScenarioButton False s
          toggleSaveScenarioA (SSSynced _ s t) = GUI.toggleSaveScenarioButton False
                                                 ("[" <> ScenarioName (pack $ formatTime defaultTimeLocale "%y.%m.%d-%H:%M:%S" t) <> "] " <> s)

          updateScenarioA (SSSynced sid _ _) lc@(LuciClient _) ci gId = sendMessage lc $ runScenarioUpdate sid (storeObjectsAsIs [gId] ci)
          updateScenarioA _ _ _ _ = return ()

          serviceRunsE = filterJust $ serviceRunsF <$> scenarioSyncB <@> groundUpdatedE
          serviceRunsF (SSSynced sid _ _) (GroundUpdated points) = Just $ VisualServiceRunPoints sid [] [] [] (fromJSArrayToTypedArray $ PS.flatten points)
          serviceRunsF _ _ = Nothing

          runIsovistServiceA lc@(LuciClient _) vsr = sendMessage lc $ makeRunRequest (VisualService "Isovist") vsr
          runIsovistServiceA _ _ = return ()

          askSubscribeForScenario (LuciClient _) SSSynced{} SSSynced{} = return ()
          askSubscribeForScenario lc@(LuciClient _) _ (SSSynced sid _ _) = sendMessage lc (runScenarioSubscribe sid)
          askSubscribeForScenario _ _ _ = return ()

      -- show reflect scenario sync state
      reactimate $ toggleSaveScenarioA <$> scenarioSyncE

      -- sync geometry with Luci
      lateObjectRecordsE <- execute $ return . fst <$> motionRecordsE
      reactimate $ updateScenarioA <$> scenarioSyncB <*> luciClientB <*> cityB <@> lateObjectRecordsE
      reactimate $ askSubscribeForScenario <$> luciClientB <*> scenarioSyncB <@> scenarioSyncE_obtained

      serviceFinishE <- fromAddHandler serviceFinishH
      (_vsManagerB, vsResultsE) <- vsManagerBehavior serviceFinishE

      -- run luci service!
      reactimate $ runIsovistServiceA <$> luciClientB <@> serviceRunsE



      return ()

      return ()

    actuate network
    play heh
    putStrLn "Program started."
    programIdle

parseLuciMessages :: (FeatureCollection -> IO ())
                  -> ((ScenarioId, UTCTime) -> IO ())
                  -> ((ServiceName, ServiceResult, JS.Array JSTA.ArrayBuffer) -> IO ())
                  -> (MessageHeader, JS.Array JSTA.ArrayBuffer) -> IO ()
parseLuciMessages jsonLoadedFire scenarioEnabledFire serviceRFire (MsgResult callID duration serviceName taskID result, att) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName ++ "' finished!"
  case serviceName of
    "ServiceList" -> print (JS.asLikeJS $ JS.asJSVal result :: LuciResultServiceList)
    "test.Fibonacci" -> print (JS.asLikeJS $ JS.asJSVal result :: LuciResultTestFibonacci)
    "scenario.GetList" -> displayScenarios result
    "scenario.geojson.Create" ->
       let LuciResultScenarioCreated scId t = (JS.asLikeJS $ JS.asJSVal result :: LuciScenarioCreated)
       in scenarioEnabledFire (scId, t)
    "scenario.geojson.Update" -> return () -- logText' "Scenario updated" -- printAny result
    "scenario.geojson.Get" ->
       let LuciResultScenario scId fc t = (JS.asLikeJS $ JS.asJSVal result :: LuciScenario)
       in jsonLoadedFire fc >> scenarioEnabledFire (scId, t)
    _ -> serviceRFire ( serviceName, result, att)
  print (callID, duration, taskID)
parseLuciMessages _ _ _ (MsgError err, _) = logText' $ "[remote error] " <> err
parseLuciMessages _ _ _ (MsgProgress callID duration serviceName taskID percentage progress, _) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName
           ++ "' is in progress, done " ++ show percentage
  case serviceName of
    "ServiceList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultServiceList)
    "test.Fibonacci" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultTestFibonacci)
--    "scenario.GetList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultScenarioList)
    s -> putStrLn ("Got some JSVal as a progress of service " ++ show s) -- >> printJSVal (JS.asJSVal progress)
  print (callID, duration, taskID)
parseLuciMessages _ _ _ (MsgNewCallID _, _) = return ()
parseLuciMessages _ _ _ _msg =
  logText' "Got unexpected message"
--  printJSVal $ JS.asJSVal msg

unionsStepper :: [Event a] -> Event a
unionsStepper [] = never
unionsStepper xs = foldr1 (unionWith (const id)) xs

