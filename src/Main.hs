{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where


--import Control.Concurrent

-- Various thins I use
--import Control.Arrow (second)
import Data.Geometry
--import JsHs
import JsHs.JSString (JSString, unpack') -- JSString, append
--import Control.Monad (void, when)
import JsHs.Useful
--import Text.Read (readMaybe)
import Data.Coerce
import qualified JsHs.Array as JS
import JsHs.WebGL.Types (GLfloat)

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

import JsHs.Debug
import Data.Maybe (fromMaybe)

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
    JFI.registerJSONFileImports geoJSONImportFire
--    JFI.registerButton geoJSONImportsHandler importButton

    -- register user clicking on "get scenario" button
    (onAskForScenarioH, onAskForScenarioFire) <- newAddHandler
    registerAskLuciForScenario (curry onAskForScenarioFire)
    (getScListHandler, getScListFire) <- newAddHandler
    registerGetScenarioList getScListFire
    (onAskSaveScenarioH, onAskSaveScenarioFire) <- newAddHandler
    GUI.registerSaveScenario onAskSaveScenarioFire

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
      let cityChangeE = unionWith (const id) (CityNew <$> geoJSONImportE) (CityErase <$ clearGeometryE)

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

      -- city
      (cityChanges, cityB, errsE) <- cityBehavior settingsB
                                           selObjIdB
                                           heldObjIdE
                                           objectTransformE
                                           cityChangeE

      -- a little bit of logging
      reactimate $ mapM_ logText' <$> errsE

      let programB = initProgram userProfile settingsB cameraB cityB

      -- render scene
      updateE <- updateEvents heh
      viewB <- viewBehavior canv resizeE cityChanges updateE programB

      -- Luci Client testing
      (luciClientB, luciClientE, luciMessageE) <- luciHandler (fromMaybe "" $ luciRoute lsettings)
      let doLuciAction LuciClientOpening = logText' "Opening connection."
          doLuciAction LuciClientClosed = logText' "LuciClient WebSocket connection closed."
          doLuciAction (LuciClientError err) = logText $ "LuciClient error: " ++ unpack' err
          doLuciAction _luciClient = logText' "Luci is ready"
--            sendMessage luciClient $ runScenarioCreate "Our first scenario" (storeCityAsIs ci)
--            sendMessage luciClient runServiceList
--            sendMessage luciClient $ runTestFibonacci 10

      -- general response to luci messages
      reactimate $ (parseLuciMessages geoJSONImportFire . msgHeaderValue) <$>  luciMessageE

      -- actions to do when luci state changes
      reactimate $ doLuciAction <$> luciClientE

      -- asking luci for a scenario list on button click
      getScListE <- fromAddHandler getScListHandler
      reactimate $ flip sendMessage runScenarioList <$> luciClientB <@  getScListE

      -- asking luci to save a scenario on button click
      askSaveScenarioE <- fromAddHandler onAskSaveScenarioH
      scenarioNameE1 <- mapEventIO id
           $ (\lc ci s -> do
              sendMessage lc $ runScenarioCreate s (storeCityAsIs ci)
              GUI.toggleSaveScenarioButton False s
              return s
            )
          <$> luciClientB <*> cityB <@> askSaveScenarioE

      -- Asking luci for a scenario on button click
      askForScenarioE <- fromAddHandler onAskForScenarioH
      scenarioNameE2 <- mapEventIO id
            $ (\lc (i,s) -> sendMessage lc (runScenarioGet i) >> return s) <$> luciClientB <@> askForScenarioE

      -- Trying to keep scenario name
      let scenarioNameE = ("" <$ clearGeometryE)
                     +^^+ scenarioNameE1
                     +^^+ scenarioNameE2
          (+^^+) = unionWith (const id)
          showSaveButtonE1 = cityLuciAct <$> cityB <@> luciClientE
          showSaveButtonE2 = luciCityAct <$> luciClientB <@> cityChangeE
          cityLuciAct ci (LuciClient _) = not $ isEmptyCity ci
          cityLuciAct _ _ = False
          luciCityAct (LuciClient _) (CityNew _) = True
          luciCityAct _ _ = False
          showSaveButtonE = showSaveButtonE1 +^^+ showSaveButtonE2
      scenarioNameB <- stepper "" scenarioNameE
      showSaveButtonB <- stepper False showSaveButtonE
      reactimate $ GUI.toggleSaveScenarioButton <$> showSaveButtonB <@> scenarioNameE
      reactimate $ flip GUI.toggleSaveScenarioButton <$> scenarioNameB <@> showSaveButtonE
      reactimate $ GUI.toggleSaveScenarioButton False "" <$ filterE (\(RequireViewUpdate s) -> isEmptyCity s) cityChanges

      -- Tracking sync status of Luci and City
--      let syncE1 = cityChangeToSync <$> cityChangeE
--          cityChangeToSync CityErase _ = CityNotBound
--          cityChangeToSync _ CityNotBound = CityNotBound
--          cityChangeToSync (CityUpdate _) CityNotBound = error "cityChangeToSync problem, need to decide on something better"
--          cityChangeToSync _ _ = CityNotBound
--          syncE2 = luciChangeToSync <$> luciClientE
--          luciChangeToSync
--      let syncE1 = cityChangeE

--data ScenarioSync
--  = CityNotBound
--  | CityUpdated ScenarioId JSString [Int]
--  | CitySynced ScenarioId JSString

--  | CityUpdate FeatureCollection
--    -- ^ update geometry
--  | CityNew FeatureCollection
--    -- ^ create whole new geometry


      return ()

--      selHelB <- stepper (Nothing, Nothing) selHelE
--      let selHelE = filterApply ((/=) <$> selHelB) $ (,) <$> selObjIdB <*> heldObjIdB <@ updateE
--      _voidE <-  mapEventIO id $ (\c t -> putStrLn (show (activeObjId c) ++ " " ++ show t)) <$> cityB <@> selHelE

      return ()

    actuate network
    play heh
    putStrLn "Program started."
    programIdle

parseLuciMessages :: (FeatureCollection -> IO ()) -> MessageHeader -> IO ()
parseLuciMessages jsonLoadedFire (MsgResult callID duration serviceName taskID result) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName ++ "' finished!"
  case serviceName of
    "ServiceList" -> print (JS.asLikeJS $ JS.asJSVal result :: LuciResultServiceList)
    "test.Fibonacci" -> print (JS.asLikeJS $ JS.asJSVal result :: LuciResultTestFibonacci)
    "scenario.GetList" -> displayScenarios result
--      let ScenarioList xs = JS.asLikeJS $ JS.asJSVal result :: LuciResultScenarioList
--      print xs
--      case xs of
--        scd:_ -> sendMessage luci $ runScenarioGet (sscId scd)
--        _ -> return ()
    "scenario.geojson.Get" ->
       let LuciResultScenario _scId fc = (JS.asLikeJS $ JS.asJSVal result :: LuciScenario)
       in jsonLoadedFire fc
    s -> putStrLn ("Got some JSVal as a result of service " ++ show s) -- >> printJSVal (JS.asJSVal result)
  print (callID, duration, taskID)
parseLuciMessages _ (MsgError err) = putStrLn "Luci returned error" >> print err
parseLuciMessages _ (MsgProgress callID duration serviceName taskID percentage progress) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName
           ++ "' is in progress, done " ++ show percentage
  case serviceName of
    "ServiceList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultServiceList)
    "test.Fibonacci" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultTestFibonacci)
--    "scenario.GetList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultScenarioList)
    s -> putStrLn ("Got some JSVal as a progress of service " ++ show s) -- >> printJSVal (JS.asJSVal progress)
  print (callID, duration, taskID)
parseLuciMessages _ (MsgNewCallID i) = putStrLn $ "Luci assigned new callID " ++ show i
parseLuciMessages _ _msg = do
  logText' "Got unexpected message"
--  printJSVal $ JS.asJSVal msg



