{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
) where


--import Control.Concurrent

-- Various thins I use
--import Control.Arrow (second)
import Data.Geometry
import JsHs
import JsHs.JSString (unpack') -- JSString, append
--import Control.Monad (void, when)
import GHCJS.Useful
--import Text.Read (readMaybe)
import Data.Coerce
import qualified JsHs.Array as JS
import JsHs.WebGL.Types (GLfloat)

-- Program Logic
--import Reactive
import Program
--import Program.Model.Camera (CState(..))

-- Events
import qualified Controllers.GeoJSONFileImport as JFI
import Controllers.LuciClient


import qualified Data.Geometry.Transform as T
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
    (geoJSONImportsHandler, geoJSONImportTrigger) <- JFI.geoJSONFileImports
--    JFI.registerButton geoJSONImportsHandler importButton

    -- register user clicking on "get scenario" button
    (onAskForScenarioH, onAskForScenarioFire) <- newAddHandler
    registerAskLuciForScenario onAskForScenarioFire
    (getScListHandler, getScListFire) <- newAddHandler
    registerGetScenarioList getScListFire

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
      (cityChanges, cityB) <- cityBehavior settingsB
                                           selObjIdB
                                           heldObjIdE
                                           objectTransformE
                                           geoJSONImportE
                                           clearGeometryE

      let programB = initProgram userProfile settingsB cameraB cityB

      -- render scene
      updateE <- updateEvents heh
      viewB <- viewBehavior canv resizeE cityChanges updateE programB

      -- Luci Client testing
      (luciClientB, luciClientE, luciMessageE) <- luciHandler (fromMaybe "" $ luciRoute lsettings)
      let (+^^+) = unionWith (\a b -> a >> b)
          _unitE1 = (\c -> parseLuciMessages geoJSONImportTrigger c . msgHeaderValue) <$> luciClientB <@> luciMessageE
          _unitE2 = doLuciAction <$> cityB <*> luciClientB <@> luciClientE
          doLuciAction _ _ LuciClientOpening = putStrLn "Opening connection."
          doLuciAction _ _ LuciClientClosed = putStrLn "LuciClient WebSocket connection closed."
          doLuciAction _ _ (LuciClientError err) = putStrLn $ "LuciClient error: " ++ unpack' err
          doLuciAction ci _ luciClient = do
--            sendMessage luciClient $ runScenarioCreate "Our first scenario" (storeCityAsIs ci)
            sendMessage luciClient runServiceList
--            sendMessage luciClient runScenarioList
            sendMessage luciClient $ runTestFibonacci 10

      getScListE <- fromAddHandler getScListHandler
      _ <- mapEventIO id $ flip sendMessage runScenarioList <$> luciClientB <@  getScListE
      askForScenarioE <- fmap runScenarioGet <$> fromAddHandler onAskForScenarioH
      let _unitE3 = sendMessage <$> luciClientB <@> askForScenarioE

      _unitEs <- mapEventIO id $ _unitE1 +^^+ _unitE2 +^^+ _unitE3
      return ()

--      selHelB <- stepper (Nothing, Nothing) selHelE
--      let selHelE = filterApply ((/=) <$> selHelB) $ (,) <$> selObjIdB <*> heldObjIdB <@ updateE
--      _voidE <-  mapEventIO id $ (\c t -> putStrLn (show (activeObjId c) ++ " " ++ show t)) <$> cityB <@> selHelE

      return ()

    actuate network
    play heh
    putStrLn "Program started."
    programIdle

parseLuciMessages :: (JFI.GeoJSONLoaded -> IO ()) -> LuciClient -> MessageHeader -> IO ()
parseLuciMessages jsonLoadedTrigger _ (MsgResult callID duration serviceName taskID result) = do
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
       in jsonLoadedTrigger (JFI.GeoJSONLoaded True fc)
    s -> putStrLn ("Got some JSVal as a result of service " ++ show s) >> printJSVal (JS.asJSVal result)
  print (callID, duration, taskID)
parseLuciMessages _ _ (MsgError err) = putStrLn "Luci returned error" >> print err
parseLuciMessages _ _ (MsgProgress callID duration serviceName taskID percentage progress) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName
           ++ "' is in progress, done " ++ show percentage
  case serviceName of
    "ServiceList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultServiceList)
    "test.Fibonacci" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultTestFibonacci)
--    "scenario.GetList" -> print (JS.asLikeJS $ JS.asJSVal progress :: LuciResultScenarioList)
    s -> putStrLn ("Got some JSVal as a progress of service " ++ show s) >> printJSVal (JS.asJSVal progress)
  print (callID, duration, taskID)
parseLuciMessages _ _ (MsgNewCallID i) = putStrLn $ "Luci assigned new callID " ++ show i
parseLuciMessages _ _ msg = do
  putStrLn "Got unexpected message"
  printJSVal $ JS.asJSVal msg



combinePointers :: JS.Array Coords2D -> JS.Array Coords2D -> [(Vector2 GLfloat, Vector2 GLfloat)]
combinePointers a b = zipWith (\p1 p2 -> ( asVector p1, asVector p2)
                              ) (JS.toList a) (JS.toList b)


