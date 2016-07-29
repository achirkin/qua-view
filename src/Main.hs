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
import JsHs.JSString (unpack') -- JSString, append
--import Control.Monad (void, when)
import GHCJS.Useful
import Text.Read (readMaybe)
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

main :: IO ()
main = do
    -- whole document
--    body <- documentBody

    -- drawing area
    canvas <- getElementById "glcanvas"
    -- "import geometry" button converts GeoJSON into internal representation
    importButton <- getElementById "jsonfileinput"
    clearGeomHandler <- getElementById "cleargeombutton" >>= clickHandler
    -- geoJSON updates
    geoJSONImportsHandler <- JFI.geoJSONImports
    JFI.registerButton geoJSONImportsHandler importButton

    -- get default scaling level
    let geomScale = readMaybe . unpack' $ getHtmlArg "scale"

    -- get request processing
    let userProfile = case getHtmlArg "role" of
                    "edit" -> ExternalEditor
                    "view" -> ExternalViewer
                    _      -> Full


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
      geoJSONImportE <- fromAddHandler $ JFI.addHandler geoJSONImportsHandler
      clearGeometryE <- fmap (const ClearingGeometry) <$> clickEvents clearGeomHandler

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



      let settingsB = pure defaultSettings { objectScale = geomScale}

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

      (luciClient, luciMessageE) <- luciHandler "ws://localhost:3000/luci"
      voidE <- mapEventIO id $ (parseLuciMessages . msgHeaderValue) <$> luciMessageE
      liftIO $ sendMessage luciClient $ toLuciMessage (MsgRun "ServiceList" []) []
      liftIO $ sendMessage luciClient $ toLuciMessage
            (MsgRun "test.Fibonacci" [("amount", JS.asJSVal (10::Int))]) []
--      selHelB <- stepper (Nothing, Nothing) selHelE
--      let selHelE = filterApply ((/=) <$> selHelB) $ (,) <$> selObjIdB <*> heldObjIdB <@ updateE
--      voidE <-  mapEventIO id $ (\c t -> putStrLn (show (activeObjId c) ++ " " ++ show t)) <$> cityB <@> selHelE

      return ()

    actuate network
    play heh
    putStrLn "Hello world!"
    programIdle

parseLuciMessages :: MessageHeader -> IO ()
parseLuciMessages (MsgResult callID duration serviceName taskID result) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName ++ "' finished!"
  printJSVal $ JS.asJSVal result
  print (callID, duration, taskID)
parseLuciMessages (MsgError err) = putStrLn "Luci returned error" >> print err
parseLuciMessages (MsgProgress callID duration serviceName taskID percentage progress) = do
  putStrLn $ "Luci service '" ++ unServiceName serviceName
           ++ "' is in progress, done " ++ show percentage
  printJSVal $ JS.asJSVal progress
  print (callID, duration, taskID)
parseLuciMessages (MsgNewCallID i) = putStrLn $ "Luci assigned new callID " ++ show i
parseLuciMessages msg = do
  putStrLn "Got unexpected message"
  printJSVal $ JS.asJSVal msg


--  = MsgRun JSString [(JSString, JSVal)]
--    -- ^ run service message, e.g. {'run': 'ServiceList'};
--    -- params: 'run', [(name, value)]
--  | MsgCancel Int
--    -- ^ cancel service message, e.g. {'cancel': 25};
--    -- params: 'callID'
--  | MsgNewCallID Int
--    -- ^ Luci call id, { newCallID: 57 };
--    -- params: 'newCallID'
--  | MsgResult Int Double JSString Int JSVal
--    -- ^ result of a service execution,
--    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result'
--  | MsgProgress Int Double JSString Int Double JSVal
--    -- ^ result of a service execution,
--    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, percentage: 0, progress: null};
--    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'percentage', 'progress'
--  | MsgError JSString
--    -- ^ error message, e.g. {'error': 'We are in trouble!'};
--    -- params: 'error'
--  | MsgUnknown JSVal
--    -- ^ unknown type of message; passed as-is

combinePointers :: JS.Array Coords2D -> JS.Array Coords2D -> [(Vector2 GLfloat, Vector2 GLfloat)]
combinePointers a b = zipWith (\p1 p2 -> ( asVector p1, asVector p2)
                              ) (JS.toList a) (JS.toList b)
