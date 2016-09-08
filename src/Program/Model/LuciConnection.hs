-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.LuciConnection
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Program.Model.LuciConnection
  ( luciBehavior
  ) where

--import Control.Concurrent

-- Various thins I use
--import Control.Arrow (second)
--import Data.Geometry
--import JsHs
import JsHs.JSString (pack) -- JSString, append
--import Control.Monad (void, when)
import JsHs.Useful
--import Text.Read (readMaybe)
--import Data.Coerce
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
--import JsHs.WebGL.Types (GLfloat)

import Data.Geometry.Structure.Feature (FeatureCollection)
import qualified Data.Geometry.Structure.PointSet as PS

import Unsafe.Coerce (unsafeCoerce)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Program.Settings
import Program.Controllers.LuciClient
import Program.Model.City
import Program.Types
import Program.VisualService
import qualified Program.Controllers.GUI as GUI

luciBehavior :: Settings
             -> (Either FeatureCollection FeatureCollection -> IO ())
                      -> Behavior City
                      -> Event GroundUpdated
                      -> Event (Either b b1)
                      -> Event b2
                      -> Event (GeomId, b3)
                      -> MomentIO (Event VisualServiceResult)
luciBehavior lsettings geoJSONImportFire cityB groundUpdatedE
             geoJSONImportE clearGeometryE motionRecordsE = mdo

      -- Luci Client testing
      (luciClientB, luciClientE, luciMessageE) <- luciHandler (fromMaybe "" $ luciRoute lsettings)
      let doLuciAction LuciClientOpening = logText' "Opening connection."
          doLuciAction LuciClientClosed = logText' "LuciClient WebSocket connection closed."
          doLuciAction (LuciClientError err) = logText' $ "LuciClient error: " <> err
          doLuciAction luciClient = do
            logText' "Luci is ready"
            sendMessage luciClient runQuaServiceList

      -- general response to luci messages
      -- get scenario
      (onScenarioGetE, onScenarioGetFire) <- newEvent
      -- service finished execution
      (serviceFinishE, serviceFinishFire) <- newEvent
      reactimate $ (\msg -> parseLuciMessages (geoJSONImportFire . Right) onScenarioGetFire serviceFinishFire
                           ( msgHeaderValue msg
                           , attachments msg
                           )) <$>  luciMessageE


      -- actions to do when luci state changes
      reactimate $ doLuciAction <$> luciClientE

      -- asking luci for a scenario list on button click
      (getScListE, getScListFire) <- newEvent
      liftIO $ registerGetScenarioList getScListFire
      reactimate $ flip sendMessage runScenarioList <$> luciClientB <@  getScListE

      -- asking luci to save a scenario on button click
      (askSaveScenarioE, onAskSaveScenarioFire) <- newEvent
      liftIO $ GUI.registerSaveScenario onAskSaveScenarioFire
      scenarioSyncE_create <- mapEventIO id
           $ (\lc ci s -> do
              sendMessage lc $ runScenarioCreate s (storeCityAsIs ci)
              GUI.toggleSaveScenarioButton False s
              return $ SSPendingCreate s
            )
          <$> luciClientB <*> cityB <@> askSaveScenarioE


      -- register user clicking on "get scenario" button
      (askForScenarioE, onAskForScenarioFire) <- newEvent
      liftIO $ registerAskLuciForScenario (curry onAskForScenarioFire)
      -- Asking luci for a scenario on button click
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
      let scenarioSyncE_obtained = getSync <$> scenarioSyncB <@> onScenarioGetE
          scenarioSyncE_clearGeom = SSEmpty <$ clearGeometryE
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

          serviceButtonF GroundUpdated{} = GUI.toggleServiceClear True
          serviceButtonF GroundCleared{} = GUI.toggleServiceClear False

          runIsovistServiceA lc@(LuciClient _) vsr = sendMessage lc $ makeRunRequest (VisualService "DistanceToWalls") vsr
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
      (_vsManagerB, vsResultsE) <- vsManagerBehavior serviceFinishE

      -- run luci service!
      reactimate $ runIsovistServiceA <$> luciClientB <@> serviceRunsE
      reactimate $ serviceButtonF <$> groundUpdatedE

      return vsResultsE


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
parseLuciMessages _ _ _ (MsgRun{} , _) = return ()
parseLuciMessages _ _ _ (MsgCancel{} , _) = return ()
parseLuciMessages _ _ _ (MsgPanic{} , _) = putStrLn "Luci panicked. Can do nothing about it :("
parseLuciMessages _ _ _ (MsgUnknown msg, _) =
  logText' $ "Got unexpected message: " <> jsonStringify (JS.asJSVal msg)
--  printJSVal $ JS.asJSVal msg

unionsStepper :: [Event a] -> Event a
unionsStepper [] = never
unionsStepper xs = foldr1 (unionWith (const id)) xs


fromJSArrayToTypedArray :: (JSTA.TypedArrayOperations a) => JS.Array a -> JSTA.TypedArray a
fromJSArrayToTypedArray = JSTA.fromArray . unsafeFromJSArrayCoerce

unsafeFromJSArrayCoerce :: JS.Array a -> JSTA.TypedArray a
unsafeFromJSArrayCoerce = unsafeCoerce
