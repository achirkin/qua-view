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
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
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

import JsHs.JSString (pack, unpack') -- JSString, append
--import Control.Monad (void, when)
import JsHs.Useful
import JsHs
import JsHs.LikeJS.Class ()
-- import JsHs.Types.Prim (jsNull)
--import Text.Read (readMaybe)
--import Data.Coerce
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
--import JsHs.WebGL.Types (GLfloat)
import qualified Data.Geometry.Transform as T
import System.IO.Unsafe

import Data.Geometry.Structure.Feature
import qualified Data.Geometry.Structure.PointSet as PS

import Unsafe.Coerce (unsafeCoerce)

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Coerce (coerce)
import Data.Time
--import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

import Program.Settings
import Program.Controllers.LuciClient
import Program.Model.City
import Program.Model.CityObject (geomId)
import Program.Types
import Program.VisualService
import qualified Program.Controllers.GUI as GUI

-- import JsHs.Debug
-- import Debug.Trace (trace)

luciBehavior :: Settings
             -> (Either SomeJSONInput SomeJSONInput -> IO ())
                      -> Behavior City
                      -> Event GroundUpdated
                      -> Event (Either b b1)
                      -> Event b2
                      -> Event (GeomId, b3)
                      -> MomentIO (Event VisualServiceResult)
luciBehavior lsettings geoJSONImportFire cityB groundUpdatedE
             geoJSONImportE clearGeometryE motionRecordsE = mdo

      -- Luci Client testing
      (luciClientB,noCallIdMsgE,unknownMsgE,luciStateE) <- luciHandler (fromMaybe "" $ luciRoute lsettings)

--      -- actions to do when luci state changes
--      let doLuciAction LuciClientOpening = logText' "Opening connection."
--          doLuciAction LuciClientClosed = logText' "LuciClient WebSocket connection closed."
--          doLuciAction (LuciClientError err) = logText' $ "LuciClient error: " <> err
--          doLuciAction _luciClient = logText' "Luci is ready"
--            -- TODO: run runQuaServiceList
----            sendMessage luciClient runQuaServiceList
--      reactimate $ doLuciAction <$> luciClientE

      -- general response to luci messages
      -- get scenario
      (onScenarioGetE, onScenarioGetFire) <- newEvent


      -- asking luci for a scenario list on button click
      (getScListE, getScListFire) <- newEvent
      liftIO $ registerGetScenarioList getScListFire
      let gotScenarioListF (SRResult _ scenarioList _) = displayScenarios scenarioList
          gotScenarioListF (SRError _ err) = logText' err
          gotScenarioListF _ = return ()
      runScenarioList luciClientB (() <$ getScListE) >>= reactimate . fmap gotScenarioListF
--      callLuci (("scenario.GetList", [], []) <$ getScListE) >>= reactimate . fmap gotScenarioListF
--      execute (runScenarioList runLuciService <$ getScListE) >>= switchE >>= reactimate . fmap gotScenarioListF

      -- asking luci to save a scenario on button click
      (askSaveScenarioE, onAskSaveScenarioFire) <- newEvent
      liftIO $ GUI.registerSaveScenario onAskSaveScenarioFire
      scenarioSavedE <- runScenarioCreate luciClientB $ (\ci s -> (s, ci)) <$> cityB <@> askSaveScenarioE
--      scenarioSavedE <- execute ((\ci s -> runScenarioCreate runLuciService s (storeCityAsIs ci)) <$> cityB <@> askSaveScenarioE) >>= switchE
      scenarioSyncE_create <- mapEventIO id
           $ (\s -> do
              GUI.toggleSaveScenarioButton False s
              return $ SSPendingCreate s
            )
          <$> askSaveScenarioE
      let createdScenarioF (SRResult _ (LuciResultScenarioCreated scId t) _) = onScenarioGetFire (scId, t)
          createdScenarioF (SRError _ err) = logText' err
          createdScenarioF _ = return ()
      reactimate $ createdScenarioF <$> scenarioSavedE

      -- register user clicking on "get scenario" button
      (askForScenarioE, onAskForScenarioFire) <- newEvent
      liftIO $ registerAskLuciForScenario (curry onAskForScenarioFire)
      -- Asking luci for a scenario on button click
      gotScenarioE <- runScenarioGet luciClientB (fst <$> askForScenarioE)
--      gotScenarioE <- execute (runScenarioGet runLuciService . fst <$> askForScenarioE) >>= switchE
      let gotScenarioF (SRResult _ (LuciResultScenario scId gi t) _) = geoJSONImportFire (Right (SJIExtended gi)) >> onScenarioGetFire (scId, t)
          gotScenarioF (SRProgress _ _ (Just (LuciResultScenario scId gi t)) _) = geoJSONImportFire (Right (SJIExtended gi)) >> onScenarioGetFire (scId, t)
          gotScenarioF (SRError _ err) = logText' err
          gotScenarioF _ = return ()
      reactimate $ gotScenarioF <$> gotScenarioE


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

--          updateScenarioA (SSSynced sid _ _) ci gId = runScenarioUpdate runLuciService sid (storeObjectsAsIs [gId] ci)
--          updateScenarioA _ _ _ = return never
          updateScenarioF (SSSynced sid _ _) ci gId = Just (sid, storeObjectsAsIs [gId] ci)
          updateScenarioF _ _ _ = Nothing
--
          serviceRunsE = filterJust $ serviceRunsF <$> scenarioSyncB <*> cityB <@> groundUpdatedE
          serviceRunsF (SSSynced sid _ _) city (GroundUpdated points) = Just $ \m -> case m of
            VS_POINTS -> VisualServiceRunPoints sid (fromJSArrayToTypedArray $ PS.flatten points)
            VS_SCENARIO -> VisualServiceRunScenario sid
            VS_OBJECTS -> VisualServiceRunObjects sid (fromJSArrayToTypedArray . JS.map (unsafeCoerce . geomId . T.unwrap) $ objectsIn city)
            _ -> VisualServiceDoNotRun
          serviceRunsF _ _ _ = Nothing

          serviceButtonF GroundUpdated{} = GUI.toggleServiceClear True
          serviceButtonF GroundCleared{} = GUI.toggleServiceClear False

          -- TODO: Something is wrong with this logic, need to rethink
          askSubscribeForScenario SSSynced{} SSSynced{} = Nothing
          askSubscribeForScenario _ (SSSynced sid _ _) = Just sid
          askSubscribeForScenario _ _ = Nothing

      -- show reflect scenario sync state
      reactimate $ toggleSaveScenarioA <$> scenarioSyncE


--      -- sync geometry with Luci
      lateObjectRecordsE <- execute $ return . fst <$> motionRecordsE
      sendScenarioUpdateE <- runScenarioUpdate luciClientB . filterJust $ updateScenarioF <$> scenarioSyncB <*> cityB <@> lateObjectRecordsE
      receiveScenarioUpdateE <- runScenarioSubscribe luciClientB . filterJust $ askSubscribeForScenario <$> scenarioSyncB <@> scenarioSyncE_obtained
      let (noCallIdMsgE', subscribeToUpdatesE)
                               = split $ (\m -> if 0 == responseCallId m
                                                then Right m
                                                else Left m
                                         ) <$> noCallIdMsgE
      reactimate $ gotScenarioF . fmap (JS.asLikeJS . srVal) <$> subscribeToUpdatesE
--      scenarioOutUpdatesE <- execute (updateScenarioA <$> scenarioSyncB <*> cityB <@> lateObjectRecordsE) >>= switchE
--      scenarioInUpdatesE <- execute (askSubscribeForScenario <$>  scenarioSyncB <@> scenarioSyncE_obtained) >>= switchE
--      reactimate $ gotScenarioF <$> receiveScenarioUpdateE
      let (errsOfScOutE, _, _) = catResponses sendScenarioUpdateE
          (errsOfScInE, _, _) = catResponses receiveScenarioUpdateE
      reactimate $ logText' <$> errsOfScInE
      reactimate $ logText' <$> errsOfScOutE
      reactimate $ gotScenarioF <$> receiveScenarioUpdateE

      -- | run luci service!
      -- Event triggered when a user selects an active service from a drop-down menu
      (selectServiceE, selectServiceFire) <- newEvent
      liftIO $ GUI.registerSetActiveService selectServiceFire
      -- Event triggered when qua-view gets a list of available qua-compliant services
      (updateSListE, updateSListFire) <- newEvent
      -- Event passes in service parameters (after they are retreived from luci or helen)
      (changeSParamE, changeSParamFire) <- newEvent
      liftIO $ GUI.registerUpdateSParamValue (curry changeSParamFire)
      -- Event passes in a configured service after ServiceInfo request finished
      (reconfServiceE, reconfServiceFire) <- newEvent
      -- Manage visual service images
      (vsManagerB, vsResultsE) <- vsManagerBehavior selectServiceE changeSParamE updateSListE reconfServiceE

      -- update list of available services
      (triggerQuaServiceListE,triggerQuaServiceListF) <- newEvent
      serviceListUpdateE <- runQuaServiceList luciClientB triggerQuaServiceListE
      let serviceListUpdateA (SRResult _ r@(ServiceList jsarray) _) = do
            updateSListFire r
            GUI.updateServiceNames $ coerce jsarray
          serviceListUpdateA (SRError _ e) = logText' e
          serviceListUpdateA _ = return ()
      -- on updated list
      reactimate $ serviceListUpdateA <$> serviceListUpdateE
      -- when to update
      liftIO $ GUI.registerRefreshServiceList triggerQuaServiceListF
      reactimate $ triggerQuaServiceListF () <$ filterE (LCSOpen ==) luciStateE
      -- when to ask to reconfigure a service
      serviceInfoResultsE <- fmap (fmap (fmap parseServiceInfoResult))
        <$> runService luciClientB $ (\sname -> ("ServiceInfo",
          [ ("inclDescr", JsHs.asJSVal True)
          , ("serviceNames", JsHs.asJSVal [sname])
          ] , [])) <$> selectServiceE
      let serviceInfoResultsA (SRResult _ (errs, vss) _) = do
            mapM_ logText' errs
            mapM_ reconfServiceFire vss
          serviceInfoResultsA SRProgress {} = return ()
          serviceInfoResultsA (SRError _ err) = logText' $ "Failed to ask luci for ServiceInfo: " <> err
      reactimate $ serviceInfoResultsA <$> serviceInfoResultsE


      -- Run service manager! Runs a service on every request event
      runVService vsManagerB luciClientB serviceRunsE


      -- Update Service clear/play button
      reactimate $ serviceButtonF <$> groundUpdatedE

      -- a little bit of debugging into the console
      reactimate $ (\(msg, _) -> print $ "Ignoring message: " ++ (unpack' . jsonStringify $ JS.asJSVal msg)) <$> unknownMsgE
      reactimate $ (\msg -> print $ "Ignoring message: " ++ show (fmap (unpack' . jsonStringify . srVal) msg)) <$> noCallIdMsgE'


      reactimate $ drawServiceParams . drawParameters <$> reconfServiceE

--      -- Further testing
--      let debugRunService sname pams atts lcB callE = runServiceOnEvent sname pams atts lcB callE >>= reactimate . fmap parseJSResult
--          parseJSResult (SRResult cId v _) = print ("service finished [" ++ show cId ++ "]") >> printSI v
--          parseJSResult (SRProgress cId p (Just v) _) = print ("service progress [" ++ show cId ++ "] " ++ show p) >> printSI v
--          parseJSResult (SRProgress cId p Nothing _)  = print ("service progress [" ++ show cId ++ "] " ++ show p)
--          parseJSResult (SRError cId e) = print $ "service error [" ++ show cId ++ "]: " ++ unpack' e
--          printSI jsv = let (errs, ss) = parseServiceInfoResult jsv
--                        in do
--                          printJSVal jsv
--                          mapM_ print errs
--                          mapM_ print ss

--      debugRunService "ServiceInfo"
--        [ ("inclDescr", JsHs.asJSVal True)
--        , ("serviceNames", JsHs.asJSVal [ "Test multi-parameter" :: JSString
--                                        --, "DistanceToWalls"
--                                        , "ServiceInfo"
--                                        ])
--        ] [] luciClientB triggerQuaServiceListE


      return vsResultsE





----------------------------------------------------------------------------------------------------
-- * Pre-defined messages
----------------------------------------------------------------------------------------------------

--runHelper :: JS.LikeJS s a => ServiceName -> [(JSString, JSVal)] -> ServiceInvocation -> MomentIO (Event (Either JSString a))
--runHelper sname pams run = filterJust . fmap f <$> run sname pams []
--  where
--    f (SRResult _ (ServiceResult res) _) = Just . Right $ JS.asLikeJS res
--    f SRProgress{} = Nothing
--    f (SRError _ s) = Just $ Left s

-- | A message to get list of available services from luci
--runServiceList :: Behavior LuciClient -> Event () -> MomentIO (Event (ServiceResponse LuciResultServiceList))
--runServiceList lcB e = runService lcB $ ("ServiceList",[],[]) <$ e


---- | run a testing service test.Fibonacci
--runTestFibonacci :: Int -> LuciMessage
--runTestFibonacci n = toLuciMessage (MsgRun "test.Fibonacci" [("amount", JS.asJSVal n)]) []

--newtype LuciResultTestFibonacci = TestFibonacci [Int]
--  deriving (Show, Eq)
--instance LikeJS "Object" LuciResultTestFibonacci where
--  asLikeJS b = case getProp "fibonacci_sequence" b of
--                 Just x  -> TestFibonacci $ JS.asLikeJS x
--                 Nothing -> TestFibonacci []
--  asJSVal (TestFibonacci xs) = setProp "fibonacci_sequence" xs (unsafePerformIO newObj)



-- | Luci scenario.
--   This is a wrapper around JSON FeatureCollection that comes from luci.
--   The data coming luci is always the same: FeatureCollection
data LuciScenario = LuciResultScenario ScenarioId ScenarioJSON UTCTime
instance JS.LikeJS "Object" LuciScenario where
  asLikeJS jsv = LuciResultScenario
                  (fromMaybe 0 $ getProp "ScID" jsv)
                  (fromMaybe (asLikeJS (unsafePerformIO newObj)) $ getProp "geometry_output" jsv)
                  (posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0 $ getProp "lastmodified" jsv)
  asJSVal (LuciResultScenario scId gi _) = unsafePerformIO $ do
    o <- newObj
    setProp o "ScID" scId
    setProp o "geometry_output" gi
    return o
  {-# NOINLINE asJSVal #-}


-- | Luci scenario
data LuciScenarioCreated = LuciResultScenarioCreated ScenarioId UTCTime
instance JS.LikeJS "Object" LuciScenarioCreated where
  asLikeJS jsv = LuciResultScenarioCreated (fromMaybe 0 $ getProp "ScID" jsv)
                                     (posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0 $ getProp "lastmodified" jsv)
  asJSVal (LuciResultScenarioCreated scId lm) = unsafePerformIO $ do
    o <- newObj
    setProp o "ScID"  scId
    setProp o "lastmodified" (round $ utcTimeToPOSIXSeconds lm :: Int)
    return o
  {-# NOINLINE asJSVal #-}

-- | Pass the name of the scenario and a feature collection with geometry
runScenarioCreate :: Behavior LuciClient
                  -> Event
                     ( ScenarioName -- ^ name of the scenario
                     , City
                     )
                  -> MomentIO (Event (ServiceResponse LuciScenarioCreated))
runScenarioCreate lcB e = runService lcB $ (\v -> ("scenario.geojson.Create", f v, [])) <$> e
  where
    {-# NOINLINE f #-}
    f (name, city) = unsafePerformIO $ do
      let ScenarioJSON o = storeCityWithProps city
      setProp o "name" name
      return
          [ ("name", JS.asJSVal name)
          , ("geometry_input", o)
          ]


-- returns: "{"created":1470932237,"lastmodified":1470932237,"name":"dgdsfg","ScID":4}"

runScenarioUpdate :: Behavior LuciClient
                  -> Event
                     ( ScenarioId -- ^ id of the scenario
                     , FeatureCollection -- ^ content of the scenario update
                     )
                  -> MomentIO (Event (ServiceResponse JSVal))
runScenarioUpdate lcB e = runService lcB $ (\v -> ("scenario.geojson.Update", f v, [])) <$> e
  where
    f (scId, collection) =
      [ ("ScID", JS.asJSVal scId)
      , ("geometry_input"
        , unsafePerformIO $ do
            o <- newObj
            setProp o "format" ("GeoJSON" :: JSString)
            setProp o "geometry" collection
            return o
        )
      ]


runScenarioGet :: Behavior LuciClient
               -> Event ScenarioId -- ^ id of the scenario
               -> MomentIO (Event (ServiceResponse LuciScenario))
runScenarioGet lcB e = runService lcB $ (\v -> ("scenario.geojson.Get", f v, [])) <$> e
  where
    f scId =
      [ ("ScID", JS.asJSVal scId)
      ]

-- returns: "{"lastmodified":1470932237,"ScID":4}"

runScenarioSubscribe :: Behavior LuciClient
                     -> Event ScenarioId -- ^ id of the scenario
                     -> MomentIO (Event (ServiceResponse LuciScenario))
runScenarioSubscribe lcB e = runService lcB $ (\v -> ("scenario.SubscribeTo", f v, [])) <$> e
  where
    f scId =
      [ ("ScIDs", JS.asJSVal [scId])
      , ("format", JS.asJSVal ("geojson" :: JSString))
      ]


runScenarioList :: Behavior LuciClient -> Event () -> MomentIO (Event (ServiceResponse ServiceResult))
runScenarioList lcB e = runService lcB $ ("scenario.GetList",[],[]) <$ e


newtype LuciResultScenarioList = ScenarioList [ScenarioDescription]
  deriving (Show)
instance JS.LikeJS "Object" LuciResultScenarioList where
  asLikeJS b = case getProp "scenarios" b of
                 Just x  -> ScenarioList x
                 Nothing -> ScenarioList []
  asJSVal (ScenarioList v) = unsafePerformIO $ newObj >>= \o -> o <$ setProp o "scenarios" v
  {-# NOINLINE asJSVal #-}


data ScenarioDescription = ScenarioDescription
  { scCreated  :: UTCTime
  , scModified :: UTCTime
  , scName     :: ScenarioName
  , sscId      :: ScenarioId
  }
  deriving (Eq,Ord,Show)
instance JS.LikeJS "Object" ScenarioDescription where
  asLikeJS jsv = ScenarioDescription
    { scCreated  = f $ getProp "created" jsv
    , scModified = f $ getProp "lastmodified" jsv
    , scName     = fromMaybe "" $ getProp "name" jsv
    , sscId      = fromMaybe (-1) $ getProp "ScID" jsv
    }
      where
        f = posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0
  asJSVal scd = unsafePerformIO $ do
    o <- newObj
    setProp o "ScID" (sscId scd)
    setProp o "name" (scName scd)
    setProp o "lastmodified" (f $ scModified scd :: Int)
    setProp o "created" (f $ scCreated scd :: Int)
    return o
      where
        f = round . utcTimeToPOSIXSeconds
  {-# NOINLINE asJSVal #-}









----------------------------------------------------------------------------------------------------
-- * Helpers
----------------------------------------------------------------------------------------------------



foreign import javascript unsafe "document.getElementById('guiServiceParams').innerHTML = $1;"
  drawServiceParams :: JSString -> IO ()




unionsStepper :: [Event a] -> Event a
unionsStepper [] = never
unionsStepper xs = foldr1 (unionWith (const id)) xs


fromJSArrayToTypedArray :: (JSTA.TypedArrayOperations a) => JS.Array a -> JSTA.TypedArray a
fromJSArrayToTypedArray = JSTA.fromArray . unsafeFromJSArrayCoerce

unsafeFromJSArrayCoerce :: JS.Array a -> JSTA.TypedArray a
unsafeFromJSArrayCoerce = unsafeCoerce
