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

import JsHs.Debug

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
      (_luciClientB, luciClientE, runLuciService) <- luciHandler (fromMaybe "" $ luciRoute lsettings)

      -- actions to do when luci state changes
      let doLuciAction LuciClientOpening = logText' "Opening connection."
          doLuciAction LuciClientClosed = logText' "LuciClient WebSocket connection closed."
          doLuciAction (LuciClientError err) = logText' $ "LuciClient error: " <> err
          doLuciAction _luciClient = logText' "Luci is ready"
            -- TODO: run runQuaServiceList
--            sendMessage luciClient runQuaServiceList
      reactimate $ doLuciAction <$> luciClientE

      -- general response to luci messages
      -- get scenario
      (onScenarioGetE, onScenarioGetFire) <- newEvent


      -- asking luci for a scenario list on button click
      (getScListE, getScListFire) <- newEvent
      liftIO $ registerGetScenarioList getScListFire
      let gotScenarioListF (SRResult _ scenarioList _) = displayScenarios scenarioList
          gotScenarioListF (SRError _ err) = logText' err
          gotScenarioListF _ = return ()
      execute (runScenarioList runLuciService <$ getScListE) >>= switchE >>= reactimate . fmap gotScenarioListF

      -- asking luci to save a scenario on button click
      (askSaveScenarioE, onAskSaveScenarioFire) <- newEvent
      liftIO $ GUI.registerSaveScenario onAskSaveScenarioFire
      scenarioSavedE <- execute ((\ci s -> runScenarioCreate runLuciService s (storeCityAsIs ci)) <$> cityB <@> askSaveScenarioE) >>= switchE
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
      gotScenarioE <- execute (runScenarioGet runLuciService . fst <$> askForScenarioE) >>= switchE
      let gotScenarioF (SRResult _ (LuciResultScenario scId fc t) _) = geoJSONImportFire (Right fc) >> onScenarioGetFire (scId, t)
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

          updateScenarioA (SSSynced sid _ _) ci gId = runScenarioUpdate runLuciService sid (storeObjectsAsIs [gId] ci)
          updateScenarioA _ _ _ = return never

          serviceRunsE = filterJust $ serviceRunsF <$> scenarioSyncB <@> groundUpdatedE
          serviceRunsF (SSSynced sid _ _) (GroundUpdated points) = Just $ VisualServiceRunPoints sid [] [] [] (fromJSArrayToTypedArray $ PS.flatten points)
          serviceRunsF _ _ = Nothing

          serviceButtonF GroundUpdated{} = GUI.toggleServiceClear True
          serviceButtonF GroundCleared{} = GUI.toggleServiceClear False


          askSubscribeForScenario SSSynced{} SSSynced{} = return never
          askSubscribeForScenario _ (SSSynced sid _ _) = runScenarioSubscribe runLuciService sid
          askSubscribeForScenario _ _ = return never

      -- show reflect scenario sync state
      reactimate $ toggleSaveScenarioA <$> scenarioSyncE


      -- sync geometry with Luci
      lateObjectRecordsE <- execute $ return . fst <$> motionRecordsE
      scenarioOutUpdatesE <- execute (updateScenarioA <$> scenarioSyncB <*> cityB <@> lateObjectRecordsE) >>= switchE
      scenarioInUpdatesE <- execute (askSubscribeForScenario <$>  scenarioSyncB <@> scenarioSyncE_obtained) >>= switchE
--      reactimate $ gotScenarioF <$> scenarioInUpdatesE
      let (errsOfScOutE, _, _) = catResponses scenarioOutUpdatesE
          (errsOfScInE, _, _) = catResponses scenarioInUpdatesE
      reactimate $ logText' <$> errsOfScInE
      reactimate $ logText' <$> errsOfScOutE

      -- run luci service!
--      (_vsManagerB, vsResultsE) <- vsManagerBehavior serviceFinishE
      (vsErrorsR, vsResultsE) <- fmap split $
                    execute (runVService runLuciService (VisualService "DistanceToWalls") <$> serviceRunsE) -- GenericIsovistService
                       >>= switchE
      reactimate $ (\t -> logText' t >> GUI.toggleServiceClear False) <$> vsErrorsR
      reactimate $ serviceButtonF <$> groundUpdatedE

      return vsResultsE




unionsStepper :: [Event a] -> Event a
unionsStepper [] = never
unionsStepper xs = foldr1 (unionWith (const id)) xs


fromJSArrayToTypedArray :: (JSTA.TypedArrayOperations a) => JS.Array a -> JSTA.TypedArray a
fromJSArrayToTypedArray = JSTA.fromArray . unsafeFromJSArrayCoerce

unsafeFromJSArrayCoerce :: JS.Array a -> JSTA.TypedArray a
unsafeFromJSArrayCoerce = unsafeCoerce
