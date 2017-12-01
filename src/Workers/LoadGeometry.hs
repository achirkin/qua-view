{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
#ifdef ISWORKER
{-# LANGUAGE TypeApplications #-}
#else
{-# LANGUAGE GADTs #-}
#endif
module Workers.LoadGeometry
    ( loadGeometryDef, LGWMessage (..), LGWRequest (..)
#ifdef ISWORKER
    , loadGeometryConduit
#else
    , runLoadGeometryWorker, QEventTag (..)
#endif
    ) where


import Workers
import Workers.Types
#ifdef ISWORKER
import Commons.NoReflex
import Numeric.DataFrame
import Model.GeoJSON.Coordinates
import Model.Scenario
import Model.Scenario.Statistics
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances

import Workers.LoadGeometry.Parser

loadGeometryConduit :: (MonadIO m, MonadLogger m, MonadState ScenarioStatistics m)
                    => Conduit LGWRequest m (LGWMessage, [Transferable])
loadGeometryConduit = awaitForever $ \emsg -> do
    errOrVal <- runExceptT $ case emsg of
      LGWLoadUrl _ url -> getUrlSync url >>= ExceptT . parseJSONValue . getTextContent
      LGWLoadTextContent _ content -> ExceptT . parseJSONValue $ getTextContent content
    case errOrVal of
      Left err -> logError (workerLS loadGeometryDef) err
      Right val@(SomeValue jsv) -> do
        logInfo' @JSString (workerLS loadGeometryDef) "Got a message!" jsv
        case fromJSON val of
           Success cs@(ObjectCentres (SomeDataFrame centres)) -> do
              logInfo' @JSString (workerLS loadGeometryDef) "Centres:" centres
              let stat = getScenarioStatistics cs
              put stat
              yield (LGWSCStat stat, [])
           Error s ->
              logWarn (workerLS loadGeometryDef) $ "Could not parse centres: " <> s
        case parse parseScenarioJSON val of
           Success sc' -> do
              stat <- get
              sc <- liftIO $ prepareScenario stat sc'
              trs <- liftIO $ getTransferables sc
              yield (LGWResult sc, trs)
           Error s -> do
              logWarn (workerLS loadGeometryDef) $ "Could not parse scenario: " <> s
              yield (LGWSError . JSError $ toJSString s, [])


#else
import Commons
import Reflex

runLoadGeometryWorker :: ( MonadIO m, Reflex t
                         , TriggerEvent t m
                         , PerformEvent t m
                         , MonadHold t m
                         , MonadIO (Performable m)
                         , MonadFix m
                         )
                      => QuaViewT Writing t m ()
runLoadGeometryWorker = do
    -- run worker every time its link changes
    inEvs <- askEvent (WorkerMessage LGWRequest)
    evs <- runWorker loadGeometryDef $ flip (,) [] <$> inEvs
    registerEvent (WorkerMessage LGWMessage) evs

#endif



loadGeometryDef :: WorkerDef
loadGeometryDef = WorkerDef
  { workerName = "LoadGeometry"
  , workerUrl  = "qua-worker-loadgeometry.js"
  }
