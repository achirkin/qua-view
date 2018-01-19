{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE Strict            #-}
#ifdef ISWORKER
{-# LANGUAGE TypeApplications  #-}
#else
{-# LANGUAGE GADTs             #-}
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
import Control.Monad.Trans.Except
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances

import Workers.LoadGeometry.Parser

loadGeometryConduit :: (MonadIO m, MonadLogger m)
                    => Conduit LGWRequest m (LGWMessage, [Transferable])
loadGeometryConduit = (yield (LGWReady, []) >>) . awaitForever $ \emsg -> runExceptAndLog $ do
    val@(SomeValue jsv) <- case emsg of
      LGWLoadUrl _ url -> getUrlSync url >>= ExceptT . parseJSONValue . getTextContent
      LGWLoadTextContent _ content -> ExceptT . parseJSONValue $ getTextContent content
    lift $ logInfo' @JSString (workerLS loadGeometryDef) "Got a message!" jsv

    cs@(ObjectCentres (SomeDataFrame centres))
      <- resultToErrorT "Could not parse centres: " $ fromJSON val
    lift $ logInfo' @JSString (workerLS loadGeometryDef) "Centres:" centres
    let stat = getScenarioStatistics cs

    sc' <- resultToErrorT "Could not parse scenario: " $ parse parseScenarioJSON val
    (sc, errs) <- liftIO $ prepareScenario stat def sc'
    trs <- liftIO $ getTransferables sc
    lift $ do
      forM_ errs $ \e -> yield (LGWSError e, [])
      yield (LGWResult sc, trs)
  where
    runExceptAndLog m = do
      r <- runExceptT m
      case r of
        Right a -> pure a
        Left  e@(JSError err) -> do
          logWarn (workerLS loadGeometryDef) err
          yield (LGWSError e, [])
    resultToErrorT _    (Success a) = ExceptT . pure $ Right a
    resultToErrorT msgpre (Error s) = ExceptT . pure . Left . JSError $ msgpre <> toJSString s

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
