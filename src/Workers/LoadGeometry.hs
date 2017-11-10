{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
#ifdef ISWORKER
{-# LANGUAGE TypeApplications #-}
#else
{-# LANGUAGE FlexibleContexts #-}
#endif
module Workers.LoadGeometry
    ( loadGeometryDef, LGWMessage (..)
#ifdef ISWORKER
    , loadGeometryConduit
#else
    , runLoadGeometryWorker
#endif
    ) where

import Model.Scenario.Object (ObjectRenderable(..))
import GHC.Generics
import Commons
import Workers
import Model.Scenario
import Model.Scenario.Statistics
#ifdef ISWORKER
import Numeric.DataFrame
import Data.Conduit
import Model.GeoJSON.Coordinates
import Model.GeoJSON.Scenario
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances

--import Control.Lens
--import Model.Scenario.Object as Object

loadGeometryConduit :: (MonadIO m, MonadLogger m)
                    => Conduit (LoadedTextContent, Scenario' 'Prepared) m (LGWMessage, [Transferable])
loadGeometryConduit = awaitForever $ \(msg, _curSc) -> do
    errOrVal <- parseJSONValue $ getTextContent msg
    case errOrVal of
      Left err -> logError (workerLS loadGeometryDef) err
      Right val@(SomeValue jsv) -> do
        logInfo' @JSString (workerLS loadGeometryDef) "Got a message!" jsv
        case fromJSON val of
           Success cs@(ObjectCentres (SomeDataFrame centres)) -> do
              logInfo' @JSString (workerLS loadGeometryDef) "Centres:" centres
              yield (LGWSCStat $ getScenarioStatistics cs, [])
           Error s ->
              logWarn (workerLS loadGeometryDef) $ "Could not parse centres: " <> s
        case fromJSON val of
           Success sc' -> do
              sc <- liftIO $ prepareScenario sc'
              trs <- liftIO $ getTransferables sc
              yield (LGWResult sc, trs)
           Error s -> do
              logWarn (workerLS loadGeometryDef) $ "Could not parse scenario: " <> s
              yield (LGWSError . JSError $ toJSString s, [])


#else
import Reflex
import qualified Data.JSString as JSString
import qualified QuaTypes
import System.FilePath ((</>))

runLoadGeometryWorker :: ( MonadIO m, Reflex t
                         , TriggerEvent t m
                         , PerformEvent t m
                         , MonadHold t m
                         , MonadIO (Performable m)
                         , MonadFix m
                         )
                      => Event t (LoadedTextContent, Scenario' 'Prepared)
                      -> QuaViewT t m (Event t LGWMessage)
runLoadGeometryWorker inEvs = do
    loadGeometryDefD
         <- fmap (( \u -> loadGeometryDef
                      { workerUrl = JSString.pack
                                    $ JSString.unpack u
                                   </> JSString.unpack (workerUrl loadGeometryDef)
                      }
                  ) . QuaTypes.jsRootUrl
                 ) <$> quaSettings
    -- remove an event if the link did not change
    let newLoadGeometryDefE = attachWithMaybe (\wc wu -> if workerUrl wc == workerUrl wu
                                                         then Nothing
                                                         else Just wu
                                              )
                                              (current loadGeometryDefD)
                                              (updated loadGeometryDefD)
    newLoadGeometryDefD <- sample (current loadGeometryDefD)
                        >>= \s -> holdDyn s newLoadGeometryDefE
    -- run worker every time its link changes
    runWorkerDyn newLoadGeometryDefD $ flip (,) [] <$> inEvs

#endif



loadGeometryDef :: WorkerDef
loadGeometryDef = WorkerDef
  { workerName = "LoadGeometry"
  , workerUrl  = "qua-worker-loadgeometry.js"
  }

data LGWMessage
  = LGWResult (Scenario' 'Prepared)
    -- ^ Send parsed Scenario
  | LGWSCStat ScenarioStatistics
    -- ^ Send general info about scenario object
  | LGWSError JSError
    -- ^ Something went wrong!
  deriving Generic

instance FromJSVal LGWMessage
instance ToJSVal   LGWMessage
