{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef ISWORKER
{-# LANGUAGE TypeApplications #-}
#else
{-# LANGUAGE FlexibleContexts #-}
#endif
module Workers.LoadGeometry
    ( loadGeometryDef
#ifdef ISWORKER
    , loadGeometryConduit
#else
    , runLoadGeometryWorker
#endif
    ) where


import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import Commons
import Workers
import Model.Scenario.Statistics
#ifdef ISWORKER
import Numeric.DataFrame
import Data.Conduit
import Model.Scenario
import Model.Scenario.Properties
import Model.GeoJSON.Coordinates
import Model.GeoJSON.Scenario ()
import Control.Lens

loadGeometryConduit :: (MonadIO m, MonadLogger m)
                    => Conduit LoadedTextContent m (LGWMessage, [Transferable])
loadGeometryConduit = awaitForever $ \msg -> do
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
           Success sc@Scenario {} -> do
              logInfo' @JSString (workerLS loadGeometryDef) "Scenario:" sc
              logInfo (workerLS loadGeometryDef) $ "scActiveColor: " <> toJSString (sc ^. defaultActiveColor)
              logInfo (workerLS loadGeometryDef) $ "scActiveColor: "
                                                <> show (sc^.defaultActiveColor.colorVeci)
              logInfo (workerLS loadGeometryDef) $ "scActiveColor: "
                                                <> show (sc^.defaultActiveColor.colorVecf)
           Error s ->
              logWarn (workerLS loadGeometryDef) $ "Could not parse scenario: " <> s
    yield (LGWString "Thanks!", [])



#else
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class

runLoadGeometryWorker :: ( MonadIO m, Reflex t
                         , TriggerEvent t m
                         , PerformEvent t m
                         , MonadIO (Performable m)
                         )
                      => Event t LoadedTextContent
                      -> m (Event t LGWMessage)
runLoadGeometryWorker inEvs = runWorker loadGeometryDef $ flip (,) [] <$> inEvs


#endif



loadGeometryDef :: WorkerDef
loadGeometryDef = WorkerDef
  { workerName = "LoadGeometry"
  , workerUrl  = "qua-worker-loadgeometry.js"
  }

data LGWMessage
  = LGWString JSString
  | LGWSCStat ScenarioStatistics
    -- ^ Send general info about scenario objects


instance ToJSVal LGWMessage where
  toJSVal (LGWString s) = pure . coerce . objectValue $ object
    [ ("lgwString", toJSON s)
    ]
  toJSVal (LGWSCStat x) = pure . coerce . objectValue $ object
    [ ("lgwCStat", coerce $ pToJSVal x)
    ]

instance FromJSVal LGWMessage where
    fromJSVal jsv = pure $ case fromJSON (SomeValue jsv) of
      Error _   -> Nothing
      Success x -> Just x

instance FromJSON LGWMessage where
    parseJSON v = flip (withObject "LGWMessage object") v $ \obj -> do
      mmsg  <- obj .:? "lgwString"
      mstat <- obj .:? "lgwCStat"
      case (mmsg, mstat) of
        (Just s, _) -> pure $ LGWString s
        (_, Just x) -> pure $ LGWSCStat x
        (_, _) -> fail "Missing key lgwString or lgwCStat."


