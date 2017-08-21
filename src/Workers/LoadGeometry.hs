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

import Commons
import Workers
#ifdef ISWORKER
import Data.Conduit
import JavaScript.JSON.Types.Internal
import Model.GeoJSON.Coordinates

loadGeometryConduit :: (MonadIO m, MonadLogger m)
                    => Conduit LoadedTextContent m (JSVal, [Transferable])
loadGeometryConduit = awaitForever $ \msg -> do
    errOrVal <- parseJSON $ getTextContent msg
    case errOrVal of
      Left err -> logError (workerLS loadGeometryDef) err
      Right (SomeValue val) -> do
        logInfo' @JSString (workerLS loadGeometryDef) "Got a message!" val
        logInfo' @JSString (workerLS loadGeometryDef) "Centres:" (js_getObjectCentres val)
    yield (pToJSVal ("Thanks!" :: JSString), [])



#else
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class

runLoadGeometryWorker :: ( MonadIO m, Reflex t
                         , TriggerEvent t m
                         , PerformEvent t m
                         , MonadIO (Performable m)
                         )
                      => Event t LoadedTextContent
                      -> m (Event t JSVal)
runLoadGeometryWorker inEvs = runWorker loadGeometryDef $ flip (,) [] <$> inEvs


#endif




loadGeometryDef :: WorkerDef
loadGeometryDef = WorkerDef
  { workerName = "LoadGeometry"
  , workerUrl  = "qua-worker-loadgeometry.js"
  }
