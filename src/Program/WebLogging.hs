{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Strict #-}
module Program.WebLogging
    ( logActions
    ) where


import JavaScript.JSON.Types
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Generic ()
import GHC.Generics ()
import Reflex

import Commons
import qualified QuaTypes

import GHC.Generics (Generic)
import Numeric.DataFrame (Mat44f, Vec3f)
import Model.Camera (Camera, CState, oldState)
import Model.Scenario.Properties (PropName, PropValue)
import Model.GeoJSON.Scenario ()
import Model.Scenario.Object (ObjectId)

import Program.Scenario

-- | This is a logging data type, convertible to and from JSON.
--   Generally, we use it to log scenario updates and camera motions,
--   which is enough to restore a full picture of what is happening in qua-view for a user.
--
--   We have to keep this data type plain (no GADT) and up to date with
--   `Program.Scenario.ScenarioUpdate` and, possibly, other events, like camera updates.
data WebLogging
  = WlLoadedSettings { _loadedSettings :: QuaTypes.Settings }
    -- ^ Qua-viewer is opened with the specified settings supplied
  | WlCameraUpdate { _cameraState :: CState }
    -- ^ Camera state updated (refer to `Model.Camera` for description)
  | WlScenarioUpdated { _scenarioUpdate :: Value }
    -- ^ New scenario chunk comes from somewhere.
    --   Content of JSON.Value object is a qua-kit scenario (or its part).
  | WlScenarioCleared
    -- ^ Erase whole scenario
  | WlScenarioPropertyUpdated
    { _propName :: PropName, _propValue :: (Maybe PropValue) }
    -- ^ Add, modify, or delete scenario property
  | WlObjectPropertyUpdated
    { _geomId :: ObjectId,  _propName :: PropName, _propValue :: Maybe PropValue }
    -- ^ Add, modify, or delete object property
  | WlObjectLocationUpdated
    { _geomIds :: [ObjectId], _tranformMatrix :: Mat44f }
    -- ^ Update object location using transformation matrix
  | WlObjectDeleted { _geomIds :: [ObjectId] }
    -- ^ Delete object from scenario
  | WlObjectCloned
    { _geomIds :: [ObjectId], _newObjCenter :: Vec3f }
    -- ^ Clone object by its id and a desired position of a center
  deriving Generic

instance ToJSON WebLogging
instance FromJSON WebLogging


logActions :: Reflex t
           => Dynamic t Camera -- ^ updates of the camera (check Model.Camera for description)
           -> QuaViewM t ()
logActions camUpdatedD = do

  settingsD <- quaSettings
  settingsI <- sample $ current settingsD

  -- request all necessary events
  scenarioUpdated     <- askEvent $ ScenarioUpdate ScenarioUpdated
  scenarioCleared     <- askEvent $ ScenarioUpdate ScenarioCleared
  scenarioPropUpdated <- askEvent $ ScenarioUpdate ScenarioPropertyUpdated
  objectPropUpdated   <- askEvent $ ScenarioUpdate ObjectPropertyUpdated
  objectLocUpdated    <- askEvent $ ScenarioUpdate ObjectLocationUpdated
  objectDeleted       <- askEvent $ ScenarioUpdate ObjectDeleted
  objectCloned        <- askEvent $ ScenarioUpdate ObjectCloned

  let webLoggingE = leftmost
        [ -- WlLoadedSettings <$> updated settingsD -- omit this, because we send this as the first msg
          fmap WlCameraUpdate . updated . fromUniqDynamic . uniqDynamic $ oldState <$> camUpdatedD
        , WlScenarioUpdated . toJSON <$> scenarioUpdated
        , WlScenarioCleared <$ scenarioCleared
        , uncurry WlScenarioPropertyUpdated <$> scenarioPropUpdated
        , (\(i,n,v) -> WlObjectPropertyUpdated i n v) <$> objectPropUpdated
        , uncurry WlObjectLocationUpdated <$> objectLocUpdated
        , WlObjectDeleted <$> objectDeleted
        , uncurry WlObjectCloned <$> objectCloned
        ]
      wlMsgE = push (fmap Just . liftIO . jsonStringify . toJSON) webLoggingE


  let withSettings Nothing QuaTypes.Settings { QuaTypes.loggingUrl = Nothing }
        = return Nothing
      withSettings (Just socket) QuaTypes.Settings { QuaTypes.loggingUrl = Nothing }
        = Nothing <$ closeSocket socket
      withSettings msocket s@QuaTypes.Settings { QuaTypes.loggingUrl = Just url }
        = do
        socket <- liftIO $ do
          mapM_ closeSocket msocket
          registerLogging url
        msg <- liftIO . jsonStringify . toJSON $ WlLoadedSettings s
        liftIO $ sendMessage socket msg
        return $ Just socket

  mSocketI <- liftIO $ withSettings Nothing settingsI
  mSocketB <- accumM (\s -> liftIO . withSettings s) mSocketI (updated settingsD)
  let sockMsgE = (\ms msg -> flip (,) msg <$> ms) <$> mSocketB <@> wlMsgE

  -- send messages when socket is available
  performEvent_ $ liftIO . uncurry sendMessage <$> fmapMaybe id sockMsgE



foreign import javascript interruptible
  "var t = true, s = new WebSocket($1); s.onopen = function(){if(t){t=false;$c(s);}}; s.onerror = function(){if(t){t=false;$c(null);}};"
  registerLogging :: JSString -> IO JSVal

foreign import javascript unsafe "if($1 != null){$1.send($2);}"
  sendMessage :: JSVal -> JSString -> IO ()

foreign import javascript unsafe "try{if($1 != null){$1.close();}}catch(e){console.log(e);}"
  closeSocket :: JSVal -> IO ()
