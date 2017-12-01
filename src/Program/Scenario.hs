{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Dynamics and events related to how scenario is updated in the program
module Program.Scenario
    ( QEventTag (..)
    , createScenario
    ) where


import qualified Data.Map.Strict as Map
import           Reflex
import           Control.Lens
import           Numeric.DataFrame (Mat44f, (%*))
import           Commons

import           Model.Scenario (Scenario, Scenario')
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Object (Object, ObjectId)
import qualified Model.Scenario.Object as Object
import           Model.Scenario.Properties
import           Workers.LoadGeometry (LGWMessage (..), QEventTag (..))


import qualified SmallGL
import qualified SmallGL.Types as SmallGL

-- | Create a scenario object by aggregating all scenario-related events.
--   We return a behavior only to restict users from exploiting high-throughput aggregated
--   scenario updates event.
createScenario :: ( Reflex t
                  , PerformEvent t m
                  , MonadIO (Performable m)
                  , TriggerEvent t m, MonadHold t m
                  , MonadFix m
                  )
               => SmallGL.RenderingApi
               -> QuaViewT Writing t m (Behavior t Scenario)
createScenario renderingApi = do
    -- Set up related event streams
    updateScenarioFromLGW
    askEvent (UserRequest AskClearGeometry) >>= registerEvent (ScenarioUpdate ScenarioCleared)

    -- request all necessary events
    scenarioUpdated     <- askEvent $ ScenarioUpdate ScenarioUpdated
    scenarioCleared     <- askEvent $ ScenarioUpdate ScenarioCleared
    scenarioPropUpdated <- askEvent $ ScenarioUpdate ScenarioPropertyUpdated
    objectPropUpdated   <- askEvent $ ScenarioUpdate ObjectPropertyUpdated
    objectLocUpdated    <- askEvent $ ScenarioUpdate ObjectLocationUpdated

    (resetGLE, resetGLcb) <- newTriggerEvent
    registerEvent (SmallGLInput SmallGL.ResetGL) resetGLE

    (updateSSE, updateSScb) <- newTriggerEvent
    registerEvent (ScenarioUpdate ScenarioStateUpdatedOut) updateSSE

    -- Assemble events into scenario behavior
    accumM (&) def $ leftmost
          [ loadScenarioPart updateSScb renderingApi <$> scenarioUpdated
          , clearScenario resetGLcb <$ scenarioCleared
          , updateObjectGeomInScenario <$> objectLocUpdated
          , updateObjectPropInScenario <$> objectPropUpdated
          , updateScenarioProp <$> scenarioPropUpdated
          ]


loadScenarioPart :: MonadIO (PushM t)
    => (Scenario.ScenarioState -> IO ())
    -> SmallGL.RenderingApi
    -> Scenario' 'Object.Prepared  -- ^ scenario update
    -> Scenario -- ^ previous scenario state
    -> PushM t Scenario
loadScenarioPart updateSScb renderingApi newSc oldSc  = do
    sc' <- liftIO $ newSc & Scenario.objects.traverse %%~
                              Object.registerRender (registerRenderFunStub renderingApi)
    let updatedSc = oldSc <> sc'
    liftIO . updateSScb $ updatedSc ^. Scenario.viewState
    return updatedSc

clearScenario :: MonadIO (PushM t)
    => (() -> IO ())
    -> Scenario
    -> PushM t Scenario
clearScenario resetGLcb _ = fmap def . liftIO $ resetGLcb ()

updateObjectGeomInScenario :: MonadIO (PushM t)
    => (ObjectId, Mat44f)
    -> Scenario
    -> PushM t Scenario
updateObjectGeomInScenario
    = Scenario.objects . updateObjectGeomInCollection

updateObjectPropInScenario ::
       (ObjectId, PropName, Maybe PropValue)
    -> Scenario
    -> PushM t Scenario
updateObjectPropInScenario p
    = pure . over Scenario.objects (updateObjectPropInCollection p)

updateScenarioProp ::
       (PropName, Maybe PropValue)
    -> Scenario
    -> PushM t Scenario
updateScenarioProp p = pure . over Scenario.properties (updateProps p)



updateObjectGeomInCollection :: MonadIO (PushM t)
    => (ObjectId, Mat44f)
    -> Object.Collection
    -> PushM t  Object.Collection
updateObjectGeomInCollection (i, m) objs
    = case Map.lookup i objs of
        Nothing -> pure objs
        Just o -> do
          o' <- updateObjectGeom m o
          return $ Map.insert i o' objs

updateObjectPropInCollection ::
       (ObjectId, PropName, Maybe PropValue)
    -> Object.Collection
    -> Object.Collection
updateObjectPropInCollection (i, n, v) = Map.adjust (updateObjectProp (n,v)) i


updateObjectGeom :: MonadIO (PushM t)
    => Mat44f
    -> Object
    -> PushM t Object
updateObjectGeom m obj = liftIO $ do
    Geometry.applyTransform (obj^.Object.geometry) m
    return $ obj & Object.center %~ (m %*)


updateObjectProp :: (PropName, Maybe PropValue)
                 -> Object -> Object
updateObjectProp = over Object.properties . updateProps

-- | Make a behavior from updated properties and initial value
updateProps :: (PropName, Maybe PropValue)
            -> Properties -> Properties
updateProps (pname, pval) = set (property pname) pval




-- | Get `WorkerMessage LGWMessage` and update scenario accordingly
updateScenarioFromLGW :: ( Reflex t
                         , PerformEvent t m
                         , MonadIO (Performable m)
                         )
                      => QuaViewT Writing t m ()
updateScenarioFromLGW = do
    loadedGeometryE <- askEvent $ WorkerMessage LGWMessage
    scUpdates <- performEvent $ updateOrComplain <$> loadedGeometryE
    registerEvent (ScenarioUpdate ScenarioUpdated) $ fmapMaybe id scUpdates
  where
    updateOrComplain (LGWResult sc) = do
      logInfo' @JSString "updateScenarioFromLGW" "Received a scenario update:" sc
      return $ Just sc
    updateOrComplain (LGWSError (JSError err)) = do
      showUserMessage . SingleMsg $
        "Error happened when I tried to receive a new geometry! " <> err
      return Nothing
    -- I guess, later we will use this to set up scenario location?..
    updateOrComplain (LGWSCStat _) = pure Nothing


-- TODO: cope with all data types
registerRenderFunStub :: SmallGL.RenderingApi
                      -> SmallGL.ObjRenderingData m
                      -> IO SmallGL.RenderedObjectId
registerRenderFunStub r d@SmallGL.ObjColoredData{} = SmallGL.addRObject r d
registerRenderFunStub _ _                          = pure $ SmallGL.RenderedObjectId (-1)


----------------------------------------------------------------------------------------------------


-- | Here we need to aggregate all possible message types related to Model.Scenario.
data instance QEventTag ScenarioUpdate evArg where
    -- | New scenario chunk comes from somewhere
    ScenarioUpdated         :: QEventTag ScenarioUpdate (Scenario' 'Object.Prepared)
    -- | Erase whole scenario
    ScenarioCleared         :: QEventTag ScenarioUpdate ()
    -- | Add, modify, or delete scenario property
    ScenarioPropertyUpdated :: QEventTag ScenarioUpdate (PropName, Maybe PropValue)
    -- | Add, modify, or delete object property
    ObjectPropertyUpdated   :: QEventTag ScenarioUpdate (ObjectId, PropName, Maybe PropValue)
    -- | Update object location using transformation matrix
    ObjectLocationUpdated   :: QEventTag ScenarioUpdate (ObjectId, Mat44f)
    -- | OUTGOING EVENT: scenario has updated its viewState.
    ScenarioStateUpdatedOut :: QEventTag ScenarioUpdate Scenario.ScenarioState



deriveEvent ''ScenarioUpdate
