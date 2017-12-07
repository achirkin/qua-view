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


import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe, maybeToList)
import           Reflex
import           Control.Lens
import           Numeric.DataFrame (Mat44f, Vec3f, (%*), fromHom, fromScalar)
import qualified Numeric.Matrix as M
import           Control.Monad (foldM, join)
import           Commons

import           Model.Scenario (Scenario, Scenario')
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Object (Object, ObjectId)
import qualified Model.Scenario.Object as Object
import           Model.Scenario.Properties
import           Program.UserAction
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
    askEvent (UserAction AskClearGeometry) >>= registerEvent (ScenarioUpdate ScenarioCleared)

    -- request all necessary events
    scenarioUpdated     <- askEvent $ ScenarioUpdate ScenarioUpdated
    scenarioCleared     <- askEvent $ ScenarioUpdate ScenarioCleared
    scenarioPropUpdated <- askEvent $ ScenarioUpdate ScenarioPropertyUpdated
    objectPropUpdated   <- askEvent $ ScenarioUpdate ObjectPropertyUpdated
    objectLocUpdated    <- askEvent $ ScenarioUpdate ObjectLocationUpdated
    objectDeleted       <- askEvent $ ScenarioUpdate ObjectDeleted
    objectCloned        <- askEvent $ ScenarioUpdate ObjectCloned

    (resetGLE, resetGLcb) <- newTriggerEvent
    registerEvent (SmallGLInput SmallGL.ResetGL) resetGLE
    (delGLObjE, delGLObjcb) <- newTriggerEvent
    registerEvent (SmallGLInput SmallGL.DeleteObject) delGLObjE

    (updateSSE, updateSScb) <- newTriggerEvent
    registerEvent (ScenarioUpdate ScenarioStateUpdatedOut) updateSSE
    (askResetCamE, askResetCamcb) <- newTriggerEvent
    registerEvent (UserAction AskResetCamera) askResetCamE

    -- unselect objects on all scenario-disturbing events
    (selectedObjE, selectedObjcb) <- newTriggerEvent
    registerEvent (UserAction AskSelectObject)
      $ leftmost [ Nothing <$ resetGLE
                 , Nothing <$ delGLObjE
                 , selectedObjE
                 ]

    -- Assemble events into scenario behavior
    accumM (&) def $ leftmost
          [ loadScenarioPart updateSScb askResetCamcb renderingApi <$> scenarioUpdated
          , clearScenario resetGLcb <$ scenarioCleared
          , updateObjectGeomInScenario <$> objectLocUpdated
          , updateObjectPropInScenario <$> objectPropUpdated
          , updateScenarioProp <$> scenarioPropUpdated
          , deleteObject delGLObjcb <$> objectDeleted
          , cloneObject selectedObjcb renderingApi <$> objectCloned
          ]




deleteObject :: MonadIO (PushM t)
    => ([SmallGL.RenderedObjectId] -> IO ())
    -> ObjectId
    -> Scenario
    -> PushM t Scenario
deleteObject delObjcb objId sc = do
    liftIO $ mapM_ (delObjcb . (:[]) . view Object.renderingId) mobj
    -- TODO: also need to delete object from objectGroups and from templates
    return sc'
  where
    (mobj, sc') = sc & Scenario.objects %%~ Map.updateLookupWithKey (\_ _ -> Nothing) objId



-- | Clone several objects and put them into a single group.
cloneObject :: MonadIO (PushM t)
    => (Maybe ObjectId -> IO ())
    -> SmallGL.RenderingApi
    -> ([ObjectId], Vec3f)
    -> Scenario
    -> PushM t Scenario
cloneObject _ _ ([], _) oldSc = pure oldSc
cloneObject selObjcb renderingApi (objIds, loc) oldSc = liftIO $ do
    newSc <- foldM f oldSc objs
    selObjcb . Just $ newSc ^. Scenario.objIdSeq
    return newSc
  where
    ogroups = Scenario.viewState . Scenario.objectGroups
    newGId = if length objs <= 1
             then Nothing
             else Just $ if Map.null (oldSc ^. ogroups)
                         then 1
                         else 1 + fst (Map.findMax $  oldSc ^. ogroups )
    updateOGroups oid = case newGId of
      Nothing -> id
      Just i  -> Map.alter (Just . (oid:) . join . maybeToList) i
    objs = objIds >>= (\i -> oldSc ^.. Scenario.objects . at i . _Just)
    centerPos = (\cs -> foldl' (+) 0 cs / fromScalar (max 1 . fromIntegral $ length cs))
               $ map (fromHom . view Object.center) objs
    transM = M.translate3 (loc - centerPos)
    f sc obj = do
      let (newOId@(Object.ObjectId oid), sc') = sc & Scenario.objIdSeq <+~ 1
      newRId <- SmallGL.cloneObject renderingApi
        (obj^.Object.renderingId, transM, sc^.Scenario.selectedDynamicColor.colorVeci, oid)
      newGeometry <- Geometry.concatGeometry (obj^.Object.geometry)
      Geometry.applyTransform newGeometry transM
      return $ sc' & Scenario.viewState.Scenario.objectGroups %~ updateOGroups newOId
                   & Scenario.objects %~ Map.insert newOId
         ( obj & Object.renderingId .~ newRId
               & Object.center %~ (transM %*)
               & Object.geometry .~ newGeometry
               & Object.geomID .~ Just newOId
               & Object.groupID .~ newGId
               & Object.properties %~
                 (\p -> p & property "special"    .~ (Nothing :: Maybe PropValue)
                          & property "selectable" .~ (Nothing :: Maybe PropValue)
                          & property "visible"    .~ (Nothing :: Maybe PropValue)
                          & property "static"     .~ (Nothing :: Maybe PropValue)
                 )
         )


loadScenarioPart :: MonadIO (PushM t)
    => (Scenario.ScenarioState -> IO ()) -- ^ "I have updated scenario" callback
    -> (() -> IO ()) -- ^ reset camera event callback in case current scenario is empty.
    -> SmallGL.RenderingApi
    -> Scenario' 'Object.Prepared  -- ^ scenario update
    -> Scenario -- ^ previous scenario state
    -> PushM t Scenario
loadScenarioPart updateSScb askResetCamcb renderingApi newSc oldSc  = do
    sc' <- liftIO $ newSc & Scenario.objects.traverse %%~
                              Object.registerRender (registerRenderFunStub renderingApi)
    -- merge two scenarios
    let updatedSc = oldSc <> sc'
    liftIO . updateSScb $ updatedSc ^. Scenario.viewState
    -- if current scenario is empty, reset camera to center on the new scenario
    -- this callback must go after updateSScb to use latest initial camera state.
    when (Map.null $ oldSc ^. Scenario.objects) $ liftIO $ askResetCamcb ()
    return updatedSc

clearScenario :: MonadIO (PushM t)
    => (() -> IO ())
    -> Scenario
    -> PushM t Scenario
clearScenario resetGLcb _ = fmap def . liftIO $ resetGLcb ()

updateObjectGeomInScenario :: MonadIO (PushM t)
    => ([ObjectId], Mat44f)
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
    => ([ObjectId], Mat44f)
    -> Object.Collection
    -> PushM t  Object.Collection
updateObjectGeomInCollection (is, m) objs
    = foldM f objs xs
  where
    xs = mapMaybe (\i -> (,) i <$> Map.lookup i objs ) is
    f c (i,o) = do
      o' <- updateObjectGeom m o
      return $ Map.insert i o' c

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
      showUserMessage . SingleMsg
         $ "Received geometry ("
         <> toJSString (show . Map.size $ sc^.Scenario.objects)
         <> " objects)."
      logInfo' @JSString "updateScenarioFromLGW" "Received a scenario update:" sc
      return $ Just sc
    updateOrComplain (LGWSError (JSError err)) = do
      showUserMessage . SingleMsg $
        "Loading geometry error. " <> err
      return Nothing
    -- I guess, later we will use this to set up scenario location?..
    updateOrComplain LGWReady = pure Nothing


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
    ObjectLocationUpdated   :: QEventTag ScenarioUpdate ([ObjectId], Mat44f)
    -- | OUTGOING EVENT: scenario has updated its viewState.
    ScenarioStateUpdatedOut :: QEventTag ScenarioUpdate Scenario.ScenarioState
    -- | Delete object from scenario
    ObjectDeleted           :: QEventTag ScenarioUpdate ObjectId
    -- | Clone object by its id and a desired position of a center
    ObjectCloned            :: QEventTag ScenarioUpdate ([ObjectId], Vec3f)



deriveEvent ''ScenarioUpdate
