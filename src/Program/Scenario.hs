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
createScenario :: Reflex t
               => SmallGL.RenderingApi
               -> QuaViewM t (Behavior t Scenario)
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

    -- Assemble events
    rec scenarioB <- switcher (pure def) $ scenarioEB
        (scenarioEB, geomGlUpdatesEEIO) <- fmap splitE
                   .  performEvent $ (updateScenario scenarioPropUpdated
                                                     objectPropUpdated
                                                     objectLocUpdated
                                     ) <$> scenarioChanges
        let scenarioChanges = leftmost [scenarioCleansE, scenarioUpdatesE]
        -- whenever scenario update is received
        scenarioUpdatesE <- performEvent $
            ( \oldSc newSc -> do
                sc' <- liftIO $ newSc & Scenario.objects.traverse %%~
                                          Object.registerRender (registerRenderFunStub renderingApi)
                return $ oldSc <> sc'
            ) <$> scenarioB <@> scenarioUpdated
        -- whenever whole geometry is cleared
        scenarioCleansE <- performEvent $
            ( do
                liftIO $ SmallGL.reset renderingApi
                return def
            ) <$ scenarioCleared
    -- reflect geometry updates in SmallGL engine
    switchPromptOnly never geomGlUpdatesEEIO
                     >>= performEvent . fmap liftIO
                     >>= registerEvent (SmallGLInput SmallGL.ObjectTransform)

    return scenarioB


updateScenario ::
      ( Reflex t, MonadHold t m, MonadFix m )
    => Event t (PropName, Maybe PropValue)
    -> Event t (ObjectId, PropName, Maybe PropValue)
    -> Event t (ObjectId, Mat44f)
    -> Scenario
    -> m (Behavior t Scenario, Event t (IO (SmallGL.RenderedObjectId, Mat44f)))
updateScenario updScPropE updObjPropE updLocE sc = do
    (objColB, sGlEvs) <- updateObjectCollection updObjPropE updLocE $ sc ^. Scenario.objects
    scPropsB <- updateProps updScPropE $ sc ^. Scenario.properties
    return ( constrScenario <$> objColB <*> scPropsB
           , sGlEvs
           )
  where
    constrScenario objCol scProps = sc & Scenario.objects .~ objCol
                                       & Scenario.properties .~ scProps


-- | Quite expensive collection behavior.
--   uses two-times traverse, looks frightening!
updateObjectCollection ::
      ( Reflex t, MonadHold t m, MonadFix m )
    => Event t (ObjectId, PropName, Maybe PropValue)
    -> Event t (ObjectId, Mat44f)
    -> Object.Collection
    -> m (Behavior t Object.Collection, Event t (IO (SmallGL.RenderedObjectId, Mat44f)))
updateObjectCollection updPropE updLocE objsI = do
    objBglEvs <- Map.traverseWithKey updateObject' objsI
    return (traverse fst objBglEvs, leftmost . Map.elems $ snd <$> objBglEvs)
  where
    updateObject' objId obj = updateObject
          (fmapMaybe (\(i,n,v) -> if i == objId then Just (n,v) else Nothing) updPropE)
          (fmapMaybe (\(i,m)   -> if i == objId then Just m     else Nothing) updLocE)
          obj


-- | Be careful, this function has side effects encapsulated in the second argument
--      geometry contains IODataFrames inside; it is rewritten in-place!
updateObject :: ( Reflex t, MonadHold t m, MonadFix m )
              => Event t (PropName, Maybe PropValue)
              -> Event t Mat44f
              -> Object
              -> m (Behavior t Object, Event t (IO (SmallGL.RenderedObjectId, Mat44f)))
updateObject updPropE updLocE objI = do
    propsB <- updateProps updPropE $ objI ^. Object.properties
    centerB <- accum (flip (%*)) (objI^.Object.center) updLocE
    return $ (constructObj <$> propsB <*> centerB, webGLUpdates <$> updLocE)
  where
    webGLUpdates m  = ( objI^.Object.renderingId, m )
                   <$ Geometry.applyTransform (objI^.Object.geometry) m
    constructObj props center = objI & Object.center .~ center
                                     & Object.properties .~ props


-- | Make a behavior from updated properties and initial value
updateProps :: (Reflex t, MonadHold t m, MonadFix m)
            => Event t (PropName, Maybe PropValue)
            -> Properties -> m (Behavior t Properties)
updateProps updE propsI = mfix holdProps
  where
    holdProps propsB = hold propsI (setPropsE propsB)
    setPropsE propsB = setProps' <$> propsB <@> updE
    setProps' props (pname, pval) = set (property pname) pval props



-- | Get `WorkerMessage LGWMessage` and update scenario accordingly
updateScenarioFromLGW :: Reflex t => QuaViewM t ()
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



deriveEvent ''ScenarioUpdate
