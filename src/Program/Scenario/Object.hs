{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
-- | Dynamics and events related to actions on scenario objects
module Program.Scenario.Object
    ( QEventTag (..)
    , objectSelectionsDyn
    , colorObjectsOnSelection
    , moveSelectedObjects
    ) where


import           Commons

--import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Reflex
import           Reflex.Dom.Widget.Animation (AnimationHandler)
import qualified Reflex.Dom.Widget.Animation as Animation
import           Control.Lens
import           Numeric.DataFrame (fromHom) -- Mat44f, (%*))

import           Model.Camera (Camera)
import           Model.Scenario (Scenario)
import qualified Model.Scenario as Scenario
--import qualified Model.Scenario.Object.Geometry as Geometry
import           Model.Scenario.Object (ObjectId (..))
import qualified Model.Scenario.Object as Object
import           Model.Scenario.Properties

import           Program.Camera
import           Program.Scenario

import qualified SmallGL
--import qualified SmallGL.Types as SmallGL



-- | Selected object id events.
--   They happen when user clicks on a canvas;
--   Either some object is selected, or nothing (clicked on empty or non-selectable space).
--   The dynamic returned is guaranteed to change on every update.
objectSelectionsDyn :: ( Reflex t, MonadIO (Performable m)
                       , PerformEvent t m
                       , MonadHold t m
                       , MonadLogger m
                       , MonadFix m
                       )
                    => AnimationHandler t
                    -> SmallGL.RenderingApi
                    -> m (Dynamic t (Maybe ObjectId))
objectSelectionsDyn aHandler renderingApi = do
    selectorClicks <- performEvent $ getClicked renderingApi
                                  <$> Animation.downPointersB aHandler
                                  <@ select (Animation.pointerEvents aHandler) PClickEvent
    rec selIdD <- holdDyn Nothing selIdE
        let selIdE = fmapMaybe (\(i,j) -> if i == j then Nothing else Just j)
                               $ (,) <$> current selIdD <@> selectorClicks
    logDebugEvents' @JSString "Program.Scenario.Object" $ (,) "selectedObjId" . Just <$> updated selIdD
    return selIdD


-- | Color objects when they are selected or unselected.
--   This function does not generate any Reflex objects and does not affect any existing Reflex objects.
--   It just updates SmallGL color buffers based on scenario settings.
colorObjectsOnSelection :: ( Reflex t, MonadIO (Performable m)
                           , PerformEvent t m
                           )
                        => SmallGL.RenderingApi
                        -> Behavior t Scenario
                        -> Dynamic t (Maybe ObjectId)
                        -> m ()
colorObjectsOnSelection renderingApi scB selObjD
    = performEvent_ $ f <$> scB <*> current selObjD <@> updated selObjD
  where
    f scenario oldObjId newObjId = liftIO $ do
        case mnewObj of
          Nothing -> return ()
          Just o  -> SmallGL.setObjectColor renderingApi
                                            (o^.Object.renderingId)
                                            (selectedColor $ o^.Object.objectBehavior)
        case moldObj of
          Nothing -> return ()
          Just o  -> SmallGL.setObjectColor renderingApi
                                            (o^.Object.renderingId)
                                            (Scenario.resolvedObjectColor scenario o ^. colorVeci)

      where
        selectedColor Object.Dynamic = scenario^.Scenario.selectedDynamicColor.colorVeci
        selectedColor Object.Static  = scenario^.Scenario.selectedStaticColor.colorVeci
        moldObj = oldObjId >>= \i -> scenario ^. Scenario.objects . at i
        mnewObj = newObjId >>= \i -> scenario ^. Scenario.objects . at i



{- | Move objects when they are selected and dragged.

     This function must be quite complicated, because we have to update object geometry only on
     end-of-transform events to avoid object shivering and other graphics artifacts and make object
     motion more stable (pointer-move events are too often, which leads to all sorts of these problems).

     Therefore, we have to keep a snapshot of object geometry everytime we select an object.
     Thus, on every pointer-move we can update visual position of an object (in webgl) while not
     touching the real recorded object position.
     Then, on pointer-up event we update real position with a well-defined transform matrix.

     This monadic function consists of several steps:

     1. Record (as a behavior) center of the last selected group of objects (used for rotation).
     2. Use `objectTransformEvents` when selected object dynamic is not Nothing.
     3. Ask SmallGL to take snapshots of geometry on checkpoint events.
     4. Ask SmallGL to temporary update geometry on pointer-move events.
     5. Persist changes on checkpoint events by firing ObjectLocationUpdated events
         (and event consumer (Program.Scenario) should ask SmallGL to update geometry one more time)
     6. Return bool behavior whether camera should be locked by object actions
-}
moveSelectedObjects :: Reflex t
                    => AnimationHandler t
                    -> SmallGL.RenderingApi
                    -> Behavior t Camera
                    -> Behavior t Scenario
                    -> Dynamic t (Maybe ObjectId)
                    -> QuaViewM t (Behavior t Bool)
moveSelectedObjects aHandler renderingApi cameraB scenarioB selObjIdD = do
    -- if the object is pointerDown'ed
    downsE <- performEvent $ getClicked renderingApi
                          <$> Animation.curPointersB aHandler
                          <@ select (Animation.pointerEvents aHandler) PDownEvent
    camLockedB <- hold False $ leftmost [ (==) <$> current selObjIdD <@> fmapMaybe (fmap Just) downsE
                                        , False <$ upsE
                                        ]
    selectedRenderingIdD <- holdDyn Nothing $ fmap (view Object.renderingId) <$> selectedObjE


    -- find center position for correct rotation
    centerPosB <- hold 0 $ fromHom . view Object.center
                        <$> fmapMaybe id selectedObjE -- does not work on obj updates
    -- events of object transforms
    let transformE = gate camLockedB
                   $ objectTransformEvents aHandler cameraB centerPosB

    registerEvent (ScenarioUpdate ObjectLocationUpdated) $ fmapMaybe (\(mx,y) -> flip (,) y <$> mx)
                                                         $ (,) <$> current selObjIdD
                                                               <@> transformE
    performEvent_ $ ( \mi -> case mi of
                        Nothing -> return ()
                        Just i  -> liftIO $ SmallGL.addToGeomCache renderingApi i
                    )
                <$> current selectedRenderingIdD
                <@ select (Animation.pointerEvents aHandler) PDownEvent

    performEvent_ $ ( \mi m -> case mi of
                        Nothing -> return ()
                        Just i  -> liftIO $ SmallGL.transformObject renderingApi i m
                    )
                <$> current selectedRenderingIdD
                <@> transformE

    logDebugEvents' @JSString "Program.Object" $ (,)  "transform!" . Just <$> transformE
    return camLockedB
--    performEvent_ $ f <$>  <*> scenarioB <*> selObjB <$> objectTransformEvents cameraB
  where
    upsE = select (Animation.pointerEvents aHandler) PUpEvent
    selectedObjE = (\s mi -> mi >>= \i -> s ^. Scenario.objects . at i)
                <$> scenarioB <@> updated selObjIdD




-- | Helper function to determine ObjectId of a currently hovered object.
getClicked :: MonadIO m => SmallGL.RenderingApi -> [(Double,Double)] -> m (Maybe ObjectId)
getClicked _ []
    = pure Nothing
getClicked renderingApi ((x,y):xs)
    = (fmap f . liftIO $ SmallGL.getHoveredSelId renderingApi (round x, round y))
      -- try one more time
      >>= \mi -> if isJust mi then pure mi else getClicked renderingApi xs
  where
    f oid = if oid == 0xFFFFFFFF then Nothing else Just (ObjectId oid)
