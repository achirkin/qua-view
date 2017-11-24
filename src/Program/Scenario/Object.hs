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
import           Numeric.DataFrame (fromHom, eye) -- Mat44f, (%*))

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
colorObjectsOnSelection :: Reflex t
                        => Behavior t Scenario
                        -> Dynamic t (Maybe ObjectId)
                        -> QuaViewM t ()
colorObjectsOnSelection scB selObjD = do

    registerEvent (SmallGLInput SmallGL.SetObjectColor) . nonEmptyOnly
       $ ( \scenario oldOId newOId ->
             do
              obj <- getObj scenario oldOId
              return ( obj^.Object.renderingId
                     , Scenario.resolvedObjectColor scenario obj ^. colorVeci )
             <>
             do
              obj <- getObj scenario newOId
              return ( obj^.Object.renderingId
                     , selectedColor scenario $ obj^.Object.objectBehavior  )
         )
      <$> scB <*> current selObjD <@> updated selObjD

  where
    getObj scenario moid = maybe [] (:[]) $ moid >>= \i -> scenario ^. Scenario.objects . at i
    selectedColor sc Object.Dynamic = sc^.Scenario.selectedDynamicColor.colorVeci
    selectedColor sc Object.Static  = sc^.Scenario.selectedStaticColor.colorVeci



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

    -- We lock camera movemement and activate object transform when a pointer is down on a selected
    -- object. If there are more than one pointer, we reset object motion every up or down event
    -- to change motion mode correcty. However, if camera is not locked before pointer up event,
    -- the event should not fire to avoid unnecessary trivial geometry updates.
    rec  camLockedD <- holdDyn False camLockedE
         ptrNB <- accum (&) (0 :: Int)
                  $ leftmost [ (+1) <$ downsE
                             , (\n -> max 0 (n-1)) <$ upsE
                             , (\n -> max 0 (n-1)) <$ clicksE
                             , (const 0 :: Int -> Int) <$ cancelsE
                             ]
         let upsE = select (Animation.pointerEvents aHandler) PUpEvent
             clicksE = select (Animation.pointerEvents aHandler) PClickEvent
             cancelsE = select (Animation.pointerEvents aHandler) PCancelEvent
             downME = downF <$> ptrNB <*> current camLockedD <*> current selObjIdD <@> downsE
             downF :: Int -> Bool -> Maybe ObjectId -> Maybe ObjectId -> Maybe Bool
             downF ptrN wasLocked wasSelected isPressed
                | ptrN > 0 && wasLocked     = Just True
                | ptrN > 0 && not wasLocked = Nothing
                | wasSelected == isPressed  = True <$ isPressed
                | otherwise                 = Nothing
             clickME = upF <$> ptrNB <*> current camLockedD <@ clicksE
             upME = upF <$> ptrNB <*> current camLockedD <@ upsE
             upF :: Int -> Bool -> Maybe Bool
             upF ptrN wasLocked
                | wasLocked && ptrN > 1 = Just True
                | wasLocked             = Just False
                | otherwise             = Nothing
             cancelME = cancelF <$> current camLockedD <@ cancelsE
             cancelF wasLocked
                | wasLocked = Just False
                | otherwise = Nothing
             camLockedE = fmapMaybe id $ leftmost [cancelME, upME, clickME, downME]



    -- events of object transforms
    let transformE = gate (current camLockedD)
                   $ objectTransformEvents aHandler cameraB centerPosB

    transformB <- hold eye $ transformE


    -- Every time camera UNLOCKED event happens, or LOCKED->LOCKED event happens,
    --  we need to persist current changes
    let persistGeomChangeE = fmapMaybe id
                           $ (\moid m wasLocked -> if wasLocked then flip (,) m <$> moid else Nothing )
                          <$> current selObjIdD
                          <*> transformB
                          <*> current camLockedD
                          <@ updated camLockedD

    registerEvent (ScenarioUpdate ObjectLocationUpdated) persistGeomChangeE
    registerEvent (SmallGLInput SmallGL.PersistGeomTransforms)
        $ (\s (i, m) -> case s ^. Scenario.objects . at i of
                        Nothing -> []
                        Just o  -> [(o ^. Object.renderingId,m)]
          )
       <$> scenarioB
       <@> persistGeomChangeE
    registerEvent (SmallGLInput SmallGL.TransformObject)
        $ fmap (:[])
        $ fmapMaybe (\(mi, m) -> flip (,) m <$> mi)
        $ (,) <$> selectedRenderingIdB <@> transformE



    logDebugEvents' @JSString "Program.Object"
         $ (,)  "Camera-locked state:" . Just
        <$> updated camLockedD
    return $ current camLockedD
  where
    selectedObjB = (\s mi -> mi >>= \i -> s ^. Scenario.objects . at i)
                <$> scenarioB <*> current selObjIdD
    selectedRenderingIdB = fmap (view Object.renderingId) <$> selectedObjB
    -- find center position for correct rotation
    centerPosB = maybe 0 (fromHom . view Object.center) <$> selectedObjB


nonEmptyOnly :: Reflex t => Event t [a] -> Event t [a]
nonEmptyOnly = ffilter (not . null)


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
