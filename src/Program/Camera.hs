{-# LANGUAGE RecursiveDo           #-}
-- | Dynamic behavior of camera
module Program.Camera
    ( dynamicCamera
    , objectTransformEvents
    ) where


import           Control.Lens
import           Commons

import           Numeric.DataFrame
import           Reflex.Class
import           Reflex.Dom.Widget.Animation ( AEventType (..), PEventType (..), AnimationHandler )
import qualified Reflex.Dom.Widget.Animation as A

import           Model.Camera
import qualified Model.Scenario as Scenario
import           Program.UserAction
import qualified Program.Scenario as Scenario




-- | Reflex camera's behavior
dynamicCamera :: Reflex t
              => AnimationHandler t
              -> Behavior t Bool
                 -- ^ whether camera is locked due to object actions
              -> QuaViewM t (Dynamic t Camera)
dynamicCamera aHandler camLockedB = mdo
    scStateUpdatedE <- askEvent $ ScenarioUpdate Scenario.ScenarioStateUpdatedOut
    resetCameraE <- askEvent $ UserAction AskResetCamera
    -- derive default camera state and its position from scenario
    initCStateB <- hold initCStateI
      $ lookAtState . view Scenario.cameraPos <$> scStateUpdatedE
    clippingDistD <- holdDyn clippingDistI
      $ view Scenario.clippingDist <$> scStateUpdatedE

    -- all changes of the viewport size come from AResizeEvent
    viewportSizeD <- holdDyn (viewportSize camI) $ resizeE
    -- projection matrix also changes only at AResizeEvents
    projMatrixD <- holdDyn (projMatrix camI)
      $ leftmost [ makeProjM      <$> current clippingDistD <@> updated viewportSizeD
                 , flip makeProjM <$> current viewportSizeD <@> updated clippingDistD
                 ]
    -- checkpoint camera states - when there are no active actions, such as mouse dragging
    oldStateD <- holdDyn (oldState camI) $ leftmost
        [ -- mouse wheel is an atomic event, so we update oldState directly
          flip wheelT <$> camB <@> select (A.animationEvents aHandler) AWheelEvent
          -- copy newState when some continuous action finishes
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PUpEvent)
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PClickEvent)
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PCancelEvent)
          -- should not be needed, but also should not harm?
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PDownEvent)
          -- reset camera
        , initCStateB <@ resetCameraE
        ]
    -- updates on every action
    newStateD <- holdDyn (newState camI) $ leftmost
        [ -- every time oldState updates, copy its state
          updated oldStateD
          -- get pairs of screen points (old-new) on each mouseDrag event
        , flip ($) <$> camB <@> fmapMaybe id movs
        ]

    let camD = Camera <$> viewportSizeD
                      <*> clippingDistD
                      <*> projMatrixD
                      <*> oldStateD
                      <*> newStateD
        camB = current camD

    return camD
  where
    initCStateI = lookAtState $ def ^. Scenario.cameraPos
    clippingDistI = def ^. Scenario.clippingDist
    camI = initCamera (realToFrac . fst $ A.viewPortSizeI aHandler)
                      (realToFrac . snd $ A.viewPortSizeI aHandler)
                      clippingDistI initCStateI
    -- resize events in local format
    resizeE = (\(A.ResizeEvent s) -> realToFrac *** realToFrac $ s)
              <$> select (A.animationEvents aHandler) AResizeEvent

    -- Modify camera with will zooming
    wheelT :: A.WheelEvent -> Camera -> CState
    wheelT A.WheelUp   = scroll (-0.1)
    wheelT A.WheelDown = scroll 0.15

    -- All pointer move events
    pointerT :: Int -> [(Vec2f,Vec2f)] -> Maybe (Camera -> CState)
    -- move unknown move (should not happen anyway)
    pointerT _ [] = Nothing
    -- Three-finger rotation
    pointerT 1 ((o1,n1):_:_:_)  = Just $ rotateCentered o1 n1
    -- Complicated two-finger control
    pointerT 1 [(o1,n1),(o2,n2)] = Just $ twoFingerControl (o1,o2) (n1,n2)
    -- Mouse control             -- do nothing if no button pressed
    pointerT b ((opos,npos):_) | b == 0 = Nothing
                                 -- Drag horizontally using left mouse button
                               | b == 1 = Just $ dragHorizontal opos npos
                                 -- Rotating using secondary button (right m b)
                               | b == 2 = Just $ rotateCentered opos npos
                                 -- Dragging vertically using wheel button press
                               | b == 4 = Just $ dragVertical opos npos
                                 -- fallback to horizontal dragging
                               | otherwise = Just $ dragHorizontal opos npos

    dTupleToVec :: (Double, Double) -> Vec2f
    dTupleToVec (x,y) = vec2 (realToFrac x) (realToFrac y)


    -- movs :: Event t (Camera -> CState)
    -- get camera motion updates
    movs = (\b as bs -> pointerT b $ zipWith (\o n -> (dTupleToVec o, dTupleToVec n)) as bs)
          <$> A.buttonsB aHandler
          <*> A.downPointersB aHandler
          <*> A.curPointersB aHandler
          <@ gate (not <$> camLockedB)
             (select (A.animationEvents aHandler) (APointerEvent PMoveEvent))


-- | Computes matrix transforms as if object with the given center position were selected.
--   This function does not check if any object is selected at the moment;
--   applying the transform to an appropriate object at an appropriate time
--   is the responsibility of the caller.
--
--   This event triggers on every pointer move when at least one pointer is down.
objectTransformEvents :: Reflex t
                      => AnimationHandler t
                      -> Behavior t Camera
                      -> Behavior t Vec3f -- ^ position of object center (used for rotation)
                      -> Event t Mat44f
objectTransformEvents aHandler camB oCenterB = fmapMaybe id movs
  where
    -- All pointer move events
    pointerT :: Camera -> Int -> Vec3f -> [(Vec2f,Vec2f)] -> Maybe Mat44f
    -- move unknown move (should not happen anyway)
    pointerT _ _ _ [] = Nothing
    -- Three-finger rotation
    pointerT cam 1 center ((o1,n1):_:_:_)  = Just $ rotateObject o1 n1 cam center
    -- Complicated two-finger control
    pointerT cam 1 _ [(o1,n1),(o2,n2)] = Just $ twoFingerObject (o1,o2) (n1,n2) cam
    -- Mouse control
    pointerT cam b center ((opos,npos):_)
         -- do nothing if no button pressed
       | b == 0 = Nothing
         -- Drag horizontally using left mouse button
       | b == 1 = Just $ dragObject opos npos cam
         -- Rotating using secondary button (right m b)
       | b == 2 = Just $ rotateObject opos npos cam center
         -- fallback to horizontal dragging
       | otherwise = Just $ dragObject opos npos cam

    dTupleToVec :: (Double, Double) -> Vec2f
    dTupleToVec (x,y) = vec2 (realToFrac x) (realToFrac y)

    movs = (\cam b c as bs -> pointerT cam b c
                               $ zipWith (\o n -> (dTupleToVec o, dTupleToVec n)) as bs
           )
          <$> camB
          <*> A.buttonsB aHandler
          <*> oCenterB
          <*> A.downPointersB aHandler
          <*> A.curPointersB aHandler
          <@ select (A.animationEvents aHandler) (APointerEvent PMoveEvent)
