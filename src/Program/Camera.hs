{-# LANGUAGE RecursiveDo           #-}
-- | Dynamic behavior of camera
module Program.Camera
    ( dynamicCamera
    ) where



import           Commons

import           Numeric.DataFrame
import           Reflex.Class
import           Reflex.Dom.Widget.Animation ( AEventType (..), PEventType (..), AnimationHandler )
import qualified Reflex.Dom.Widget.Animation as A

import           Model.Camera



defaultCState :: CState
defaultCState = CState
  { viewPoint  = vec3 (-2) 3 0
  , viewAngles = (2.745, 0.995)
  , viewDist = 668
  }


-- | Reflex camera's behavior
dynamicCamera :: Reflex t
              => AnimationHandler t
              -> Behavior t Vec2f
              -> QuaViewM t (Dynamic t Camera)
dynamicCamera aHandler defCenterPointB = mdo
    resetCameraE <- askEvent $ UserRequest AskResetCamera
    -- all changes of the viewport size come from AResizeEvent (or resetCameraE)
    viewportSizeD <- holdDyn (viewportSize icam) $ leftmost [resizeE, viewportSize icam <$ resetCameraE]
    -- projection matrix also changes only at AResizeEvents (or resetCameraE)
    projMatrixD <- holdDyn (projMatrix icam) $ leftmost [makeProjM <$> resizeE, projMatrix icam <$ resetCameraE]
    -- checkpoint camera states - when there are no active actions, such as mouse dragging
    oldStateD <- holdDyn (oldState icam) $ leftmost
        [ -- mouse wheel is an atomic event, so we update oldState directly
          flip wheelT <$> camB <@> select (A.animationEvents aHandler) AWheelEvent
          -- copy newState when some continuous action finishes
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PUpEvent)
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PClickEvent)
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PCancelEvent)
          -- should not be needed, but also should not harm?
        , newState <$> camB <@ select (A.animationEvents aHandler) (APointerEvent PDownEvent)
          -- reset camera
        , (\cp -> (oldState icam){ viewPoint = cp <+:> 0 } ) <$> defCenterPointB <@ resetCameraE
        ]
    -- updates on every action
    newStateD <- holdDyn (newState icam) $ leftmost
        [ -- every time oldState updates, copy its state
          updated oldStateD
          -- get pairs of screen points (old-new) on each mouseDrag event
        , flip ($) <$> camB <@> fmapMaybe id movs
        ]

    let camD = Camera <$> viewportSizeD
                      <*> projMatrixD
                      <*> oldStateD
                      <*> newStateD
        camB = current camD

    return camD
  where
    icam = initCamera (realToFrac . fst $ A.viewPortSizeI aHandler)
                      (realToFrac . snd $ A.viewPortSizeI aHandler)
                      defaultCState
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
          <@ select (A.animationEvents aHandler) (APointerEvent PMoveEvent)




