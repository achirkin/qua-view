{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
module Model.Camera
    ( Camera (..)
    , viewMatrix
    , CState (..)
    , initCamera
    , scroll, dragHorizontal, dragVertical, rotateCentered, twoFingerControl
    , dragObject, rotateObject, twoFingerObject
    , makeProjM
    ) where

import           Data.Fixed                  (mod')

import           Numeric.DataFrame
import           Numeric.Dimensions
import qualified Numeric.Matrix              as Matrix
import qualified Numeric.Quaternion          as Q

----------------------------------------------------------------------------------------------
-- Definitions -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Object-Centered Camera
data Camera = Camera
    { viewportSize :: !(Float, Float)
    , clippingDist :: !(Float, Float)
    , projMatrix   :: !Mat44f
    , oldState     :: !CState
      -- ^ This state changes at the end of user action, e.g. PointerUp or MouseWheel.
      --   At such moments newState is copied to oldState
    , newState     :: !CState
      -- ^ This state changes all the time and represents current state of the camera.
      --   Even if a mouse in drag state, newState keeps being updated.
    } deriving Show


viewMatrix :: Camera -> Mat44f
viewMatrix = stateToView . newState


-- | State of the Camera
data CState = CState {
        viewPoint     :: !Vec3f,
        viewAngles    :: !(Float, Float),
        viewDist      :: !Float
    } deriving Show

-- | Create camera
initCamera :: Float -- ^ width of the viewport
           -> Float -- ^ height of the viewport
           -> CState -- ^ look position and direction
           -> Camera
initCamera width height state = Camera
    { viewportSize = (width,height)
    , clippingDist = clippingD
    , projMatrix   = makeProjM clippingD (width,height)
    , oldState     = state
    , newState     = state
    }
  where
    clippingD = (0.1, 2000)


makeProjM :: (Float, Float) -> (Float, Float) -> Mat44f
makeProjM (n, f) (width, height) = Matrix.perspective n f fovy ratio
  where
    ratio = width / height
    fovy = (1*) . atan2 height . sqrt $ height*height + width*width


relativeToOldPoint :: Camera -> Camera
relativeToOldPoint cam@Camera{oldState = os, newState = ns} = cam
    { oldState = os {viewPoint = 0}
    , newState = ns {viewPoint = viewPoint ns - viewPoint os}
    }


----------------------------------------------------------------------------------------------
-- Camera convertions ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Create view matrix out of camera state
stateToView :: CState -> Mat44f
stateToView CState {
        viewPoint  = v,
        viewAngles = (φ, theta),
        viewDist   = ρ
    } = Matrix.lookAt (vec3 0 0 1) (v + dv) v
  where
    dv = vec3 (t * cos φ) (t * sin φ)  (ρ * sin theta)
    t = ρ * cos theta

--eyePos :: CState -> Vec3f
--eyePos CState {
--        viewPoint  = v,
--        viewAngles = (φ, theta),
--        viewDist   = ρ
--    } = v + dv
--  where
--    dv = vec3 (t * cos φ) (t * sin φ)  (ρ * sin theta)
--    t = ρ * cos theta

-- | Camera position in NDC coordinates as per
--    https://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations#Viewport_Transformation
eyeNDC :: Camera -> Vec4f
eyeNDC Camera { clippingDist = (n,f)}
  = vec4 0 0 (- (f+n)/(f-n)) 1

-- | Assume the pointer is on the far clipping plane;
--   thus, z coordinate is 1.
--   The position of a pointer is given in screen coorinates (pixels)
ptrNDC :: Camera -> Vec2f -> Vec4f
ptrNDC Camera { viewportSize = (width, height) } p
  | (px, py) <- unpackV2 p
  = vec4 (2 * px / width - 1)
         (1 - 2 * py / height) 1 1

----------------------------------------------------------------------------------------------
-- Camera movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Dragging - pan world on xy plane (e.g. using left mouse button)
dragHorizontal :: Vec2f -- ^ Old screen coordinates
               -> Vec2f -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> CState
dragHorizontal oldPoint newPoint camera@Camera {
        oldState   = ostate@CState {
            viewPoint = vp
        }
    } = ostate {
            viewPoint = vp - proj newPoint + proj oldPoint
        }
  where
    proj = screenToWorld cam' (vp ! 3)
    cam' = relativeToOldPoint camera

-- | Dragging - pan world on a plane parallel to screen
dragVertical :: Vec2f -- ^ Old screen coordinates
             -> Vec2f -- ^ New screen coordinates
             -> Camera -- ^ Modify the camera state
             -> CState
dragVertical op np cam@Camera {
        projMatrix = projmat,
        oldState   = ostate@CState {
            viewPoint = v,
            viewDist  = ρ
        }
    } = ostate {
            viewPoint = v + dv
        }
  where
    imat = inverse (projmat %* stateToView ostate)
    ndcOp = ptrNDC cam op
    ndcNp = ptrNDC cam np
    worldOfar = fromHom $ imat %* ndcOp
    worldNfar = fromHom $ imat %* ndcNp
    c = fromScalar $ scalar ρ / (3:!Z !. worldOfar)
    dv = (worldNfar - worldOfar) * c


-- | Rotating around viewPoint
rotateCentered :: Vec2f -- ^ Old screen coordinates
               -> Vec2f -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> CState
rotateCentered (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) Camera {
        viewportSize = (width, height),
        oldState   = ostate@CState {
            viewAngles = (φ, theta)
        }
    } = ostate {
            viewAngles = (φ', theta')
        }
  where
    dφ = 2*pi*(ox-x) / width
    dtheta = pi*(y-oy) / height
    φ' = mod' (φ+dφ+pi) (2*pi) - pi
    theta' = max (-0.35*pi) . min (0.45*pi) $ theta + dtheta

-- | Scroll camera in and out
scroll :: Float -- ^ Scrolling amout in fractions (i.e. `dist := dist*(1+x)`)
       -> Camera -- ^ Modify the camera state
       -> CState
scroll s Camera {
        oldState = ostate@CState { viewDist = ρ }
    } = ostate { viewDist = max 0.1 (ρ * (1 + min (8 / (1 + ρ)) (max (max (-0.8) (- 8 / (1 + ρ))) s))) }

-- | Rotate, scale, and pan camera with two fingers
twoFingerControl :: (Vec2f, Vec2f) -- ^ Old screen coordinates
                 -> (Vec2f, Vec2f) -- ^ New screen coordinates
                 -> Camera -- ^ Modify the camera state
                 -> CState
twoFingerControl (op1,op2)
                 (np1,np2)
                 cam@Camera {
                    oldState     = ostate@CState {
                        viewPoint  = ovp@((! 3) -> h),
                        viewAngles = (φ, theta),
                        viewDist   = ρ
                    }
    } = ostate {
            viewPoint  = nvp,
            viewAngles = (φ', theta),
            viewDist   = max 0.1 (ρ * dlen)
        }
  where
    proj = screenToWorld cam h
    up = vec3 0 0 1
    worldO1 = proj op1
    worldO2 = proj op2
    worldN1 = proj np1
    worldN2 = proj np2
    dOld = worldO2 - worldO1
    dNew = worldN2 - worldN1
    worldN = 0.5 * (worldN1 + worldN2)
    worldO = 0.5 * (worldO1 + worldO2)
    qs = Q.getRotScale dNew dOld
    -- scaling
    dlen = sqrt $ Q.square qs
    -- rotating
    dφ = let da = signum (unScalar . dot up $ Q.imVec qs) * Q.qArg qs
         in if abs da < 0.05 then 0 else da
    φ' = mod' (φ+dφ+pi) (2*pi) - pi
    -- panning
    -- combine actions
    nvp = Q.rotScale qs (ovp - worldN) + worldO



----------------------------------------------------------------------------------------------
-- Object movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------


-- | Dragging - pan object on xy plane (e.g. using left mouse button)
dragObject :: Vec2f -- ^ Old screen coordinates
           -> Vec2f -- ^ New screen coordinates
           -> Camera -- ^ Get matrices
           -> Mat44f  -- ^ transformation matrix
dragObject oldScreenPos newScreenPos camera
    = Matrix.translate3 $ proj newScreenPos - proj oldScreenPos
  where
    proj = screenToWorld cam' 0
    cam' = relativeToOldPoint camera

-- | Rotating - rotate object on w.r.t. z axis (e.g. using right mouse button)
rotateObject :: Vec2f -- ^ Old screen coordinates
             -> Vec2f -- ^ New screen coordinates
             -> Camera -- ^ Get matrices
             -> Vec3f -- ^ World position of rotation center
             -> Mat44f  -- ^ transformation matrix
rotateObject oldScreenPos newScreenPos camera p = trans %* Matrix.rotateZ a %* trans'
  where
    trans = Matrix.translate3 p
    trans' = Matrix.translate3 ( - p )
    proj = screenToWorld camera 0
    op = proj oldScreenPos
    np = proj newScreenPos

    -- rotation angle
    dv1 = unit . update (3:!Z) (0 :: Scf) $ np - p
    dv0 = unit . update (3:!Z) (0 :: Scf) $ op - p
    a = unScalar $ atan2 (3 !. cross dv0 dv1) (dot dv0 dv1)


-- | Rotate and pan with two fingers
twoFingerObject :: (Vec2f, Vec2f) -- ^ Old screen coordinates
                -> (Vec2f, Vec2f) -- ^ New screen coordinates
                -> Camera -- ^ Get matrices
                -> Mat44f -- ^ transformation matrix
twoFingerObject (oScreenPos1, oScreenPos2)
                (nScreenPos1, nScreenPos2)
                camera
    = Matrix.translate3 newPoint %* Q.toMatrix44 rotq %* Matrix.translate3 (-oldPoint)
  where
    proj = screenToWorld camera 0
    oldPoint = (op1 + op2) / 2
    newPoint = (np1 + np2) / 2
    op1 = proj oScreenPos1
    op2 = proj oScreenPos2
    np1 = proj nScreenPos1
    np2 = proj nScreenPos2
    rotq = signum $ Q.getRotScale (op2 - op1) (np2 - np1)


----------------------------------------------------------------------------------------------
-- Helpers  ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------



-- | Transform a point in screen coordinates into a point on a ground in world coordinates.
--   z coordinate of the result is given as an argument to the function.
--
--   Use this function partially applied if you need a projection several times.
--
--   Note:
--
--   The formulae are taken from
--     https://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations#Viewport_Transformation
--
--   Visible objects in NDC coordinates are always in MinMax (vec4 (-1) (-1) (-1) 1) (vec4 1 1 1 1).
--   It is important that z coordinates varies from -1 (near clipping plane) to 1 (far clipping plane).
--
--   Camera (eye) position in NDC is slightly behind the near clipping plane @vec4 0 0 (- (f+n)/(f-n)) 1@.
screenToWorld :: Camera -> Scf -> Vec2f -> Vec3f
screenToWorld camera z = imat `seq` campos `seq` f
  where
    imat = inverse $ projMatrix camera %* stateToView (oldState camera)
    campos = fromHom $ imat %* eyeNDC camera
    f p = findPos campos (fromHom (imat %* ptrNDC camera p) - campos) z



-- | find position of the intersection of a ray traced from camera point to ground
findPos :: Vec3f -- ^ camera position
        -> Vec3f -- ^ camera sight vector
        -> Scf -- ^ height level of the point
        -> Vec3f -- ^ position of the point in 3D
findPos (unpackV3 -> (c1, c2, c3)) (unpackV3 -> (v1, v2, v3)) h = vec3 x y (unScalar h)
    where l = (unScalar h - c3)/v3'
          x = c1 + v1*l
          y = c2 + v2*l
          v3' = if abs v3 < 0.0000000001 then signum v3 * 0.0000000001 else v3

unit :: Vec3f -> Vec3f
unit v | n == 0 = 0
       | otherwise = ewmap (/n) v
    where
     n = normL2 v

