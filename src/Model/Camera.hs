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
--import Numeric.Dimensions
import qualified Numeric.Matrix              as Matrix
import qualified Numeric.Quaternion          as Q

----------------------------------------------------------------------------------------------
-- Definitions -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Object-Centered Camera
data Camera = Camera
    { viewportSize :: !(Float, Float)
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
    , projMatrix   = makeProjM (width,height)
    , oldState     = state
    , newState     = state
    }


makeProjM :: (Float, Float) -> Mat44f
makeProjM (width, height) = Matrix.perspective 0.1 2000 fovy ratio
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

----------------------------------------------------------------------------------------------
-- Camera movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Dragging - pan world on xz plane (e.g. using left mouse button)
dragHorizontal :: Vec2f -- ^ Old screen coordinates
               -> Vec2f -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> CState
dragHorizontal oldPoint newPoint camera@Camera {
        oldState   = ostate@CState {
            viewPoint = vp
        }
    } = ostate {
            viewPoint = vp + proj newPoint - proj oldPoint
        }
  where
    proj = screenToWorld cam' (vp ! 3)
    cam' = relativeToOldPoint camera

-- | Dragging - pan world on xy plane
dragVertical :: Vec2f -- ^ Old screen coordinates
             -> Vec2f -- ^ New screen coordinates
             -> Camera -- ^ Modify the camera state
             -> CState
dragVertical (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) Camera {
        viewportSize = (width, height),
        projMatrix = projmat,
        oldState   = ostate@CState {
            viewPoint = v,
            viewDist  = ρ
        }
    } = ostate {
            viewPoint = v + dv
        }
  where
    imat = inverse (projmat %* stateToView ostate) :: Mat44f
    sdx = scalar $ ρ * (x-ox) / width
    dz = ρ * (y-oy) / height
    (dx, dy, _) = unpackV3 $ fromScalar sdx *
                (unit . fromHom $ imat %* vec4 (-1) 0 0 0 )-- 0 0 1 1 - imat %* vec4 1 0 1 1)
    dv = vec3 dx dy dz


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

-- | Rotate, scale, and pan with two fingers
twoFingerControl :: (Vec2f, Vec2f) -- ^ Old screen coordinates
                 -> (Vec2f, Vec2f) -- ^ New screen coordinates
                 -> Camera -- ^ Modify the camera state
                 -> CState
twoFingerControl (unpackV2 -> (opx1,opy1),unpackV2 -> (opx2,opy2))
                 (unpackV2 -> (npx1,npy1),unpackV2 -> (npx2,npy2))
                 Camera {
                    viewportSize = (width, height),
                    projMatrix   = projmat,
                    oldState     = ostate@CState {
                        viewPoint  = ovp@((! 2) -> h),
                        viewAngles = (φ, theta),
                        viewDist   = ρ
                    }
    } = ostate {
            viewPoint  = nvp, -- ovp + dvp,
            viewAngles = (φ', theta),
            viewDist   = max 0.1 (ρ * dlen)
        }
  where
    imat = inverse $ projmat %* stateToView ostate
    screenO1 = vec4 (2 * opx1 / width - 1) (1 - 2 * opy1 / height) 1 1
    screenO2 = vec4 (2 * opx2 / width - 1) (1 - 2 * opy2 / height) 1 1
    screenN1 = vec4 (2 * npx1 / width - 1) (1 - 2 * npy1 / height) 1 1
    screenN2 = vec4 (2 * npx2 / width - 1) (1 - 2 * npy2 / height) 1 1
    up = vec3 0 0 1
    campos = fromHom $ imat %* vec4 0 0 0 1
    realO1 = findPos campos (fromHom (imat %* screenO1) - campos) h
    realO2 = findPos campos (fromHom (imat %* screenO2) - campos) h
    realN1 = findPos campos (fromHom (imat %* screenN1) - campos) h
    realN2 = findPos campos (fromHom (imat %* screenN2) - campos) h
    dOld = realO2 - realO1
    dNew = realN2 - realN1
    realN = 0.5 * (realN1 + realN2)
    realO = 0.5 * (realO1 + realO2)
    qs = Q.getRotScale dNew dOld
    -- scaling
    dlen = sqrt $ Q.square qs
--            olen = normL2 dOld
--            nlen = normL2 dNew
--            dlen = if abs (olen/nlen - 1) < 0.05
--                then 1
--                else let dl0 = olen/nlen
--                     in 1 + (dl0 - 1) * min 1 (50 / (1 + ρ)) -- prevent going too far away on large distances
    -- rotating
    dφ = let da = signum (unScalar . dot up $ Q.imVec qs) * Q.qArg qs -- atan2 (indexVector 2 $ cross dNew dOld) (dot dNew dOld)
         in if abs da < 0.05 then 0 else da
    φ' = mod' (φ+dφ+pi) (2*pi) - pi
    -- panning
    -- combine actions
    nvp = Q.rotScale qs (ovp - realN) + realO
--            nvp = rotScale (realToFrac dlen * axisRotation up (φ - φ')) (ovp-realN1)
--                  + realO1 -- + 2*newPoint



----------------------------------------------------------------------------------------------
-- Object movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------


-- | Dragging - pan object on xz plane (e.g. using left mouse button)
dragObject :: Vec2f -- ^ Old screen coordinates
           -> Vec2f -- ^ New screen coordinates
           -> Camera -- ^ Get matrices
           -> Mat44f  -- ^ transformation matrix
dragObject oldScreenPos newScreenPos camera
    = Matrix.translate3 $ proj newScreenPos - proj oldScreenPos
  where
    proj = screenToWorld cam' 0
    cam' = relativeToOldPoint camera

-- | Rotating - rotate object on w.r.t. y axis (e.g. using right mouse button)
rotateObject :: Vec2f -- ^ Old screen coordinates
             -> Vec2f -- ^ New screen coordinates
             -> Camera -- ^ Get matrices
             -> Vec3f -- ^ World position of rotation center
             -> Mat44f  -- ^ transformation matrix
rotateObject oldScreenPos newScreenPos camera p = Matrix.rotateZ a
  where
    proj = screenToWorld camera 0
    op = proj oldScreenPos
    np = proj newScreenPos

    -- rotation angle
    dv1 = unit $ np - p
    dv0 = unit $ op - p
    a = unScalar $ atan2 (2 !. cross dv0 dv1) (dot dv1 dv0)


-- | Rotate, scale, and pan with two fingers
twoFingerObject :: (Vec2f, Vec2f) -- ^ Old screen coordinates
                -> (Vec2f, Vec2f) -- ^ New screen coordinates
                -> Camera -- ^ Get matrices
                -> Mat44f -- ^ transformation matrix
twoFingerObject (oScreenPos1, oScreenPos2)
                (nScreenPos1, nScreenPos2)
                camera
    = Matrix.translate3 dv %* Q.toMatrix44 rotq
  where
    proj = screenToWorld camera 0
    oldPoint = (op1 + op2) / 2
    newPoint = (np1 + np2) / 2
    op1 = proj oScreenPos1
    op2 = proj oScreenPos2
    np1 = proj nScreenPos1
    np2 = proj nScreenPos2
    dv = newPoint - oldPoint
    rotq = signum $ Q.getRotScale (op2 - op1) (np2 - np1)


----------------------------------------------------------------------------------------------
-- Helpers  ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Transform a point in screen coordinates into a point on a ground in world coordinates.
--   z coordinate of the result is given as an argument to the function.
--
--   Use this function partially applied if you need a projection several times.
screenToWorld :: Camera -> Scf -> Vec2f -> Vec3f
screenToWorld camera z = imat `seq` campos `seq` width `seq` height `seq` f
  where
    imat = inverse (projMatrix camera %* viewMatrix camera)
    (width, height) = viewportSize camera
    campos = fromHom $ imat %* vec4 0 0 0 1
    f (unpackV2 -> (px,py)) = findPos campos (p - campos) z
        where
          p = fromHom $ imat %* vec4
                (2 * px / width - 1)
                (1 - 2 * py / height) (-1) 1



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

