-----------------------------------------------------------------------------
--
-- Module      :  Drawable.OCCamera
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Drawable.OCCamera where


import GHCJS.WebGL

import Data.Fixed as DF

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Space.Quaternion

import Drawable.World



-- | Object-Centered Camera
data OCCamera = OCCamera
    { viewportSize :: Vector2 GLfloat
    , projMatrix   :: Matrix4x4 GLfloat
    , oldState     :: OCCState
    , newState     :: OCCState
    }

instance Camera OCCamera where
    prepareView = stateToView . newState
    prepareProjection = projMatrix
    viewSize = fmap round . viewportSize

-- | State of the Camera
data OCCState = OCCState {
        viewPoint     :: Vector3 GLfloat,
        viewAngles    :: Vector2 GLfloat,
        viewDist      :: GLfloat
    }


----------------------------------------------------------------------------------------------
-- Camera convertions ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------


-- | Create camera
initOCCamera :: GLfloat -- ^ width of the viewport
             -> GLfloat -- ^ height of the viewport
             -> OCCState -- ^ look position and direction
             -> OCCamera
initOCCamera width height state = OCCamera
    { viewportSize = Vector2 width height
    , projMatrix   = perspective 0.1 1000 fovy ratio
    , oldState     = state
    , newState     = state
    } where ratio = width / height
            fovy = (1*) . atan2 height . sqrt $ height*height + width*width

-- | Create view matrix out of camera state
stateToView :: OCCState -> Matrix4x4 GLfloat
stateToView OCCState {
        viewPoint  = v,
        viewAngles = Vector2 φ theta,
        viewDist   = ρ
    } = lookAtMatrix (Vector3 0 1 0) (v .+ dv) v
        where dv = Vector3 (t * cos φ) (ρ * sin theta) (t * sin φ) 
              t = ρ * cos theta

----------------------------------------------------------------------------------------------
-- Camera rotation functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Dragging - pan world on xz plane (e.g. using left mouse button)
dragHorizontal :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> OCCamera -- ^ Modify the camera state
               -> OCCamera
dragHorizontal (Vector2 ox oy) (Vector2 x y) camera@OCCamera {
        viewportSize = Vector2 width height,
        projMatrix = projmat,
        oldState   = ostate@OCCState {
            viewPoint = v@(Vector3 _ py _)
        }
    } = camera {
        newState = ostate {
            viewPoint = v .+ dv
        }
    } where imat = invert (projmat `prod` stateToView ostate)
            campos = fromHom $ imat `prod` Vector4 0 0 0 1
            oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
            newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
            oldPoint = findPos campos (oldpos .- campos) py
            newPoint = findPos campos (newpos .- campos) py
            dv = oldPoint .- newPoint

-- | Dragging - pan world on xy plane
dragVertical :: Vector2 GLfloat -- ^ Old screen coordinates
             -> Vector2 GLfloat -- ^ New screen coordinates
             -> OCCamera -- ^ Modify the camera state
             -> OCCamera
dragVertical (Vector2 ox oy) (Vector2 x y) camera@OCCamera {
        viewportSize = Vector2 width height,
        projMatrix = projmat,
        oldState   = ostate@OCCState {
            viewPoint = v,
            viewDist  = ρ
        }
    } = camera {
        newState = ostate {
            viewPoint = v .+ dv
        }
    } where imat = invert (projmat `prod` stateToView ostate)
            sdx = ρ * (x-ox) / width
            dy = ρ * (y-oy) / height
            Vector3 dx _ dz = sdx ..* (unit . fromHom $ imat `prod` Vector4 0 0 1 1 .- imat `prod` Vector4 1 0 1 1)
            dv = Vector3 dx dy dz


-- | Rotating around viewPoint
rotateCentered :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> OCCamera -- ^ Modify the camera state
               -> OCCamera
rotateCentered (Vector2 ox oy) (Vector2 x y) camera@OCCamera {
        viewportSize = Vector2 width height,
        oldState   = ostate@OCCState {
            viewAngles = Vector2 φ theta
        }
    } = camera {
        newState = ostate {
            viewAngles = Vector2 φ' theta'
        }
    } where dφ = 2*pi*(x-ox) / width
            dtheta = pi*(y-oy) / height
            φ' = DF.mod' (φ+dφ+pi) (2*pi) - pi
            theta' = max (-0.35*pi) . min (0.45*pi) $ theta + dtheta

-- | Scroll camera in and out
scroll :: GLfloat -- ^ Scrolling amout in fractions (i.e. `dist := dist*(1+x)`)
       -> OCCamera -- ^ Modify the camera state
       -> OCCamera
scroll s camera@OCCamera {
        newState = ostate@OCCState { viewDist = ρ }
    } = camera {
        newState = nstate,
        oldState = nstate
    } where nstate = ostate { viewDist = max 0.1 (ρ * (1 + min 9 (max (-0.9) s))) }



-- | Rotate, scale, and pan with two fingers
twoFingerControl :: (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                 -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                 -> OCCamera -- ^ Modify the camera state
                 -> OCCamera
twoFingerControl (Vector2 ox1 oy1, Vector2 ox2 oy2)
                 (Vector2 x1 y1, Vector2 x2 y2)
                 camera@OCCamera {
                    viewportSize = Vector2 width height,
                    projMatrix   = projmat,
                    oldState     = ostate@OCCState {
                        viewPoint  = ovp@(Vector3 _ h _),
                        viewAngles = Vector2 φ theta,
                        viewDist   = ρ
                    }
    } = camera {
        newState = ostate {
            viewPoint  = ovp .+ dvp,
            viewAngles = Vector2 φ' theta,
            viewDist   = max 0.1 (ρ*dlen)
        }
    } where imat = invert (projmat `prod` stateToView ostate)
            ox = (ox1 + ox2) / 2
            oy = (oy1 + oy2) / 2
            x = (x1 + x2) / 2
            y = (y1 + y2) / 2
            -- scaling
            olen = sqrt $ (ox1-ox2)^(2:: Int) + (oy1-oy2)^(2:: Int)
            len = sqrt $ (x1-x2)^(2:: Int) + (y1-y2)^(2:: Int)
            dlen = if abs (olen/len - 1) < 0.05 then 1 else olen/len
            -- rotating
            oangle = atan2 (ox1 - ox2) (oy1 - oy2)
            nangle = atan2 (x1 - x2) (y1 - y2)
            dφ = if abs (nangle-oangle) < 0.05 then 0 else nangle-oangle
            φ' = DF.mod' (φ+dφ+pi) (2*pi) - pi
            -- panning
            campos = fromHom $ imat `prod` Vector4 0 0 0 1
            oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
            newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
            oldPoint = findPos campos (oldpos .- campos) h
            newPoint = findPos campos (newpos .- campos) h
            vpdiff = ovp .- newPoint
            dvp = rotScale (realToFrac dlen * axisRotation (Vector3 0 1 0) (φ-φ')) vpdiff
                .- vpdiff .+ oldPoint .- newPoint


----------------------------------------------------------------------------------------------
-- Helpers  ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | find position of the intersection of a ray traced from camera point to ground
findPos :: Vector3 GLfloat -- ^ camera position
        -> Vector3 GLfloat -- ^ camera sight vector
        -> GLfloat -- ^ height level of the point
        -> Vector3 GLfloat -- ^ position of the point in 3D
findPos (Vector3 c1 c2 c3) (Vector3 v1 v2 v3) h = Vector3 x h z
    where l = (h - c2)/v2
          x = c1 + v1*l
          z = c3 + v3*l
