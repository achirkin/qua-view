{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Model.Camera
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Model.Camera
    ( Camera (..)
    , viewMatrix
    , CState (..)
    , initCamera
    , scroll, dragHorizontal, dragVertical, rotateCentered, twoFingerControl
    , dragObject, rotateObject, twoFingerObject
    ) where

import Control.Monad (ap)
import GHCJS.WebGL
import Data.Fixed as DF

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Space.Quaternion


----------------------------------------------------------------------------------------------
-- Definitions -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Object-Centered Camera
data Camera = Camera
    { viewportSize :: !(Vector2 GLfloat)
    , projMatrix   :: !(Matrix4x4 GLfloat)
    , oldState     :: !CState
    , newState     :: !CState
    }


viewMatrix :: Camera -> Matrix4x4 GLfloat
viewMatrix = stateToView . newState


-- | State of the Camera
data CState = CState {
        viewPoint     :: !(Vector3 GLfloat),
        viewAngles    :: !(Vector2 GLfloat),
        viewDist      :: !GLfloat
    } deriving Show

-- | Create camera
initCamera :: GLfloat -- ^ width of the viewport
           -> GLfloat -- ^ height of the viewport
           -> CState -- ^ look position and direction
           -> Camera
initCamera width height state = Camera
    { viewportSize = Vector2 width height
    , projMatrix   = perspective 0.1 1000 fovy ratio
    , oldState     = state
    , newState     = state
    } where ratio = width / height
            fovy = (1*) . atan2 height . sqrt $ height*height + width*width



----------------------------------------------------------------------------------------------
-- Camera convertions ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Create view matrix out of camera state
stateToView :: CState -> Matrix4x4 GLfloat
stateToView CState {
        viewPoint  = v,
        viewAngles = Vector2 φ theta,
        viewDist   = ρ
    } = lookAtMatrix (Vector3 0 1 0) (v .+ dv) v
        where dv = Vector3 (t * cos φ) (ρ * sin theta) (t * sin φ)
              t = ρ * cos theta

----------------------------------------------------------------------------------------------
-- Camera movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Dragging - pan world on xz plane (e.g. using left mouse button)
dragHorizontal :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> Camera
dragHorizontal (Vector2 ox oy) (Vector2 x y) camera@Camera {
        viewportSize = Vector2 width height,
        projMatrix = projmat,
        oldState   = ostate@CState {
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
             -> Camera -- ^ Modify the camera state
             -> Camera
dragVertical (Vector2 ox oy) (Vector2 x y) camera@Camera {
        viewportSize = Vector2 width height,
        projMatrix = projmat,
        oldState   = ostate@CState {
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
            Vector3 dx _ dz = sdx ..*
                (unit . fromHom $ imat `prod` Vector4 0 0 1 1 .- imat `prod` Vector4 1 0 1 1)
            dv = Vector3 dx dy dz


-- | Rotating around viewPoint
rotateCentered :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> Camera
rotateCentered (Vector2 ox oy) (Vector2 x y) camera@Camera {
        viewportSize = Vector2 width height,
        oldState   = ostate@CState {
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
       -> Camera -- ^ Modify the camera state
       -> Camera
scroll s camera@Camera {
        newState = ostate@CState { viewDist = ρ }
    } = camera {
        newState = nstate,
        oldState = nstate
    } where nstate = ostate { viewDist = max 0.1 (ρ * (1 + min 9 (max (-0.9) s))) }



-- | Rotate, scale, and pan with two fingers
twoFingerControl :: (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                 -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                 -> Camera -- ^ Modify the camera state
                 -> Camera
twoFingerControl (Vector2 ox1 oy1, Vector2 ox2 oy2)
                 (Vector2 x1 y1, Vector2 x2 y2)
                 camera@Camera {
                    viewportSize = Vector2 width height,
                    projMatrix   = projmat,
                    oldState     = ostate@CState {
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
            dvp = rotScale (realToFrac dlen
                    * axisRotation (Vector3 0 1 0) (φ-φ')) vpdiff
                .- vpdiff .+ oldPoint .- newPoint



----------------------------------------------------------------------------------------------
-- Object movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------


-- | Dragging - pan object on xz plane (e.g. using left mouse button)
dragObject :: (SpaceTransform tname GLfloat)
           => Vector2 GLfloat -- ^ Old screen coordinates
           -> Vector2 GLfloat -- ^ New screen coordinates
           -> Camera -- ^ Get matrices
           -> STransform tname GLfloat a  -- ^ object to transform
           -> STransform tname GLfloat a
dragObject (Vector2 ox oy) (Vector2 x y) camera = ap $ translate dv id
    where imat = invert (projMatrix camera `prod` viewMatrix camera)
          Vector2 width height = viewportSize camera
          campos = fromHom $ imat `prod` Vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos .- campos) 0
          newPoint = findPos campos (newpos .- campos) 0
          dv = newPoint .- oldPoint


-- | Rotating - rotate object on w.r.t. y axis (e.g. using right mouse button)
rotateObject :: (SpaceTransform tname GLfloat)
             => Vector2 GLfloat -- ^ Old screen coordinates
             -> Vector2 GLfloat -- ^ New screen coordinates
             -> Camera -- ^ Get matrices
             -> STransform tname GLfloat a -- ^ object to transform
             -> STransform tname GLfloat a
rotateObject (Vector2 ox oy) (Vector2 x y) camera = f
    where imat = invert (projMatrix camera `prod` viewMatrix camera)
          Vector2 width height = viewportSize camera
          campos = fromHom $ imat `prod` Vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos .- campos) 0
          newPoint = findPos campos (newpos .- campos) 0
          f t = t >>= rotateY a
                where dv1 = unit $ newPoint .- p
                      dv0 = unit $ oldPoint .- p
                      p = transform $ wrap zeros t
                      Vector3 _ sina _ = cross dv0 dv1
                      a = atan2 sina (dv1 .*. dv0)


-- | Rotate, scale, and pan with two fingers
twoFingerObject :: (SpaceTransform tname GLfloat)
                => (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                -> Camera -- ^ Get matrices
                -> STransform tname GLfloat a -- ^ object to transform
                -> STransform tname GLfloat a
twoFingerObject (Vector2 ox1 oy1, Vector2 ox2 oy2)
                 (Vector2 x1 y1, Vector2 x2 y2)
                 camera = f
    where imat = invert (projMatrix camera `prod` viewMatrix camera)
          Vector2 width height = viewportSize camera
          ox = (ox1 + ox2) / 2
          oy = (oy1 + oy2) / 2
          x = (x1 + x2) / 2
          y = (y1 + y2) / 2
          -- rotating
          oangle = atan2 (ox1 - ox2) (oy1 - oy2)
          nangle = atan2 (x1 - x2) (y1 - y2)
          dφ = if abs (nangle-oangle) < 0.05 then 0 else nangle-oangle
          -- panning
          campos = fromHom $ imat `prod` Vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos .- campos) 0
          newPoint = findPos campos (newpos .- campos) 0
          dv = newPoint .- oldPoint
          f t = translate dv id <*> (t >>= rotateY dφ)

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
