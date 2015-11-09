{-# LANGUAGE FlexibleContexts, ViewPatterns, DataKinds #-}
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

import Data.Geometry
import Data.Geometry.Transform
--import Geometry.Space.Transform
--import Geometry.Space.Quaternion

import Debug.Trace

----------------------------------------------------------------------------------------------
-- Definitions -------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Object-Centered Camera
data Camera = Camera
    { viewportSize :: !(GLfloat, GLfloat)
    , projMatrix   :: !(Matrix4 GLfloat)
    , oldState     :: !CState
    , newState     :: !CState
    }


viewMatrix :: Camera -> Matrix4 GLfloat
viewMatrix = stateToView . newState


-- | State of the Camera
data CState = CState {
        viewPoint     :: !(Vector3 GLfloat),
        viewAngles    :: !(GLfloat, GLfloat),
        viewDist      :: !GLfloat
    } deriving Show

-- | Create camera
initCamera :: GLfloat -- ^ width of the viewport
           -> GLfloat -- ^ height of the viewport
           -> CState -- ^ look position and direction
           -> Camera
initCamera width height state = Camera
    { viewportSize = (width,height)
    , projMatrix   = perspectiveM 0.1 1000 fovy ratio
    , oldState     = state
    , newState     = state
    } where ratio = width / height
            fovy = (1*) . atan2 height . sqrt $ height*height + width*width



----------------------------------------------------------------------------------------------
-- Camera convertions ------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Create view matrix out of camera state
stateToView :: CState -> Matrix4 GLfloat
stateToView CState {
        viewPoint  = v,
        viewAngles = (φ, theta),
        viewDist   = ρ
    } = lookAtMatrix (vector3 0 0 1) (v + dv) v
        where dv = vector3 (t * cos φ) (t * sin φ)  (ρ * sin theta)
              t = ρ * cos theta

----------------------------------------------------------------------------------------------
-- Camera movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Dragging - pan world on xz plane (e.g. using left mouse button)
dragHorizontal :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> Camera
dragHorizontal (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) camera@Camera {
        viewportSize = (width, height),
        projMatrix = projmat,
        oldState   = ostate@CState {
            viewPoint = v@(unpackV3 -> (_, _, pz))
        }
    } = camera {
        newState = ostate {
            viewPoint = v + dv
        }
    } where imat = inverse (projmat `prod` stateToView ostate)
            campos = fromHom $ imat `prod` vector4 0 0 0 1
            oldpos = fromHom $ imat `prod` vector4
                (2 * ox / width - 1)
                (1 - 2 * oy / height) (-1) 1
            newpos = fromHom $ imat `prod` vector4
                (2 * x / width - 1)
                (1 - 2 * y / height) (-1) 1
            oldPoint = findPos campos (oldpos - campos) pz
            newPoint = findPos campos (newpos - campos) pz
            dv =  newPoint - oldPoint

-- | Dragging - pan world on xy plane
dragVertical :: Vector2 GLfloat -- ^ Old screen coordinates
             -> Vector2 GLfloat -- ^ New screen coordinates
             -> Camera -- ^ Modify the camera state
             -> Camera
dragVertical (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) camera@Camera {
        viewportSize = (width, height),
        projMatrix = projmat,
        oldState   = ostate@CState {
            viewPoint = v,
            viewDist  = ρ
        }
    } = camera {
        newState = ostate {
            viewPoint = v + dv
        }
    } where imat = inverse (projmat `prod` stateToView ostate) :: Matrix4 GLfloat
            sdx = ρ * (x-ox) / width
            dz = ρ * (y-oy) / height
            (dx, dy, _) = unpackV3 $ broadcastVector sdx *
                (unit . resizeVector $ imat `prod` vector4 (-1) 0 0 0 )-- 0 0 1 1 - imat `prod` vector4 1 0 1 1)
            dv = vector3 dx dy dz


-- | Rotating around viewPoint
rotateCentered :: Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> Camera -- ^ Modify the camera state
               -> Camera
rotateCentered (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) camera@Camera {
        viewportSize = (width, height),
        oldState   = ostate@CState {
            viewAngles = (φ, theta)
        }
    } = camera {
        newState = ostate {
            viewAngles = (φ', theta')
        }
    } where dφ = 2*pi*(ox-x) / width
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
    } where nstate = ostate { viewDist = max 0.1 (ρ * (1 + min (9 / (1 + ρ)) (max (max (-0.9) (- 9 / (1 + ρ))) s))) }



-- | Rotate, scale, and pan with two fingers
twoFingerControl :: (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                 -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                 -> Camera -- ^ Modify the camera state
                 -> Camera
twoFingerControl (unpackV2 -> (ox1,oy1), unpackV2 -> (ox2,oy2))
                 (unpackV2 -> (x1,y1)  , unpackV2 -> (x2,y2)  )
                 camera@Camera {
                    viewportSize = (width, height),
                    projMatrix   = projmat,
                    oldState     = ostate@CState {
                        viewPoint  = ovp@(indexVector 2 -> h),
                        viewAngles = (φ, theta),
                        viewDist   = ρ
                    }
    } = camera {
        newState = ostate {
            viewPoint  = ovp + dvp,
            viewAngles = (φ', theta),
            viewDist   = max 0.1 (ρ*dlen)
        }
    } where imat = inverse (projmat `prod` stateToView ostate)
            ox = (ox1 + ox2) / 2
            oy = (oy1 + oy2) / 2
            x = (x1 + x2) / 2
            y = (y1 + y2) / 2
            -- scaling
            olen = sqrt $ (ox1-ox2)^(2:: Int) + (oy1-oy2)^(2:: Int)
            len = sqrt $ (x1-x2)^(2:: Int) + (y1-y2)^(2:: Int)
            dlen = if abs (olen/len - 1) < 0.05
                then 1
                else let dl0 = olen/len
                     in 1 + (dl0 - 1) * min 1 (50 / (1 + ρ)) -- prevent going too far away on large distances
            -- rotating
            oangle = atan2 (ox1 - ox2) (oy1 - oy2)
            nangle = atan2 (x1 - x2) (y1 - y2)
            dφ = if abs (oangle-nangle) < 0.05 then 0 else oangle-nangle
            φ' = DF.mod' (φ+dφ+pi) (2*pi) - pi
            -- panning
            campos = fromHom $ imat `prod` vector4 0 0 0 1
            oldpos = fromHom $ imat `prod` vector4
                (2 * ox / width - 1)
                (1 - 2 * oy / height) (-1) 1
            newpos = fromHom $ imat `prod` vector4
                (2 * x / width - 1)
                (1 - 2 * y / height) (-1) 1
            oldPoint = findPos campos (oldpos - campos) h
            newPoint = findPos campos (newpos - campos) h
            -- combine actions
            vpdiff = newPoint - ovp
            dvp = rotScale (realToFrac dlen
                    * axisRotation (vector3 0 0 1) (φ'-φ)) vpdiff
                 - vpdiff - oldPoint + newPoint
--            vpdiff = ovp - newPoint
--            dvp = rotScale (realToFrac dlen
--                    * axisRotation (vector3 0 0 1) (φ'-φ)) vpdiff
--                + vpdiff + newPoint - oldPoint



----------------------------------------------------------------------------------------------
-- Object movement functions -----------------------------------------------------------------
----------------------------------------------------------------------------------------------


-- | Dragging - pan object on xz plane (e.g. using left mouse button)
dragObject :: (SpaceTransform s 3 GLfloat)
           => Vector2 GLfloat -- ^ Old screen coordinates
           -> Vector2 GLfloat -- ^ New screen coordinates
           -> Camera -- ^ Get matrices
           -> s a  -- ^ object to transform
           -> s a
dragObject (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) camera = ap $ translate dv id
    where imat = inverse (projMatrix camera `prod` viewMatrix camera)
          (width, height) = viewportSize camera
          campos = fromHom $ imat `prod` vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos - campos) 0
          newPoint = findPos campos (newpos - campos) 0
          dv = newPoint - oldPoint


-- | Rotating - rotate object on w.r.t. y axis (e.g. using right mouse button)
rotateObject :: ( SpaceTransform s 3 GLfloat
                , Space3DTransform s GLfloat QFloat)
             => Vector2 GLfloat -- ^ Old screen coordinates
             -> Vector2 GLfloat -- ^ New screen coordinates
             -> Camera -- ^ Get matrices
             -> s a -- ^ object to transform
             -> s a
rotateObject (unpackV2 -> (ox,oy) ) (unpackV2 -> (x,y)) camera = f
    where imat = inverse (projMatrix camera `prod` viewMatrix camera)
          (width, height) = viewportSize camera
          campos = fromHom $ imat `prod` vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos - campos) 0
          newPoint = findPos campos (newpos - campos) 0
          f t = t >>= rotateY a
                where dv1 = unit $ newPoint - p
                      dv0 = unit $ oldPoint - p
                      p = transform $ wrap 0 t
                      sina = indexVector 1 $ cross dv0 dv1
                      a = atan2 sina (dot dv1 dv0)


-- | Rotate, scale, and pan with two fingers
twoFingerObject :: ( SpaceTransform s 3 GLfloat
                   , Space3DTransform s GLfloat QFloat)
                => (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                -> Camera -- ^ Get matrices
                -> s a -- ^ object to transform
                -> s a
twoFingerObject (unpackV2 -> (ox1,oy1), unpackV2 -> (ox2,oy2))
                (unpackV2 -> (x1,y1)  , unpackV2 -> (x2,y2)  )
                 camera = f
    where imat = inverse (projMatrix camera `prod` viewMatrix camera)
          (width, height) = viewportSize camera
          ox = (ox1 + ox2) / 2
          oy = (oy1 + oy2) / 2
          x = (x1 + x2) / 2
          y = (y1 + y2) / 2
          -- rotating
          oangle = atan2 (ox1 - ox2) (oy1 - oy2)
          nangle = atan2 (x1 - x2) (y1 - y2)
          dφ = if abs (nangle-oangle) < 0.05 then 0 else nangle-oangle
          -- panning
          campos = fromHom $ imat `prod` vector4 0 0 0 1
          oldpos = fromHom $ imat `prod` vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
          newpos = fromHom $ imat `prod` vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
          oldPoint = findPos campos (oldpos - campos) 0
          newPoint = findPos campos (newpos - campos) 0
          dv = newPoint - oldPoint
          f t = translate dv id <*> (t >>= rotateY dφ)

----------------------------------------------------------------------------------------------
-- Helpers  ----------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | find position of the intersection of a ray traced from camera point to ground
findPos :: Vector3 GLfloat -- ^ camera position
        -> Vector3 GLfloat -- ^ camera sight vector
        -> GLfloat -- ^ height level of the point
        -> Vector3 GLfloat -- ^ position of the point in 3D
findPos (unpackV3 -> (c1, c2, c3)) (unpackV3 -> (v1, v2, v3)) h = vector3 x y h
    where l = (h - c3)/v3'
          x = c1 + v1*l
          y = c2 + v2*l
          v3' = if abs v3 < 0.01 then signum v3 * 0.01 else v3
