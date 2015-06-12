{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Model.Building
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

module Model.City where

import Control.Applicative
import qualified Control.Monad as M
import qualified Data.IntMap.Strict as IM
--import qualified Data.Graph as G
--import qualified Data.Foldable as F
import Data.Bits
import Data.Monoid

import GHCJS.Foreign (toJSString)
import GHCJS.WebGL

import Geometry.Space
--import Geometry.Space.Quaternion
--import Geometry.Structure
import Geometry.Space.Transform

import Drawable.World

import Data.IORef

import SmallGL.Shader
--import SmallGL.Helpers

import Model.CityObject


-- | Map of all city object (buildings, roads, etc).
--   Values: object, location (transformed mesh)
data City = City
    { activeObj    :: !(Int, Int)
    , buildShader  :: !ShaderProgram
    , selectShader :: !ShaderProgram
    , objectsIn    :: !(IM.IntMap CityObjRep)
    }

data CityObjRep = CityObjRep
    { orObj  :: !CityObject
    , orMesh :: !CityObjectMesh
    , orLocs :: !(IM.IntMap (QTransform GLfloat (Maybe Texture)))
    }

-- | Add a city object on a pointed place in the city.
--   Returns a modified city (a city wih modified map of buildings)
addCityObject :: World
              -> CityObject
              -> Vector3 GLfloat -- ^ position
              -> GLfloat -- ^ rotation (w.r.t. Y axis)
              -> City
              -> IO (City)
addCityObject w o p r c@City{objectsIn = m} =
    createObjectMesh w o >>= \om ->
    return $ c{objectsIn = IM.insert i CityObjRep
            { orObj = o
            , orMesh = om
            , orLocs = IM.fromAscList [(1,translate p Nothing >>= rotateY r)]
            } m }
    where i = fst (IM.findMax m) + 1

-- | The same as `addCityObject`, but modifies a city inplace by its reference
addCityObject' :: World
               -> CityObject
               -> Vector3 GLfloat -- ^ position
               -> GLfloat -- ^ rotation (w.r.t. Y axis)
               -> IORef City
               -> IO ()
addCityObject' w o p r cref = do
    om <- createObjectMesh w o
    modifyIORef' cref $ \c@City{objectsIn = m} ->
        c{objectsIn = IM.insert (fst (IM.findMax m) + 1) CityObjRep
            { orObj = o
            , orMesh = om
            , orLocs = IM.fromAscList [(1,translate p Nothing >>= rotateY r)]
            } m}

--addObjectTexture :: World
--                 -> Int
--                 -> Int
--

-- | Helper for creation of the city from the list of city objects
buildCity :: World
          -> [CityObject]
          -> [[Vector3 GLfloat]] -- ^ positions
          -> [[GLfloat]] -- ^ rotations (w.r.t. Y axis)
          -> [[Maybe Texture]]
          -> IO (City)
buildCity w@World{glctx = gl} bs ps rs texs = do
    enableVertexAttribArray gl 0
    enableVertexAttribArray gl 1
    enableVertexAttribArray gl 2
    buProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragBuilding)
                                ,(gl_VERTEX_SHADER, vertBuilding)]
    useProgram gl (programId buProgram)
    bindAttribLocation gl (programId buProgram) 0 (toJSString "aVertexPosition")
    bindAttribLocation gl (programId buProgram) 1 (toJSString "aVertexNormal")
    bindAttribLocation gl (programId buProgram) 2 (toJSString "aTextureCoord")

    seProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragSelector)
                                ,(gl_VERTEX_SHADER, vertSelector)]
    useProgram gl (programId seProgram)
    bindAttribLocation gl (programId seProgram) 0 (toJSString "aVertexPosition")

    createdObjects <- mapM (\(olocs, oobj, omaction) -> do
        omesh <- omaction
        return $ CityObjRep oobj omesh olocs) . zip3 locs bs $ map (createObjectMesh w) bs :: IO [CityObjRep]

    disableVertexAttribArray gl 0
    disableVertexAttribArray gl 1
    disableVertexAttribArray gl 2
    return $ City (0,0) buProgram seProgram . IM.fromAscList . zip [1..] $ createdObjects
    where trans p r t = translate p t >>= rotateY r
          locs = zipWith3 (\pp rr -> IM.fromAscList . zip [1..] . zipWith3 trans pp rr) ps rs texs



----------------------------------------------------------------------------------------------------
-- Embedding city into the program engine
----------------------------------------------------------------------------------------------------

-- Drawing a city means drawing all its objects
instance Drawable City where
    draw w@World{glctx = gl} (City (j,k) bProg _ buildings) = do
        enableVertexAttribArray gl 0
        enableVertexAttribArray gl 1
        enableVertexAttribArray gl 2
        useProgram gl . programId $ bProg
        activeTexture gl gl_TEXTURE0
        uniform1i gl (unifLoc bProg "uSampler") 0
        uniformMatrix4fv gl (unifLoc bProg "uProjM") False (projectLoc w)
        uniform3f gl (unifLoc bProg "uSunDir") sx sy sz
        uniform4f gl colLoc 0.7 0.7 0.7 1
        IM.foldMapWithKey g buildings
        uniform4f gl colLoc 0.75 0.75 0.7 1
        IM.foldMapWithKey f buildings
        disableVertexAttribArray gl 0
        disableVertexAttribArray gl 1
        disableVertexAttribArray gl 2
        where f i (CityObjRep o m locs) | behavior o == Static = return ()
                                        | i /= j = IM.foldMapWithKey
                    (\_ t -> applyTransform' w viewLoc t >>= maybeWithTexture m 0.6) locs
                                        | otherwise = IM.foldMapWithKey
                    (\l t -> if l /= k then applyTransform' w viewLoc t >>= maybeWithTexture m 0.6
                                       else do
                            uniform4f gl colLoc 1 0.65 0.65 1
                            applyTransform' w viewLoc t >>= maybeWithTexture m 0.2
                            uniform4f gl colLoc 0.75 0.75 0.7 1) locs
              g _ (CityObjRep o m locs) | behavior o == Dynamic = return ()
                                        | otherwise = IM.foldMapWithKey
                    (\_ t -> applyTransform' w viewLoc t >>= maybeWithTexture m 0.6) locs
              viewLoc = unifLoc bProg "uModelViewM"
              colLoc = unifLoc bProg "uVertexColor"
              userLoc = unifLoc bProg "uTexUser"
              Vector4 sx sy sz _ = currentView w `prod` Vector4 (-0.5) (-1) 0.6 0
              maybeWithTexture m _ Nothing   = drawSurface gl m
              maybeWithTexture m l (Just tex) = do
                uniform1f gl userLoc l
                enable gl gl_BLEND
                depthMask gl False
                --disable gl gl_DEPTH_TEST
                bindTexture gl gl_TEXTURE_2D tex
                drawSurface gl m
                uniform1f gl userLoc 0
                disable gl gl_BLEND
                depthMask gl True
                --enable gl gl_DEPTH_TEST


-- | Apply current transform of an object (including perspective) and save shader uniforms
applyTransform' :: (SpaceTransform s GLfloat)
               => World -> UniformLocation -> s a -> IO a
applyTransform' w@(World{glctx = gl}) loc tr = do
        let MTransform matrix x = transform (MTransform (currentView w) id) tr
        fillTypedArray (modelViewLoc w) matrix
        uniformMatrix4fv gl loc False (modelViewLoc w)
        return x

-- City selectable means one can select objects in a city
instance Selectable City where
    selectArea w@World{glctx = gl} (City _ _ sProg buildings) = do
        enableVertexAttribArray gl 0
        useProgram gl . programId $ sProg
        uniformMatrix4fv gl (unifLoc sProg "uProjM") False (projectLoc w)
        IM.foldMapWithKey f buildings
        disableVertexAttribArray gl 0
        where f i (CityObjRep o m locs) = M.when (behavior o == Dynamic) $
                IM.foldMapWithKey (f' m i) locs
              f' m i j loc = do
                uniform4f gl selValLoc
                            (fromIntegral (i .&. 0xFF) / 255)
                            ((fromIntegral (shift i (-8) .&. 0x000000FF))/255)
                            (fromIntegral (j .&. 0xFF) / 255)
                            ((fromIntegral (shift j (-8) .&. 0x000000FF))/255)
                applyTransform' w viewLoc loc >> drawSurface gl m
              viewLoc = unifLoc sProg "uModelViewM"
              selValLoc = unifLoc sProg "uSelector"


----------------------------------------------------------------------------------------------------
-- Functions for interacting with the city objects
----------------------------------------------------------------------------------------------------

-- | Dragging - pan building on xz plane (e.g. using left mouse button)
dragBuilding :: (Camera c)
             => Vector2 GLfloat -- ^ Old screen coordinates
             -> Vector2 GLfloat -- ^ New screen coordinates
             -> c -- ^ Get matrices
             -> City -- ^ Modify the city state
             -> City
dragBuilding (Vector2 ox oy) (Vector2 x y) camera city@City
    { activeObj = (i,j)
    , objectsIn = m
    } = city {
        objectsIn = IM.adjust f i m
    } where imat = invert (prepareProjection camera `prod` prepareView camera)
            Vector2 width height = fmap fromIntegral $ viewSize camera
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
            f cor@CityObjRep{orLocs = locs} = cor {orLocs = IM.adjust f' j locs}
            f' t = translate dv id <*> t


-- | Dragging - pan building on xz plane (e.g. using left mouse button)
rotateBuilding :: (Camera c)
               => Vector2 GLfloat -- ^ Old screen coordinates
               -> Vector2 GLfloat -- ^ New screen coordinates
               -> c -- ^ Get matrices
               -> City -- ^ Modify the city state
               -> City
rotateBuilding (Vector2 ox oy) (Vector2 x y) camera city@City
    { activeObj = (i,j)
    , objectsIn = m
    } = city {
        objectsIn = IM.adjust f i m
    } where imat = invert (prepareProjection camera `prod` prepareView camera)
            Vector2 width height = fmap fromIntegral $ viewSize camera
            campos = fromHom $ imat `prod` Vector4 0 0 0 1
            oldpos = fromHom $ imat `prod` Vector4 
                (2 * ox / width - 1)
                (1 - 2 * oy / height) 1 1
            newpos = fromHom $ imat `prod` Vector4 
                (2 * x / width - 1)
                (1 - 2 * y / height) 1 1
            oldPoint = findPos campos (oldpos .- campos) 0
            newPoint = findPos campos (newpos .- campos) 0
            f cor@CityObjRep{orLocs = locs} = cor {orLocs = IM.adjust f' j locs}
            f' t@(QTransform _ p _) = t >>= rotateY a
                where dv1 = unit $ newPoint .- p
                      dv0 = unit $ oldPoint .- p
                      Vector3 _ sina _ = cross dv0 dv1
                      a = atan2 sina (dv1 .*. dv0)


-- | Rotate, scale, and pan with two fingers
twoFingerBuilding :: (Camera c)
                  => (Vector2 GLfloat, Vector2 GLfloat) -- ^ Old screen coordinates
                  -> (Vector2 GLfloat, Vector2 GLfloat) -- ^ New screen coordinates
                  -> c -- ^ Get matrices
                  -> City -- ^ Modify the city state
                  -> City
twoFingerBuilding (Vector2 ox1 oy1, Vector2 ox2 oy2)
                 (Vector2 x1 y1, Vector2 x2 y2)
                 camera city@City
    { activeObj = (i,j)
    , objectsIn = m
    } = city {
        objectsIn = IM.adjust f i m
    } where imat = invert (prepareProjection camera `prod` prepareView camera)
            Vector2 width height = fmap fromIntegral $ viewSize camera
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
            f cor@CityObjRep{orLocs = locs} = cor {orLocs = IM.adjust f' j locs}
            f' t = translate dv id <*> (t >>= rotateY dφ)
--            f (b, bm) = (b, translate dv id <*> (bm >>= rotateY dφ))

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

-- | find position of the intersection of a ray traced from camera point to ground
findPos :: Vector3 GLfloat -- ^ camera position
        -> Vector3 GLfloat -- ^ camera sight vector
        -> GLfloat -- ^ height level of the point
        -> Vector3 GLfloat -- ^ position of the point in 3D
findPos (Vector3 c1 c2 c3) (Vector3 v1 v2 v3) h = Vector3 x h z
    where l = (h - c2)/v2
          x = c1 + v1*l
          z = c3 + v3*l



instance Monoid (IO ()) where
    mempty = return ()
    mappend a b = a >> b
    mconcat as = M.sequence_ as


-- Render shader

fragBuilding :: String
fragBuilding = unlines [
  "precision lowp float;",
  "varying vec4 vColor;",
  "varying vec2 vTextureCoord;",
  "uniform sampler2D uSampler;",
  "uniform float uTexUser;",
  "void main(void) {",
  "    gl_FragColor = uTexUser * texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t)) + (1.0-uTexUser)*vColor;",
  "}"]

vertBuilding :: String
vertBuilding = unlines [
--  "precision lowp float;",
  "attribute vec3 aVertexPosition;",
  "attribute vec3 aVertexNormal;",
  "attribute vec2 aTextureCoord;",
  "uniform mat4 uModelViewM;",
  "uniform mat4 uProjM;",
  "uniform vec3 uSunDir;",
  "uniform vec4 uVertexColor;",
  "varying vec4 vColor;",
  "varying vec2 vTextureCoord;",
  "void main(void) {",
  "  vec4 globalPos = uModelViewM * vec4(aVertexPosition, 1.0);",
  "  gl_Position = uProjM * globalPos;",
  "  vec3 vDist = globalPos.xyz/globalPos.w/200.0;",
  "  float z = clamp(dot(vDist,vDist), 0.0, 3.0);",
  "  vColor = clamp(uVertexColor * (1.0 + 0.3*max(0.0, dot(-vec4(uSunDir, 0.0), uModelViewM * vec4(aVertexNormal, 0.0)))), vec4(z-0.0,z-0.0,z-0.0,0.0), vec4(1.0,1.0,1.0,min(3.0-z, 1.0)));",
--  "  vColor = vec4(aVertexNormal,1) + 0.00001*vColor;",
  "  vTextureCoord = aTextureCoord;",
  "}"]


-- Selector shader

fragSelector :: String
fragSelector = unlines [
  "precision highp float;",
  "uniform vec4 uSelector;",
  "void main(void) {",
  "    gl_FragColor = uSelector;",
  "}"]

vertSelector :: String
vertSelector = unlines [
--  "precision lowp float;",
  "attribute vec3 aVertexPosition;",
  "uniform mat4 uModelViewM;",
  "uniform mat4 uProjM;",
  "void main(void) {",
  "  gl_Position = uProjM * uModelViewM * vec4(aVertexPosition, 1.0);",
  "}"]
