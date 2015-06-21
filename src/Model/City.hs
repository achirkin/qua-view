{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified Data.Traversable as TR
import qualified Control.Arrow as A
--import qualified Data.Graph as G
--import qualified Data.Foldable as F
import Data.Bits
import Data.Monoid

--import GHCJS.Foreign (toJSString)
import GHCJS.WebGL

import Geometry.Space
--import Geometry.Space.Quaternion
import Geometry.Structure
import Geometry.Space.Transform

import Drawable.World

import Data.IORef

import SmallGL.Shader
--import SmallGL.Helpers

import Model.CityObject
import Model.ScalarField

-- | Map of all city object (buildings, roads, etc).
--   Values: object, location (transformed mesh)
data City = City
    { activeObj    :: !(Int, Int)
    , buildShader  :: !ShaderProgram
    , selectShader :: !ShaderProgram
    , groundMesh   :: !(Maybe (CityObjectMesh, Texture))
    , objectsIn    :: !(IM.IntMap CityObjRep)
    }

data CityObjRep = CityObjRep
    { orObj  :: !CityObject
    , orMesh :: !CityObjectMesh
    , orLocs :: !(IM.IntMap (STransform "Quaternion" GLfloat (Maybe Texture)))
    }

-- | Bounding Box for the City in 2D
ground :: City -> Ground
ground = minBBox

-- | Add a city object on a pointed place in the city.
--   Returns a modified city (a city wih modified map of buildings)
addCityObject :: World
              -> CityObject
              -> Vector3 GLfloat -- ^ position
              -> GLfloat -- ^ rotation (w.r.t. Y axis)
              -> City
              -> IO (City)
addCityObject w o p r c@City{objectsIn = m} = do
    om <- createObjectMesh w o
    let city = c{objectsIn = IM.insert i CityObjRep
            { orObj = o
            , orMesh = om
            , orLocs = IM.fromAscList [(1,translate p Nothing >>= rotateY r)]
            } m }
    updateCityGround w city
    return city
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
    readIORef cref >>= updateCityGround w

-- | Update mesh for the city ground if exists
updateCityGround :: World -> City -> IO ()
updateCityGround _ City{ groundMesh = Nothing} = return ()
updateCityGround w c@City{ groundMesh = Just (gm,_) } = updateGroundMesh w (ground c) gm

clearCityTextures :: World -> City -> IO City
clearCityTextures World{glctx = gl} city = do
    gm <- removeGTex $ groundMesh city
    buildings <- TR.mapM removeAllTexs $ objectsIn city
    return city{groundMesh = gm, objectsIn = buildings}
    where removeTex tr = case unwrap tr of
                          Nothing -> return tr
                          Just tex -> deleteTexture gl tex >> return (wrap Nothing tr)
          removeGTex Nothing = return Nothing
          removeGTex (Just (com,tex)) = deleteTexture gl tex >> deleteCityObjectMesh gl com >> return Nothing
          removeAllTexs b@CityObjRep{orLocs = im} = TR.mapM removeTex im >>= \im' -> return b{orLocs = im'}

clearCityTextures' :: IORef World -> IORef City -> IO ()
clearCityTextures' wRef cRef = do
    world <- readIORef wRef
    readIORef cRef >>= clearCityTextures world >>= writeIORef cRef

-- | Helper for creation of the city from the list of city objects
buildCity :: World
          -> [CityObject]
          -> [[Vector3 GLfloat]] -- ^ positions
          -> [[GLfloat]] -- ^ rotations (w.r.t. Y axis)
          -> [[Maybe Texture]]
          -> IO (City)
buildCity w@World{glctx = gl} bs ps rs texs = do
    buProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragBuilding)
                                ,(gl_VERTEX_SHADER, vertBuilding)]
--    useProgram gl (programId buProgram)
    seProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragSelector)
                                ,(gl_VERTEX_SHADER, vertSelector)]
--    useProgram gl (programId seProgram)
    createdObjects <- mapM (\(olocs, oobj, omaction) -> do
        omesh <- omaction
        return $ CityObjRep oobj omesh olocs) . zip3 locs bs $ map (createObjectMesh w) bs :: IO [CityObjRep]
    let objs = IM.fromAscList . zip [1..] $ createdObjects
    return $ City (0,0) buProgram seProgram Nothing objs
    where trans p r t = translate p t >>= rotateY r
          locs = zipWith3 (\pp rr -> IM.fromAscList . zip [1..] . zipWith3 trans pp rr) ps rs texs

-- | get the points on all surfaces and put them into Scalar field
cityEvaluationGrid :: GLfloat -> City -> ScalarField
cityEvaluationGrid cs (City _ _ _ _ buildings) = ScalarField
    { cellSize  = cs
    , sfPoints  = IM.foldr' f [] buildings ++ groundEvalGrid (boundSet buildings) cs
    , sfRange   = zeros
    , sfValues  = []
    } where f :: CityObjRep -> [Vector3 GLfloat] -> [Vector3 GLfloat]
            f CityObjRep
                { orObj = obj
                , orLocs = m
                } acc = let pts = evaluationGrid obj cs
                            g tr ac = map (applyV3 . flip fmap tr . const) pts ++ ac
                        in IM.foldr' g [] m ++ acc

-- | Setup textures from scalar field
updateCityTextures :: World -> ScalarField -> ColorPalette -> City -> IO City
updateCityTextures world@World{glctx = gl}
                   sf@ScalarField{ cellSize  = cs}
                   palette
                   city = do
    objs <- TR.sequence ioseq
    gr' <- groundf (ground city) $ groundMesh city
    return city{ objectsIn = objs, groundMesh = gr' }
    where (gcolors, ioseq) = IM.mapAccum updateObj (makeColors palette sf) (objectsIn city)
          groundf gr (Just (gm, tex)) = do
            let (_, mtf) = groundGridToTexArray gr cs gcolors
            mtarr <- mtf
            mtex <- g mtarr (Just tex)
            return $ M.liftM ((,) gm) mtex
          groundf gr Nothing = do
            gm <- createGroundMesh world gr
            let (_, mtf) = groundGridToTexArray gr cs gcolors
            mtarr <- mtf
            mtex <- g mtarr Nothing
            return $ M.liftM ((,) gm) mtex
          g Nothing Nothing = return Nothing
          g Nothing (Just tex) = deleteTexture gl tex >> return Nothing
          g (Just ars) Nothing = M.liftM Just $ initTexture gl (Right ars)
          g (Just ars) (Just tex) = updateTexture gl (Right ars) tex >> (return $ Just tex)
          updateObj colors obj = (cleft, TR.sequence locs >>= \ls -> return obj{orLocs = ls})
            where (cleft,locs) = IM.mapAccum f colors $ orLocs obj
                  f cc tr = A.second (>>= liftTransform . flip M.liftM tr . g ) $ gridToTextureArray (orObj obj) cs cc


----------------------------------------------------------------------------------------------------
-- Embedding city into the program engine
----------------------------------------------------------------------------------------------------


-- Drawing a city means drawing all its objects
instance Drawable City where
    drawInCurrContext w c = drawCity w c
    updateDrawContext City{buildShader = bProg}
                      w@World{curContext = cc} = w
        { curContext = cc
            { wProjLoc = unifLoc bProg "uProjM"
            , wViewLoc = unifLoc bProg "uModelViewM"
            }
        }


drawCity :: World -> City -> IO ()
drawCity w@World
    { glctx = gl
    , curContext = WorldContext{wSunDir = Vector3 sx sy sz, wProjLoc = projLoc}
    } (City (j,k) bProg _ mgr buildings) = do
    enableVertexAttribArray gl ploc
    enableVertexAttribArray gl nloc
    enableVertexAttribArray gl tloc
    useProgram gl . programId $ bProg
    activeTexture gl gl_TEXTURE0
    uniform1i gl (unifLoc bProg "uSampler") 0
    uniformMatrix4fv gl projLoc False (projectLoc w)
    uniform3f gl (unifLoc bProg "uSunDir") sx sy sz
    case mgr of
        Nothing -> return ()
        Just (gm, tex) -> do
            uniform1f gl userLoc 1
            uniform4f gl colLoc 1 1 1 1
            bindTexture gl gl_TEXTURE_2D tex
            applyTransform w (return () :: MTransform GLfloat ())
            depthMask gl False
            drawGround gl alocs gm
            depthMask gl True
            uniform1f gl userLoc 0
    uniform4f gl colLoc 0.5 0.5 0.55 1
    IM.foldMapWithKey g buildings
    IM.foldMapWithKey f buildings
    disableVertexAttribArray gl tloc
    disableVertexAttribArray gl nloc
    disableVertexAttribArray gl ploc
    where f i (CityObjRep o m locs) | behavior o == Static = return ()
                                    | i /= j = IM.foldMapWithKey
                (\_ t -> applyTransform w t >>= maybeWithTexture m IdleOS) locs
                                    | otherwise = IM.foldMapWithKey
                (\l t -> applyTransform w t >>= maybeWithTexture m (if l /= k then IdleOS else SelectedOS)) locs
          g _ (CityObjRep o m locs) | behavior o == Dynamic = return ()
                                    | otherwise = IM.foldMapWithKey
                (\_ t -> applyTransform w t >>= maybeWithTexture m StaticOS) locs
          colLoc = unifLoc bProg "uVertexColor"
          userLoc = unifLoc bProg "uTexUser"
          alocs@(ploc,Just (nloc,tloc)) =
                  ( attrLoc bProg "aVertexPosition"
                  , Just ( attrLoc bProg "aVertexNormal"
                         , attrLoc bProg "aTextureCoord"))
          maybeWithTexture m state Nothing = do
            case state of
                StaticOS -> return ()
                IdleOS -> uniform4f gl colLoc 0.75 0.75 0.7 1
                SelectedOS -> uniform4f gl colLoc 1 0.6 0.6 1
            drawSurface gl alocs m
          maybeWithTexture m state (Just tex) = do
            case state of
                StaticOS -> uniform1f gl userLoc 0.5
                IdleOS -> do
                    uniform1f gl userLoc 1
                    uniform4f gl colLoc 0.8 0.8 0.8 1
                SelectedOS -> do
                    uniform1f gl userLoc 0.6
                    uniform4f gl colLoc 1 0.4 0.4 1
            bindTexture gl gl_TEXTURE_2D tex
            drawSurface gl alocs m
            uniform1f gl userLoc 0

data ObjectState = IdleOS | SelectedOS | StaticOS


-- City selectable means one can select objects in a city
instance Selectable City where
    selectInCurrContext w c = selectCityArea w c
    updateSelectContext City{selectShader = sProg}
                        w@World{curContext = cc} = w
        { curContext = cc
            { wProjLoc = unifLoc sProg "uProjM"
            , wViewLoc = unifLoc sProg "uModelViewM"
            }
        }

selectCityArea :: World -> City -> IO ()
selectCityArea w@World{glctx = gl, curContext = WorldContext{wProjLoc = projLoc}}
               (City _ _ sProg _ buildings) = do
    enableVertexAttribArray gl ploc
    useProgram gl . programId $ sProg
    uniformMatrix4fv gl projLoc False (projectLoc w)
    IM.foldMapWithKey f buildings
    disableVertexAttribArray gl ploc
    where f i (CityObjRep o m locs) = M.when (behavior o == Dynamic) $
            IM.foldMapWithKey (f' m i) locs
          f' m i j loc = do
            uniform4f gl selValLoc
                        (fromIntegral (i .&. 0x000000FF) / 255)
                        (fromIntegral (i .&. 0x0000FF00)/65280)
                        (fromIntegral (j .&. 0x000000FF) / 255)
                        (fromIntegral (j .&. 0x0000FF00)/65280)
            applyTransform w loc >> drawSurface gl alocs m
          selValLoc = unifLoc sProg "uSelector"
          alocs@(ploc, _) = ( attrLoc sProg "aVertexPosition"
                  , Nothing)


instance Boundable CityObjRep 3 GLfloat where
    minBBox (CityObjRep o _ locs) = boundSet $ IM.map (wrap bb) locs
        where bb = minBBox o :: BoundingBox 3 GLfloat

instance Boundable CityObjRep 2 GLfloat where
    minBBox cor = boundingBox (Vector2 lx lz) (Vector2 hx hz)
        where bb = minBBox cor :: BoundingBox 3 GLfloat
              Vector3 lx _ lz = lowBound bb
              Vector3 hx _ hz = highBound bb

instance Boundable City 2 GLfloat where
    minBBox City{ objectsIn = objs } = boundSet objs

--    minBBox Building{ objPolygon = poly} = boundingBox (Vector2 lx lz)
--                                                       (Vector2 hx hz)
--        where bound3 = minBBox poly
--              Vector3 lx _ lz = lowBound bound3
--              Vector3 hx _ hz = lowBound bound3
--    minBBox Road{ roadPoints = poly, roadWidth = rw} = boundingBox lb hb
--        where bound2 = boundSet poly
--              lb = fmap (+(-r2)) $ lowBound bound2
--              hb = fmap (+r2) $ highBound bound2
--              r2 = rw / 2
--    minBBox BoxHut { hutSize = hs } = boundingBox (Vector2 (-x) (-z))
--                                                  (Vector2 x z)
--        where Vector3 x _ z = (hs /.. 2)

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



instance (Monoid m) => Monoid (IO m) where
    mempty = return mempty
    mappend a b = mappend <$> a <*> b
    mconcat as = mconcat <$> M.sequence as


-- Render shader

fragBuilding :: String
fragBuilding = unlines [
  "precision mediump float;",
  "varying vec4 vColor;",
  "varying vec2 vTextureCoord;",
  "uniform sampler2D uSampler;",
  "uniform float uTexUser;",
  "void main(void) {",
  "    gl_FragColor = (uTexUser * texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t)) + (1.0-uTexUser))*vColor;",
  "}"]

vertBuilding :: String
vertBuilding = unlines [
  "precision mediump float;",
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
  "  vec3 vDist = globalPos.xyz/globalPos.w/150.0;",
  "  float z = clamp(dot(vDist,vDist), 0.0, 3.0);",
  "  vColor = uVertexColor * (1.0 + 0.3*max(0.0, dot(-vec4(uSunDir, 0.0), uModelViewM * vec4(aVertexNormal, 0.0))));",
  "  vColor = clamp(vColor, vec4(0.0,0.0,0.0,0.0), vec4(1.0,1.0,1.0,min(3.0-z, 1.0)));",
  "  vTextureCoord = aTextureCoord;",
  "}"]


-- Selector shader

fragSelector :: String
fragSelector = unlines [
  "precision mediump float;",
  "uniform vec4 uSelector;",
  "void main(void) {",
  "    gl_FragColor = uSelector;",
  "}"]

vertSelector :: String
vertSelector = unlines [
  "precision mediump float;",
  "attribute vec3 aVertexPosition;",
  "uniform mat4 uModelViewM;",
  "uniform mat4 uProjM;",
  "void main(void) {",
  "  gl_Position = uProjM * uModelViewM * vec4(aVertexPosition, 1.0);",
  "}"]
