{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.CityView
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.CityView where


import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback ( Callback (), OnBlocked (..)
                              , syncCallback1, syncCallback3, releaseCallback)
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.WebGL
import SmallGL.Shader
import Data.Geometry
import Data.Geometry.Transform
--import Geometry.Space
--import Geometry.Space.Transform

import Program.Model.City
import Program.Model.CityObject
--import Program.Model.CityGround
--import Program.Model.WiredGeometry
import Program.View
import Program.View.CityObjectView
--import Program.View.CityGroundView
--import Program.View.WiredGeometryView ()
import Data.Bits (Bits(..))

import GHCJS.Useful

newtype COViewCollection = COViewCollection JSVal

data CityView = CityView
    { viewShader   :: !ShaderProgram
    , selectShader :: !ShaderProgram
    , viewsIn      :: !COViewCollection
--    , groundView   :: !(View CityGround)
--    , clutterView  :: !(View WiredGeometry)
    }


instance Drawable City where
    type View City = CityView
    createView gl city = do
        buProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragBuilding)
                                    ,(gl_VERTEX_SHADER, vertBuilding)]
        seProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragSelector)
                                    ,(gl_VERTEX_SHADER, vertSelector)]
        objs <- createObjViewCollection gl (objectsIn city)
--        gr <- createView gl (ground city)
--        clview <- createView gl (clutter city)
        return CityView
            { viewShader   = buProgram
            , selectShader = seProgram
            , viewsIn      = objs
--            , groundView   = gr
--            , clutterView  = clview
            }
    drawInCurrContext vc@ViewContext
        { glctx = gl
        , curState = cs@ViewState{vSunDir = unpackV3 -> (sx,sy,sz)}
        } city@City{activeObjId = ai} cview@CityView{viewShader = prog} = do
        enableVertexAttribArray gl ploc
        enableVertexAttribArray gl nloc
        enableVertexAttribArray gl tloc
        useProgram gl . programId $ prog
        activeTexture gl gl_TEXTURE0
        uniform1i gl (unifLoc prog "uSampler") 0
        uniformMatrix4fv gl (vGLProjLoc cs) False (projectArr vc)
        uniform3f gl (unifLoc prog "uSunDir") sx sy sz
        -- draw ground
        uniform1f gl userLoc 1
        uniform4f gl colLoc 1 1 1 1
        applyTransform vc (return () :: MTransform 3 GLfloat ())
--        drawCityGround gl (ploc,nloc,tloc) (ground city) (groundView cview)
        -- draw buildings
        uniform1f gl userLoc 0 -- disable textures for now
        uniform4f gl colLoc 0.5 0.5 0.55 1
        zipIObuildings drawObject (objectsIn city) (viewsIn cview)
        disableVertexAttribArray gl tloc
        disableVertexAttribArray gl nloc
        disableVertexAttribArray gl ploc
        where drawObject i tobj oview = applyTransform vc tobj >>= \obj -> do
                case behavior obj of
                    Static  -> uniform4f gl colLoc 0.5 0.5 0.55 1
                    Dynamic -> if i == ai
                               then uniform4f gl colLoc 1 0.6 0.6 1
                               else uniform4f gl colLoc 0.75 0.75 0.7 1
                drawSurface gl alocs obj oview
              colLoc = unifLoc prog "uVertexColor"
              userLoc = unifLoc prog "uTexUser"
              alocs@(ploc,Just (nloc,tloc)) =
                      ( attrLoc prog "aVertexPosition"
                      , Just ( attrLoc prog "aVertexNormal"
                             , attrLoc prog "aTextureCoord"))
    updateDrawState _ CityView{viewShader = prog} cs = cs
        { vGLProjLoc = unifLoc prog "uProjM"
        , vGLViewLoc = unifLoc prog "uModelViewM"
        }
    updateView gl city@City{objectsIn = objs} cv@CityView{ viewsIn = views } = do
        mviews' <- zipIOMaybeBuildings f objs views
--        gr <- updateView gl (ground city) (groundView cv)
--        cl <- updateView gl (clutter city) (clutterView cv)
        return cv
            { viewsIn = mviews'
--            , groundView = gr
--            , clutterView = cl
            } where f _ Nothing  Nothing  = return Nothing
                    f _ Nothing  (Just v) = deleteView gl (undefined :: LocatedCityObject) v >> return Nothing
                    f _ (Just o) Nothing  = Just <$> createView gl o
                    f _ (Just o) (Just v) = Just <$> updateView gl o v
    deleteView gl _ CityView
            { viewsIn      = views
--            , groundView   = gr
--            , clutterView  = clutterv
            } = do -- TODO :: delete shaders
        mapIOViews (deleteView gl (undefined :: LocatedCityObject)) views
--        deleteView gl (undefined :: CityGround) gr
--        deleteView gl (undefined :: WiredGeometry) clutterv
    draw vc city view = -- draw vc (clutter city) (clutterView view) >>
                        drawInCurrContext vc' city view
        where vc' = vc{ curState = updateDrawState city view $ curState vc}

-- City selectable means one can select objects in a city
instance Selectable City where
    selectInCurrContext vc@ViewContext
        { glctx = gl
        , curState = cs
        } city cview@CityView{selectShader = prog} = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        uniformMatrix4fv gl (vGLProjLoc cs) False (projectArr vc)
        zipIObuildings drawObject (objectsIn city) (viewsIn cview)
        disableVertexAttribArray gl ploc
        where drawObject i tobj oview | behavior (unwrap tobj) == Static = return ()
                                      | otherwise = applyTransform vc tobj >>= \obj -> do
                uniform4f gl selValLoc
                            (fromIntegral (i .&. 0x000000FF) / 255)
                            (fromIntegral (i .&. 0x0000FF00) / 65280)
                            (fromIntegral (i .&. 0x00FF0000) / 16711680)
                            1
                drawSurface gl alocs obj oview
              selValLoc = unifLoc prog "uSelector"
              alocs@(ploc, _) = ( attrLoc prog "aVertexPosition"
                      , Nothing)
    updateSelectState _ CityView{selectShader = prog} cs = cs
        { vGLProjLoc = unifLoc prog "uProjM"
        , vGLViewLoc = unifLoc prog "uModelViewM"
        }

createObjViewCollection :: WebGLRenderingContext -> CityObjectCollection -> IO COViewCollection
createObjViewCollection gl objs = do
        call <- syncCallbackUnsafe1 (\x -> coerce <$> createView gl (unsafeCoerce x :: CityObject))
        rez <- js_createObjViewCollection call objs
        releaseCallback call
        return rez

foreign import javascript unsafe "$2.map($1)"
    js_createObjViewCollection :: Callback (JSVal -> IO JSVal) -> CityObjectCollection -> IO COViewCollection


zipIObuildings :: (Int -> LocatedCityObject -> CityObjectView -> IO ())
               -> CityObjectCollection
               -> COViewCollection
               -> IO ()
zipIObuildings f objs views = do
        call <- syncCallback3 ContinueAsync
                (\i o v -> case pFromJSVal o of
                            Nothing  -> return ()
                            Just obj -> f (toNum i) obj (coerce v))
        js_zipIObuildings call objs views
        releaseCallback call

foreign import javascript unsafe "$2.forEach(function(e,i){if(e && $3[i]){$1(i+1,e,$3[i]);}})"
    js_zipIObuildings :: Callback (JSVal -> JSVal -> JSVal -> IO ())
                      -> CityObjectCollection
                      -> COViewCollection
                      -> IO ()


zipIOMaybeBuildings :: (Int -> Maybe CityObject -> Maybe CityObjectView -> IO (Maybe CityObjectView))
                    -> CityObjectCollection
                    -> COViewCollection
                    -> IO COViewCollection
zipIOMaybeBuildings f objs views = do
        call <- syncCallbackUnsafe3
                (\i o v -> maybeCoerce <$>
                            f (toNum i) (if isTruthy o then Just (unsafeCoerce o) else Nothing)
                                        (if isTruthy v then Just (coerce v) else Nothing)
                            )
        rez <- js_zipIOMaybeBuildings call objs views
        releaseCallback call
        return rez
        where maybeCoerce Nothing = jsNull
              maybeCoerce (Just r) = coerce r


foreign import javascript unsafe "$2.map(function(e,i){if(e && $3[i]){return $1(i+1,e,$3[i]);}else{return null;}})"
    js_zipIOMaybeBuildings :: Callback (JSVal -> JSVal -> JSVal -> IO JSVal)
                           -> CityObjectCollection
                           -> COViewCollection
                           -> IO COViewCollection

mapIOViews :: (CityObjectView -> IO ())
           -> COViewCollection
           -> IO ()
mapIOViews f views = do
        call <- syncCallback1 ContinueAsync (f . coerce)
        js_mapIOViews call views
        releaseCallback call

foreign import javascript unsafe "$2.forEach(function(e){if(e){$1(e);}})"
    js_mapIOViews :: Callback (JSVal -> IO ())
                  -> COViewCollection
                  -> IO COViewCollection

--instance (Monoid m) => Monoid (IO m) where
--    mempty = return mempty
--    mappend a b = mappend <$> a <*> b
--    mconcat as = mconcat <$> sequence as

--instance (Monoid m, Monad mm) => Monoid (mm m) where
--    mempty = return mempty
--    mappend a b = mappend <$> a <*> b
--    mconcat as = mconcat <$> sequence as

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
  "  vColor = uVertexColor * (1.0 + 0.3*max(0.0, dot(-vec4(uSunDir, 0.0), normalize(uModelViewM * vec4(aVertexNormal, 0.0)))));",
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
