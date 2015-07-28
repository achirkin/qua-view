{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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


import qualified Data.IntMap.Strict as IM
import Control.Monad (liftM)

import GHCJS.WebGL
import SmallGL.Shader
import Geometry.Space
import Geometry.Space.Transform

import Program.Model.City
import Program.Model.CityObject
import Program.Model.CityGround
import Program.View
import Program.View.CityObjectView
import Program.View.CityGroundView
import Data.Bits (Bits(..))


data CityView = CityView
    { viewShader   :: !ShaderProgram
    , selectShader :: !ShaderProgram
    , viewsIn      :: !(IM.IntMap (View CityObject))
    , groundView   :: !(View CityGround)
    }


instance Drawable City where
    type View City = CityView
    createView gl city = do
        buProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragBuilding)
                                    ,(gl_VERTEX_SHADER, vertBuilding)]
        seProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragSelector)
                                    ,(gl_VERTEX_SHADER, vertSelector)]
        objs <- mapM (createView gl . unwrap) (objectsIn city)
        gr <- createView gl (ground city)
        return CityView
            { viewShader   = buProgram
            , selectShader = seProgram
            , viewsIn      = objs
            , groundView   = gr
            }
    drawInCurrContext vc@ViewContext
        { glctx = gl
        , curState = cs@ViewState{vSunDir = Vector3 sx sy sz}
        } city@City{activeObj = ai} cview@CityView{viewShader = prog} = do
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
        applyTransform vc (return () :: MTransform GLfloat ())
        drawCityGround gl (ploc,nloc,tloc) (ground city) (groundView cview)
        -- draw buildings
        uniform1f gl userLoc 0 -- disable textures for now
        uniform4f gl colLoc 0.5 0.5 0.55 1
        IM.foldMapWithKey drawObject buildings :: IO ()
        disableVertexAttribArray gl tloc
        disableVertexAttribArray gl nloc
        disableVertexAttribArray gl ploc
        where buildings = IM.intersectionWith (,) (objectsIn city) (viewsIn cview)
              drawObject i (tobj, oview) = applyTransform vc tobj >>= \obj -> do
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
        mviews' <- sequence $ IM.mergeWithKey updateFunc addFunc deleteFunc objs views
        gr <- updateView gl (ground city) (groundView cv)
        return cv
            { groundView = gr
            , viewsIn = IM.mapMaybe id mviews'
            } where updateFunc _ o = Just . liftM Just . updateView gl o
                    addFunc = fmap (liftM Just . createView gl)
                    deleteFunc = fmap ( liftM (const Nothing)
                                      . deleteView gl (undefined :: LocatedCityObject)
                                      )
    deleteView gl _ CityView
            { viewsIn      = views
            , groundView   = gr
            } = do -- TODO :: delete shaders
        mapM_ (deleteView gl (undefined :: LocatedCityObject)) views
        deleteView gl (undefined :: CityGround) gr

-- City selectable means one can select objects in a city
instance Selectable City where
    selectInCurrContext vc@ViewContext
        { glctx = gl
        , curState = cs
        } city cview@CityView{selectShader = prog} = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        uniformMatrix4fv gl (vGLProjLoc cs) False (projectArr vc)
        IM.foldMapWithKey drawObject buildings :: IO ()
        disableVertexAttribArray gl ploc
        where buildings = IM.intersectionWith (,) (objectsIn city) (viewsIn cview)
              drawObject i (tobj, oview) | behavior (unwrap tobj) == Static = return ()
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



instance (Monoid m) => Monoid (IO m) where
    mempty = return mempty
    mappend a b = mappend <$> a <*> b
    mconcat as = mconcat <$> sequence as

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
