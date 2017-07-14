{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.CityView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.CityView where

import Control.Monad (when)
-- import GHCJS.Foreign
import JsHs.Types
import JsHs.WebGL
import SmallGL.Shader
import Data.Geometry
import Data.Maybe (fromMaybe)
import Data.Geometry.Transform
--import Geometry.Space
--import Geometry.Space.Transform
import JsHs.Array as JS
import Data.Geometry.Structure.PointSet as PS

import Program.Model.City
import Program.Model.CityObject
import Program.Model.CityGround
import Program.Model.WiredGeometry
import Program.Types
import Program.View
import Program.View.CityObjectView
import Program.View.CityGroundView
import Program.View.WiredGeometryView ()
import Program.View.GroundMapView
import Data.Bits (Bits(..))
import JsHs.Nullable (Nullable(..))


newtype COViewCollection = COViewCollection JSVal
instance LikeJS "Array" COViewCollection
instance LikeJSArray "Object" COViewCollection where
    type ArrayElem COViewCollection = CityObjectView



--         } prog view = do
--            cview <- createView (glctx $ context view) (city prog)
----            let (errors, city) = buildCity 3 200 col
----            let (scale,shift) = scenarioViewScaling 200 col
----                (errors, cityObjs) = processScenario 3 scale shift col
----            mapM_ print errors
----            printVal . unsafeCoerce . clutter $ city prog
--            return view{cityView = cview}


data CityView = CityView
    { viewShaderTex   :: !ShaderProgram
    , viewShaderNoTex :: !ShaderProgram
    , selectShader :: !ShaderProgram
    , viewsIn      :: !COViewCollection
    , groundView   :: !(View CityGround)
    , clutterView  :: !(View WiredGeometry)
    , groundMap    :: !(Maybe GroundMapView)
    }


instance Drawable City where
    type View City = CityView
    createView gl city = do
        buProgramTex <- initShaders gl [(gl_VERTEX_SHADER, vertBuilding)
                                       ,(gl_FRAGMENT_SHADER, fragBuildingTex)
                                       ]
                                       [(0,"aVertexPosition")
                                       ,(1,"aVertexNormal")
                                       ,(2,"aTextureCoord")]
        buProgramNoTex <- initShaders gl [(gl_VERTEX_SHADER, vertBuilding)
                                         ,(gl_FRAGMENT_SHADER, fragBuildingNoTex)
                                         ]
                                       [(0,"aVertexPosition")
                                       ,(1,"aVertexNormal")
                                       ,(2,"aTextureCoord")
                                       ]
        seProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragSelector)
                                    ,(gl_VERTEX_SHADER, vertSelector)]
                                    []
        objs <- createObjViewCollection gl (objectsIn city)
        gr <- createView gl (ground city)
        clview <- createView gl (snd $ clutter city)
        mgmc <- case originLonLatAlt city of
            Nothing -> return Nothing
            Just lonlatalt -> let scprop = csettings city
                              in if useMapLayer scprop
                                 then Just <$> createGroundMapView gl (mapZoomLevel scprop) (cityTransform city) lonlatalt
                                 else return Nothing
        return CityView
            { viewShaderTex   = buProgramTex
            , viewShaderNoTex = buProgramNoTex
            , selectShader = seProgram
            , viewsIn      = objs
            , groundView   = gr
            , clutterView  = clview
            , groundMap    = mgmc
            }
    drawInCurrContext vc@ViewContext
        { glctx = gl
        , curState = ViewState{vSunDir = unpackV3 -> (sx,sy,sz)}
        } city@City{activeObjId = ai} cview@CityView{viewShaderTex = progTex, viewShaderNoTex = progNoTex} = do
        enableVertexAttribArray gl ploc
        -- draw map
        case groundMap cview of
            Nothing -> return ()
            Just gmc -> do
                enableVertexAttribArray gl tloc
                applyTransform vc (return () :: MTransform 3 GLfloat ())
                useProgram gl . programId $ progTex
                activeTexture gl gl_TEXTURE0
                uniformMatrix4fv gl (unifLoc progTex "uProjM") False (projectArr vc)
                uniform1i gl (unifLoc progTex "uSampler") 0
                uniformMatrix4fv gl (unifLoc progTex "uModelViewM") False (modelViewArr vc)
                drawGroundMapView gl (ploc,nloc,tloc) gmc
                disableVertexAttribArray gl tloc

        -- draw ground
        when (indexArrayLength (groundPoints $ ground city) > 0) $ do
          applyTransform vc (return () :: MTransform 3 GLfloat ())
          drawCityGround vc (progNoTex,progTex) (ground city) (groundView cview)

        -- draw buildings
        enableVertexAttribArray gl nloc
        useProgram gl . programId $ progNoTex
        uniformMatrix4fv gl (unifLoc progNoTex "uProjM") False (projectArr vc)
        uniform3f gl (unifLoc progNoTex "uSunDir") sx sy sz
        when (not $ isEmptyCity city) $ do
          uniform4f gl colLocNoTex (sr*sa) (sg*sa) (sb*sa) sa
          JS.zipiIO_ drawObject (objectsIn city) (viewsIn cview)
        disableVertexAttribArray gl nloc
        disableVertexAttribArray gl ploc
        where drawObject i tobj oview = applyTransform vc tobj >>= \obj -> do
                uniformMatrix4fv gl (unifLoc progNoTex "uModelViewM") False (modelViewArr vc)
                setColor (buildingColors city) i obj
                drawSurface gl alocs obj oview
              colLocNoTex = unifLoc progNoTex "uVertexColor"
              alocs@(ploc,Just (nloc,tloc)) = ( 0, Just ( 1, 2))
              setColor Nothing i obj = uniform4f gl colLocNoTex (r*a) (g*a) (b*a) a
                where
                  (r, g, b, a) = unpackV4 $ case (behavior obj, i+1 == ai) of
                                              (Static, _)      -> staticColor
                                              (Dynamic, True)  -> activeColor
                                              (Dynamic, False) -> itemColor
                  (HexColor itemColor) = fromMaybe (HexColor blockColor) $ getCityObjectColor obj
              setColor (Just arr) i obj = case unpackV4 $ PS.index i arr of
                    (r, g, b, a)  -> if behavior obj == Dynamic && i+1 == ai
                                     then uniform4f gl colLocNoTex (r*a*0.5) (g*a*0.2) (b*a*0.2) a
                                     else uniform4f gl colLocNoTex (r*a) (g*a) (b*a) a
              (HexColor blockColor) = defaultBlockColor $ csettings city
              (HexColor activeColor) = defaultActiveColor $ csettings city
              (HexColor staticColor) = defaultStaticColor $ csettings city
              (sr, sg, sb, sa) = unpackV4 $ staticColor

    updateView gl city@City{objectsIn = objs} cv@CityView{ viewsIn = views }
      | isEmptyCity city = do
        views' <- createObjViewCollection gl (objectsIn city)
        gr <- createView gl (ground city)
        clview <- updateView gl (snd $ clutter city) (clutterView cv)
        return cv
            { viewsIn      = views'
            , groundView   = gr
            , clutterView  = clview
            , groundMap    = Nothing
            }
      | otherwise = do
        views' <- fromJSArray <$> JS.unionZipIO f objs views
        gr <- updateView gl (ground city) (groundView cv)
        cl <- updateView gl (snd $ clutter city) (clutterView cv)
        ngmc <- case (groundMap cv, originLonLatAlt city) of
            (Just gmc, _)       -> return $ Just gmc
            (Nothing, Just lla) -> let scprop = csettings city
                                   in if useMapLayer scprop
                                      then Just <$> createGroundMapView gl (mapZoomLevel scprop) (cityTransform city) lla
                                      else return Nothing
            (Nothing, Nothing)  -> return Nothing
        return cv
            { viewsIn = views'
            , groundView = gr
            , clutterView = cl
            , groundMap = ngmc
            } where f _ Nothing  Nothing  = return nullRef
                    f _ Nothing  (Just v) = deleteView gl (undefined :: LocatedCityObject) v >> return nullRef
                    f _ (Just o) Nothing  = createView gl o
                    f _ (Just o) (Just v) = updateView gl o v
    deleteView gl _ CityView
            { viewsIn      = views
            , groundView   = gr
            , clutterView  = clutterv
            } = do -- TODO :: delete shaders
        JS.mapIO_ (deleteView gl (undefined :: LocatedCityObject)) views
        deleteView gl (undefined :: CityGround) gr
        deleteView gl (undefined :: WiredGeometry) clutterv
    draw vc city view = draw vc (snd $ clutter city) (clutterView view) >>
                        drawInCurrContext vc city view
--        where vc' = vc{ curState =  $ curState vc}

-- City selectable means one can select objects in a city
-- Note, we render index (i+1) on screen, so that 0 encodes no object there
instance Selectable City where
    selectInCurrContext vc@ViewContext
        { glctx = gl
        } city cview@CityView{selectShader = prog} = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        uniformMatrix4fv gl (unifLoc prog "uProjM") False (projectArr vc)
        JS.zipiIO_ drawObject (objectsIn city) (viewsIn cview)
        disableVertexAttribArray gl ploc
        where drawObject i tobj oview | behavior (unwrap tobj) == Static = return ()
                                      | otherwise = applyTransform vc tobj >>= \obj -> do
                uniformMatrix4fv gl (unifLoc prog "uModelViewM") False (modelViewArr vc)
                uniform4f gl selValLoc
                            (fromIntegral ((i+1) .&. 0x000000FF) / 255)
                            (fromIntegral ((i+1) .&. 0x0000FF00) / 65280)
                            (fromIntegral ((i+1) .&. 0x00FF0000) / 16711680)
                            1
                drawSurface gl alocs obj oview
              selValLoc = unifLoc prog "uSelector"
              alocs@(ploc, _) = ( attrLoc prog "aVertexPosition"
                      , Nothing)
    updateSelectState _ _ = id

createObjViewCollection :: WebGLRenderingContext -> CityObjectCollection -> IO COViewCollection
createObjViewCollection gl objs = fromJSArray <$>
            JS.mapIO (createView gl :: LocatedCityObject -> IO CityObjectView) objs


-- Render shader

fragBuildingTex :: String
fragBuildingTex = unlines [
  "precision mediump float;",
  "varying vec3 vDist;",
  "varying vec2 vTextureCoord;",
  "uniform sampler2D uSampler;",
  "void main(void) {",
  " mediump float fade = clamp(3.0 - dot(vDist,vDist), 0.0, 1.0);",
  " gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t)) * fade;",
  "}"]

fragBuildingNoTex :: String
fragBuildingNoTex = unlines [
  "precision mediump float;",
  "varying vec4 vColor;",
  "varying vec3 vDist;",
  "void main(void) {",
  " mediump float fade = clamp(3.0 - dot(vDist,vDist), 0.0, 1.0);",
  " gl_FragColor = vColor * fade;",
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
  "varying vec3 vDist;",
  "void main(void) {",
  "  vec4 globalPos = uModelViewM * vec4(aVertexPosition, 1.0);",
  "  gl_Position = uProjM * globalPos;",
  "  vDist = globalPos.xyz/(globalPos.w*200.0);",
  "  vec4 tNormal = normalize(uModelViewM * vec4(aVertexNormal, 0.0));",
  "  mediump float brightness = 1.0 + 0.3 * dot(tNormal.xyz,uSunDir) * sign(dot(tNormal,globalPos));",
  "  mediump float a = uVertexColor.w;",
  "  vColor = vec4(clamp(uVertexColor.xyz * brightness, vec3(0,0,0), vec3(a,a,a)), a);",
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
