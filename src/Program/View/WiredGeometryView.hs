{-# LANGUAGE TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.WiredGeometryView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.WiredGeometryView
    ( WiredGeometryView (..)
    )where

import JsHs.TypedArray.IO
import Data.Coerce (coerce)
import JsHs.WebGL
import SmallGL.Shader
import Data.Geometry

import Program.Model.WiredGeometry
import Program.View

data WiredGeometryView = WGView !WebGLBuffer !ShaderProgram


instance Drawable WiredGeometry where
    type View WiredGeometry = WiredGeometryView
    createView gl (WiredGeometry _ _ _ arr) = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
        shProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragStaticMesh)
                                    ,(gl_VERTEX_SHADER, vertStaticMesh)]
        return $ WGView buf shProgram
    drawInCurrContext vc@ViewContext{glctx = gl, curState = cs}
                      (WiredGeometry _ (unpackV4 -> (r,g,b,a)) size _)
                      (WGView buf prog) = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        bindBuffer gl gl_ARRAY_BUFFER buf
        uniformMatrix4fv gl (vGLProjLoc cs) False (projectArr vc)
        setIndex 0 (vView cs) (coerce $ modelViewArr vc)
        uniformMatrix4fv gl (vGLViewLoc cs) False (modelViewArr vc)
        uniform4f gl (unifLoc prog "uColor") r g b a
        vertexAttribPointer gl ploc 3 gl_FLOAT False 12 0
        drawArrays gl gl_LINES 0 size
        disableVertexAttribArray gl ploc
            where ploc = attrLoc prog "aVertexPosition"
    updateDrawState _ (WGView _ prog) cs = cs
        { vGLProjLoc = unifLoc prog "uProjM"
        , vGLViewLoc = unifLoc prog "uModelViewM"
        }
    updateView gl (WiredGeometry _ _ _ arr) dgv@(WGView buf _) = do
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
        return dgv
    deleteView gl _ (WGView buf _) =
        deleteBuffer gl buf


fragStaticMesh :: String
fragStaticMesh = unlines [
  "precision lowp float;",
  "uniform vec4 uColor;",
  "varying vec3 vDist;",
  "void main(void) {",
  "    lowp float z = clamp(dot(vDist,vDist), 0.0, 3.0);",
  "    gl_FragColor = clamp(uColor, vec4(0.0,0.0,0.0,0.0), vec4(1.0,1.0,1.0,min(3.0-z, 1.0)));",
  "}"]

vertStaticMesh :: String
vertStaticMesh = unlines [
  "precision lowp float;",
  "attribute vec3 aVertexPosition;",
  "uniform mat4 uModelViewM;",
  "uniform mat4 uProjM;",
  "varying vec3 vDist;",
  "void main(void) {",
  "  vec4 globalPos = uModelViewM * vec4(aVertexPosition, 1.0);",
  "  gl_Position = uProjM * globalPos;",
  "  vDist = globalPos.xyz/globalPos.w/150.0;",
  "}"]
