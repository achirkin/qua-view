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

import JsHs.TypedArray
import JsHs.TypedArray.IO as JsHs
import Data.Coerce (coerce)
import JsHs.WebGL
import SmallGL.Shader
import Data.Geometry

import Program.Model.WiredGeometry
import Program.View

data WiredGeometryView = WGView !WebGLBuffer !ShaderProgram !GLsizei


instance Drawable WiredGeometry where
    type View WiredGeometry = WiredGeometryView
    createView gl (WiredGeometry _ _ size arr) = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
        shProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragStaticMesh)
                                    ,(gl_VERTEX_SHADER, vertStaticMesh)]
                                    []
        return $ WGView buf shProgram size
    drawInCurrContext vc@ViewContext{glctx = gl, curState = cs}
                      (WiredGeometry _ (unpackV4 -> (r,g,b,a)) _ _)
                      (WGView buf prog size) | size <= 1 = return ()
                                        | otherwise = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        bindBuffer gl gl_ARRAY_BUFFER buf
        uniformMatrix4fv gl (unifLoc prog "uProjM") False (projectArr vc)
        setIndex 0 (vView cs) (coerce $ modelViewArr vc)
        uniformMatrix4fv gl (unifLoc prog "uModelViewM") False (modelViewArr vc)
        uniform4f gl (unifLoc prog "uColor") (r*a) (g*a) (b*a) a
        vertexAttribPointer gl ploc 3 gl_FLOAT False 12 0
        drawArrays gl gl_LINES 0 size
        disableVertexAttribArray gl ploc
            where ploc = attrLoc prog "aVertexPosition"

    updateView gl (WiredGeometry _ _ s arr) (WGView buf p _) = do
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
        return (WGView buf p s)
    deleteView gl _ (WGView buf _ _) =
        deleteBuffer gl buf


fragStaticMesh :: String
fragStaticMesh = unlines [
  "precision mediump float;",
  "uniform vec4 uColor;",
  "varying vec3 vDist;",
  "void main(void) {",
  "    gl_FragColor = clamp(3.0 - dot(vDist,vDist), 0.0, 1.0) * uColor;",
  "}"]

vertStaticMesh :: String
vertStaticMesh = unlines [
  "precision mediump float;",
  "attribute vec3 aVertexPosition;",
  "uniform mat4 uModelViewM;",
  "uniform mat4 uProjM;",
  "varying vec3 vDist;",
  "void main(void) {",
  "  vec4 globalPos = uModelViewM * vec4(aVertexPosition, 1.0);",
  "  gl_Position = uProjM * globalPos;",
  "  vDist = globalPos.xyz/(globalPos.w*200.0);",
  "}"]
