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

module Model.Grid where

--import Control.Applicative
--import qualified Control.Monad as M
--import qualified Data.Graph as G
--import qualified Data.Foldable as F
--import Data.Bits

import GHCJS.Foreign
import GHCJS.WebGL

import SmallGL.Shader
--import SmallGL.Helpers

import Geometry.Space
import Drawable.World

-- | Collection of lines in one entity
data Grid = Grid !(Vector4 GLfloat)
   !GLsizei !ArrayBuffer !Buffer !ShaderProgram

createGrid :: World
           -> GLfloat -- size of the grid (width and length)
           -> Int -- number of cells
           -> Vector4 GLfloat -- ^ color of the mesh
           -> IO Grid
createGrid World{glctx = gl} size cells color = do
    enableVertexAttribArray gl 0
    arr <- newArrayBuffer $ coords >>= \(Vector3 x y z) -> [x,y,z]
    buf <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
    shProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragStaticMesh)
                                ,(gl_VERTEX_SHADER, vertStaticMesh)]
    useProgram gl (programId shProgram)
    bindAttribLocation gl (programId shProgram) 0 (toJSString "aVertexPosition")
    disableVertexAttribArray gl 0
    return $ Grid color (fromIntegral $ length coords) arr buf shProgram
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          coords = (grid >>= \x -> [Vector3 x 0 (-size/2), Vector3 x 0 (size/2)])
                ++ (grid >>= \x -> [Vector3 (-size/2) 0 x, Vector3 (size/2) 0 x])



instance Drawable Grid where
    draw w@World{glctx = gl} (Grid (Vector4 r g b a) size _ buf prog) = do
        enableVertexAttribArray gl 0
        useProgram gl . programId $ prog
        bindBuffer gl gl_ARRAY_BUFFER buf
        uniformMatrix4fv gl (unifLoc prog "uProjM") False (projectLoc w)
        fillTypedArray (modelViewLoc w) (currentView w)
        uniformMatrix4fv gl (unifLoc prog "uModelViewM") False (modelViewLoc $ w)
        uniform4f gl (unifLoc prog "uColor") r g b a
        vertexAttribPointer gl 0 3 gl_FLOAT False 12 0
        drawArrays gl gl_LINES 0 size
        disableVertexAttribArray gl 0


fragStaticMesh :: String
fragStaticMesh = unlines [
  "precision lowp float;",
  "uniform vec4 uColor;",
  "varying vec3 vDist;",
  "void main(void) {",
  "    lowp float z = clamp(dot(vDist,vDist), 0.0, 3.0);",
  "    gl_FragColor = clamp(uColor, vec4(z-0.0,z-0.0,z-0.0,0.0), vec4(1.0,1.0,1.0,min(3.0-z, 1.0)));",
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
  "  vDist = globalPos.xyz/globalPos.w/200.0;",
  "}"]
