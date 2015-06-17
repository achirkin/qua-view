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

--import GHCJS.Foreign
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
    arr <- newArrayBuffer $ coords >>= \(Vector3 x y z) -> [x,y,z]
    buf <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER arr gl_STATIC_DRAW
    shProgram <- initShaders gl [(gl_FRAGMENT_SHADER, fragStaticMesh)
                                ,(gl_VERTEX_SHADER, vertStaticMesh)]
    return $ Grid color (fromIntegral $ length coords) arr buf shProgram
    where grid = map (\k -> size*((fromIntegral k / fromIntegral cells) - 0.5)) [1..cells-1]
          coords = (grid >>= \x -> [Vector3 x 0 (-size/2), Vector3 x 0 (size/2)])
                ++ (grid >>= \x -> [Vector3 (-size/2) 0 x, Vector3 (size/2) 0 x])



instance Drawable Grid where
    drawInCurrContext w@World{glctx = gl, curContext = cc}
                      (Grid (Vector4 r g b a) size _ buf prog) = do
        enableVertexAttribArray gl ploc
        useProgram gl . programId $ prog
        bindBuffer gl gl_ARRAY_BUFFER buf
        uniformMatrix4fv gl (wProjLoc cc) False (projectLoc w)
        fillTypedArray (modelViewLoc w) (wView cc)
        uniformMatrix4fv gl (wViewLoc cc) False (modelViewLoc w)
        uniform4f gl (unifLoc prog "uColor") r g b a
        vertexAttribPointer gl ploc 3 gl_FLOAT False 12 0
        drawArrays gl gl_LINES 0 size
        disableVertexAttribArray gl ploc
            where ploc = attrLoc prog "aVertexPosition"
    updateDrawContext (Grid _ _ _ _ prog)
                      w@World{curContext = cc} = w
        { curContext = cc
            { wProjLoc = unifLoc prog "uProjM"
            , wViewLoc = unifLoc prog "uModelViewM"
            }
        }


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
