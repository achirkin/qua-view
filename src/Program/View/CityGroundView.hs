{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.CityGroundView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.CityGroundView where


import JsHs.WebGL
import Data.Geometry

import Program.Model.CityObject
import Program.Model.CityGround
import Program.View
import SmallGL.Shader

import JsHs.TypedArray

data CityGroundView = CityGroundView !WebGLBuffer !WebGLBuffer !(Maybe WebGLTexture) !GLsizei



drawCityGround :: ViewContext -> (ShaderProgram,ShaderProgram) -> CityGround -> View CityGround -> IO ()
drawCityGround vc@ViewContext
        { glctx = gl
        , curState = ViewState{vSunDir = unpackV3 -> (sx,sy,sz)}
        } (prog,_) _ (CityGroundView buf ibuf Nothing isize) = do
    enableVertexAttribArray gl nloc
    useProgram gl $ programId prog
    depthMask gl False
    uniformMatrix4fv gl (unifLoc prog "uProjM") False (projectArr vc)
    uniformMatrix4fv gl (unifLoc prog "uModelViewM") False (modelViewArr vc)
    uniform3f gl (unifLoc prog "uSunDir") sx sy sz
    uniform4f gl (unifLoc prog "uVertexColor") 0.8 0.8 0.8 0.8
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl (attrLoc prog "aVertexPosition") 3 gl_FLOAT False 20 0
    vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES isize gl_UNSIGNED_SHORT 0
    depthMask gl True
    disableVertexAttribArray gl nloc
  where
    nloc = attrLoc prog "aVertexNormal"
drawCityGround vc@ViewContext
        { glctx = gl
        } (_,prog) _ (CityGroundView buf ibuf (Just tex) isize) = do
    enableVertexAttribArray gl tloc
    useProgram gl $ programId prog
    bindTexture gl gl_TEXTURE_2D tex
    depthMask gl False
    uniformMatrix4fv gl (unifLoc prog "uProjM") False (projectArr vc)
    uniformMatrix4fv gl (unifLoc prog "uModelViewM") False (modelViewArr vc)
    uniform1i gl (unifLoc prog "uSampler") 0
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl (attrLoc prog "aVertexPosition") 3 gl_FLOAT False 20 0
    vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES isize gl_UNSIGNED_SHORT 0
    depthMask gl True
    disableVertexAttribArray gl tloc
  where
    tloc = attrLoc prog "aTextureCoord"


instance Drawable CityGround where
    type View CityGround = CityGroundView
    createView gl gr = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        ibuf <- createBuffer gl
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
        bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return $ CityGroundView buf ibuf Nothing (indexArrayLength dat)
        where dat = groundPoints gr
    drawInCurrContext _ _ _ = return ()
    updateView ctx g = updateGroundView ctx g Nothing
    deleteView ctx _ (CityGroundView buf ibuf mtex _) = do
        deleteBuffer ctx buf
        deleteBuffer ctx ibuf
        maybe (return ()) (deleteTexture ctx) mtex

updateGroundView :: WebGLRenderingContext
                 -> CityGround
                 -> Maybe (Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei)))
                 -> View CityGround
                 -> IO (View CityGround)
updateGroundView gl gr mts (CityGroundView buf ibuf mtex _) = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
    mntex <- case (mts, mtex) of
        (Nothing, Nothing) -> return Nothing
        (Nothing, Just tex) -> deleteTexture gl tex >> return Nothing
        (Just ts, Nothing) -> Just <$> initTexture gl ts
        (Just ts, Just tex) -> updateTexture gl ts tex >> return mtex
    return $ CityGroundView buf ibuf mntex (indexArrayLength dat)
    where dat = groundPoints gr
