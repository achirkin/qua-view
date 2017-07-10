{-# LANGUAGE TypeFamilies #-}
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
--import Data.Geometry

import Program.Model.CityObject
import Program.Model.CityGround
import Program.View

import JsHs.TypedArray

data CityGroundView = CityGroundView !WebGLBuffer !WebGLBuffer !(Maybe WebGLTexture)




drawCityGround :: WebGLRenderingContext -> (WebGLUniformLocation,GLuint,GLuint,GLuint) -> CityGround -> View CityGround -> IO ()
drawCityGround gl (useTexLoc,ploc,nloc,tloc) gr (CityGroundView buf ibuf Nothing) = do
    depthMask gl False
    uniform1f gl useTexLoc 0
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
    vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES (indexArrayLength $ groundPoints gr) gl_UNSIGNED_SHORT 0
    depthMask gl True
drawCityGround gl (useTexLoc,ploc,nloc,tloc) gr (CityGroundView buf ibuf (Just tex)) = do
    bindTexture gl gl_TEXTURE_2D tex
    depthMask gl False
    uniform1f gl useTexLoc 1
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
    vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES  (indexArrayLength $ groundPoints gr) gl_UNSIGNED_SHORT 0
    depthMask gl True


instance Drawable CityGround where
    type View CityGround = CityGroundView
    createView gl gr = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        ibuf <- createBuffer gl
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
        bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return $ CityGroundView buf ibuf Nothing
        where dat = groundPoints gr
    drawInCurrContext _ _ _ = undefined
    updateDrawState _ _ _ = undefined
    updateView ctx g = updateGroundView ctx g Nothing
    deleteView ctx _ (CityGroundView buf ibuf mtex) = do
        deleteBuffer ctx buf
        deleteBuffer ctx ibuf
        maybe (return ()) (deleteTexture ctx) mtex

updateGroundView :: WebGLRenderingContext
                 -> CityGround
                 -> Maybe (Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei)))
                 -> View CityGround
                 -> IO (View CityGround)
updateGroundView gl gr mts (CityGroundView buf ibuf mtex) = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
    mntex <- case (mts, mtex) of
        (Nothing, Nothing) -> return Nothing
        (Nothing, Just tex) -> deleteTexture gl tex >> return Nothing
        (Just ts, Nothing) -> Just <$> initTexture gl ts
        (Just ts, Just tex) -> updateTexture gl ts tex >> return mtex
    return $ CityGroundView buf ibuf mntex
    where dat = groundPoints gr
