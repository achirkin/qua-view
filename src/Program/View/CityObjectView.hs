{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.CityObjectView
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.CityObjectView where

import JsHs.Array
--import JsHs.LikeJS.Class

import JsHs.WebGL
import JsHs.Types

import Program.Model.CityObject
import Program.View
import JsHs.Nullable (Nullable)

--import Unsafe.Coerce (unsafeCoerce)
--import GHCJS.Useful
--import JsHs.TypedArray

newtype CityObjectView = CityObjectView JSVal
instance LikeJS "Object" CityObjectView
instance Nullable CityObjectView

foreign import javascript "$r = {}; $r.pointBuffer = $1; $r.indexBuffer = $2; $r.indexLength = $3;"
    cityObjectView :: WebGLBuffer -> WebGLBuffer -> GLsizei -> CityObjectView
foreign import javascript "$1.pointBuffer"
    pointBuffer :: CityObjectView -> WebGLBuffer
foreign import javascript "$1.indexBuffer"
    indexBuffer :: CityObjectView -> WebGLBuffer
foreign import javascript "$1.indexLength"
    indexLength :: CityObjectView -> GLsizei



instance Show CityObjectView where
    show _ = "CityObjectView"



drawSurface :: WebGLRenderingContext
            -> (GLuint,Maybe (GLuint,GLuint) )
            -> CityObject
            -> View CityObject -> IO ()
drawSurface gl (ploc,olocs) _ cov = do
    bindBuffer gl gl_ARRAY_BUFFER (pointBuffer cov)
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    case olocs of
        Just (nloc,tloc) -> do
            vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
            vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
        Nothing -> return ()
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (indexBuffer cov)
    drawElements gl gl_TRIANGLES (indexLength cov) gl_UNSIGNED_SHORT 0


instance Drawable CityObject where
    type View CityObject = CityObjectView
    createView gl obj = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        ibuf <- createBuffer gl
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
        bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return $ cityObjectView buf ibuf (indexArrayLength dat)
        where dat = objPoints obj
    drawInCurrContext _ _ _ = return ()
    updateDrawState _ _ = id
    updateView gl obj cov = do
        bindBuffer gl gl_ARRAY_BUFFER (pointBuffer cov)
        bufferData gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (indexBuffer cov)
        bufferData gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return cov
        where dat = objPoints obj
    deleteView gl _ cov = do
        deleteBuffer gl (pointBuffer cov)
        deleteBuffer gl (indexBuffer cov)
