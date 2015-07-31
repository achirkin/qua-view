{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View.CityObjectView
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View.CityObjectView where

import GHCJS.WebGL

import Program.Model.CityObject
import Program.View


data CityObjectView = CityObjectView !Buffer !Buffer

instance Show CityObjectView where
    show _ = "CityObjectView"



drawSurface :: Ctx -> (GLuint,Maybe (GLuint,GLuint) ) -> CityObject -> View CityObject -> IO ()
drawSurface gl (ploc,olocs) obj (CityObjectView buf ibuf) = do
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl ploc 3 gl_FLOAT False 20 0
    case olocs of
        Just (nloc,tloc) -> do
            vertexAttribPointer gl nloc 3 gl_BYTE True 20 12
            vertexAttribPointer gl tloc 2 gl_UNSIGNED_SHORT True 20 16
        Nothing -> return ()
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
    drawElements gl gl_TRIANGLES (indexArrayLength $ points obj) gl_UNSIGNED_SHORT 0


instance Drawable CityObject where
    type View CityObject = CityObjectView
    createView gl obj = do
        buf <- createBuffer gl
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferDataS gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        ibuf <- createBuffer gl
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
        bufferDataS gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return $ CityObjectView buf ibuf
        where dat = points obj
    drawInCurrContext _ _ _ = undefined
    updateDrawState _ _ _ = undefined
    updateView gl obj cov@(CityObjectView buf ibuf) = do
        bindBuffer gl gl_ARRAY_BUFFER buf
        bufferDataS gl gl_ARRAY_BUFFER (vertexArray dat) gl_STATIC_DRAW
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
        bufferDataS gl gl_ELEMENT_ARRAY_BUFFER (indexArray dat) gl_STATIC_DRAW
        return cov
        where dat = points obj
    deleteView gl _ (CityObjectView buf ibuf) = do
        deleteBuffer gl buf
        deleteBuffer gl ibuf
