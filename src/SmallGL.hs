-----------------------------------------------------------------------------
-- |
-- Module      :  SmallGL
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- * WebGL rendering backend
--
-- This module and the whole module tree behind it is responsible solely for rendering abstract geometry.
-- It should not know anything about other qua-view modules;
-- that is, non of SmallGL.* modules should import anything from other modules in this project.
--
-- Rendering engine consumes various input events, such as view matrix transformations, or object buffer updates.
-- It exposes only rendering callback function and current selected object id.
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module SmallGL
    ( ProjMatrix (..), ViewMatrix (..)
    , RenderingApi (..), SmallGLInput (..)
    , createRenderingEngine
    ) where

import Control.Monad.Trans.State.Strict
import Unsafe.Coerce (unsafeCoerce)
import qualified GHCJS.DOM.JSFFI.Generated.Element as JSFFI

import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Animation as Animation
import JavaScript.WebGL

import Data.IORef
import Data.Bits

import Prelude hiding (unlines)
import Data.JSString hiding (length, map)

import Numeric.DataFrame
import Numeric.DataFrame.IO
import qualified Numeric.Matrix as M
--import Numeric.Dimensions


import Commons
import SmallGL.Types
import SmallGL.Shader

import SmallGL.RenderingCell

data RenderingEngine = RenderingEngine
  { gl       :: !WebGLRenderingContext
    -- ^ WebGL context
  , vpSize   :: !(GLsizei, GLsizei)
    -- ^ size of the viewport
  , uProjLoc :: !WebGLUniformLocation
  , uViewLoc :: !WebGLUniformLocation
  , uProjM   :: !ProjMatrix
  , uViewM   :: !ViewMatrix
  , rCell    :: !(RenderingCell ModeColored)
  }



-- | Exposed functionality of
data RenderingApi = RenderingApi
  { render     :: AnimationTime -> IO ()
  , addRObject :: RenderingData ModeColored -> IO RenderedObjectId
  }


data SmallGLInput a where
    ViewPortResize      :: SmallGLInput Animation.ResizeEvent
    -- ^ Every time windows is resized
    ProjTransformChange :: SmallGLInput ProjMatrix
    -- ^ Camera updates of viewport projection
    ViewTransformChange :: SmallGLInput ViewMatrix
    -- ^ Camera motions



createRenderingEngine :: (MonadIO m, Reflex t, MonadIO (Performable m), PerformEvent t m)
                      => Element EventResult GhcjsDomSpace t -> EventSelector t SmallGLInput -> m RenderingApi
createRenderingEngine canvasElem evS = do
    gl <- getRenderingContext canvasElem
    curW <- floor <$> JSFFI.getClientWidth (_element_raw canvasElem)
    curH <- floor <$> JSFFI.getClientHeight (_element_raw canvasElem)
    let vpSize = (curW, curH)


    -- init shaders and get attribute locations
    program <-liftIO $ initShaders gl [(gl_VERTEX_SHADER, vertexShaderText)
                                      ,(gl_FRAGMENT_SHADER, fragmentShaderText)
                                      ]
                                      [(attrLocCoords, "aVertexPosition")
                                      ,(attrLocNormals,"aVertexNormal")
                                      ,(attrLocColors, "aVertexColor")
                                      ]
    -- create objects (including sending data to device)
    rCell <- fmap snd . liftIO . (initRenderingCell gl >>=) . runStateT $ do
       rectData <- liftIO rectangle
       rId1 <- StateT $ addRenderedObject gl rectData
       rId2 <- StateT $ addRenderedObject gl rectData
       rId3 <- StateT $ addRenderedObject gl rectData
       rId4 <- StateT $ addRenderedObject gl rectData
       rId5 <- StateT $ addRenderedObject gl rectData
       rId6 <- StateT $ addRenderedObject gl rectData
       rId7 <- StateT $ addRenderedObject gl rectData
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId3 (M.translate3 $ vec3 0 20 0)
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId2 (M.translate4 $ vec4 0 10 0 0)
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId4 (M.translate4 $ vec4 0 (-10) 0 0)
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId5 (M.translate4 $ vec4 0 (-20) 0 0)
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId6 (M.translate4 $ vec4 0 (-30) 0 0)
       StateT $ \c -> flip (,) c <$> transformRenderedObject gl c rId7 (M.translate4 $ vec4 0 (-40) 0 0)
       StateT $ \c -> flip (,) c <$> setRenderedObjectColor gl c rId1 (vec4 255 0 25 255)
       StateT $ \c -> flip (,) c <$> setRenderedObjectColor gl c rId2 (vec4 0 0 25 127)
       StateT $ \c -> (,) () <$> deleteRenderedObject gl c rId4
       StateT $ \c -> (,) () <$> deleteRenderedObject gl c rId7
       return ()



    let uProjLoc = unifLoc program "uProjM"
        uViewLoc = unifLoc program "uViewM"
        uProjM = ProjM eye
        uViewM = ViewM eye
        re = RenderingEngine {..}

    liftIO $ do
        useProgram gl $ programId program
        enableCoordsNormalsBuf gl
        enableColorsBuf gl
        setupViewPort re
    rre <- liftIO $ newIORef re

    -- update camera matrices
    performEvent_ . ffor (select evS ProjTransformChange) $ \m -> liftIO $
        atomicModifyIORef' rre (\r -> (r{uProjM = m}, ()))
    performEvent_ . ffor (select evS ViewTransformChange) $ \m -> liftIO $
        atomicModifyIORef' rre (\r -> (r{uViewM = m}, ()))


    performEvent_ . ffor (select evS ViewPortResize) $ \(ResizeEvent newVPSize) -> liftIO $ do
        r' <- atomicModifyIORef' rre (\r -> let r' = r{vpSize = (floor *** floor) newVPSize } in (r', r'))
        setupViewPort r'

    return RenderingApi
        { render = \t -> readIORef rre >>= flip renderFunction t
        , addRObject = addRObjectFunction rre
        }



-- | Create a WebGL rendering context for a canvas
getRenderingContext :: MonadIO m => Element r s t -> m WebGLRenderingContext
getRenderingContext = liftIO . getWebGLContext . unsafeCoerce . _element_raw


setupViewPort :: RenderingEngine -> IO ()
setupViewPort RenderingEngine {..} = do
    -- setup WebGL
    clearColor gl 0 0 0 0
    enable gl gl_DEPTH_TEST
    enable gl gl_BLEND
    blendFunc gl gl_ONE gl_ONE_MINUS_SRC_ALPHA
    depthFunc gl gl_LEQUAL
    -- supply shader with uniform matrix
    uniformMatrix4fv gl uProjLoc False (getProjM uProjM)
    uniformMatrix4fv gl uViewLoc False (getViewM uViewM)
    uncurry (viewport gl 0 0) vpSize



renderFunction :: RenderingEngine -> AnimationTime -> IO ()
renderFunction RenderingEngine {..} _ = do
    -- clear viewport
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    -- supply shader with uniform matrix
    uniformMatrix4fv gl uProjLoc False (getProjM uProjM)
    uniformMatrix4fv gl uViewLoc False (getViewM uViewM)
    -- draw objects
    renderCell gl rCell

addRObjectFunction :: IORef RenderingEngine
                   -> RenderingData ModeColored -> IO RenderedObjectId
addRObjectFunction rre cd = do
    re <- readIORef rre
    (roId, rc') <- addRenderedObject (gl re) cd (rCell re)
    writeIORef rre $ re { rCell = rc'}
    return roId

----------------------------------------------------------------------------------------------------

rectangle :: IO (RenderingData ModeColored)
rectangle
  | SomeDataFrame ixs <- fromList [0,1,2,0,2,3::Scalar GLushort]
  , crsnrs <-   (vec4  5 9  0 1 <::> vec4 0 0 1 0)
           <::> (vec4 19 9  0 1 <::> vec4 0 0 1 0)
           <+:> (vec4 19 0  0 1 <::> vec4 0 0 1 0)
           <+:> (vec4  5 0  0 1 <::> vec4 0 0 1 0)
  , colors <-   vec4 255 0 0 255
           <::> vec4 0 255 0 255
           <+:> vec4 0 0 255 255
           <+:> vec4 0 127 127 255
  = do
    crsnrs' <- thawDataFrame crsnrs
    colors' <- thawDataFrame colors
    ixs'    <- thawDataFrame ixs
    return $ ColoredData (CoordsNormals crsnrs') (Colors colors') (Indices ixs')




fragmentShaderText :: JSString
fragmentShaderText =
  [jsstring|
     precision mediump float;
     varying vec4 vColor;
     void main(void) {
       gl_FragColor = vColor;
     }
  |]

vertexShaderText :: JSString
vertexShaderText =
  [jsstring|
    attribute vec4 aVertexPosition;
    attribute vec4 aVertexNormal;
    attribute vec4 aVertexColor;
    uniform mat4 uViewM;
    uniform mat4 uProjM;
    uniform mat4 uPMV;
    varying vec4 vColor;
    void main(void) {
      gl_Position = uProjM * uViewM * aVertexPosition;
      vColor = aVertexColor + 0.001*aVertexNormal;
    }
  |]

