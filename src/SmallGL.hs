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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module SmallGL
    ( RenderingApi (..), SmallGLInput (..)
    , createRenderingEngine
    ) where


import Unsafe.Coerce (unsafeCoerce)
import qualified GHCJS.DOM.JSFFI.Generated.Element as JSFFI

import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Animation as Animation
import JavaScript.WebGL

import Data.IORef


import Prelude hiding (unlines)
import Data.JSString hiding (length, map)
import JavaScript.TypedArray.ArrayBuffer

import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions


import Commons

data RenderingEngine = RenderingEngine
  { gl  :: !WebGLRenderingContext
    -- ^ WebGL context
  , vpSize :: !(GLsizei, GLsizei)
    -- ^ size of the viewport
  , matLoc :: !WebGLUniformLocation
  , posLoc :: !GLuint
  , colLoc :: !GLuint
  , trigbuf :: !(GLsizei, WebGLBuffer, Maybe WebGLBuffer)
  , rectbuf :: !(GLsizei, WebGLBuffer, Maybe WebGLBuffer)
  }

-- | Exposed functionality of
newtype RenderingApi = RenderingApi
  { render :: AnimationTime -> IO ()
  }


data SmallGLInput a where
    ViewPortResize :: SmallGLInput Animation.ResizeEvent


createRenderingEngine :: (MonadIO m, Reflex t, MonadIO (Performable m), PerformEvent t m)
                      => Element EventResult GhcjsDomSpace t -> EventSelector t SmallGLInput -> m RenderingApi
createRenderingEngine canvasElem evS = do
    gl <- getRenderingContext canvasElem
    curW <- floor <$> JSFFI.getClientWidth (_element_raw canvasElem)
    curH <- floor <$> JSFFI.getClientHeight (_element_raw canvasElem)
    let vpSize = (curW, curH)


    -- init shaders and get attribute locations
    (matLoc, posLoc, colLoc) <- liftIO $ initShaders gl
    -- create objects (including sending data to device)
    trigbuf <- liftIO $ createBuffers gl triangle
    rectbuf <- liftIO $ createBuffers gl rectangle


    let re = RenderingEngine {..}

    liftIO $ setupViewPort re
    rre <- liftIO $ newIORef re

    performEvent_ . ffor (select evS ViewPortResize) $ \(ResizeEvent newVPSize) -> liftIO $ do
        r' <- atomicModifyIORef' rre (\r -> let r' = r{vpSize = (floor *** floor) newVPSize } in (r', r'))
        setupViewPort r'

    return RenderingApi
        { render = \t -> readIORef rre >>= flip renderFunction t
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
    uniformMatrix4fv gl matLoc False eye
    liftIO $ uncurry (viewport gl 0 0) vpSize



renderFunction :: RenderingEngine -> AnimationTime -> IO ()
renderFunction RenderingEngine {..} _ = do
    -- clear viewport
    clear gl gl_COLOR_BUFFER_BIT
    -- supply shader with uniform matrix
    uniformMatrix4fv gl matLoc False eye
    -- draw objects
    drawBuffers gl (posLoc,colLoc) trigbuf
    drawBuffers gl (posLoc,colLoc) rectbuf


----------------------------------------------------------------------------------------------------



-- | geometry data type for our test: stores coordinates, colors, and indices (for indexed drawing)
data Geometry = Geometry {
    coords ::  [Vector GLfloat 3],
    colors ::  [Vector GLubyte 3],
    indices :: Maybe [GLushort]
    }

rectangle :: Geometry
rectangle = Geometry {
    coords = [ vec3 0.05 0.9  0
             , vec3 0.95 0.9  0
             , vec3 0.95 (-0.9) 0
             , vec3 0.05 (-0.9) 0],
    colors = [ vec3 255 0 0
             , vec3 0 255 0
             , vec3 0   0 255
             , vec3 0 127 127],
    indices = Just [0,1,2,0,2,3]
    }

triangle :: Geometry
triangle = Geometry {
    coords = [ vec3 (-0.95) (-0.8) 0
             , vec3 (-0.50)   0.8  0
             , vec3 (-0.05) (-0.8) 0],
    colors = [ vec3 255 0 0
             , vec3 0 255 0
             , vec3 0 0 255],
    indices = Nothing
    }

-- | initialize shader program and get attribute and uniform locations
initShaders :: WebGLRenderingContext -> IO (WebGLUniformLocation, GLuint, GLuint)
initShaders gl = do
    -- create program
    shaderProgram <- createProgram gl
    -- attach all shaders
    getShader gl gl_VERTEX_SHADER vertexShaderText >>= attachShader gl shaderProgram
    getShader gl gl_FRAGMENT_SHADER fragmentShaderText >>= attachShader gl shaderProgram
    -- link program
    linkProgram gl shaderProgram
    -- get attribute locations (getAttribLocation returns GLint, but we use always GLuint)
    posLoc <- fromIntegral <$> getAttribLocation gl shaderProgram "aVertexPosition"
    colorLoc <- fromIntegral <$> getAttribLocation gl shaderProgram "aVertexColor"
    -- get uniform locations
    matLoc <- getUniformLocation gl shaderProgram "uPMV"
    -- activate shader program
    useProgram gl shaderProgram
    -- enable attributes in shaders
    enableVertexAttribArray gl posLoc
    enableVertexAttribArray gl colorLoc
    return (matLoc, posLoc, colorLoc)

-- | Helper function to load shader: create, feed source code, and compile shader
getShader :: WebGLRenderingContext -> GLenum -> JSString -> IO WebGLShader
getShader gl t src = do
    shaderId <- createShader gl t
    shaderSource gl shaderId src
    compileShader gl shaderId
    return shaderId

-- | Pack points and colors tightly in one buffer:
--   3 floats of 4 bytes each for positions,
--   4 unsigned bytes for colors (aplha is 255)
packVertColors :: [Vector GLfloat 3] -> [Vector GLubyte 3]
               -> MutableArrayBuffer -> IO ()
packVertColors pnts cls buf = do
    SomeIODataFrame floatView <- viewFloatArray @'[4] buf
    SomeIODataFrame byteView  <- viewWord8Array @'[16] buf
    let f :: [Vector GLfloat 3] -> [Vector GLubyte 3] -> Int -> IO ()
        f (xyz:ps) (rgb:cs) i = do
            writeDataFrame floatView (1:!i:!Z) (xyz ! 1)
            writeDataFrame floatView (2:!i:!Z) (xyz ! 2)
            writeDataFrame floatView (3:!i:!Z) (xyz ! 3)
            writeDataFrame byteView (13:!i:!Z) (rgb ! 1)
            writeDataFrame byteView (14:!i:!Z) (rgb ! 2)
            writeDataFrame byteView (15:!i:!Z) (rgb ! 3)
            writeDataFrame byteView (16:!i:!Z)  255
            f ps cs (i+1)
        f _ _ _ = return ()
    f pnts cls 1

-- | Pack geometry data into buffers
createBuffers :: WebGLRenderingContext -> Geometry -> IO (GLsizei, WebGLBuffer, Maybe WebGLBuffer)
createBuffers gl Geometry { coords = pts, colors = cls, indices = mids } = do
    -- allocate host buffer for coordinates and colors
    barr <- newArrayBuffer (size * 16)
    packVertColors pts cls barr
    -- create and bind device buffer
    buf <- createBuffer gl
    bindBuffer gl gl_ARRAY_BUFFER buf
    -- send data to device
    bufferData gl gl_ARRAY_BUFFER barr gl_STATIC_DRAW
    -- the same for indices
    case fromList . fmap scalar <$> mids :: Maybe (DataFrame GLushort '[XN 2]) of
        Nothing -> return (fromIntegral size, buf, Nothing)
        Just (SomeDataFrame ids) -> do
            ibuf <- createBuffer gl
            bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
            idsbuf <- thawDataFrame ids >>= arrayBuffer
            bufferData gl gl_ELEMENT_ARRAY_BUFFER idsbuf gl_STATIC_DRAW
            return (fromIntegral $ totalDim ids, buf, Just ibuf)
        where size = length pts

-- | Draw our tiny geometry
drawBuffers :: WebGLRenderingContext -> (GLuint, GLuint) -> (GLsizei, WebGLBuffer, Maybe WebGLBuffer) -> IO ()
drawBuffers gl (posLoc, colLoc) (size,buf,mibuf) = do
    -- bind packed buffer with interleaved array inside
    bindBuffer gl gl_ARRAY_BUFFER buf
    vertexAttribPointer gl posLoc 3 gl_FLOAT False 16 0
    vertexAttribPointer gl colLoc 4 gl_UNSIGNED_BYTE True 16 12
    -- bind indices if needed and draw everything
    case mibuf of
        Nothing -> drawArrays gl gl_TRIANGLES 0 size
        Just ibuf -> do
            bindBuffer gl gl_ELEMENT_ARRAY_BUFFER ibuf
            drawElements gl gl_TRIANGLES size gl_UNSIGNED_SHORT 0

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
    attribute vec3 aVertexPosition;
    attribute vec4 aVertexColor;
    uniform mat4 uPMV;
    varying vec4 vColor;
    void main(void) {
      gl_Position = uPMV * vec4(aVertexPosition, 1.0);
      vColor = aVertexColor;
    }
  |]

