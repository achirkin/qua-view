{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
--
-- Module      :  Drawable.World
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

module Drawable.World where


import GHCJS.WebGL
import GHCJS.Foreign

import Foreign
import Data.IORef
import qualified Data.Foldable as F
import qualified Control.Monad as M


import Geometry.Space
import Geometry.Space.Transform

--import SmallGL.Helpers
--import SmallGL.Shader

-- | World global parameters
data World = forall cam . Camera cam => World {
    glctx        :: !Ctx, -- ^ WebGL context
    cameraRef    :: !(IORef cam),
    currentTime  :: !GLfloat,
    currentView  :: !(Matrix4x4 GLfloat),
    projectLoc   :: !(TypedArray GLfloat),
    modelViewLoc :: !(TypedArray GLfloat),
    selector     :: !SelectorObject
    }


data WorldContext = WorldContext
    { wview    :: !(Matrix4x4 GLfloat)
    , wsundir  :: !(Vector4 GLfloat)
    , wprojLoc :: !UniformLocation
    , wviewLoc :: !UniformLocation
    , wtime    :: !GLfloat
    }


-- | Shader attributes - to configure VBOs
data ShAttrLocations = ShAttrLocations {
        positionAttr :: !AttribLocation,
        colorAttr    :: !AttribLocation
    }

data SelectorObject = SelectorObject {
        sbuffer   :: !FrameBuffer, -- buffer to draw selector to
       -- svalue    :: !UniformLocation,  -- selector value for object identification
        pixProber :: !(TypedArray GLubyte) -- typed array to read pixel to
    }

type AttribLocation = GLuint

-- | Class used by WorldContext to get projection and view matrices
class Camera c where
    -- | Create a view matrix
    prepareView :: c -> Matrix4x4 GLfloat
    -- | Create a projection matrix
    prepareProjection :: c -> Matrix4x4 GLfloat
    -- | Viewport size
    viewSize :: c -> Vector2 GLsizei


-- | Our meshes together with transforms could be drawn - so they implement this interface
class Drawable a where
    draw :: World -> a -> IO ()

class Selectable a where
    selectArea :: World -> a -> IO ()

data Drawing = forall a . (Drawable a) => Draw a

instance Drawable Drawing where
    draw w (Draw a) = draw w a


--instance ( SpaceTransform s GLfloat
--         , Drawable d
--         ) => Drawable (s d) where
--    draw w s = applyTransform w s >>= draw w

--instance ( SpaceTransform s GLfloat
--         , Selectable d
--         ) => Selectable (s d) where
--    selectArea w s = applyTransform w s >>= selectArea w

-- | Create default world
initWorld :: Camera cam
          => Ctx
          -> IORef cam -- ^ active camera
          -> GLfloat -- ^ current time
          -> IO World
initWorld gl c t = do
    -- create uniforms
    pptr <- newTypedArray 16
    mvptr <- newTypedArray 16
    -- create selector
    s <- M.liftM viewSize $ readIORef c
    selB <- initSelectorFramebuffer gl s
    pickedColorArr <- newTypedArray 4 --mallocArrayBuffer 4 >>= typedArrayView
    return World {
        glctx        = gl,
        cameraRef    = c,
        currentTime  = t,
        currentView  = eye,
        projectLoc   = pptr,
        modelViewLoc = mvptr,
        selector     = SelectorObject selB
                                      --(unifLoc p "uModelViewM") -- (unifLoc p "uSelector")
                                      pickedColorArr
    }



-- | This function is called every frame to set up correct matrices and time
prepareWorldRender :: World -> GLfloat -> IO World
prepareWorldRender w@(World{cameraRef = cRef, glctx = gl}) t = do
    cam <- readIORef cRef
    let viewM = prepareView cam
    fillTypedArray (projectLoc w) (prepareProjection cam)
    fillTypedArray (modelViewLoc w) viewM
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    return w {
        currentTime = t,
        currentView = viewM
    }

applySelector :: (F.Foldable s, Selectable a)=> World -> s a -> IO ()
applySelector wrld@(World{glctx = gl, cameraRef = camr}) xs = do
    Vector2 w h <- M.liftM viewSize . readIORef $ camr
    bindFramebuffer gl gl_FRAMEBUFFER (sbuffer $ selector wrld)
    viewport gl 0 0 w h
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    F.mapM_ (selectArea wrld) xs
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    viewport gl 0 0 w h


-- | Apply current transform of an object (including perspective) and save shader uniforms
applyTransform :: (SpaceTransform s GLfloat)
               => World
               -> UniformLocation
               -> s a -> IO a
applyTransform w@(World{glctx = gl}) mloc tr = do
        let MTransform matrix x = transform (MTransform (currentView w) id) tr
        fillTypedArray (modelViewLoc w) matrix
        uniformMatrix4fv gl mloc False (modelViewLoc w)
        return x


initSelectorFramebuffer :: Ctx -> Vector2 GLsizei -> IO FrameBuffer
initSelectorFramebuffer gl (Vector2 width height) = do
    fb <- createFramebuffer gl
    bindFramebuffer gl gl_FRAMEBUFFER fb
    rbc <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER rbc
    renderbufferStorage gl gl_RENDERBUFFER gl_RGBA4 width height
    rbd <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER rbd
    renderbufferStorage gl gl_RENDERBUFFER gl_DEPTH_COMPONENT16 width height
    bindRenderbuffer gl gl_RENDERBUFFER jsNull
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_RENDERBUFFER rbc
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rbd
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    return fb



initTexture :: Ctx -> TexImageSource -> IO Texture
initTexture gl img = do
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D tex
    pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
    texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    bindTexture gl gl_TEXTURE_2D jsNull
    return tex


