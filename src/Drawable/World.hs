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

import GHCJS.Types
--import GHCJS.Marshal
--import Unsafe.Coerce

import Foreign
import Data.IORef
import qualified Data.Foldable as F
import qualified Control.Monad as M


import Geometry.Space
import Geometry.Space.Transform

--import SmallGL.Helpers
--import SmallGL.Shader



-- | World global parameters
data World = forall cam . Camera cam => World
    { glctx        :: !Ctx -- ^ WebGL context
    , cameraRef    :: !(IORef cam)
    , projectLoc   :: !(TypedArray GLfloat)
    , modelViewLoc :: !(TypedArray GLfloat)
    , selector     :: !SelectorObject
    , sunDir       :: !(Vector3 GLfloat)
    , curContext   :: !WorldContext
    }

-- | World state snapshot
data WorldContext = WorldContext
    { wView    :: !(Matrix4x4 GLfloat)
    , wSunDir  :: !(Vector3 GLfloat)
    , wProjLoc :: UniformLocation
    , wViewLoc :: UniformLocation
    , wTime    :: !GLfloat
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
    drawInCurrContext :: World -> a -> IO ()
    updateDrawContext :: a -> World -> World


draw :: (Drawable a) => World -> a -> IO ()
draw w a = drawInCurrContext w' a
    where w' = updateDrawContext a w

class Selectable a where
    selectInCurrContext :: World -> a -> IO ()
    updateSelectContext :: a -> World -> World

selectArea :: (Selectable a) => World -> a -> IO ()
selectArea w a = selectInCurrContext w' a
    where w' = updateSelectContext a w

data Drawing = forall a . (Drawable a) => Draw a

instance Drawable Drawing where
    drawInCurrContext w (Draw a) = drawInCurrContext w a
    updateDrawContext (Draw a) = updateDrawContext a

instance ( SpaceTransform s GLfloat
         , Drawable d
         ) => Drawable (STransform s GLfloat d) where
    drawInCurrContext w s = applyTransform w s >>= drawInCurrContext w
    updateDrawContext s w = updateDrawContext (unwrap s) w

instance ( SpaceTransform s GLfloat
         , Selectable d
         ) => Selectable (STransform s GLfloat d) where
    selectInCurrContext w s = applyTransform w s >>= selectInCurrContext w
    updateSelectContext s w = updateSelectContext (unwrap s) w

-- | Create default world
initWorld :: Camera cam
          => Ctx
          -> IORef cam -- ^ active camera
          -> GLfloat -- ^ start time
          -> Vector3 GLfloat -- ^ sun direction
          -> IO World
initWorld gl c t sd = do
    -- create uniforms
    pptr <- newTypedArray 16
    mvptr <- newTypedArray 16
    -- create selector
    s <- M.liftM viewSize $ readIORef c
    selB <- initSelectorFramebuffer gl s
    pickedColorArr <- newTypedArray 4
    return World
        { glctx        = gl
        , cameraRef    = c
        , projectLoc   = pptr
        , modelViewLoc = mvptr
        , selector     = SelectorObject selB
                                      pickedColorArr
        , sunDir       = sd
        , curContext   = WorldContext
            { wView    = eye
            , wSunDir  = sd
            , wProjLoc = undefined
            , wViewLoc = undefined
            , wTime    = t
            }
        }

foreign import javascript safe "console.log($1)"
    printRef' :: JSRef a -> IO ()

-- | This function is called every frame to set up correct matrices and time
prepareWorldRender :: World -> GLfloat -> IO World
prepareWorldRender w@(World
        { cameraRef = cRef
        , glctx = gl
        , curContext = cc
        , sunDir = Vector3 sx sy sz
        }) t = do
    cam <- readIORef cRef
    let viewM = prepareView cam
        Vector4 sx' sy' sz' _ = viewM `prod` Vector4 sx sy sz 0
    fillTypedArray (projectLoc w) (prepareProjection cam)
    fillTypedArray (modelViewLoc w) viewM
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    return w {
        curContext = cc
            { wView    = viewM
            , wSunDir  = Vector3 sx' sy' sz'
            , wTime    = t
            }
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
               => World -> STransform s GLfloat a -> IO a
applyTransform w@(World{glctx = gl, curContext = cc}) tr = do
        let MTransform matrix x = mergeSecond (MTransform (wView cc) id) tr
        fillTypedArray (modelViewLoc w) matrix
        uniformMatrix4fv gl (wViewLoc cc) False (modelViewLoc w)
        return x



initSelectorFramebuffer :: Ctx -> Vector2 GLsizei -> IO FrameBuffer
initSelectorFramebuffer gl (Vector2 width height) = do
    fb <- createFramebuffer gl
    bindFramebuffer gl gl_FRAMEBUFFER fb
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D tex
    texImage2D gl gl_TEXTURE_2D 0 gl_RGBA width height 0 gl_RGBA gl_UNSIGNED_BYTE jsNull
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    framebufferTexture2D gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0
    bindTexture gl gl_TEXTURE_2D jsNull
--    rbc <- createRenderbuffer gl
--    bindRenderbuffer gl gl_RENDERBUFFER rbc
--    renderbufferStorage gl gl_RENDERBUFFER gl_RGBA4 width height
--    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_RENDERBUFFER rbc
    rbd <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER rbd
    renderbufferStorage gl gl_RENDERBUFFER gl_DEPTH_COMPONENT16 width height
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rbd
    bindRenderbuffer gl gl_RENDERBUFFER jsNull
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    return fb

-- void texImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height
----   , GLint border, GLenum format, GLenum type, ArrayBufferView? pixels)
--foreign import javascript unsafe "$1.texImage2D($2, $3, $4, $5, $6, $7, $8, $9, $10)"
--    texImage2D :: Ctx -> GLenum -> GLint-> GLenum -> GLsizei-> GLsizei-> GLint-> GLenum -> GLenum -> TypedArray a -> IO ()

initTexture :: Ctx -> Either TexImageSource (TypedArray GLubyte, Vector2 GLsizei) -> IO Texture
initTexture gl texdata = do
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D tex
    case texdata of
        Left img -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
        Right (arr, Vector2 w h) -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE arr
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    bindTexture gl gl_TEXTURE_2D jsNull
    return tex

updateTexture :: Ctx
              -> Either TexImageSource (TypedArray GLubyte, Vector2 GLsizei)
              -> Texture
              -> IO ()
updateTexture gl texdata tex = do
    bindTexture gl gl_TEXTURE_2D tex
    case texdata of
        Left img -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
        Right (arr, Vector2 w h) -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE arr
    bindTexture gl gl_TEXTURE_2D jsNull
