{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View where


import Control.Monad (liftM)
import Data.Bits (Bits(..))

import GHCJS.WebGL
import GHCJS.Foreign

import Geometry.Space
import Geometry.Space.Transform
import Program.Model.Camera (viewMatrix, Camera(..))


-- | Rendering global parameters
data ViewContext = ViewContext
    { glctx        :: !Ctx -- ^ WebGL context
    , projectArr   :: !(TypedArray GLfloat)
    , modelViewArr :: !(TypedArray GLfloat)
    , selector     :: !SelectorObject
    , sunDir       :: !(Vector3 GLfloat)
    , curState     :: !ViewState
    }

-- | View state snapshot
data ViewState = ViewState
    { vView      :: !(Matrix4x4 GLfloat)
    , vSunDir    :: !(Vector3 GLfloat)
    , vGLProjLoc :: UniformLocation
    , vGLViewLoc :: UniformLocation
    , vTime      :: !GLfloat
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


-- | Our meshes together with transforms could be drawn - so they implement this interface
class Drawable obj where
    -- | Object view contains data necessary for drawing
    type View obj
    -- | View creation should only require object itself and GL context
    createView :: Ctx -> obj -> IO (View obj)
    -- | Update view to correspond to current object
    updateView :: Ctx -> obj -> View obj -> IO (View obj)
    -- | Delete unmanaged objects in view, if any. obj may be undefined
    deleteView :: Ctx -> obj -> View obj -> IO ()
    -- | Draw the object (without setting up shaders)
    drawInCurrContext :: ViewContext -> obj -> View obj -> IO ()
    -- | Set up necessary context (e.g. shader params)
    updateDrawState   :: obj -> View obj -> ViewState -> ViewState


draw :: (Drawable obj) => ViewContext -> obj -> View obj -> IO ()
draw vc obj view = drawInCurrContext vc' obj view
    where vc' = vc{ curState = updateDrawState obj view $ curState vc}

class Drawable a => Selectable a where
    selectInCurrContext :: ViewContext -> a -> View a -> IO ()
    updateSelectState :: a -> View a -> ViewState -> ViewState

selectArea :: (Selectable obj) => ViewContext -> obj -> View obj -> IO ()
selectArea vc obj view = selectInCurrContext vc' obj view
    where vc' = vc{ curState = updateSelectState obj view $ curState vc}

instance ( SpaceTransform s GLfloat
         , Drawable obj
         ) => Drawable (STransform s GLfloat obj) where
    type View (STransform s GLfloat obj) = View obj
    createView gl = createView gl . unwrap
    drawInCurrContext w s view = applyTransform w s >>= \obj -> drawInCurrContext w obj view
    updateDrawState s = updateDrawState (unwrap s)
    updateView ctx s = updateView ctx (unwrap s)
    deleteView ctx s = deleteView ctx (f s)
        where f :: STransform s GLfloat obj -> obj
              f _ = undefined

instance ( SpaceTransform s GLfloat
         , Selectable obj
         ) => Selectable (STransform s GLfloat obj) where
    selectInCurrContext w s view = applyTransform w s >>= \obj -> selectInCurrContext w obj view
    updateSelectState s = updateSelectState (unwrap s)


-- | Create default world
setupViewContext :: Ctx
                 -> Camera-- ^ active camera
                 -> GLfloat -- ^ start time
                 -> Vector3 GLfloat -- ^ sun direction
                 -> IO ViewContext
setupViewContext gl cam t sd = do
    -- setup WebGL
    clearColor gl 0 0 0 0
    enable gl gl_DEPTH_TEST
    blendFunc gl gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
    depthFunc gl gl_LEQUAL
    viewport gl 0 0 vpWidth vpHeight
    -- create uniforms
    pptr <- newTypedArray 16
    mvptr <- newTypedArray 16
    -- create selector
    selB <- initSelectorFramebuffer gl vps
    pickedColorArr <- newTypedArray 4
    return ViewContext
        { glctx        = gl
        , projectArr   = pptr
        , modelViewArr = mvptr
        , selector     = SelectorObject selB
                                      pickedColorArr
        , sunDir       = sd
        , curState     = ViewState
            { vView      = eye
            , vSunDir    = sd
            , vGLProjLoc = undefined
            , vGLViewLoc = undefined
            , vTime      = t
            }
        }
    where vps@(Vector2 vpWidth vpHeight) = round <$> viewportSize cam

updateViewPortSize :: Camera-- ^ active camera
                   -> ViewContext
                   -> IO ViewContext
updateViewPortSize cam c@ViewContext
        { glctx = gl
        , selector = sobj@SelectorObject { sbuffer = sbuf}
        } = do
    viewport gl 0 0 vpWidth vpHeight
    deleteFramebuffer gl sbuf -- TODO: do proper delete and update of framebuffers
    sbuf' <- initSelectorFramebuffer gl vps
    return c{selector = sobj{sbuffer = sbuf'}}
    where vps@(Vector2 vpWidth vpHeight) = round <$> viewportSize cam


prepareRenderState :: ViewContext -> Camera -> GLfloat -> IO ViewContext
prepareRenderState vc@ViewContext
        { glctx    = gl
        , sunDir   = Vector3 sx sy sz
        , curState = cs
        } cam t = do
    fillTypedArray (projectArr vc) (projMatrix cam)
    fillTypedArray (modelViewArr vc) viewM
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    return vc
        { curState = cs
            { vView      = viewM
            , vSunDir    = Vector3 sx' sy' sz'
            , vTime      = t
            }
        }
    where viewM = viewMatrix cam
          Vector4 sx' sy' sz' _ = viewM `prod` Vector4 sx sy sz 0


applySelector :: (Selectable a)=> ViewContext -> Camera -> a -> View a -> IO ViewContext
applySelector vc'@ViewContext
        { glctx    = gl
        } cam obj view = do
    bindFramebuffer gl gl_FRAMEBUFFER (sbuffer $ selector vc)
    viewport gl 0 0 vpWidth vpHeight
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    selectArea vc obj view
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    viewport gl 0 0 vpWidth vpHeight
    return vc
    where Vector2 vpWidth vpHeight = round <$> viewportSize cam
          vc = vc'{curState = updateSelectState obj view (curState vc')}




newtype SelectionEvent = SelectionEvent Int
newtype SelectionConfirmEvent = SelectionConfirmEvent Int

getSelection :: ViewContext -> Camera -> Vector2 GLfloat -> IO SelectionEvent
getSelection ViewContext
    { glctx = gl
    , selector  = SelectorObject
        { sbuffer   = sbuf
        , pixProber = pcarr
        }
    } cam (Vector2 x y) = do
    bindFramebuffer gl gl_FRAMEBUFFER sbuf
    viewport gl 0 0 w h
    readPixels gl (round x) (fromIntegral h - round y) 1 1 gl_RGBA gl_UNSIGNED_BYTE pcarr
    r <- liftM fromIntegral $ getIdx pcarr 0
    g <- liftM fromIntegral $ getIdx pcarr 1
    b <- liftM fromIntegral $ getIdx pcarr 2
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    viewport gl 0 0 w h
    return . SelectionEvent $ r + shift g 8 + shift b 16
    where Vector2 w h = round <$> viewportSize cam



-- | Apply current transform of an object (including perspective) and save shader uniforms
applyTransform :: (SpaceTransform s GLfloat)
               => ViewContext -> STransform s GLfloat a -> IO a
applyTransform vc@(ViewContext{glctx = gl, curState = cs}) tr = do
        let MTransform matrix x = mergeSecond (MTransform (vView cs) id) tr
        fillTypedArray (modelViewArr vc) matrix
        uniformMatrix4fv gl (vGLViewLoc cs) False (modelViewArr vc)
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
