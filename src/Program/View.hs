{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.View
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.View where

import Data.Bits (Bits(..))
import Data.Coerce (coerce)

import Control.Arrow ((***))

--import JsHs.Types
import JsHs.WebGL
import qualified JsHs.Array as JS
---- import GHCJS.Foreign
--import GHCJS.Marshal.Pure (pFromJSVal)
import GHCJS.Useful (asVector)

import Data.Geometry
import Data.Geometry.Transform
import JsHs.TypedArray
import JsHs.TypedArray.IO
import JsHs.Nullable
import Program.Model.Camera (viewMatrix, Camera(..))
import Reactive.Banana.JsHs
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
--import Program.Types

-- | Rendering global parameters
data ViewContext = ViewContext
    { glctx        :: !WebGLRenderingContext -- ^ WebGL context
    , projectArr   :: !(IOTypedArray GLfloat)
    , modelViewArr :: !(IOTypedArray GLfloat)
    , selector     :: !SelectorObject
    , sunDir       :: !(Vector3 GLfloat)
    , curState     :: !ViewState
    }

-- | View state snapshot
data ViewState = ViewState
    { vView      :: !(Matrix4 GLfloat)
    , vSunDir    :: !(Vector3 GLfloat)
    , vGLProjLoc :: WebGLUniformLocation
    , vGLViewLoc :: WebGLUniformLocation
    , vTime      :: !Time
    }


-- | Shader attributes - to configure VBOs
data ShAttrLocations = ShAttrLocations {
        positionAttr :: !AttribLocation,
        colorAttr    :: !AttribLocation
    }

data SelectorObject = SelectorObject {
        sbuffer   :: !WebGLFramebuffer, -- buffer to draw selector to
       -- svalue    :: !WebGLUniformLocation,  -- selector value for object identification
        pixProber :: !(IOTypedArray GLubyte) -- typed array to read pixel to
    }

type AttribLocation = GLuint

-- | ID of an object that was clicked on last
selectedObjectIdBehavior :: Event PointerEvent
                         -> Behavior Camera
                         -> Behavior ViewContext
                         -> MomentIO (Behavior (Maybe Int))
selectedObjectIdBehavior pointerE cameraB viewB = do
    events <- fmap (fmap g)
            . mapEventIO id
            $ getSelection <$> viewB <*> cameraB <@> filterJust (f <$> pointerE)
    stepper Nothing events
  where
    g (SelectionEvent i) | i <= 0 = Nothing
                         | otherwise = Just i
    f (PointerClick p) | ptrs <- pointers p = if JS.length ptrs == 0
                                              then Nothing
                                              else Just $ asVector (ptrs JS.! 0)
    f _ = Nothing

-- | ID of an object under pointer when it is dragged
heldObjectIdBehavior :: Event PointerEvent
                     -> Behavior Camera
                     -> Behavior ViewContext
                     -> MomentIO (Behavior (Maybe Int))
heldObjectIdBehavior pointerE cameraB viewB = mdo
    -- on pointerFinish shows actual remaining points
    let releaseE = fmap (const Nothing)
                 . filterE (Nothing==)
                 . filterJust $ releaseF <$> pointerE
    pressE <- fmap (fmap g)
            . mapEventIO id
            $ getSelection <$> viewB <*> cameraB <@> filterJust (pressF <$> pointerE)
    stepper Nothing (unionWith (const id) pressE releaseE)
  where
    g (SelectionEvent i) | i <= 0 = Nothing
                         | otherwise = Just i
    releaseF (PointerUp p) = Just $ oneOrZero p
    releaseF (PointerClick p) = Just $ oneOrZero p
    releaseF (PointerCancel p) = Just $ oneOrZero p
    releaseF _ = Nothing
    pressF (PointerDown p) | ptrs <- pointers p = if JS.length ptrs <= 0
                                                  then Nothing
                                                  else Just $ asVector (ptrs JS.! 0)
    pressF _ = Nothing
    oneOrZero p | ptrs <- pointers p = if JS.length ptrs <= 1
                                       then Nothing
                                       else Just $ asVector (ptrs JS.! 0)

-- wheelE resizeE viewUpdateE

viewContextBehavior :: WebGLRenderingContext
                    -> Time -- ^ start time
                    -> (GLfloat,GLfloat) -- ^ initial vp size
                    -> Vector3 GLfloat -- ^ sun direction
                    -> Event ResizeEvent -- ^ resize
                    -> MomentIO (Behavior ViewContext)
viewContextBehavior gl t vpsize sd resEv = mdo
  iview <- liftIO $ setupViewContext gl vpsize t sd

  -- update viewport on canvas resize
  viewE <- mapEventIO (\(v, ResizeEvent c) -> updateViewPortSize c v)
                      $ fmap (,) viewB <@> resEv


  viewB <- stepper iview viewE
  return viewB


-- | Our meshes together with transforms could be drawn - so they implement this interface
class Drawable obj where
    -- | Object view contains data necessary for drawing
    type View obj
    -- | View creation should only require object itself and GL context
    createView :: WebGLRenderingContext -> obj -> IO (View obj)
    -- | Update view to correspond to current object
    updateView :: WebGLRenderingContext -> obj -> View obj -> IO (View obj)
    -- | Delete unmanaged objects in view, if any. obj may be undefined
    deleteView :: WebGLRenderingContext -> obj -> View obj -> IO ()
    -- | Draw the object (without setting up shaders)
    drawInCurrContext :: ViewContext -> obj -> View obj -> IO ()
    -- | Set up necessary context (e.g. shader params)
    updateDrawState   :: obj -> View obj -> ViewState -> ViewState
    -- | Update context and draw
    draw :: ViewContext -> obj -> View obj -> IO ()
    draw vc obj view = drawInCurrContext vc' obj view
        where vc' = vc{ curState = updateDrawState obj view $ curState vc}

class Drawable a => Selectable a where
    selectInCurrContext :: ViewContext -> a -> View a -> IO ()
    updateSelectState :: a -> View a -> ViewState -> ViewState

selectArea :: (Selectable obj) => ViewContext -> obj -> View obj -> IO ()
selectArea vc obj view = selectInCurrContext vc' obj view
    where vc' = vc{ curState = updateSelectState obj view $ curState vc}

instance ( SpaceTransform s 3 GLfloat
         , Drawable obj
         ) => Drawable (s obj) where
    type View (s obj) = View obj
    createView gl = createView gl . unwrap
    drawInCurrContext w s view = applyTransform w s >>= \obj -> drawInCurrContext w obj view
    updateDrawState s = updateDrawState (unwrap s)
    updateView ctx s = updateView ctx (unwrap s)
    deleteView ctx s = deleteView ctx (f s)
        where f :: s obj -> obj
              f _ = undefined

instance ( SpaceTransform s 3 GLfloat
         , Selectable obj
         ) => Selectable (s obj) where
    selectInCurrContext w s view = applyTransform w s >>= \obj -> selectInCurrContext w obj view
    updateSelectState s = updateSelectState (unwrap s)


-- | Create default world
setupViewContext :: WebGLRenderingContext
                 -> (GLfloat,GLfloat)  -- ^ active camera
                 -> Time -- ^ start time
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
    pptr <- newIOTypedArray 16
    mvptr <- newIOTypedArray 16
    -- create selector
    selB <- initSelectorFramebuffer gl vps
    pickedColorArr <- newIOTypedArray 4
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
    where vps@(vpWidth, vpHeight) = round *** round $ cam

updateViewPortSize :: Coords2D -- ^ active camera
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
    where vps@(vpWidth, vpHeight) = round *** round $ unpackCoords2D cam


clearScreen :: ViewContext -> IO ()
clearScreen c = clear (glctx c) (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

prepareRenderState :: ViewContext -> Camera -> Time -> IO ViewContext
prepareRenderState vc@ViewContext
        { sunDir   = unpackV3 -> (sx, sy, sz)
        , curState = cs
        } cam t = do
    setIndex 0 (projMatrix cam) (coerce $ projectArr vc)
    setIndex 0 viewM (coerce $ modelViewArr vc)
    return vc
        { curState = cs
            { vView      = viewM
            , vSunDir    = vector3 sx' sy' sz'
            , vTime      = t
            }
        }
    where viewM = viewMatrix cam
          (sx', sy', sz', _) = unpackV4 $ viewM `prod` vector4 sx sy sz 0



applySelector :: (Selectable a) => ViewContext -> Camera -> a -> View a -> IO ViewContext
applySelector vc'@ViewContext
        { glctx    = gl
        } cam obj view = do
    bindFramebuffer gl gl_FRAMEBUFFER (sbuffer $ selector vc)
    viewport gl 0 0 vpWidth vpHeight
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    selectArea vc obj view
    bindFramebuffer gl gl_FRAMEBUFFER nullRef
    viewport gl 0 0 vpWidth vpHeight
    return vc
    where (vpWidth, vpHeight) = round *** round $ viewportSize cam
          vc = vc'{curState = updateSelectState obj view (curState vc')}




newtype SelectionEvent = SelectionEvent Int
newtype SelectionConfirmEvent = SelectionConfirmEvent Int


-- Note, we render index (i+1) on screen, so that 0 encodes no object there
getSelection :: ViewContext -> Camera -> Vector2 GLfloat -> IO SelectionEvent
getSelection ViewContext
    { glctx = gl
    , selector  = SelectorObject
        { sbuffer   = sbuf
        , pixProber = pcarr
        }
    } cam (unpackV2 -> (x, y)) = do
    bindFramebuffer gl gl_FRAMEBUFFER sbuf
    viewport gl 0 0 w h
    readPixels gl (round x) (fromIntegral h - round y) 1 1 gl_RGBA gl_UNSIGNED_BYTE pcarr
    r <- fromIntegral <$> index 0 pcarr
    g <- fromIntegral <$> index 1 pcarr
    b <- fromIntegral <$> index 2 pcarr
    bindFramebuffer gl gl_FRAMEBUFFER nullRef
    viewport gl 0 0 w h
    return . SelectionEvent $ r + shift g 8 + shift b 16
    where (w,h) = round *** round $ viewportSize cam



-- | Apply current transform of an object (including perspective) and save shader uniforms
applyTransform :: (SpaceTransform s 3 GLfloat)
               => ViewContext -> s a -> IO a
applyTransform vc@(ViewContext{glctx = gl, curState = cs}) tr = do
        let MTransform matrix x = mergeSecond (MTransform (vView cs) id) tr
        setIndex 0 matrix (coerce $ modelViewArr vc)
        uniformMatrix4fv gl (vGLViewLoc cs) False (modelViewArr vc)
        return x



initSelectorFramebuffer :: WebGLRenderingContext -> (GLsizei, GLsizei) -> IO WebGLFramebuffer
initSelectorFramebuffer gl (width,height) = do
    fb <- createFramebuffer gl
    bindFramebuffer gl gl_FRAMEBUFFER fb
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D tex
    texImage2D gl gl_TEXTURE_2D 0 gl_RGBA width height 0 gl_RGBA gl_UNSIGNED_BYTE Nothing
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    framebufferTexture2D gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0
    bindTexture gl gl_TEXTURE_2D nullRef
--    rbc <- createRenderbuffer gl
--    bindRenderbuffer gl gl_RENDERBUFFER rbc
--    renderbufferStorage gl gl_RENDERBUFFER gl_RGBA4 width height
--    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_RENDERBUFFER rbc
    rbd <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER rbd
    renderbufferStorage gl gl_RENDERBUFFER gl_DEPTH_COMPONENT16 width height
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rbd
    bindRenderbuffer gl gl_RENDERBUFFER nullRef
    bindFramebuffer gl gl_FRAMEBUFFER nullRef
    return fb


initTexture :: WebGLRenderingContext -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei)) -> IO WebGLTexture
initTexture gl texdata = do
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D tex
    case texdata of
        Left img -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
        Right (arr, (w,h)) -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST
    bindTexture gl gl_TEXTURE_2D nullRef
    return tex

updateTexture :: WebGLRenderingContext
              -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei))
              -> WebGLTexture
              -> IO ()
updateTexture gl texdata tex = do
    bindTexture gl gl_TEXTURE_2D tex
    case texdata of
        Left img -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
        Right (arr, (w,h)) -> do
            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
    bindTexture gl gl_TEXTURE_2D nullRef
