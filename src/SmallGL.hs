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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SmallGL
    ( ProjMatrix (..), ViewMatrix (..)
    , RenderingApi (addRObject, getHoveredSelId, render)
    , QEventTag (..)
    , createRenderingEngine
    ) where

import Control.Monad.Trans.State.Strict
import Control.Concurrent.MVar
import Unsafe.Coerce (unsafeCoerce)
import qualified GHCJS.DOM.JSFFI.Generated.Element as JSFFI

import Reflex.Class
import Reflex.Dom
import Reflex.Dom.Widget.Animation as Animation
import JavaScript.WebGL

import Data.Bits


import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Prelude hiding (unlines)
import Data.JSString hiding (length, map)

import Numeric.DataFrame
import Numeric.DataFrame.IO
import qualified Numeric.Matrix as M
import Numeric.Dimensions


import Commons
import SmallGL.Types
import SmallGL.Shader

import SmallGL.RenderingCell

data RenderingEngine = RenderingEngine
  { gl          :: !WebGLRenderingContext
    -- ^ WebGL context
  , vpSize      :: !(GLsizei, GLsizei)
    -- ^ size of the viewport
  , viewProgram :: !RenderingProgram
  , selProgram  :: !RenderingProgram
  , selectorObj :: !SelectorObject
  , uProjM      :: !ProjMatrix
  , uViewM      :: !ViewMatrix
  , rCell       :: !(RenderingCell ModeColored)
  , geomCache   :: !(IntMap (SomeIODataFrame Float '[N 4, XN 0]))
    -- ^ Last submitted versions of active geometries
  }

data RenderingProgram = RenderingProgram
  { shader      :: !ShaderProgram
  , uProjLoc    :: !WebGLUniformLocation
  , uViewLoc    :: !WebGLUniformLocation
  , uCustomLoc3 :: WebGLUniformLocation
  , uCustomLoc4 :: WebGLUniformLocation
  }

uSunDirLoc :: RenderingProgram -> WebGLUniformLocation
uSunDirLoc = uCustomLoc3

uClippingDist :: RenderingProgram -> WebGLUniformLocation
uClippingDist = uCustomLoc4

data SelectorObject = SelectorObject
  { selFrameBuf  :: !WebGLFramebuffer
  , selUbyteView :: !(IODataFrame GLubyte '[4])
  , selUintView  :: !(IODataFrame GLuint '[])
  }

-- | Exposed functionality of
data RenderingApi = RenderingApi
  { render          :: AnimationTime -> IO ()
  , addRObject      :: ObjRenderingData ModeColored -> IO RenderedObjectId
  , getHoveredSelId :: (GLint, GLint) -> IO GLuint
  , setObjectColor  :: [(RenderedObjectId, Vector GLubyte 4)] -> IO ()
  , transformObject :: [(RenderedObjectId, Mat44f)] -> IO ()
  , resetGeomCache  :: IO ()
  , reset           :: IO ()
  }


-- | Some changes that affect WebGL rendering.
data instance QEventTag SmallGLInput a where
    -- | Every time windows is resized
    ViewPortResize        :: QEventTag SmallGLInput Animation.ResizeEvent
    -- | Camera updates of viewport projection
    ProjTransformChange   :: QEventTag SmallGLInput ProjMatrix
    -- | Camera motions
    ViewTransformChange   :: QEventTag SmallGLInput ViewMatrix
    -- | Move objects
    TransformObject       :: QEventTag SmallGLInput [(RenderedObjectId, Mat44f)]
    -- | Move objects
    SetObjectColor        :: QEventTag SmallGLInput [(RenderedObjectId, Vector GLubyte 4)]
    -- | Persist o
    PersistGeomTransforms :: QEventTag SmallGLInput [(RenderedObjectId, Mat44f)]
    -- | Clean up all viewed geometry, empty all buffers, release resource
    ResetGL               :: QEventTag SmallGLInput ()

deriveEvent ''SmallGLInput

createRenderingEngine :: (MonadIO m, Reflex t, MonadIO (Performable m), PerformEvent t m)
                      => Element EventResult GhcjsDomSpace t
                      -> QuaViewT Writing t m RenderingApi
createRenderingEngine canvasElem = do
    gl <- getRenderingContext canvasElem
    curW <- floor <$> JSFFI.getClientWidth (_element_raw canvasElem)
    curH <- floor <$> JSFFI.getClientHeight (_element_raw canvasElem)
    let vpSize = (curW, curH)


    -- init shaders and get attribute locations
    viewProgram <-liftIO $ do
        shader <- initShaders gl [(gl_VERTEX_SHADER, vertexShaderText)
                              ,(gl_FRAGMENT_SHADER, fragmentShaderText)
                              ]
                              [(attrLocCoords, "aVertexPosition")
                              ,(attrLocNormals,"aVertexNormal")
                              ,(attrLocColors, "aVertexColor")
                              ]
        let uProjLoc = unifLoc shader "uProjM"
            uViewLoc = unifLoc shader "uViewM"
            uCustomLoc3 = unifLoc shader "uSunDir"
            uCustomLoc4 = unifLoc shader "uClippingDist"
        return RenderingProgram {..}
    selProgram <- liftIO $ do
        shader <- initShaders gl [(gl_VERTEX_SHADER, vertexSelShaderText)
                              ,(gl_FRAGMENT_SHADER, fragmentSelShaderText)
                              ]
                              [(attrLocCoords, "aVertexPosition")
                              ,(attrLocSelIds, "aSelector")
                              ]
        let uProjLoc = unifLoc shader "uProjM"
            uViewLoc = unifLoc shader "uViewM"
            uCustomLoc3 = error "Selector shader does not have third uniform location."
            uCustomLoc4 = error "Selector shader does not have fourth uniform location."
        return RenderingProgram {..}
    selectorObj <- liftIO $ do
      selFrameBuf <- initSelectorFramebuffer gl vpSize
      selUbyteView <- newDataFrame
      selUintView <- arrayBuffer selUbyteView >>= viewWord32Array @'[] >>=
          \(SomeIODataFrame d) -> return (unsafeCoerce d)
      return SelectorObject {..}

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



    let uProjM = ProjM eye
        uViewM = ViewM eye
        geomCache = mempty
        re = RenderingEngine {..}

    liftIO $ do
        useProgram gl $ programId $ shader viewProgram
        enableCoordsNormalsBuf gl
        enableColorsBuf gl
        setupViewPort re
    rre <- liftIO $ newMVar re
    let rApi = RenderingApi
            { render = withMVar rre . renderFunction
            , addRObject = modifyMVar rre . addRObjectFunction
            , getHoveredSelId = withMVar rre . getSelection
            , setObjectColor = withMVar rre . setObjectColor'
            , transformObject = modifyMVar_ rre . transformObject'
            , resetGeomCache = modifyMVar_ rre $ \r -> pure r{geomCache = mempty}
            , reset = modifyMVar_ rre resetCells
            }

    -- React on all SmallGL input events
    projTransformChangeE <- askEvent $ SmallGLInput ProjTransformChange
    performEvent_ . ffor projTransformChangeE $ \m -> liftIO $
        modifyMVar_ rre (\r -> return r{uProjM = m})

    viewTransformChangeE <- askEvent $ SmallGLInput ViewTransformChange
    performEvent_ . ffor viewTransformChangeE $ \m -> liftIO $
        modifyMVar_ rre (\r -> return r{uViewM = m})

    viewPortResizeE <- askEvent $ SmallGLInput ViewPortResize
    performEvent_ . ffor viewPortResizeE $ \(ResizeEvent newVPSize) ->
        liftIO . modifyMVar_ rre $ \r ->
           let r' = r{vpSize = (floor *** floor) newVPSize } in r' <$ setupViewPort r'

    askEvent (SmallGLInput TransformObject)
      >>= performEvent_ . fmap (liftIO . transformObject rApi)

    askEvent (SmallGLInput SetObjectColor)
      >>= performEvent_ . fmap (liftIO . setObjectColor rApi)

    askEvent (SmallGLInput PersistGeomTransforms)
      >>= performEvent_ . fmap (\xs -> liftIO $ do
                                         transformObject rApi xs
                                         resetGeomCache rApi
                               )

    askEvent (SmallGLInput ResetGL)
      >>= performEvent_ . (liftIO (reset rApi) <$)

    return rApi


projMToClippingDist :: ProjMatrix -> Scf
projMToClippingDist (ProjM m) = f
  where
    a = 3:!3:!Z !. m
    b = 3:!4:!Z !. m
    f = b / (a + 1)
    -- n = b / (a - 1)


resetCells :: RenderingEngine -> IO RenderingEngine
resetCells  re@RenderingEngine {..} = do
  deleteRenderingCell gl rCell
  rCell' <- initRenderingCell gl
  return $ re { rCell = rCell', geomCache = mempty }


-- | Create a WebGL rendering context for a canvas
getRenderingContext :: MonadIO m => Element r s t -> m WebGLRenderingContext
getRenderingContext = liftIO . getWebGLContext . unsafeCoerce . _element_raw


setupViewPort :: RenderingEngine -> IO ()
setupViewPort RenderingEngine {..} | RenderingProgram {..} <- viewProgram = do
    useProgram gl $ programId shader
    enableCoordsNormalsBuf gl
    enableColorsBuf gl
    -- setup WebGL
    clearColor gl 0 0 0 0
    enable gl gl_DEPTH_TEST
    enable gl gl_BLEND
    blendFunc gl gl_ONE gl_ONE_MINUS_SRC_ALPHA
    depthFunc gl gl_LEQUAL
    depthMask gl True
    depthRange gl 0 1
    -- supply shader with uniform matrix
    uniformMatrix4fv gl uProjLoc False (getProjM uProjM)
    uniformMatrix4fv gl uViewLoc False (getViewM uViewM)
    uniform4f gl (uSunDirLoc viewProgram) (-0.5) (-0.6) (-1) 0 -- sx sy sz
    uniform1fv gl (uClippingDist viewProgram) (projMToClippingDist uProjM)
    uncurry (viewport gl 0 0) vpSize

setupSelViewPort :: RenderingEngine -> IO ()
setupSelViewPort RenderingEngine {..} | RenderingProgram {..} <- selProgram = do
    useProgram gl $ programId shader
    enableCoordsBuf gl
    enableSelIdsBuf gl
    -- setup WebGL
    clearColor gl 1 1 1 1
    enable gl gl_DEPTH_TEST
    disable gl gl_BLEND
    blendFunc gl gl_ONE gl_ZERO
    depthFunc gl gl_LEQUAL
    -- supply shader with uniform matrix
    uniformMatrix4fv gl uProjLoc False (getProjM uProjM)
    uniformMatrix4fv gl uViewLoc False (getViewM uViewM)
    uncurry (viewport gl 0 0) vpSize


renderFunction :: AnimationTime -> RenderingEngine -> IO ()
renderFunction _ re@RenderingEngine {..} | RenderingProgram {..} <- viewProgram = do
    -- TODO probably this is too much of overhead
    setupViewPort re
    -- clear viewport
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    -- draw objects
    renderCell gl rCell


--renderSelFunction :: RenderingEngine -> AnimationTime -> IO ()
--renderSelFunction re@RenderingEngine {..} _ | RenderingProgram {..} <- selProgram
--                                            , SelectorObject {..} <- selectorObj = do
--    bindFramebuffer gl gl_FRAMEBUFFER $ Just selFrameBuf
--    -- TODO probably this is too much of overhead
--    setupSelViewPort re
--    -- clear viewport
--    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
--    -- supply shader with uniform matrix (don't need id as long as I have setupViewPort re above)
--    --uniformMatrix4fv gl uProjLoc False (getProjM uProjM)
--    --uniformMatrix4fv gl uViewLoc False (getViewM uViewM)
--    -- draw objects
--    renderCellSelectors gl rCell
--    bindFramebuffer gl gl_FRAMEBUFFER Nothing


addRObjectFunction :: ObjRenderingData ModeColored
                   -> RenderingEngine
                   -> IO (RenderingEngine, RenderedObjectId)
addRObjectFunction cd re = do
    (roId, rc') <- addRenderedObject (gl re) cd (rCell re)
    return (re { rCell = rc'}, roId)

initSelectorFramebuffer :: WebGLRenderingContext -> (GLsizei, GLsizei) -> IO WebGLFramebuffer
initSelectorFramebuffer gl (width,height) = do
    fb <- createFramebuffer gl
    bindFramebuffer gl gl_FRAMEBUFFER $ Just fb
    tex <- createTexture gl
    bindTexture gl gl_TEXTURE_2D $ Just tex
    texImage2D gl gl_TEXTURE_2D 0 gl_RGBA width height 0 gl_RGBA gl_UNSIGNED_BYTE Nothing
    setTexParameters gl
    framebufferTexture2D gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D tex 0
    bindTexture gl gl_TEXTURE_2D Nothing
    rbd <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER $ Just rbd
    renderbufferStorage gl gl_RENDERBUFFER gl_DEPTH_COMPONENT16 width height
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER rbd
    bindRenderbuffer gl gl_RENDERBUFFER Nothing
    bindFramebuffer gl gl_FRAMEBUFFER Nothing
    return fb

setObjectColor' :: [(RenderedObjectId, Vector GLubyte 4)] -> RenderingEngine -> IO ()
setObjectColor' ((roId, c):xs) re@RenderingEngine {..}
  = setRenderedObjectColor gl rCell roId c >> setObjectColor' xs re
setObjectColor' [] _ = pure ()

-- | Update object geometry and put previous version of geometry into cache if necessary.
transformObject' :: [(RenderedObjectId, Mat44f)] -> RenderingEngine -> IO RenderingEngine
transformObject' ((roId@(RenderedObjectId i), m):xs) re@RenderingEngine {..}
  = case IntMap.lookup i geomCache of
      Nothing -> do
        mgeom <- copyObjectGeometry rCell roId
        case mgeom of
          Nothing -> transformObject' xs re
          Just cache -> do
            transformRenderedObject' gl rCell roId cache m
            transformObject' xs re{ geomCache = IntMap.insert i cache geomCache }
      Just cache -> transformRenderedObject' gl rCell roId cache m >> transformObject' xs re
transformObject' [] re = pure re




--initTexture :: WebGLRenderingContext -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei)) -> IO WebGLTexture
--initTexture gl texdata = do
--    tex <- createTexture gl
--    bindTexture gl gl_TEXTURE_2D tex
--    case texdata of
--        Left img -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
--            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
--        Right (arr, (w,h)) -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
--            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
--    setTexParameters gl
--    bindTexture gl gl_TEXTURE_2D nullRef
--    return tex

setTexParameters :: WebGLRenderingContext -> IO ()
setTexParameters gl = do
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST

--updateTexture :: WebGLRenderingContext
--              -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei))
--              -> WebGLTexture
--              -> IO ()
--updateTexture gl texdata tex = do
--    bindTexture gl gl_TEXTURE_2D tex
--    case texdata of
--        Left img -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
--            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
--        Right (arr, (w,h)) -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
--            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
--    bindTexture gl gl_TEXTURE_2D nullRef

getSelection :: (GLint, GLint) -> RenderingEngine -> IO GLuint
getSelection (x, y) re@RenderingEngine {..} | SelectorObject {..} <- selectorObj = do
    bindFramebuffer gl gl_FRAMEBUFFER $ Just selFrameBuf
    setupSelViewPort re
    -- clear viewport
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    -- draw objects
    renderCellSelectors gl rCell
    readPixels gl x (fromIntegral h - y) 1 1 gl_RGBA gl_UNSIGNED_BYTE selUbyteView
    bindFramebuffer gl gl_FRAMEBUFFER Nothing
    unScalar <$> unsafeFreezeDataFrame selUintView
  where
    (_,h) = vpSize


----------------------------------------------------------------------------------------------------

rectangle :: IO (ObjRenderingData ModeColored)
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
    return $ ObjColoredData (CoordsNormals crsnrs') (Colors colors') 0xFFFFFFFF (Indices ixs')



-- | This variable is used to control how smooth is fading near the far clipping plane.
--   such that object fades when squared distance goes from fadeConst - 1 to fadeConst.
fadeConst :: Double
fadeConst = 10

fragmentShaderText :: JSString
fragmentShaderText =
    [jsstring|
      precision mediump float;
      varying vec4 vColor;
      varying vec3 vDist;
      void main(void) {
        mediump float fade = clamp(#{x} - dot(vDist,vDist), 0.0, 1.0);
        gl_FragColor = vColor * fade;
      }
    |]
  where
   x = toJSString $ show fadeConst

vertexShaderText :: JSString
vertexShaderText =
    [jsstring|
      precision mediump float;
      attribute vec4 aVertexPosition;
      attribute vec4 aVertexNormal;
      attribute vec4 aVertexColor;
      uniform mat4 uViewM;
      uniform mat4 uProjM;
      uniform vec4 uSunDir;
      uniform float uClippingDist;
      varying vec4 vColor;
      varying vec3 vDist;
      void main(void) {
        vec4 globalPos = uViewM * aVertexPosition;
        gl_Position = uProjM * globalPos;
        vDist = globalPos.xyz/(globalPos.w*uClippingDist*#{x});
        vec4 tNormal = normalize(uViewM * aVertexNormal);
        mediump float brightness = 0.7 + 0.3 * abs(dot(aVertexNormal,uSunDir)); // * sign(dot(tNormal,globalPos));
        mediump float a = aVertexColor.w;
        vColor = vec4(clamp(aVertexColor.xyz * brightness, vec3(0,0,0), vec3(a,a,a)), a);
      }
    |]
  where
    x = toJSString . show $ 1 / sqrt fadeConst




fragmentSelShaderText :: JSString
fragmentSelShaderText =
  [jsstring|
     precision mediump float;
     varying vec4 vSelector;
     void main(void) {
       gl_FragColor = vSelector;
     }
  |]


vertexSelShaderText :: JSString
vertexSelShaderText =
  [jsstring|
    precision mediump float;
    attribute vec4 aVertexPosition;
    attribute vec4 aSelector;
    uniform mat4 uViewM;
    uniform mat4 uProjM;
    varying vec4 vSelector;
    void main(void) {
      vSelector = aSelector;
      gl_Position = uProjM * uViewM * aVertexPosition;
    }
  |]


