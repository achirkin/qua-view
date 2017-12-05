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
    , RenderingApi (addRObject, getHoveredSelId, render, renderToImage)
    , QEventTag (..)
    , createRenderingEngine
    ) where

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
import Numeric.Dimensions
import Numeric.TypeLits


import Commons
import SmallGL.Types
import SmallGL.Shader

import SmallGL.RenderingCell
import SmallGL.SelectorObject
import SmallGL.RenderingMapTiles

data RenderingEngine = RenderingEngine
  { gl            :: !WebGLRenderingContext
    -- ^ WebGL context
  , vpSize        :: !(GLsizei, GLsizei)
    -- ^ size of the viewport
  , viewProgram   :: !RenderingProgram
  , selProgram    :: !RenderingProgram
  , renderMapProg :: !RenderMTilesProgram
  , selectorObj   :: !SelectorObject
  , uProjM        :: !ProjMatrix
  , uViewM        :: !ViewMatrix
  , rCell         :: !(RenderingCell ModeColored)
  , geomCache     :: !(IntMap (SomeIODataFrame Float '[N 4, XN 0]))
    -- ^ Last submitted versions of active geometries
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
  , renderToImage   :: (GLsizei, GLsizei) -> ProjMatrix -> ViewMatrix -> IO JSString
    -- ^ Render scene to image of given size
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
    -- | Set objects' colors
    SetObjectColor        :: QEventTag SmallGLInput [(RenderedObjectId, Vector GLubyte 4)]
    -- | Move objects and delete their cached old positions
    PersistGeomTransforms :: QEventTag SmallGLInput [(RenderedObjectId, Mat44f)]
    -- | Set map rendering parameters
    SetMapTileOpacity     :: QEventTag SmallGLInput Scf
    -- | Add a new map cell to rendering
    AddMapTileToRendering :: QEventTag SmallGLInput (DataFrame Float '[4,4], TexImageSource)
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
    viewProgram <- liftIO $ do
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
            uCustomLoc5 = unifLoc shader "Selector shader does not have fifth uniform location."
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
            uCustomLoc5 = unifLoc shader "Selector shader does not have fifth uniform location."
        return RenderingProgram {..}
    selectorObj <- liftIO $ initSelectorObject gl vpSize

    -- create objects (including sending data to device)
    rCell <- liftIO $ initRenderingCell gl

    renderMapProg <- liftIO $ initMapTilesProgram gl 0.8


    let uProjM = ProjM eye
        uViewM = ViewM eye
        geomCache = mempty
        re = RenderingEngine {..}


    rre <- liftIO $ newMVar re
    let rApi = RenderingApi
            { render = \t -> withMVar rre $ \r -> do
                  setupRenderViewPort r
                  renderFunction t r
            , addRObject = modifyMVar rre . addRObjectFunction
            , getHoveredSelId = modifyMVar rre . getSelection
            , setObjectColor = withMVar rre . setObjectColor'
            , transformObject = modifyMVar_ rre . transformObject'
            , resetGeomCache = modifyMVar_ rre $ \r -> pure r{geomCache = mempty}
            , reset = modifyMVar_ rre (resetCells >=> clearMapTiles')
            , renderToImage = \s p -> withMVar rre . renderToImage' s p
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
           let r' = r{vpSize = (floor *** floor) newVPSize } in r' <$ setupRenderViewPort r'

    askEvent (SmallGLInput TransformObject)
      >>= performEvent_ . fmap (liftIO . transformObject rApi)

    askEvent (SmallGLInput SetObjectColor)
      >>= performEvent_ . fmap (liftIO . setObjectColor rApi)

    askEvent (SmallGLInput PersistGeomTransforms)
      >>= performEvent_ . fmap (\xs -> liftIO $ do
                                         transformObject rApi xs
                                         resetGeomCache rApi
                               )

    askEvent (SmallGLInput SetMapTileOpacity)
      >>= performEvent_ . fmap (liftIO . modifyMVar_ rre . setMapTileOpacity')

    askEvent (SmallGLInput AddMapTileToRendering)
      >>= performEvent_ . fmap (liftIO . modifyMVar_ rre . addMapTile')


    askEvent (SmallGLInput ResetGL)
      >>= performEvent_ . (liftIO (reset rApi) <$)

    return rApi


resetCells :: RenderingEngine -> IO RenderingEngine
resetCells  re@RenderingEngine {..} = do
  deleteRenderingCell gl rCell
  rCell' <- initRenderingCell gl
  clearMapTiles' re { rCell = rCell', geomCache = mempty }


-- | Create a WebGL rendering context for a canvas
getRenderingContext :: MonadIO m => Element r s t -> m WebGLRenderingContext
getRenderingContext = liftIO . getWebGLContext . unsafeCoerce . _element_raw



setupRenderViewPort :: RenderingEngine -> IO ()
setupRenderViewPort RenderingEngine {..} | RenderingProgram {..} <- viewProgram = do
    uncurry (viewport gl 0 0) vpSize
    clearColor gl 0 0 0 0
    enable gl gl_BLEND
    enable gl gl_DEPTH_TEST
    blendFunc gl gl_ONE gl_ONE_MINUS_SRC_ALPHA
    depthFunc gl gl_LEQUAL


setupSelectViewPort :: RenderingEngine -> IO ()
setupSelectViewPort RenderingEngine {..} | RenderingProgram {..} <- selProgram = do
    clearColor gl 1 1 1 1
    enable gl gl_DEPTH_TEST
    disable gl gl_BLEND
    blendFunc gl gl_ONE gl_ZERO
    depthFunc gl gl_LEQUAL


renderFunction :: AnimationTime -> RenderingEngine -> IO ()
renderFunction _ re@RenderingEngine {..}  = do
    -- clear viewport
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    -- draw map tiles
    renderMapTiles' re
    renderCells re

renderCells :: RenderingEngine -> IO ()
renderCells RenderingEngine {..} = do
    -- shader for normal objects
    useProgram gl $ programId (shader viewProgram)
    enableCoordsNormalsBuf gl
    enableColorsBuf gl
    -- supply shader with uniform matrix
    uniformMatrix4fv gl (uProjLoc         viewProgram) False (getProjM uProjM)
    uniformMatrix4fv gl (uViewLoc         viewProgram) False (getViewM uViewM)
    uniform4f        gl (uSunDirLoc       viewProgram) (-0.5) (-0.6) (-1) 0 -- sx sy sz
    uniform1fv       gl (uClippingDistLoc viewProgram) (projMToClippingDist uProjM)
    -- draw objects
    renderCell gl rCell
    disableCoordsNormalsBuf gl
    disableColorsBuf gl

addRObjectFunction :: ObjRenderingData ModeColored
                   -> RenderingEngine
                   -> IO (RenderingEngine, RenderedObjectId)
addRObjectFunction cd re = do
    (roId, rc') <- addRenderedObject (gl re) cd (rCell re)
    return (re { rCell = rc'}, roId)




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


getSelection :: (GLint, GLint) -> RenderingEngine -> IO (RenderingEngine, GLuint)
getSelection (x, y) re@RenderingEngine {..} = do
    so@SelectorObject {..} <- updateSelectorSizeIfNeeded gl vpSize selectorObj
    bindFramebuffer gl gl_FRAMEBUFFER $ Just selFrameBuf
    setupSelectViewPort re
    useProgram gl . programId $ shader selProgram
    enableCoordsBuf gl
    enableSelIdsBuf gl
    -- clear viewport
    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    -- supply shader with uniform matrix
    uniformMatrix4fv gl (uProjLoc selProgram) False (getProjM uProjM)
    uniformMatrix4fv gl (uViewLoc selProgram) False (getViewM uViewM)
    -- draw objects
    renderCellSelectors gl rCell
    disableCoordsBuf gl
    disableSelIdsBuf gl
    -- get selection back
    readPixels gl x (fromIntegral h - y) 1 1 gl_RGBA gl_UNSIGNED_BYTE selUbyteView
    bindFramebuffer gl gl_FRAMEBUFFER Nothing
    (,) re{selectorObj = so} . unScalar <$> unsafeFreezeDataFrame selUintView
  where
    (_,h) = vpSize






renderToImage' :: (GLsizei, GLsizei) -> ProjMatrix -> ViewMatrix
               -> RenderingEngine -> IO JSString
renderToImage' (width,height) projMat viewMat re'
      | re@RenderingEngine {..} <- re'
          { uProjM = invertedY projMat
          , uViewM = viewMat
          , vpSize = (width,height) }
      , RenderingProgram {..} <- viewProgram
      , Just (SomeIntNat (Proxy :: Proxy width)) <- someIntNatVal $ fromIntegral width
      , Just (SomeIntNat (Proxy :: Proxy height)) <- someIntNatVal $ fromIntegral height
      = do
    -- create buffer to render stuff into it
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

    -- draw everything with adjusted matrices
    setupRenderViewPort re
    renderFunction 0 re

    -- get texture content
    imgData <- newDataFrame :: IO (IODataFrame GLubyte '[4,width,height])
    readPixels gl 0 0 width height gl_RGBA gl_UNSIGNED_BYTE imgData
    imgjsval <- js_dfToImageData width height imgData

    -- release all context
    bindFramebuffer gl gl_FRAMEBUFFER Nothing
    deleteRenderbuffer gl rbd
    deleteTexture gl tex
    deleteFramebuffer gl fb
    return imgjsval
  where
    invertedY :: ProjMatrix -> ProjMatrix
    invertedY (ProjM m) = ProjM $ m
                        & update (2:!1:!Z) (negate $ 2:!1:!Z !. m :: Scf)
                        & update (2:!2:!Z) (negate $ 2:!2:!Z !. m :: Scf)
                        & update (2:!3:!Z) (negate $ 2:!3:!Z !. m :: Scf)
                        & update (2:!4:!Z) (negate $ 2:!4:!Z !. m :: Scf)
renderToImage' _ _ _ _ = (newDataFrame :: IO (IODataFrame GLubyte '[4,1,1]))
                     >>= js_dfToImageData 1 1




foreign import javascript unsafe
    "var canvas = document.createElement('canvas');\
    \canvas.width = $1;\
    \canvas.height = $2;\
    \var context = canvas.getContext('2d');\
    \var imageData = context.createImageData($1, $2);\
    \imageData.data.set($3);\
    \context.putImageData(imageData, 0, 0);\
    \$r = canvas.toDataURL();"
    js_dfToImageData :: GLsizei -- ^ width
                     -> GLsizei -- ^ height
                     -> IODataFrame GLubyte '[4, w, h] -- ^ image data
                     -> IO JSString -- ^ image url


----------------------------------------------------------------------------------------------------

setMapTileOpacity' :: Scf -> RenderingEngine -> IO RenderingEngine
setMapTileOpacity' op r
  = return r{renderMapProg = setMapTileOpacity op (renderMapProg r)}

addMapTile' :: (DataFrame Float '[4,4], TexImageSource)
            -> RenderingEngine -> IO RenderingEngine
addMapTile' (df, tex) r = do
  renderMapProg' <- addMapTile (gl r) df tex (renderMapProg r)
  return r{renderMapProg = renderMapProg'}

clearMapTiles' :: RenderingEngine -> IO RenderingEngine
clearMapTiles' r = do
  renderMapProg' <- clearMapTiles (gl r) (renderMapProg r)
  return r{renderMapProg = renderMapProg'}

renderMapTiles' :: RenderingEngine -> IO ()
renderMapTiles' RenderingEngine {..}
  = renderMapTiles  gl uProjM uViewM renderMapProg


----------------------------------------------------------------------------------------------------

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
        // vertex normal that is always directed to an eye
        vec4 tVertexNormal = aVertexNormal * sign(dot(uViewM * aVertexNormal, - globalPos));
        mediump float brightness = 0.8 - 0.3 * dot(tVertexNormal,uSunDir);
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

