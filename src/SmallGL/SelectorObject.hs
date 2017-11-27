{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module SmallGL.SelectorObject
    ( SelectorObject (..)
    , initSelectorObject
    , releaseSelectorObjectRes
    , updateSelectorSizeIfNeeded
    ) where

import Numeric.DataFrame.IO
import Unsafe.Coerce (unsafeCoerce)

import SmallGL.Types
import JavaScript.WebGL

data SelectorObject = SelectorObject
  { selFrameBuf  :: !WebGLFramebuffer
  , selRenderBuf :: !WebGLRenderbuffer
  , selTexture   :: !WebGLTexture
  , selUbyteView :: !(IODataFrame GLubyte '[4])
  , selUintView  :: !(IODataFrame GLuint '[])
  , selFrameSize :: !(GLsizei, GLsizei)
  }


initSelectorObject :: WebGLRenderingContext -> (GLsizei, GLsizei) -> IO SelectorObject
initSelectorObject gl selFrameSize@(width,height) = do
    selFrameBuf <- createFramebuffer gl
    bindFramebuffer gl gl_FRAMEBUFFER $ Just selFrameBuf
    selTexture <- createTexture gl
    bindTexture gl gl_TEXTURE_2D $ Just selTexture
    texImage2D gl gl_TEXTURE_2D 0 gl_RGBA width height 0 gl_RGBA gl_UNSIGNED_BYTE Nothing
    setTexParameters gl
    framebufferTexture2D gl gl_FRAMEBUFFER gl_COLOR_ATTACHMENT0 gl_TEXTURE_2D selTexture 0
    bindTexture gl gl_TEXTURE_2D Nothing
    selRenderBuf <- createRenderbuffer gl
    bindRenderbuffer gl gl_RENDERBUFFER $ Just selRenderBuf
    renderbufferStorage gl gl_RENDERBUFFER gl_DEPTH_COMPONENT16 width height
    framebufferRenderbuffer gl gl_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_RENDERBUFFER selRenderBuf
    bindRenderbuffer gl gl_RENDERBUFFER Nothing
    bindFramebuffer gl gl_FRAMEBUFFER Nothing

    selUbyteView <- newDataFrame
    selUintView <- arrayBuffer selUbyteView >>= viewWord32Array @'[] >>=
          \(SomeIODataFrame d) -> return (unsafeCoerce d)
    return SelectorObject {..}


releaseSelectorObjectRes :: WebGLRenderingContext -> SelectorObject -> IO ()
releaseSelectorObjectRes gl SelectorObject {..} = do
    deleteRenderbuffer gl selRenderBuf
    deleteTexture gl selTexture
    deleteFramebuffer gl selFrameBuf

updateSelectorSizeIfNeeded :: WebGLRenderingContext -> (GLsizei, GLsizei)
                           -> SelectorObject -> IO SelectorObject
updateSelectorSizeIfNeeded gl (nw,nh) so@SelectorObject {..}
  | (ow, oh) <- selFrameSize
  , ow < nw || oh < nh
  , width <- max ow nw
  , height <- max oh nh
  = do releaseSelectorObjectRes gl so
       initSelectorObject gl (width, height)
updateSelectorSizeIfNeeded _ _ so = pure so


