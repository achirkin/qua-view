-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.ElementResizing
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.ElementResizing
    ( ResizeEvent (..)
    , ResizeCallBack
    , onElementResize
    ) where

import GHCJS.Foreign.Callback
import JsHs.WebGL
import GHCJS.Useful

-- | Resizing element - gives new width and height
data ResizeEvent = ResizeEvent GLfloat GLfloat deriving (Eq,Show)

type ResizeCallBack = ResizeEvent -> IO ()

foreign import javascript unsafe "addResizeListener($1, $2);"
   onElementResize' :: JSElement -> Callback (IO ()) -> IO ()

-- | Run addResizeListener, which is in misc.js
onElementResize :: JSElement -> ResizeCallBack -> IO ()
onElementResize self func = asyncCallback ( do
    w <- getElementWidth self
    h <- getElementHeight self
    func $ ResizeEvent w h ) >>= onElementResize' self
