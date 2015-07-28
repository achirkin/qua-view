-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.ElementResizing
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.WebGL
import GHCJS.Useful

-- | Resizing element - gives new width and height
data ResizeEvent = ResizeEvent GLfloat GLfloat

type ResizeCallBack = ResizeEvent -> IO ()

foreign import javascript unsafe "addResizeListener($1, $2);"
   onElementResize' :: JSRef elem -> JSFun (IO ()) -> IO ()

-- | Run addResizeListener, which is in misc.js
onElementResize :: JSElement -> ResizeCallBack -> IO ()
onElementResize self func = asyncCallback AlwaysRetain ( do
    w <- getElementWidth self
    h <- getElementHeight self
    func $ ResizeEvent w h ) >>= onElementResize' self
