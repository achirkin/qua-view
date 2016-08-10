{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  JsHs.Useful
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module JsHs.Useful where


import Prelude hiding (lines)
import JsHs.JSString as JS
import Control.Concurrent (threadDelay)
import JsHs.WebGL.Types (GLfloat, WebGLCanvas)
import qualified JsHs.Array as JS

import Data.Coerce
import Reactive.Banana.JsHs.Types (Coords2D (..), HTMLElement)
import Data.Geometry.VectorMath (Vector (..), Vector2)

-- | convert between reactive banana's Coords2D and fastvec vectors
asVector :: Coords2D -> Vector2 GLfloat
asVector = coerce

-- | Shortcut to get the element from the DOM
foreign import javascript unsafe "$r = document.getElementById($1)"
   getElementById :: JSString -> IO HTMLElement

-- | Shortcut to get the element from the DOM
foreign import javascript unsafe "$r = document.getElementById($1)"
    getCanvasById :: JSString -> IO WebGLCanvas


-- | Display loading splash
programInProgress :: IO ()
programInProgress = programInProgress' >> threadDelay 0

foreign import javascript interruptible "document.getElementById('loadingSplash').style.display = 'block';setTimeout($c(), 0);"
    programInProgress' :: IO ()

-- | Hide loading splash
foreign import javascript unsafe "document.getElementById('loadingSplash').style.display = 'none';"
    programIdle :: IO ()


foreign import javascript unsafe "logExternalProcess($1)"
    logExternalProcess' :: JSString -> IO JSString
foreign import javascript unsafe "notifyFinishExternalProcess($1)"
    notifyFinishExternalProcess' :: JSString -> IO ()



foreign import javascript unsafe "logText($1)"
    logText' :: JSString -> IO ()

-- | Log into in-program console
logText :: String -> IO ()
logText s = logText' (intercalate "<br/>" . lines . pack $ s)

logShowable :: Show a => a -> IO ()
logShowable = logText . show



combinePointers :: JS.Array Coords2D -> JS.Array Coords2D -> [(Vector2 GLfloat, Vector2 GLfloat)]
combinePointers a b = Prelude.zipWith (\p1 p2 -> ( asVector p1, asVector p2)
                              ) (JS.toList a) (JS.toList b)

