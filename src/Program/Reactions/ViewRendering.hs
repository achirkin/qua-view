{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.ViewRendering
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.ViewRendering () where

--import JavaScript.Web.AnimationFrame

import Reactive

import Program

import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents

import Program.View

import Program.Reactions.ServiceFinish

import Control.Exception (onException)
import JsHs.Types
import JsHs.LikeJS.Class
import JsHs.Callback

renderScene :: Program -> PView -> IO PView
renderScene program view = do
    -- prepare rendering
    ctime <- waitForAnimationFrame
    ctx <- prepareRenderState (context view) (camera program) ctime
    -- render
    clearScreen ctx
    draw ctx (decGrid program) (dgView view)
    draw ctx (city program) (cityView view)
    -- done!
    return view{context = ctx}

instance Reaction Program PView WheelEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerUpEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerCancelEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView PointerMoveEvent "Render" 1 where
    response _ _ _ = renderScene

instance Reaction Program PView ResizeEvent "Resize & Render" 1 where
    response _ _ _ p v@PView{context = ctx} = do
        ctx' <- updateViewPortSize (camera p) ctx
        renderScene p v{context = ctx'}


instance Reaction Program PView SelectionEvent "Render" 9 where
    response _ _ _ = renderScene


instance Reaction Program PView ServiceRunFinish "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView ClearingGeometry "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView GeoJSONLoaded "Render" 9 where
    response _ _ _ = renderScene

instance Reaction Program PView ClearServiceResults "Render" 9 where
    response _ _ _ = renderScene

-- In order to show a preview of the scenario as an image,
-- we need to render staff in a buffer (not to get empty image)
instance Reaction Program PView SubmitScenario "Render" 0 where
    response _ _ _ = renderScene


-----------------------------------------------------------------
-- copied from JavaScript-Web-AnimationFrame.html
-----------------------------------------------------------------



newtype AnimationFrameHandle = AnimationFrameHandle JSVal

{- |
     Wait for an animation frame callback to continue running the current
     thread. Use 'GHCJS.Concurrent.synchronously' if the thread should
     not be preempted. This will return the high-performance clock time
     stamp once an animation frame is reached.
 -}
waitForAnimationFrame :: IO Double
waitForAnimationFrame = do
  h <- js_makeAnimationFrameHandle
  js_waitForAnimationFrame h `onException` js_cancelAnimationFrame h

{- |
     Run the action in an animationframe callback. The action runs in a
     synchronous thread, and is passed the high-performance clock time
     stamp for that frame.
 -}
inAnimationFrame :: OnBlocked       -- ^ what to do when encountering a blocking call
                 -> (Double -> IO ())  -- ^ the action to run
                 -> IO AnimationFrameHandle
inAnimationFrame onBlocked x = do
  cb <- syncCallback1 onBlocked (x . asLikeJS)
  h  <- js_makeAnimationFrameHandleCallback (jsval cb)
  js_requestAnimationFrame h
  return h

cancelAnimationFrame :: AnimationFrameHandle -> IO ()
cancelAnimationFrame h = js_cancelAnimationFrame h
{-# INLINE cancelAnimationFrame #-}

-- -----------------------------------------------------------------------------

foreign import javascript unsafe "{ handle: null, callback: null }"
  js_makeAnimationFrameHandle :: IO AnimationFrameHandle
foreign import javascript unsafe "{ handle: null, callback: $1 }"
  js_makeAnimationFrameHandleCallback :: JSVal -> IO AnimationFrameHandle
foreign import javascript unsafe "h$animationFrameCancel"
  js_cancelAnimationFrame :: AnimationFrameHandle -> IO ()
foreign import javascript interruptible
  "$1.handle = window.requestAnimationFrame($c);"
  js_waitForAnimationFrame :: AnimationFrameHandle -> IO Double
foreign import javascript unsafe "h$animationFrameRequest"
  js_requestAnimationFrame :: AnimationFrameHandle -> IO ()
