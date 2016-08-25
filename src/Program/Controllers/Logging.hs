{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Controllers.Logging
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Program.Controllers.Logging
  ( logActions
  ) where



import Data.Geometry
import Program.Types
import Program.Settings (jsonStringify, fromProps)
import JsHs.WebGL (GLfloat)
import JsHs.Types (JSString, JSVal)
import JsHs.LikeJS.Class (asJSVal)

-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators



logActions :: JSString -> Event (GeomId, Matrix4 GLfloat) -> MomentIO ()
logActions url ev = do
    logger <- liftIO $ registerLogging url
    reactimate $ (sendMessage logger . jsonStringify . action) <$> ev
  where
    action (GeomId i, mat) = fromProps
       [ ("geomID", asJSVal i)
       , ("transform", asJSVal mat)
       ]


foreign import javascript interruptible "var t = true, s = new WebSocket($1); s.onopen = function(){if(t){t=false;$c(s);}}; s.onerror = function(){if(t){t=false;$c(null);}};"
  registerLogging :: JSString -> IO JSVal

foreign import javascript unsafe "if($1 != null){$1.send($2);}"
  sendMessage :: JSVal -> JSString -> IO ()
