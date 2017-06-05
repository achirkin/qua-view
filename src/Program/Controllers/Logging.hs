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
import Data.Geometry.Structure.Feature (GeometryInput)
import Program.Types
import Program.Settings (jsonStringify, fromProps)
import JsHs.WebGL (GLfloat)
import JsHs.Types (JSString, JSVal)
import JsHs.LikeJS.Class (asJSVal)

-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators



logActions :: JSString
           -> Event (GeomId, Matrix4 GLfloat)
           -> Event (GeometryInput, GLfloat)
           -> Event GeometryInput
           -> MomentIO ()
logActions url actionE rebuildE updateE = do
    logger <- liftIO $ registerLogging url
    reactimate $ (sendMessage logger . jsonStringify . action ) <$> actionE
    reactimate $ (sendMessage logger . jsonStringify . rebuild) <$> rebuildE
    reactimate $ (sendMessage logger . jsonStringify . update ) <$> updateE
  where
    action (GeomId i, mat) = fromProps
       [ ("geomID", asJSVal i)
       , ("transform", asJSVal mat)
       ]
    rebuild (fc, s) = fromProps
       [ ("scale", asJSVal s)
       , ("load", asJSVal . jsonStringify $ asJSVal fc)
       ]
    update fc = fromProps
       [ ("update", asJSVal . jsonStringify $ asJSVal fc)
       ]

foreign import javascript interruptible "var t = true, s = new WebSocket($1); s.onopen = function(){if(t){t=false;$c(s);}}; s.onerror = function(){if(t){t=false;$c(null);}};"
  registerLogging :: JSString -> IO JSVal

foreign import javascript unsafe "if($1 != null){$1.send($2);}"
  sendMessage :: JSVal -> JSString -> IO ()
