{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Widgets.Canvas
    ( webGLCanvas
    , registerAnimationHandler
    , Animation.play, Animation.pause
    ) where

import Reflex.Dom ((=:))
import qualified Reflex.Dom as Dom
import Control.Monad.IO.Class
import qualified Reflex.Dom.Widget.Animation as Animation

import CommonTypes
import Widgets.Generation

-- | Create WebGL canvas
webGLCanvas :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
webGLCanvas = fst <$> Dom.elAttr' "canvas" ("id" =: elementId) Dom.blank
  where
    elementId = $(do
      -- create unique css identifiers
      glcanvas <- newVar
      -- generate a chunk of css in qua-view.css
      qcss
        [cassius|
          ##{glcanvas}
            position: fixed
            left: 0
            top: 0
            padding: 0
            margin: 0
            z-index: 2
            overflow: hidden
            width: 100%
            height: 100%

        |]
      returnVars [glcanvas]
     )

-- | Listen to all events produced by a canvas
registerAnimationHandler :: (MonadIO m, MonadIO (Dom.Performable m), Reflex t, Dom.MonadHold t m, Dom.TriggerEvent t m, Dom.PerformEvent t m)
                         => Element Dom.EventResult Dom.GhcjsDomSpace t -> (AnimationTime -> IO ()) -> m (AnimationHandler t)
registerAnimationHandler e = Animation.registerHandler (Dom._element_raw e) . Just
