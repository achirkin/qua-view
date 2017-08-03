{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Widgets.Canvas
    ( getWebGLCanvas
    , registerAnimationHandler
    , Animation.play, Animation.pause
    ) where

import qualified Reflex.Dom as Dom
import qualified Reflex.Dom.Widget.Animation as Animation

import Commons
import Widgets.Generation

-- | Create WebGL canvas
--   Note, this is one of our special element that exist even before JS is loaded.
--   We need this to make sure the canvas has correctly evaluated dimensions.
getWebGLCanvas :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
getWebGLCanvas = getElementById def "qua-view-webgl-canvas" >>= \me -> pure $ case me of
    Nothing -> error "Could not find 'qua-view-webgl-canvas' Dom element. Are you sure it is in the qua-view.html?"
    Just e  -> e

-- | generate a chunk of css in qua-view.css
$(do
    qcss
      [cassius|
        #qua-view-webgl-canvas
          position: fixed
          left: 0
          top: 0
          padding: 0
          margin: 0
          z-index: 0
          overflow: hidden
          width: 100%
          height: 100%
      |]
    return []
 )

-- | Listen to all events produced by a canvas
registerAnimationHandler :: (MonadIO m, MonadIO (Dom.Performable m), Reflex t, Dom.MonadHold t m, Dom.TriggerEvent t m, Dom.PerformEvent t m)
                         => Element Dom.EventResult Dom.GhcjsDomSpace t -> (AnimationTime -> IO ()) -> m (AnimationHandler t)
registerAnimationHandler e = Animation.registerHandler (Dom._element_raw e) . Just
