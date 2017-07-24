{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where

import Control.Monad.IO.Class (liftIO)

import Reflex.Dom
import Reflex.Dom.Widget.Animation (resizeEvents)
import qualified Reflex.Class as Reflex

import CommonTypes

import           Widgets.Generation
import qualified Widgets.LoadingSplash  as Widgets
import qualified Widgets.Canvas         as Widgets
import qualified Widgets.ControlButtons as Widgets ()

import qualified SmallGL

main :: IO ()
main = mainWidgetInElementById "qua-view-widgets" $ do
  -- Change the state of the program
  (isProgramBusy, setIsProgramBusy) <- newTriggerEvent
  (isStartedUp, doStartUp ) <- newTriggerEvent
  -- register loading splash so that we can change its visibility
  Widgets.loadingSplashD isProgramBusy
  -- add canvas element
  -- Note: we have to create canvas before "main build" block,
  --       so that registerAnimationHandler happens after canvas settles down;
  --       this is important to calculate canvas size correctly
  canvas <- Widgets.getWebGLCanvas
  -- Main build block.
  -- We assume that building can take a while, so we run a special "loading splash"
  -- during this block execution.
  ((), mainBuilt) <- Reflex.runWithReplace (liftIO $ doStartUp ()) . (<$ isStartedUp) $ mdo


      el "p" $ text "Reflex is:"
      el "ul" $ do
        el "li" $ text "Efficient"
        el "li" $ text "Higher-order"
        el "li" $ text "Glitch-free"

      -- initialize WebGL rendering context
      let smallGLESel :: forall t a . Reflex t => SmallGL.SmallGLInput a -> Event t a
          smallGLESel SmallGL.ViewPortResize = resizeEvents aHandler

      renderingApi <- SmallGL.createRenderingEngine canvas (EventSelector smallGLESel)
      -- initialize animation handler (and all pointer events).
      aHandler <- Widgets.registerAnimationHandler canvas (SmallGL.render renderingApi)



      Widgets.play aHandler


  -- Notify everyone that the program h finished starting up now
  performEvent_ $ liftIO (setIsProgramBusy Idle) <$ mainBuilt


-- | Create a global css splice.
--   Do not abuse this!
$(do
  qcss
    [cassius|
      body
        position: fixed
        left: 0
        top: 0
        padding: 0
        margin: 0
        width: 100%
        height: 100%
        overflow: hidden
        background-color: #FFFFFF
        touch-action: none
        color: #BF360C

      #qua-view-widgets
        position: fixed
        left: 0
        top: 0
        padding: 0
        margin: 0
        z-index: 1
        overflow: hidden
        width: 100%
        height: 100%
    |]
  return []
 )
