{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where


import Reflex.Dom
import Reflex.Dom.Widget.Animation (resizeEvents, viewPortSizeI)
import Numeric.DataFrame


import Commons

import qualified Model.Camera           as Model

import           Widgets.Generation
import qualified Widgets.LoadingSplash  as Widgets
import qualified Widgets.Canvas         as Widgets
import qualified Widgets.ControlPanel   as Widgets

import qualified SmallGL

main :: IO ()
main = mainWidgetInElementById "qua-view-widgets" $ mdo
    -- Change the state of the program
    (isProgramBusy, setIsProgramBusy) <- newTriggerEvent

    -- register loading splash so that we can change its visibility
    Widgets.loadingSplashD isProgramBusy

    -- register canvas element
    canvas <- Widgets.getWebGLCanvas

    -- add the control panel to the page
    _panelStateD <- Widgets.controlPanel

    -- initialize WebGL rendering context
    let smallGLESel :: forall t a . Reflex t => SmallGL.SmallGLInput a -> Event t a
        smallGLESel SmallGL.ViewPortResize = resizeEvents aHandler
        smallGLESel SmallGL.ProjTransformChange = SmallGL.ProjM . Model.projMatrix <$> updated cameraD
        smallGLESel SmallGL.ViewTransformChange = SmallGL.ViewM . Model.viewMatrix <$> updated cameraD

    renderingApi <- SmallGL.createRenderingEngine canvas (EventSelector smallGLESel)
    -- initialize animation handler (and all pointer events).
    aHandler <- Widgets.registerAnimationHandler canvas (SmallGL.render renderingApi)
    -- supply animation events to camera
    let icamera = Model.initCamera (realToFrac . fst $ viewPortSizeI aHandler)
                                   (realToFrac . snd $ viewPortSizeI aHandler)
                                   Model.CState { Model.viewPoint  = vec3 (-2) 3 0
                                                , Model.viewAngles = (2.745, 0.825)
                                                , Model.viewDist = 68 }
    cameraD <- Model.dynamicCamera icamera aHandler
--    performEvent_ $ liftIO . print <$> updated (Model.oldState <$> cameraD)


    -- Notify everyone that the program h finished starting up now
    mainBuilt <- getPostBuild
    performEvent_ $ (liftIO (setIsProgramBusy Idle) >> Widgets.play aHandler) <$ mainBuilt


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
        overflow: visible
        width: 0
        height: 0
    |]
  return []
 )
