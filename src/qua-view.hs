{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import Reflex.Dom
import Reflex.Dom.Widget.Animation -- (resizeEvents)

import Commons

import qualified Model.Camera               as Model
import qualified Model.Scenario             as Scenario

import           Widgets.Generation
import qualified Widgets.LoadingSplash  as Widgets
import qualified Widgets.Canvas         as Widgets
import qualified Widgets.ControlPanel   as Widgets

import qualified SmallGL
import qualified Workers.LoadGeometry as Workers

import           Program.Camera
import           Program.Scenario
import           Program.Scenario.Object
import           Program.MapTiles
import qualified Program.WebLogging as WebLogging

import qualified QuaTypes

main :: IO ()
main = mainWidgetInElementById "qua-view-widgets" $ runQuaWidget $ do
    -- Change the state of the program
    (isProgramBusy, setIsProgramBusy) <- newTriggerEvent

    -- register loading splash so that we can change its visibility
    Widgets.loadingSplashD isProgramBusy

    -- register canvas element
    canvas <- Widgets.getWebGLCanvas

    -- initialize web workers
    Workers.runLoadGeometryWorker

    renderingApi <- SmallGL.createRenderingEngine canvas
    scenarioB <- createScenario renderingApi
    -- enable map loading
    downloadMapTiles scenarioB

    -- initialize animation handler (and all pointer events).
    aHandler <- Widgets.registerAnimationHandler canvas (SmallGL.render renderingApi)
    -- make GHCJS think we performed GC on pointer down event,
    --  such that first 5 seconds of pointer movement there are no more GCs.
    performEvent_ $ liftIO performPhantomGC <$ select (pointerEvents aHandler) PDownEvent
    -- selected object id events
    selectedObjIdD <- objectSelectionsDyn aHandler renderingApi
    inQuaWidget $ colorObjectsOnSelection scenarioB selectedObjIdD
    -- move objects events
    -- supply animation events to camera
    rec camLockedB <- inQuaWidget
          $ moveSelectedObjects aHandler renderingApi (current cameraD) scenarioB selectedObjIdD
        cameraD <- inQuaWidget $ dynamicCamera aHandler camLockedB

    -- initialize WebGL rendering context
    registerEvent (SmallGLInput SmallGL.ViewPortResize)
        $ resizeEvents aHandler
    registerEvent (SmallGLInput SmallGL.ProjTransformChange)
        $ SmallGL.ProjM . Model.projMatrix <$> updated cameraD
    registerEvent (SmallGLInput SmallGL.ViewTransformChange)
        $ SmallGL.ViewM . Model.viewMatrix <$> updated cameraD


    -- get an event of loaded geometry text, combine it with current state of scenario,
    -- and make a new event to be consumed by the LoadGeometryWorker
    do
      geomLoadedE <- askEvent GeometryLoaded
      registerEvent (WorkerMessage Workers.LGWRequest)
        $ Workers.LGWLoadTextContent . Scenario.withoutObjects <$> scenarioB <@> geomLoadedE


    -- Notify everyone that the program h finished starting up now
    mainBuilt <- getPostBuild
    performEvent_ . flip fmap mainBuilt . const $
        liftIO (setIsProgramBusy Idle)
    --    Widgets.play aHandler
    glUpdates <- SmallGL.askAllGLEvents
    glUpdatesAfterBuilt <- switchPromptOnly never (glUpdates <$ mainBuilt)
    performEvent_ $ step aHandler <$ glUpdatesAfterBuilt


    -- add the control panel to the page
    _panelStateD <- Widgets.controlPanel renderingApi scenarioB selectedObjIdD cameraD


    -- enable websockets logging
    inQuaWidget $ WebLogging.logActions cameraD selectedObjIdD

    -- other init things
    -- load geometry from url if it is supplied
    quaSettings >>= sample . fmap QuaTypes.getSubmissionGeometryUrl . current
                >>= \ms -> case ms of
      Nothing -> return ()
      Just url -> do
        let f Workers.LGWReady  = Just url
            f _                 = Nothing
        workerMsgE <- askEvent (WorkerMessage Workers.LGWMessage) >>= headE
        registerEvent (WorkerMessage Workers.LGWRequest)
          $ Workers.LGWLoadUrl . Scenario.withoutObjects <$> scenarioB <@> fmapMaybe f workerMsgE




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
