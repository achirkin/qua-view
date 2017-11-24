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
import Numeric.DataFrame

import Commons


import qualified Model.Camera               as Model
import qualified Model.Scenario             as Scenario
import qualified Model.Scenario.Statistics  as Scenario

import           Widgets.Generation
import qualified Widgets.LoadingSplash  as Widgets
import qualified Widgets.Canvas         as Widgets
import qualified Widgets.ControlPanel   as Widgets

import qualified SmallGL
import qualified Workers.LoadGeometry as Workers

import           Program.Camera
import           Program.Scenario
import           Program.Scenario.Object

import qualified QuaTypes
import Control.Concurrent (forkIO, threadDelay)

main :: IO ()
main = mainWidgetInElementById "qua-view-widgets" $ runQuaWidget $ mdo
    -- Change the state of the program
    (isProgramBusy, setIsProgramBusy) <- newTriggerEvent

    -- register loading splash so that we can change its visibility
    Widgets.loadingSplashD isProgramBusy

    -- register canvas element
    canvas <- Widgets.getWebGLCanvas

    -- add the control panel to the page
    _panelStateD <- Widgets.controlPanel renderingApi scenarioB selectedObjIdD

    -- get an event of loaded geometry text, combine it with current state of scenario,
    -- and make a new event to be consumed by the LoadGeometryWorker
    do
      geomLoadedE <- askEvent GeometryLoaded
      registerEvent (WorkerMessage Workers.LGWRequest)
        $ Workers.LGWLoadTextContent . Scenario.withoutObjects <$> scenarioB <@> geomLoadedE

    -- initialize web workers
    Workers.runLoadGeometryWorker
    loadedGeometryE <- askEvent $ WorkerMessage Workers.LGWMessage


    renderingApi <- SmallGL.createRenderingEngine canvas
    -- initialize animation handler (and all pointer events).
    aHandler <- Widgets.registerAnimationHandler canvas (SmallGL.render renderingApi)
    -- selected object id events
    selectedObjIdD <- objectSelectionsDyn aHandler renderingApi
    inQuaWidget $ colorObjectsOnSelection scenarioB selectedObjIdD
    -- move objects events
    camLockedB <- inQuaWidget
      $ moveSelectedObjects aHandler renderingApi (current cameraD) scenarioB selectedObjIdD

    -- supply animation events to camera
    cameraD <- inQuaWidget $ dynamicCamera aHandler (current scenarioCenterD) camLockedB

    -- initialize WebGL rendering context
    registerEvent (SmallGLInput SmallGL.ViewPortResize)
        $ resizeEvents aHandler
    registerEvent (SmallGLInput SmallGL.ProjTransformChange)
        $ SmallGL.ProjM . Model.projMatrix <$> updated cameraD
    registerEvent (SmallGLInput SmallGL.ViewTransformChange)
        $ SmallGL.ViewM . Model.viewMatrix <$> updated cameraD

    scenarioB <- createScenario renderingApi

    let scenarioCenterE = fmapMaybe
                          (\m -> case m of
                             Workers.LGWSCStat st -> Just $ Scenario.centerPoint st
                             _ -> Nothing
                          ) loadedGeometryE
    scenarioCenterD <- holdDyn (vec2 0 0) scenarioCenterE

    -- Notify everyone that the program h finished starting up now
    mainBuilt <- getPostBuild
    performEvent_ . flip fmap mainBuilt . const $ do
        liftIO (setIsProgramBusy Idle)
        Widgets.play aHandler

    -- other init things
    -- load geometry from url if it is supplied
    quaSettings >>= sample . fmap QuaTypes.getSubmissionGeometryUrl . current
                >>= \ms -> case ms of
      Nothing -> return ()
      Just url -> do
        (ev, trigger) <- newTriggerEvent
        registerEvent (WorkerMessage Workers.LGWRequest)
          $ Workers.LGWLoadUrl . Scenario.withoutObjects <$> scenarioB <@> ev
        liftIO . void . forkIO $ threadDelay 2000000 >> trigger url



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
