{-# LANGUAGE JavaScriptFFI #-}
module Main (
    main
) where


-- Various thins I use
import Control.Monad (void)
import GHCJS.Useful
import Geometry.Space


-- Program Logic
import Reactive
import Program
import Program.Model.Camera (CState(..))

-- Events
import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents
import Controllers.GeoJSONFileImport

-- Get EventSense instances so that we can pass events into processing cycle
import Program.Reactions ()


main :: IO ()
main = do
    -- whole document
    body <- documentBody
    viewWidth <- getElementWidth body
    viewHeight <- getElementHeight body
    -- drawing area
    canvas <- getElementById "glcanvas"

    -- run main reactive programming cycle and get event submission functions (in EventHole)
    let program = initProgram viewWidth viewHeight
            CState { viewPoint  = Vector3 (-3) 0 2,
                     viewAngles = Vector2 (-pi/5) (pi/12),
                     viewDist   = 40 }
    view <- initView program canvas
    eventHole <- reactiveCycle program view

    -- mouse/touch events
    addEventlisteners canvas (reqEvent eventHole . EBox)
                             (reqEvent eventHole . EBox)
                             (void . optEvent eventHole . EBox)
                             (reqEvent eventHole . EBox)
                             (reqEvent eventHole . EBox)
    -- add mouse wheel separately
    onMouseWheel canvas (reqEvent eventHole . EBox)
    -- resize viewport when body (and canvas) is resized
    let canvasResize re@(ResizeEvent w h) = do
            setElementStyleWidth canvas w
            setElementStyleHeight canvas h
            setElementWidth canvas w
            setElementHeight canvas h
            reqEvent eventHole $ EBox re
    onElementResize body canvasResize

    -- "evaluate" button runs current service
    evaluateButton <- getElementById "evaluatebutton"
    elementOnClick evaluateButton . const $ reqEvent eventHole $ EBox ServiceRunBegin
    clearServiceButton <- getElementById "clearbutton"
    elementOnClick clearServiceButton . const $ reqEvent eventHole $ EBox ClearServiceResults

    -- "import geometry" button converts GeoJSON into internal representation
    importButton <- getElementById "jsonfileinput"
    onGeoJSONFileImport importButton (reqEvent eventHole . EBox)

    -- "clear geometry" button removes all buildings from the city
    clearGeomButton <- getElementById "cleargeombutton"
    elementOnClick clearGeomButton . const $ reqEvent eventHole $ EBox ClearingGeometry

    -- Connect to Luci
    luciConnectButton <- getElementById "loginbutton"
    elementOnClick luciConnectButton . const $ do
        host <- getElementById "inputip" >>= getInputValue
        name <- getElementById "inputlogin" >>= getInputValue
        pass <- getElementById "inputpass" >>= getInputValue
        reqEvent eventHole $ EBox LuciConnect
            { cHost = host
            , cUser = name
            , cPass = pass
            }

    -- experiments
    logText "Hello World!"
    logText "Printing to panel\nin two lines!\n(or event three)"
    loadGeoJSONFromLink "lines.js" False (reqEvent eventHole . EBox)
    loadGeoJSONFromLink "outsidePolys.js" False (reqEvent eventHole . EBox)
    loadGeoJSONFromLink "insidePolys.js" True  (reqEvent eventHole . EBox)

    -- done!
    -- simulate an event to force render picture
    canvasResize $ ResizeEvent viewWidth viewHeight
    -- remove loading splash
    programIdle
