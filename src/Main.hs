{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
) where


-- Various thins I use
import Data.Geometry
import Data.JSString (JSString, append)
import Control.Monad (void)
import GHCJS.Useful

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

    -- get request processing
    let userProfile = case getHtmlArg "role" of
                    "edit" -> ExternalEditor
                    "view" -> ExternalViewer
                    _      -> Full
    customGreetings userProfile
    -- create program and view
    let program = initProgram viewWidth viewHeight
            CState { viewPoint  = vector3 3 0 0,
                     viewAngles = (-pi/5, pi/12),
                     viewDist   = 30 }
            userProfile
    canv <- getCanvasById "glcanvas"
    view <- initView program canv
    -- run main reactive programming cycle and get event submission functions (in EventHole)
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
    let canvasResize cre@(ResizeEvent w h) = do
            setElementStyleWidth canvas w
            setElementStyleHeight canvas h
            setElementWidth canvas w
            setElementHeight canvas h
            reqEvent eventHole $ EBox cre
    onElementResize body canvasResize

    -- "submit geometry" button opens popup to save the geometry on server
    submitButton <- getElementById "submitbutton"
    case userProfile of
        ExternalViewer -> elementParent submitButton >>= hideElement
        ExternalEditor -> elementOnClick submitButton . const . reqEvent eventHole . EBox
                $ SubmitScenario "http://www.archevolve.com/process.php"
        Full -> elementOnClick submitButton . const . reqEvent eventHole . EBox
                $ SubmitScenario "http://httpbin.org/post"


    -- hide everything that is not related to the full profile
    if userProfile /= Full
    then do
        getElementById "evaluatebutton" >>= elementParent >>= hideElement
        getElementById "itabGeometry" >>= hideElement
        getElementById "itabServices" >>= hideElement
        getElementById "itabLuci" >>= hideElement
        getElementById "tabGeometry" >>= hideElement
        getElementById "tabServices" >>= hideElement
        getElementById "tabLuci" >>= hideElement
    else do
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
    logText $ "Started " ++ show userProfile ++ " session of modeler."
    loadGeoJSONFromLink "insidePolys.js" True  (reqEvent eventHole . EBox)
--    loadGeoJSONFromLink "outsidePolys.js" False (reqEvent eventHole . EBox)
    loadGeoJSONFromLink "lines.js" False (reqEvent eventHole . EBox)

    -- done!
    -- simulate an event to force render picture
    canvasResize $ ResizeEvent viewWidth viewHeight
    -- remove loading splash
    programIdle


customGreetingHTML :: Profile -> JSString
customGreetingHTML profile = wrapf $ case profile of
    Full ->
        " You are in a standard Luci-enabled mode. Use control panel on the right hand-side to \
        \ work with scenarios, available Luci computing services, and GeoJSON geometry."
    ExternalEditor ->
        " You are in the editor mode. \
        \ Edit the geometry according to a given task, and then \
        \ save it on our server."
    ExternalViewer ->
        " You are in the viewer mode. \
        \ You can browse and change geometry locally, but no changes would be saved on our server."
    where thead = "<hr><div style=\"font-size: 125%; text-align: justify;\">"
          ttail = "</div>"
          wrapf t = thead `append` t `append` ttail

customGreetings :: Profile -> IO ()
customGreetings profile = getElementById "greetings"
    >>= flip insertAfterHTML (customGreetingHTML profile)

