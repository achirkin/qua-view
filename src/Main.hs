{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main (
    main
) where

import GHCJS.Types
--import GHCJS.Marshal
--import GHCJS.Foreign
--import Unsafe.Coerce
--import Data.Coerce (coerce)
import Data.JSString

import Data.Geometry

-- Various thins I use
import Control.Monad (void)
import GHCJS.Useful

--import Geometry.Space
----import GHCJS.Instances
--
---- Program Logic
import Reactive
import Program
import Program.Model.Camera (CState(..))
--
-- Events
import Controllers.Pointer
import Controllers.ElementResizing
import Controllers.GUIEvents
import Controllers.GeoJSONFileImport
--
---- Get EventSense instances so that we can pass events into processing cycle
import Program.Reactions ()
--import Geometry.Structure
--
--import Program.Model.CityObject
--import Program.Model.GeoJSON

import Data.Geometry.Structure.LinearRing as LRing
import Data.Geometry.Structure.PointSet as PSet
import Data.Geometry.Structure.Polygon as Poly

import SmallGL.WritableVectors

main :: IO ()
main = do
    logText "Hello!"
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
    logShowable userProfile
    customGreetings userProfile
    -- create program and view
    let program = initProgram viewWidth viewHeight
            CState { viewPoint  = vector3 (-3) (-2) 0,
                     viewAngles = (-pi/5, pi/12),
                     viewDist   = 40 }
            userProfile
    canv <- getCanvasById "glcanvas"
    view <- initView program canv
    -- run main reactive programming cycle and get event submission functions (in EventHole)
    eventHole <- reactiveCycle program view
    print $ userRole program

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
--        evaluateButton <- getElementById "evaluatebutton"
--        elementOnClick evaluateButton . const $ reqEvent eventHole $ EBox ServiceRunBegin
--        clearServiceButton <- getElementById "clearbutton"
--        elementOnClick clearServiceButton . const $ reqEvent eventHole $ EBox ClearServiceResults

        -- "import geometry" button converts GeoJSON into internal representation
        importButton <- getElementById "jsonfileinput"
        onGeoJSONFileImport importButton (reqEvent eventHole . EBox)

--        -- "clear geometry" button removes all buildings from the city
--        clearGeomButton <- getElementById "cleargeombutton"
--        elementOnClick clearGeomButton . const $ reqEvent eventHole $ EBox ClearingGeometry
--
--        -- Connect to Luci
--        luciConnectButton <- getElementById "loginbutton"
--        elementOnClick luciConnectButton . const $ do
--            host <- getElementById "inputip" >>= getInputValue
--            name <- getElementById "inputlogin" >>= getInputValue
--            pass <- getElementById "inputpass" >>= getInputValue
--            reqEvent eventHole $ EBox LuciConnect
--                { cHost = host
--                , cUser = name
--                , cPass = pass
--                }
--
--
--    let vec = Vector3 0.3 0.2 0.1 :: Vector3 Double
--    vecRef <- toJSVal
--    mvec <- fromJSVal
--    print vec
--    printRef vecRef
--    print mvec
--
----    toJSRef (Vector4 0.8 0.4 0.4 1) >>= printRef
----    toJSRef (Vector3 0.3 0.2 0.1) >>= printRef
--    mapM toJSRef [Vector3 0.1 0.2 1, Vector3 0.2 4 0, Vector3 1 5 (0::Double)] >>= toArray >>= printRef
----    toArray (unsafeCoerce [1,23,5,2::Float] :: [JSElement]) >>= printRef
----    jsrMultiPolygon1 1
--    let poly = SimplePolygon
--                             [ Vector3 (-1) 2   (-1)
--                             , Vector3   1  1.5 (-1)
--                             , Vector3   1  2     1
--                             , Vector3 (-1) 1.5   (1:: Double)
--                             ]
--    polyRef <- toJSVal
--    mpoly <- fromJSVal
--
--    print poly
--    printRef polyRef
--    print mpoly
--
--    let poly1 = SimpleConvexPolygon
--                             [ Vector3 (-1) 2   (-1.5)
--                             , Vector3   1  2     1
--                             , Vector3 (-1) 1.5   (2.1:: Float)
--                             ]
--    polyRef1 <- toJSVal
--    mpoly1 <- fromJSVal
--
--    print poly1
--    printRef polyRef1
--    print mpoly1
--
--    let poly2 = GenericPolygon
--                             [SimplePolygon [ Vector3 (-1) 2   (-1)
--                             , Vector3   1  1.5 (-1)
--                             , Vector3   1  2     1
--                             , Vector3 (-1) 1.5   (1:: Double)
--                             ]]
--    polyRef2 <- toJSVal
--    mpoly2 <- fromJSVal
--
--    print poly2
--    printRef polyRef2
--    print mpoly2
--
--
--    let poly3 = GenericPolygon
--                             [SimplePolygon [
--                               Vector3 (-1) 1.5   (1:: Double)
--                             ], SimplePolygon [ Vector3 (-1) 2 (-1)
--                             , Vector3 (-1) 1.5   (2.1:: Double)
--                             ]]
--    polyRef3 <- toJSVal
--    mpoly3 <- fromJSVal
--
--    print poly3
--    printRef polyRef3
--    print mpoly3
--
--    b <- toJSRef $ ScenarioObject SLbuildings 162 Nothing (pure $ building Dynamic poly1 )
--    b2 <- toJSRef $ ScenarioObject SLfootprints 162 Nothing (pure $ building Dynamic poly1 )
--    printRef b
--    printRef b2
--
--    mb <- fromJSRef . unsafeCoerce $ b :: IO (Maybe ImportedScenarioObject)
--    print mb
--    mb2 <- fromJSRef . unsafeCoerce $ b2 :: IO (Maybe ImportedScenarioObject)
--    print mb2
--
--    -- experiments
--    logText $ "Started " ++ show userProfile ++ " session of modeler."
----    loadGeoJSONFromLink "lines.js" False (reqEvent eventHole . EBox)
------    loadGeoJSONFromLink "outsidePolys.js" False (reqEvent eventHole . EBox)
----    loadGeoJSONFromLink "insidePolys.js" True  (reqEvent eventHole . EBox)

    -- done!
    -- simulate an event to force render picture
    canvasResize $ ResizeEvent viewWidth viewHeight
    -- remove loading splash
    programIdle


customGreetingHTML :: Profile -> String
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
          wrapf t = thead ++ t ++ ttail

customGreetings :: Profile -> IO ()
customGreetings profile = getElementById "greetings"
    >>= flip insertAfterHTML (pack $ customGreetingHTML profile)

--
--foreign import javascript safe "console.log({ \
--    \ 'type': 'Feature', properties: { Layer: 'buildings', 'SubClasses': $1, 'ExtendedEn': null, \
--    \ 'Linetype': null, 'EntityHand': '6F', 'Text': null }, 'geometry': { 'type': 'Polygon', 'coordinates': \
--    \[ [ [ 17093338.071588061749935, 39302064.126328192651272 ], [ 17093698.622214030474424, 39298130.289272576570511 ],
--     [ 17083208.390065658837557, 39297168.820936642587185 ], [ 17082847.839439678937197, 39301102.657992236316204 ],
--     [ 17093338.071588061749935, 39302064.126328192651272 ] ] ] } });"
--    jsrMultiPolygon1 :: Int -> IO ()

