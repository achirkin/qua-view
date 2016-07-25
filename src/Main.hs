{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
) where


-- Various thins I use
import Control.Arrow (second)
import Data.Geometry
import JsHs.JSString (JSString, append, unpack')
import Control.Monad (void, when)
import GHCJS.Useful
import Text.Read (readMaybe)
import Data.Coerce
import qualified JsHs.Array as JS
import JsHs.WebGL.Types (GLfloat)

-- Program Logic
--import Reactive
import Program
--import Program.Model.Camera (CState(..))

-- Events
--import Controllers.Pointer
--import Controllers.ElementResizing
import Controllers.GUIEvents
import qualified Controllers.GeoJSONFileImport as JFI

-- Get EventSense instances so that we can pass events into processing cycle
--import Program.Reactions ()

-- functional reactive programming
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs
import Program.Model.Camera
import Program.Model.City
import Program.View

main :: IO ()
main = do
    -- whole document
    body <- documentBody
    viewWidth <- getElementWidth body
    viewHeight <- getElementHeight body
--    print $ viewWidth
--    print $ viewHeight
    -- drawing area
    canvas <- getElementById "glcanvas"
    -- "import geometry" button converts GeoJSON into internal representation
    importButton <- getElementById "jsonfileinput"
    clearGeomButton <- getElementById "cleargeombutton"
    (clearGeomH, clearGeomFire) <- newAddHandler
    elementOnClick clearGeomButton (const $ clearGeomFire ClearingGeometry)
    -- geoJSON updates
    geoJSONImportsHandler <- JFI.geoJSONImports
    JFI.registerButton geoJSONImportsHandler importButton

    -- get default scaling level
    let geomScale = readMaybe . unpack' $ getHtmlArg "scale"

    -- get request processing
    let userProfile = case getHtmlArg "role" of
                    "edit" -> ExternalEditor
                    "view" -> ExternalViewer
                    _      -> Full
--    customGreetings userProfile
    -- create program and view
    let program = initProgram viewWidth viewHeight
            CState { viewPoint  = vector3 3 0 0,
                     viewAngles = (-pi/5, pi/12),
                     viewDist   = 30 }
            userProfile
            geomScale
    canv <- getCanvasById "glcanvas"
    view <- initView program canv

    -- reactive-banana event network
    heh <- elementHandler $ coerce canvas
    network <- compile $ mdo
      -- GeoJSON updates
      geoJSONImportE <- fromAddHandler $ JFI.addHandler geoJSONImportsHandler
      clearGeometryE <- fromAddHandler clearGeomH
      let cityUpdatesE = unions
                [ cityUpdates $ loadingCityJSONEvent geoJSONImportE
                , cityClears clearGeometryE
                ]
      -- canvas events
      pointerE <- pointerEvents heh
      wheelE   <- wheelEvents heh
      resizeE  <- resizeEvents heh
      curPointersB <- curPointers heh
      oldPointersB <- downPointers heh
      buttonsB' <- buttons heh
      ctrlKeyB <- ctrlKey heh
      shiftKeyB <- shiftKey heh

      let modButtons True True 1 = 4
          modButtons True False 1 = 2
          modButtons False True 1 = 2
          modButtons _ _ b = b
          buttonsB = modButtons <$> shiftKeyB <*> ctrlKeyB <*> buttonsB'
          coordsB = combinePointers <$> oldPointersB <*> curPointersB
      cameraB <- cameraBehavior (camera program)
                                 pointerE
                                 wheelE
                                 resizeE
                                 buttonsB
                                 coordsB

      -- update city with geoJSON
      programViewE' <- return (second return <$> programViewE) +^^+ accumE (program, return view) cityUpdatesE
          >>= mapEventIO (\(p,iov) -> (,) p <$> iov)
      programViewB <- stepper (program, view) programViewE'
      -- update camera
      let programViewB'' = (\(p,v) c -> (p{camera=c}, v)) <$> programViewB <*> cameraB
      -- render scene
      updateE <- updateEvents heh
      programViewE <- mapEventIO (\(p,v,t) -> (,) p <$> renderScene t p v)
                      (((\(p,v) t -> (p,v,t)) <$> programViewB'') <@> updateE)

      selectionE <- selection programViewB pointerE
      let updateSelE = unions
              [ selectOnScene wheelE
              , selectOnScene resizeE
              , selectOnScene ]


--      let programViewB = unionWith (\_ b -> b) pviewRenderE cityUpdatesE
      -- print camera state
--      reactimate $ (print . newState) <$> cameraB <@ updateE
      -- print current pressed buttons
--      reactimate $ (\b -> when (b /= 0) $ print b) <$> buttonsB <@ updateE
--      reactimate $ print <$> resizeE
      return ()
    actuate network
    play heh


--unionPviews [] = never
--unions xs = foldr1 (unionWith (.)) xs

--    -- run main reactive programming cycle and get event submission functions (in EventHole)
--    eventHole <- reactiveCycle program view
--
--    -- mouse/touch events
--    addEventlisteners canvas (reqEvent eventHole . EBox)
--                             (reqEvent eventHole . EBox)
--                             (void . optEvent eventHole . EBox)
--                             (reqEvent eventHole . EBox)
--                             (reqEvent eventHole . EBox)
--    -- add mouse wheel separately
--    onMouseWheel canvas (reqEvent eventHole . EBox)
--    -- resize viewport when body (and canvas) is resized
--    let canvasResize cre@(ResizeEvent w h) = do
--            setElementStyleWidth canvas w
--            setElementStyleHeight canvas h
--            setElementWidth canvas w
--            setElementHeight canvas h
--            reqEvent eventHole $ EBox cre
--    onElementResize body canvasResize
--
--    -- "submit geometry" button opens popup to save the geometry on server
--    submitButton <- getElementById "submitbutton"
--    case userProfile of
--        ExternalViewer -> elementParent submitButton >>= hideElement
--        ExternalEditor -> elementOnClick submitButton . const . reqEvent eventHole . EBox
--                $ SubmitScenario "http://www.archevolve.com/process.php"
--        Full -> elementOnClick submitButton . const . reqEvent eventHole . EBox
--                $ SubmitScenario "http://httpbin.org/post"
--
--
--    -- hide everything that is not related to the full profile
--    if userProfile /= Full
--    then do
--        getElementById "evaluatebutton" >>= elementParent >>= hideElement
--        getElementById "itabGeometry" >>= hideElement
--        getElementById "itabServices" >>= hideElement
--        getElementById "itabLuci" >>= hideElement
--        getElementById "tabGeometry" >>= hideElement
--        getElementById "tabServices" >>= hideElement
--        getElementById "tabLuci" >>= hideElement
--    else do
--        -- "evaluate" button runs current service
--        evaluateButton <- getElementById "evaluatebutton"
--        elementOnClick evaluateButton . const $ reqEvent eventHole $ EBox ServiceRunBegin
--        clearServiceButton <- getElementById "clearbutton"
--        elementOnClick clearServiceButton . const $ reqEvent eventHole $ EBox ClearServiceResults
--
--        -- "import geometry" button converts GeoJSON into internal representation
--        importButton <- getElementById "jsonfileinput"
--        onGeoJSONFileImport importButton (reqEvent eventHole . EBox)
--
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

--    -- load geometry from url
--    case getHtmlArg "geomUrl" of
--        "" -> do
--            loadGeoJSONFromLink "insidePolys.js" True  (reqEvent eventHole . EBox)
----            loadGeoJSONFromLink "outsidePolys.js" False (reqEvent eventHole . EBox)
--            loadGeoJSONFromLink "lines.js" False (reqEvent eventHole . EBox)
--        u -> loadGeoJSONFromLink u True  (reqEvent eventHole . EBox)
--
--    -- experiments
--    logText $ "Started " ++ show userProfile ++ " session of modeler."
--
--
--    -- done!
--    -- simulate an event to force render picture
--    canvasResize $ ResizeEvent viewWidth viewHeight
    -- remove loading splash
    programIdle


--customGreetingHTML :: Profile -> JSString
--customGreetingHTML profile = wrapf $ case profile of
--    Full ->
--        " You are in a standard Luci-enabled mode. Use control panel on the right hand-side to \
--        \ work with scenarios, available Luci computing services, and GeoJSON geometry."
--    ExternalEditor ->
--        " You are in the editor mode. \
--        \ Edit the geometry according to a given task, and then \
--        \ save it on our server."
--    ExternalViewer ->
--        " You are in the viewer mode. \
--        \ You can browse and change geometry locally, but no changes would be saved on our server."
--    where thead = "<hr><div style=\"font-size: 125%; text-align: justify;\">"
--          ttail = "</div>"
--          wrapf t = thead `append` t `append` ttail
--
--customGreetings :: Profile -> IO ()
--customGreetings profile = getElementById "greetings"
--    >>= flip insertAfterHTML (customGreetingHTML profile)



(+^^+) :: Applicative m => m (Event e) -> m (Event e) -> m (Event e)
a +^^+ b = unionWith (\_ b -> b) <$> a <*> b


combinePointers :: JS.Array PointerPos -> JS.Array PointerPos -> [(Vector2 GLfloat, Vector2 GLfloat)]
combinePointers a b = zipWith (\p1 p2 -> ( vector2 (realToFrac $ posX p1) (realToFrac $ posY p1)
                                         , vector2 (realToFrac $ posX p2) (realToFrac $ posY p2)
                                         )
                              ) (JS.toList a) (JS.toList b)
