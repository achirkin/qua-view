{-# LANGUAGE JavaScriptFFI, DataKinds #-}
-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}
module Main (
    main
) where

-- import Language.Haskell.TH

import GHCJS.WebGL
import Control.Monad
import Control.Concurrent (threadDelay)
--import Control.Arrow ((&&&))
--import Control.Applicative ((<*>))
--import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (liftIO)


import Data.Geospatial
--import qualified Data.ByteString.Lazy as BSL (putStrLn)

import qualified Data.Aeson as A -- (FromJSON(), fromJSON, Result(..))
import Data.List (intercalate)
import Data.IORef
import Data.Bits (shift)
--import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody, documentGetElementById) -- , documentCreateElement
--import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML)
import GHCJS.DOM.Element (IsElement, elementGetClientWidth, elementGetClientHeight, elementOnchange)
import GHCJS.DOM.EventM (Signal,target)
import GHCJS.DOM.Types (Element,unElement)

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import Geometry.Space
import Geometry.Space.Transform
import Geometry.Structure
import Geometry.Math


import GUI.PointerEvent
import GUI.LuciClient

import Drawable.OCCamera
import Drawable.World

import Model.City
import Model.CityObject
import Model.Grid

import Model.ScalarField
import Model.RadianceService

import Model.GeoJsonImport

--import System.Random
-- closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js
-- style="position:fixed;left:0;top:0;padding:0;margin:0;width:100%;height:100%;overflow:hidden;"


--vpWidth :: GLsizei
--vpWidth = 800
--vpHeight :: GLsizei
--vpHeight = 1000

--import TextImporter

--primes :: String
--primes = [litFile|uploadia.sh|]

main :: IO ()
main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    vpWidth <- liftM (round . max 100) $ elementGetClientWidth body
    vpHeight <- liftM (round . max 100) $ elementGetClientHeight body
--    let (vpWidth, vpHeight) = (512,512)

--    let (vpWidth, vpHeight) = if vpWidth' > 500 || vpHeight' > 500
--                              then (vpWidth' `div` 2, vpHeight' `div` 2)
--                              else (vpWidth', vpHeight')

--    htmlElementSetInnerHTML body $
----        "<div id=\"divview\" "
----        ++ "style=\"position: fixed; right: 0; top: 0; padding: 0; margin: 0; width: "
----        ++ show (30::Int) ++ "%; height: "
----        ++ show (100::Int) ++ "%; z-index = 1; overflow: hidden;\" width=\""
----        ++ show (30::Int) ++ "%;\" height=\""
----        ++ show (100::Int) ++ "%;\"></div>"
--        "<canvas id=\"glcanvas\" width=\""
--        ++ show vpWidth ++ "px;\" height=\""
--        ++ show vpHeight ++ "px;\"></canvas>"

    Just canvas <- documentGetElementById doc "glcanvas"
--    Just divview <- documentGetElementById doc "divview"
    let canv = unElement $ canvas
    elementSetSize canv vpWidth vpHeight
    elementSetStyleSize canv vpWidth vpHeight
    ctx <- getCtx canv
    clearColor ctx 0 0 0 0
    enable ctx gl_DEPTH_TEST
    blendFunc ctx gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA
    depthFunc ctx gl_LEQUAL
    viewport ctx 0 0 vpWidth vpHeight

--    atlaspic <- loadImage "atlaspic.png"
--    atex <- initTexture ctx $ Left atlaspic

--    img <- loadImage "ia512tex.png"
----    printRef img
--    tex <- initTexture ctx $ Left img
--    printRef tex

    -- Setup world
    camRef <- newIORef $ initOCCamera (fromIntegral vpWidth) (fromIntegral vpHeight)
        OCCState { viewPoint     = Vector3 0 0 0,
                   viewAngles    = Vector2 (pi/3) (pi/4),
                   viewDist      = 40 }
    iworld <- initWorld ctx camRef 0 (Vector3 (-0.5) (-1) 0.6)
    worldRef <- newIORef iworld

    -- Create sample objects
    grid <- createGrid iworld 500 100 (Vector4 0.6 0.6 0.8 1)
    let numb2 = 8
    cityRef <- buildCity iworld [building1,building2,building3,hut1,building4]
            [ [Vector3 8 0 0],[Vector3 (-8) 0 0],[Vector3 (-8) 0 (-6)]
            , [Vector3 (-i*4 - 12) 0 (j*4) | i <- [1..numb2], j <- [1..numb2]]
            , [Vector3 0 0 4]]
            [[pi/3],[0],[0],map ((pi/100)*) [1..], [1]]
            [[Nothing],[Nothing],[Nothing],repeat Nothing, [Nothing]]
        >>= newIORef
--    cityRef <- buildCity iworld (repeat building4)
--            [Vector3 (-i*4 - 12) 0 (j*4) | i <- [1..numb2], j <- [1..numb2]]
--            (map ((pi/100)*) [1..])
--        >>= newIORef
--    readIORef cityRef >>= print . (minBBox :: City -> BoundingBox 2 GLfloat)
    addCityObject' iworld road1 (Vector3 0 0 0) 0 cityRef
--    readIORef cityRef >>= print . (minBBox :: City -> BoundingBox 2 GLfloat)

    let applyService =
          readIORef cityRef >>= \city -> do
          updateCityGround iworld city
          city' <- updateCityTextures
                iworld
                (distFromPoint (Vector3 (-10) 4 5) $ cityEvaluationGrid 1 city)
                palette
                city
          writeIORef cityRef city'
            where palette = Bezier3Palette (Vector4 255 0 0 255)
                                           (Vector4 100 255 0 255)
                                           (Vector4 0 255 100 255)
                                           (Vector4 0 0 255 255)

    -- Everything to draw in the world
    let drawSequence world = do
            city <- readIORef cityRef
            draw world grid
            draw world city

    -- Mouse controls
    curTime <- getTime
    ltimeRef <- newIORef (curTime-1)
    posStateRef <- newIORef (Idle, NoInteraction)

    let selectNow = selectOnce worldRef [cityRef]
    displayOnTime <- liftM animate
        . syncCallback NeverRetain False $ displayOnce ltimeRef worldRef drawSequence
    dispAndSelectOnTime <- liftM animate
        . syncCallback NeverRetain False $ displayOnce ltimeRef worldRef drawSequence >> selectNow


    -- Resize body
    _ <- elementOnResize body $ \_ -> do
            w <- liftM (round . max 100) $ elementGetClientWidth body
            h <- liftM (round . max 100) $ elementGetClientHeight body
            elementSetSize canv w h
            elementSetStyleSize canv w h
            viewport ctx 0 0 w h
            modifyIORef' camRef $ \OCCamera{newState = ns} -> initOCCamera (fromIntegral w) (fromIntegral h) ns
            fb <- initSelectorFramebuffer ctx $ Vector2 w h
            modifyIORef' worldRef $ \wrld@World{selector = SelectorObject _ pickedColorArr} -> wrld{selector = SelectorObject fb pickedColorArr}
            dispAndSelectOnTime

    Just console <- documentGetElementById doc "consolecontent"

    onMouseWheel canvas $ \x -> modifyIORef' camRef (scroll (if x > 0 then 0.25 else -0.2)) >> dispAndSelectOnTime
    onCancel canvas dispAndSelectOnTime
    onMove canvas True
        (setInteractionContext worldRef cityRef camRef posStateRef)
        ((>>= flip when displayOnTime) . guiMove camRef cityRef posStateRef)
        ((>> dispAndSelectOnTime) . afterAction posStateRef)
        (\eve -> do
            updated <- guiClick worldRef cityRef eve
            if updated then displayOnTime else return ()
        )


    Just fsbutton <- documentGetElementById doc "fullscreenbutton"
    onSimpleClick fsbutton False $ const toggleFullScreen

    Just evalbutton <- documentGetElementById doc "evaluatebutton"
    Just clearbutton <- documentGetElementById doc "clearbutton"
    let clearb = unElement clearbutton
    onSimpleClick evalbutton False $ const (
        programInProgress >> applyService >> showParentElem clearb >> dispAndSelectOnTime
        >> programIdle)
    onSimpleClick clearbutton False $ const (clearCityTextures' worldRef cityRef >> hideParentElem clearb  >> dispAndSelectOnTime)

    Just guipanel <- documentGetElementById doc "guipanel"
    let guip = unElement guipanel
    Just toolboxbutton <- documentGetElementById doc "toolboxbutton"
    onSimpleClick toolboxbutton False . const $ toggleGUIPanel guip

    -- submit... very ugly!
    Just submitbutton <- documentGetElementById doc "loginbutton"
    onSimpleClick submitbutton False . const $ do
        programInProgress
        host <- getInputValue "inputip"
        name <- getInputValue "inputlogin"
        pass <- getInputValue "inputpass"
        lc <- connectToLuci host
        logText console $ "Connected to Luci on " ++ host
        ans <- authenticate lc name pass
        setInnerHTML "ipaddressinfo" (hostOf lc)
        logText console ans
        getElementById "loginform" >>= hideElem
        getElementById "logondiv" >>= showElem
        programIdle


    Just jsonfileinput <- documentGetElementById doc "jsonfileinput"
    _ <- elementOnchange jsonfileinput $ do
        el <- liftM unElement target
        liftIO $ do
            logText console $ "Trying to parse GeoJSON FeatureCollection..."
            c <- getFiles el >>= fromJSRef_aeson :: IO (Maybe (GeoFeatureCollection A.Value))
            case c of
                Nothing -> logText console "Could not read geometry"
                Just gfc -> do
                    isBehChecked <- isChecked $ toJSString "dynamicstaticswitcher"
                    let (geoms,msgs) = featureCollection2DtoObjects behav 0.15 gfc
                        sh = neg . mean $ map (applyV3 . wrap zeros) geoms
                        geoms' = map (translate sh () >>) geoms
                        behav = if isBehChecked then Dynamic else Static
                    logText console $ unlines msgs
                    addCityObjectsT' iworld geoms' cityRef
                    logText console $ "Successfully imported "
                        ++ show (length geoms) ++ " geometries, skipped " ++ show (length msgs) ++ "."
                    dispAndSelectOnTime

    -- Draw world
    dispAndSelectOnTime
    threadDelay 1000000
    programIdle


-- | Convert JSON object in JavaScript back to Haskell data that implements fromJSON a class
fromJSRef_aeson :: A.FromJSON a => JSRef a -> IO (Maybe a)
fromJSRef_aeson = liftM (>>= f . A.fromJSON) . fromJSRef . castRef
    where f (A.Error _) = Nothing
          f (A.Success x) = Just x

--fromJSRef_aeson2 :: A.FromJSON a => JSRef a -> IO (Maybe (A.Result a))
--fromJSRef_aeson2 = liftM (liftM A.fromJSON) . fromJSRef . castRef

displayOnce :: IORef (GLfloat)
            -> IORef World
            -> (World -> IO ())
            -> IO ()
displayOnce luref wref drawCall = do
    lastUpdate <- readIORef luref
    ctime <- getTime
    when (lastUpdate + 0.005 < ctime) $ do
        writeIORef luref (lastUpdate+10)
        oldworld <- readIORef wref
        world <- prepareWorldRender oldworld ctime
        drawCall world
        writeIORef wref world
        getTime >>= writeIORef luref


selectOnce :: (F.Foldable s, T.Traversable s, Selectable a)
           => IORef World
           -> s (IORef a)
           -> IO ()
selectOnce wref selectables = do
    world <- readIORef wref
    -- print "Draw selection buffer"
    T.forM selectables readIORef >>= applySelector world

----------------------------------------------------------------------------------------------
-- Interaction  ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
--FileReader.readAsText

foreign import javascript unsafe "$r = document.getElementById($1).checked;"
    isChecked :: JSString -> IO Bool

foreign import javascript interruptible "var r = new FileReader(); \
    \ var load = function() { \
    \ if (r.readyState != FileReader.EMPTY ) { $c(JSON.parse(r.result)); }}; \
    \ r.onloadend = load;  \
    \ r.readAsText($1.files[0]);"
    getFiles :: JSRef a -> IO (JSRef b)

--config' = $(do
--    s <- runIO $ readFile "src/model/Grid.hs"
--    return $ LitE $ stringL s)

foreign import javascript unsafe "$r = document.getElementById($1);"
    getElementById' :: JSString -> IO (JSRef a)

getElementById :: String -> IO (JSRef a)
getElementById = getElementById' . toJSString

foreign import javascript unsafe "document.getElementById($1).innerHTML = $2;"
    setInnerHTML' :: JSString -> JSString -> IO ()

setInnerHTML :: String -> String -> IO ()
setInnerHTML el val = setInnerHTML' (toJSString el) (toJSString val)


foreign import javascript unsafe "$r = document.getElementById($1).value;"
    getInputValue' :: JSString -> IO JSString

getInputValue :: String -> IO String
getInputValue = liftM fromJSString . getInputValue' . toJSString

foreign import javascript unsafe "if ($1.className == 'idleguipanel') { \
    \    $1.className = 'activeguipanel'; \
    \    document.getElementById('guiplaceholder').className = 'activeplaceholder'; \
    \ } else {  \
    \    $1.className = 'idleguipanel';  \
    \    document.getElementById('guiplaceholder').className = 'idleplaceholder'; \
    \ }"
    toggleGUIPanel :: JSRef a -> IO ()

foreign import javascript unsafe "$1.style.display = 'none';"
    hideElem :: JSRef a -> IO ()

foreign import javascript unsafe "$1.style.display = 'block';"
    showElem :: JSRef a -> IO ()

foreign import javascript unsafe "$1.parentNode.style.visibility = 'hidden';"
    hideParentElem :: JSRef a -> IO ()

foreign import javascript unsafe "$1.parentNode.style.visibility = 'visible';"
    showParentElem :: JSRef a -> IO ()

foreign import javascript unsafe "var n = $1.children.length; \
    \ var panelr = document.getElementById('guipanel').getBoundingClientRect(); \
    \ var z = panelr.top + 0.6*panelr.height; \
    \ while(n > 0 && $1.children[0].getBoundingClientRect().top < z) { \
    \   $1.removeChild($1.children[0]); n--; \
    \ } \
    \ for(var i = 0; i < n; i++) { \
    \    $1.children[i].className = 'consolem' + Math.max(i-n+9,0); \
    \ } \
    \ var newDiv = document.createElement(\"div\"); \
    \ newDiv.innerHTML = $2; \
    \ newDiv.className = 'consolem9'; \
    \ $1.appendChild(newDiv); "
    logText' :: JSRef a -> JSString -> IO ()

logText :: Element -> String -> IO ()
logText el s = logText' (unElement el) (toJSString . intercalate "<br/>" . lines $ s)

--foreign import javascript interruptible "var img = new Image();img.crossOrigin='anonymous';img.onload=function(){$c(img);};img.src = $1;"
--    loadImage' :: JSString -> IO TexImageSource
--
--loadImage :: String -> IO TexImageSource
--loadImage = loadImage' . toJSString


foreign import javascript unsafe "if (!document['fullscreenElement'] && \
    \  !document['mozFullScreenElement'] && \
    \  !document['webkitFullscreenElement'] && !document['msFullscreenElement'] && !document['fullScreen']) { \
    \    if (document.documentElement['requestFullscreen']) { \
    \      document.documentElement.requestFullscreen(); \
    \    } else if (document.documentElement['msRequestFullscreen']) { \
    \      document.documentElement.msRequestFullscreen(); \
    \    } else if (document.documentElement['mozRequestFullScreen']) { \
    \      document.documentElement.mozRequestFullScreen(); \
    \    } else if (document.documentElement['webkitRequestFullscreen']) { \
    \      document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT); \
    \    } \
    \    document.getElementById('fullscreenbshape').setAttribute('d','M14,14H19V16H16V19H14V14M5,14H10V19H8V16H5V14M8,5H10V10H5V8H8V5M19,8V10H14V5H16V8H19Z'); \
    \  } else { \
    \    if (document['exitFullscreen']) { \
    \      document.exitFullscreen(); \
    \    } else if (document['msExitFullscreen']) { \
    \      document.msExitFullscreen(); \
    \    } else if (document['mozCancelFullScreen']) { \
    \      document.mozCancelFullScreen(); \
    \    } else if (document['webkitExitFullscreen']) { \
    \      document.webkitExitFullscreen(); \
    \    } else { \
    \      document.cancelFullScreen(); \
    \      document.exitFullscreen(); \
    \    } \
    \    document.getElementById('fullscreenbshape').setAttribute('d','M5,5H10V7H7V10H5V5M14,5H19V10H17V7H14V5M17,14H19V19H14V17H17V14M10,17V19H5V14H7V17H10Z'); \
    \  }"
    toggleFullScreen :: IO ()

--foreign import javascript safe "document.getElementById('divview').innerHTML = $1;"
--    printToDiv' :: JSString -> IO ()
--
--printToDiv :: (Show a) => a -> IO ()
--printToDiv = printToDiv' . toJSString . show


foreign import javascript unsafe "window.requestAnimationFrame($1);"
   animate :: JSFun (IO ()) -> IO ()

foreign import javascript unsafe "addResizeListener($1, $2);"
   elementOnResize' :: JSRef elem -> JSFun (IO ()) -> IO ()

elementOnResize :: IsElement self => Signal self (self -> IO ())
elementOnResize self func = do
    el <- toJSRef self
    syncCallback AlwaysRetain False (func self) >>= elementOnResize' el >> return (return ())


foreign import javascript unsafe "$1.setAttribute(\"width\", $2 + \"px\");$1.setAttribute(\"height\", $3 + \"px\");"
    elementSetSize :: JSRef a -> GLsizei -> GLsizei -> IO ()


foreign import javascript unsafe "$1.style.width=$2 + \"px\";$1.style.height=$3 + \"px\";"
    elementSetStyleSize :: JSRef a -> GLsizei -> GLsizei -> IO ()


foreign import javascript safe "$r = new Date().getTime()/1000"
    getTime :: IO (GLfloat)

--foreign import javascript safe "console.log($1)"
--    printRef :: JSRef a -> IO ()


----------------------------------------------------------------------------------------------
-- Pointer actions ---------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- | Interaction regime
data InteractionContext = Idle | CameraMotion | CityChange (Int,Int) deriving (Eq)

-- | How do we handle previous and current state of the pointers together
data MoveAction = MouseAction MouseButton (Vector2 GLfloat) (Vector2 GLfloat)
                | FingerAction [Vector2 GLfloat] [Vector2 GLfloat]


-- | Says whether city was changed or not
afterAction :: IORef (InteractionContext, Interaction)
            -> PointerClickEvent
            -> IO Bool
afterAction icref _ = readIORef icref >>= \(ic,_) -> return $ case ic of
    CityChange _ -> True
    _ -> False

-- | Set interaction context for building or camera movement
setInteractionContext :: IORef World
                      -> IORef City
                      -> IORef OCCamera
                      -> IORef (InteractionContext, Interaction)
                      -> PointerClickEvent
                      -> IO ()
setInteractionContext worldref cityRef camRef icref (Click interaction) = case interaction of
        (Mouse LeftButton pos) -> update pos
        (Mouse RightButton pos) -> update pos
        (Touches (pos:_))      -> update pos
        _ -> writeIORef icref (CameraMotion, interaction)
    >> modifyIORef' camRef (\c@OCCamera{newState = nstate} -> c{oldState = nstate})
    where update pos = do
            i <- readIORef worldref >>= getSelection pos
            j  <- liftM activeObj $ readIORef cityRef
            writeIORef icref $ if i == j && fst i /= 0 && snd i /= 0
                then (CityChange i, interaction)
                else (CameraMotion, interaction)

-- | At this moment, the only action is to select a building
guiClick :: IORef World
         -> IORef City
         -> PointerClickEvent
         -> IO Bool
guiClick worldRef cityRef (Click interaction) = case interaction of
    (Touches (_:_:_:_:_)) -> toggleFullScreen >> return True
    (Mouse LeftButton pos) -> readIORef worldRef >>= getSelection pos >>= update
    (Touches (pos:_)) -> readIORef worldRef >>= getSelection pos >>= update
    (Mouse _ _) -> update (0,0)
    _ -> return False
    where update i = do
            city <- readIORef cityRef
            if i == activeObj city
            then return False
            else writeIORef cityRef city{activeObj = i} >> return True

-- | Move building or camera
guiMove :: IORef OCCamera
        -> IORef City
        -> IORef (InteractionContext, Interaction)
        -> PointerMoveEvent
        -> IO Bool
guiMove camRef cityRef icRef (Move ls cs _) = case maction of
    Nothing -> return False
    Just action -> readIORef icRef >>= \(ic, sint) -> case ic of
        Idle         -> return False
        CameraMotion -> cameraMove camRef $ recomb sint action
        CityChange _ -> buildingMove camRef cityRef action
    where maction = combine ls cs
          combine _ NoInteraction = Nothing
          combine os (Mouse but npos) = case os of
            Mouse _ opos  -> Just $ MouseAction but opos npos
            Touches (opos:_)  -> Just $ MouseAction but opos npos
            _ -> Nothing
          combine os (Touches nposs) = case os of
            Mouse _ opos  -> Just $ FingerAction [opos] nposs
            Touches oposs -> Just $ FingerAction oposs nposs
            _ -> Nothing
          recomb os (FingerAction oposs nposs) = case os of
            Mouse _ spos  -> FingerAction [spos] nposs
            Touches sposs -> FingerAction sposs nposs
            _ -> FingerAction oposs nposs
          recomb os (MouseAction mp opos npos) = case os of
            Mouse _ spos     -> MouseAction mp spos npos
            Touches (spos:_) -> MouseAction mp spos npos
            _ -> MouseAction mp opos npos



cameraMove :: IORef OCCamera
           -> MoveAction
           -> IO Bool
cameraMove camRef (MouseAction LeftButton opos npos) =
    modifyIORef' camRef (dragHorizontal opos npos) >> return True
cameraMove camRef (MouseAction RightButton opos npos) =
    modifyIORef' camRef (rotateCentered opos npos) >> return True
cameraMove camRef (MouseAction MiddleButton opos npos) =
    modifyIORef' camRef (dragVertical opos npos) >> return True
cameraMove camRef (FingerAction (opos:_) [npos]) =
    modifyIORef' camRef (dragHorizontal opos npos) >> return True
cameraMove camRef (FingerAction (o1:o2:_) [n1,n2]) =
    modifyIORef' camRef (twoFingerControl (o1,o2) (n1,n2)) >> return True
cameraMove camRef (FingerAction (o1:_:_:_) (n1:_:_:_)) =
    modifyIORef' camRef (rotateCentered o1 n1) >> return True
cameraMove _ (FingerAction _ _ ) = return False


buildingMove :: IORef OCCamera
             -> IORef City
             -> MoveAction
             -> IO Bool
buildingMove camRef cityRef action = do
    cam <- readIORef camRef
    case action of
        MouseAction LeftButton opos npos -> modifyIORef' cityRef (dragBuilding opos npos cam) >> return True
        MouseAction RightButton opos npos -> modifyIORef' cityRef (rotateBuilding opos npos cam) >> return True
        FingerAction [o1] [n1] -> modifyIORef' cityRef (dragBuilding o1 n1 cam) >> return True
        FingerAction (o1:o2:_) (n1:n2:_) -> modifyIORef' cityRef (twoFingerBuilding (o1,o2) (n1,n2) cam) >> return True
        _ -> return False


getSelection :: Vector2 GLfloat -> World -> IO (Int,Int)
getSelection (Vector2 x y) World
    { glctx     = gl
    , cameraRef = camRef
    , selector  = SelectorObject
        { sbuffer   = sbuf
        , pixProber = pcarr
        }
    } = do
    Vector2 w h <- liftM viewSize $ readIORef camRef
    bindFramebuffer gl gl_FRAMEBUFFER sbuf
    viewport gl 0 0 w h
    readPixels gl (round x) (fromIntegral h - round y) 1 1 gl_RGBA gl_UNSIGNED_BYTE pcarr
    r <- liftM fromIntegral $ getIdx pcarr 0
    g <- liftM fromIntegral $ getIdx pcarr 1
    b <- liftM fromIntegral $ getIdx pcarr 2
    a <- liftM fromIntegral $ getIdx pcarr 3
    let i = r + shift g 8
        j = b + shift a 8
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    viewport gl 0 0 w h
    return (i,j)

----------------------------------------------------------------------------------------------
-- Next goes our test data  ------------------------------------------------------------------
----------------------------------------------------------------------------------------------

hut1 :: CityObject
hut1 = BoxHut Dynamic $ Vector3 2 3 3

building1 :: CityObject
building1 = Building Dynamic $ SimpleConvexPolygon
    [ Vector3 (-1) 2 (-1)
    , Vector3   0  2 (-2)
    , Vector3   2  3   0
    , Vector3   2  3   5
    , Vector3 (-1) 3   4
    ]

building2 :: CityObject
building2 = Building Static $ SimpleConvexPolygon
    [ Vector3 (-1) 4 (-1)
    , Vector3   1  4 (-1)
    , Vector3   1  4   5
    , Vector3 (-1) 4   5
    ]

building3 :: CityObject
building3 = Building Dynamic $ SimpleConvexPolygon
    [ Vector3 (-1) 4 (-2)
    , Vector3   1  4 (-2)
    , Vector3   1  4.5 2
    , Vector3 (-1) 4.5 2
    ]

building4 :: CityObject
building4 = Building Dynamic $ SimpleConvexPolygon
    [ Vector3 (-1) 3 (-1.2)
    , Vector3   1  3 (-1.2)
    , Vector3   1  3   1.2
    , Vector3 (-1) 3   1.2
    ]

road1 :: CityObject
road1 = Road Static 4 $
    [Vector2 0 0, Vector2 5 10, Vector2 15 7, Vector2 15 37, Vector2 0 38, Vector2 (-52) 37
    , Vector2 (-52) (-10), Vector2 (-40) (-10), Vector2 (-12) (2), Vector2 (-8) (2)]
