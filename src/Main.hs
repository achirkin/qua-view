{-# LANGUAGE JavaScriptFFI, DataKinds #-}
module Main (
    main
) where

import GHCJS.WebGL
import Control.Monad
import Control.Concurrent (threadDelay)
import Control.Arrow ((&&&))
--import Control.Applicative ((<*>))
--import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (documentGetBody, documentGetElementById) -- , documentCreateElement
import GHCJS.DOM.HTMLElement (htmlElementSetInnerHTML) --, htmlElementSetInnerText)
import GHCJS.DOM.Element (IsElement,elementOnmousemove,elementOnwheel, elementOnmousedown
                         ,elementOnmouseup,elementOnmouseleave -- ,elementOnmouseout
                         ,elementOncontextmenu, elementGetClientWidth, elementGetClientHeight
                         ,elementOntouchstart,elementOntouchmove,elementOntouchend,elementOntouchcancel) -- ,elementOnclick
--import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
--import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.DOM.EventM (EventM,Signal,mouseClientXY, mouseButton,event,returnValue
                        ,stopPropagation, stopImmediatePropagation,preventDefault)
import GHCJS.DOM.Types (MouseEvent,IsEvent, UIEvent, unElement,unUIEvent)

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import Geometry.Space
--import Geometry.Space.Transform
import Geometry.Structure

--import Data.Foldable (toList)

--import Foreign
import Drawable.OCCamera
import Drawable.World

--import Data.List (intercalate)
import Data.IORef
import Data.Bits (shift)
import Data.Maybe (fromMaybe)

import Model.City
import Model.CityObject
import Model.Grid

import Model.ScalarField
import Model.RadianceService
--import System.Random
-- closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js
-- style="position:fixed;left:0;top:0;padding:0;margin:0;width:100%;height:100%;overflow:hidden;"

mouseClickTime :: GLfloat
mouseClickTime = 0.2

--vpWidth :: GLsizei
--vpWidth = 800
--vpHeight :: GLsizei
--vpHeight = 1000



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

    htmlElementSetInnerHTML body $
--        "<div id=\"divview\" "
--        ++ "style=\"position: fixed; right: 0; top: 0; padding: 0; margin: 0; width: "
--        ++ show (30::Int) ++ "%; height: "
--        ++ show (100::Int) ++ "%; z-index = 1; overflow: hidden;\" width=\""
--        ++ show (30::Int) ++ "%;\" height=\""
--        ++ show (100::Int) ++ "%;\"></div>"
        "<canvas id=\"glcanvas\" "
        ++ "style=\"position: fixed; left: 0; top: 0; padding: 0; margin: 0; width: "
        ++ show vpWidth ++ "px; height: "
        ++ show vpHeight ++ "px; z-index = 2; overflow: hidden;\" width=\""
        ++ show vpWidth ++ "px;\" height=\""
        ++ show vpHeight ++ "px;\"></canvas>"
    Just canvas <- documentGetElementById doc "glcanvas"
--    Just divview <- documentGetElementById doc "divview"
    let canv = unElement $ canvas
    ctx <- getCtx canv
--    ctx <- makeDebugCtx cts

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
    camera <- newIORef $ initOCCamera (fromIntegral vpWidth) (fromIntegral vpHeight)
        OCCState { viewPoint     = Vector3 0 0 0,
                   viewAngles    = Vector2 (pi/3) (pi/4),
                   viewDist      = 40 }
    iworld <- initWorld ctx camera 0 (Vector3 (-0.5) (-1) 0.6)
    worldRef <- newIORef iworld

    -- Create sample objects
--    grid <- liftM wrap $ createGrid iworld 500 100 (Vector3 160 160 200)
--        :: IO (QTransform GLfloat Grid)
--    grid <- liftM wrap $ createGrid iworld 500 100 (Vector4 0.6 0.6 0.8 1)
--        :: IO (QTransform GLfloat Grid)
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
--            applySelector world [city]


    -- Mouse controls
    curTime <- getTime
    ltimeRef <- newIORef (curTime-1)
    posStateRef <- newIORef InterState
            { lastPos  = []
            , lastTime = curTime
            , iType    = toEnum $ -1
            , iContext = Idle
            }

    let selectNow = selectOnce worldRef [cityRef] >> applyService
    displayOnTime <- liftM animate
        . syncCallback NeverRetain False $ displayOnce ltimeRef worldRef drawSequence
    dispAndSelectOnTime <- liftM animate
        . syncCallback NeverRetain False $ displayOnce ltimeRef worldRef drawSequence >> selectNow

    -- Remember state to start moving
    _ <- elementOnmousedown canvas $ rememberState posStateRef worldRef camera
    _ <- elementOntouchstart canvas $ rememberStateT posStateRef worldRef camera

    -- mouse scrolling
    _ <- elementOnwheel canvas $ do
        ev <- event
        liftIO $ toJSRef ev >>= mouseWheelChange
                >>= modifyIORef' camera . scroll
                >> dispAndSelectOnTime

    -- moving
    _ <- elementOnmousemove canvas $ mouseMove posStateRef camera cityRef
        >>= flip when (liftIO $ displayOnTime)
    _ <- elementOntouchmove canvas $ touchMove posStateRef camera cityRef
        >>= flip when (liftIO $ displayOnTime)

    -- Selecting
    _ <- elementOnmouseup canvas $ -- elementOnclick
        selectObject posStateRef cityRef -- >>= (\x -> liftIO (print "mouseup") >> return x)
        >>= flip when (liftIO $ displayOnTime)
        >> forgetState posStateRef >> liftIO selectNow
    _ <- elementOntouchend canvas $
        selectObject posStateRef cityRef -- >>= (\x -> liftIO (print "touchend") >> return x)
        >>= flip when (liftIO $ displayOnTime)
        >> forgetState posStateRef >> liftIO selectNow

    -- Forget state by hard!
    let drawAndForget :: (IsElement self, IsEvent e) => EventM e self ()
        drawAndForget = forgetState posStateRef
            >> liftIO dispAndSelectOnTime
    _ <- elementOnmouseleave canvas drawAndForget
    _ <- elementOntouchcancel canvas drawAndForget

    -- Do not react on right mouse click
    _ <- elementOncontextmenu canvas preventOthers

    -- Resize body
    _ <- elementOnResize body $ \_ -> do
            w <- liftM (round . max 100) $ elementGetClientWidth body
            h <- liftM (round . max 100) $ elementGetClientHeight body
            elementSetSize canv w h
            elementSetStyleSize canv w h
            viewport ctx 0 0 w h
            modifyIORef' camera $ \OCCamera{newState = ns} -> initOCCamera (fromIntegral w) (fromIntegral h) ns
            fb <- initSelectorFramebuffer ctx $ Vector2 w h
            modifyIORef' worldRef $ \wrld@World{selector = SelectorObject _ pickedColorArr} -> wrld{selector = SelectorObject fb pickedColorArr}
            dispAndSelectOnTime


    -- Draw world
    threadDelay 1000000
    dispAndSelectOnTime
--    displayOnTime
    --displayOnce ltimeRef worldRef drawSequence
--    selectOnce worldRef [cityRef]




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


--foreign import javascript interruptible "var img = new Image();img.crossOrigin='anonymous';img.onload=function(){$c(img);};img.src = $1;"
--    loadImage' :: JSString -> IO TexImageSource
--
--loadImage :: String -> IO TexImageSource
--loadImage = loadImage' . toJSString

--foreign import javascript unsafe "\
--  \ function validateNoneOfTheArgsAreUndefined(functionName, args) { \
--  \     for (var ii = 0; ii < args.length; ++ii) { \
--  \         if (args[ii] === undefined) { \
--  \             console.error(\"undefined passed to \" + functionName + \"(\" + \
--  \                   WebGLDebugUtils.glFunctionArgsToString(functionName, args) + \")\"); \
--  \         } \
--  \     } \
--  \ }; \
--  \ function throwOnGLError(err, funcName, args) { \
--  \     document.getElementById('divview').innerHTML = (WebGLDebugUtils.glEnumToString(err) + \" was caused by call to: \" + funcName); \
--  \ }; \
--  \ $r = WebGLDebugUtils.makeDebugContext($1, throwOnGLError, validateNoneOfTheArgsAreUndefined);"
--    makeDebugCtx :: Ctx -> IO Ctx

--foreign import javascript unsafe "if (!document.fullscreenElement && \
--    \  !document.mozFullScreenElement && \
--    \  !document.webkitFullscreenElement && !document.msFullscreenElement ) { \
--    \    if (document.documentElement.requestFullscreen) { \
--    \      document.documentElement.requestFullscreen(); \
--    \    } else if (document.documentElement.msRequestFullscreen) { \
--    \      document.documentElement.msRequestFullscreen(); \
--    \    } else if (document.documentElement.mozRequestFullScreen) { \
--    \      document.documentElement.mozRequestFullScreen(); \
--    \    } else if (document.documentElement.webkitRequestFullscreen) { \
--    \      document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT); \
--    \    } \
--    \  } else { \
--    \    if (document.exitFullscreen) { \
--    \      document.exitFullscreen(); \
--    \    } else if (document.msExitFullscreen) { \
--    \      document.msExitFullscreen(); \
--    \    } else if (document.mozCancelFullScreen) { \
--    \      document.mozCancelFullScreen(); \
--    \    } else if (document.webkitExitFullscreen) { \
--    \      document.webkitExitFullscreen(); \
--    \    } \
--    \  }"
--    toggleFullScreen :: IO ()

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

foreign import javascript unsafe "var e = window.event || $1;$r = e.wheelDelta>0 || e.detail<0 || e['deltaY']<0 ? 0.25 : -0.2;"
    mouseWheelChange :: JSRef a -> IO GLfloat


foreign import javascript unsafe "$r=[];for(var i = 0; i < $1['touches'].length; i++){$r.push($1['touches'][i].clientX);$r.push($1['touches'][i].clientY);}"
    getTouches' :: JSRef UIEvent -> JSRef [GLfloat]


getTouches :: UIEvent -> IO [Vector2 GLfloat]
getTouches e = liftM (f . fromMaybe []) . fromJSRefListOf . getTouches' $ unUIEvent e
    where f (a:b:xs) = (Vector2 a b):f xs
          f _ = []

data InteractionState = InterState
    { lastPos  :: ![Vector2 GLfloat] -- position on screen
    , lastTime :: !GLfloat -- time of interaction start
    , iType    :: !InteractionType
    , iContext :: !InteractionContext
    }

data InteractionType = MainPointer
                 | SecondaryPointer
                 | AltPointer
                 | Touches
                 | NoInteraction deriving (Eq)

instance Enum InteractionType where
    fromEnum MainPointer = 0
    fromEnum SecondaryPointer = 1
    fromEnum AltPointer = 2
    fromEnum Touches = 99
    fromEnum NoInteraction = -1
    toEnum 0 = MainPointer
    toEnum 1 = SecondaryPointer
    toEnum 2 = AltPointer
    toEnum 99 = Touches
    toEnum _ = NoInteraction

data InteractionContext = Idle | CameraMotion | CityChange (Int,Int) deriving (Eq)


rememberState :: (IsElement self)
              => IORef InteractionState
              -> IORef World
              -> IORef OCCamera
              -> EventM MouseEvent self ()
rememberState posStateRef worldref camRef = do
    preventOthers
    (x, y) <- mouseClientXY
    but <- mouseButton
    liftIO $ do
--        printToDiv (x,y)
        t <- getTime
        i' <- readIORef worldref >>= getSelection (x,y)
--        printToDiv (i', (x,y))
        writeIORef posStateRef InterState
            { lastPos  = [fmap fromIntegral $ Vector2 x y]
            , lastTime = t
            , iType    = toEnum $ fromIntegral but
            , iContext = case i' of
                (0,_) -> CameraMotion
                (_,0) -> CameraMotion
                i     -> CityChange i
            }
        modifyIORef' camRef (\camera@OCCamera{newState = nstate} -> camera{oldState = nstate})

rememberStateT :: (IsElement self)
              => IORef InteractionState
              -> IORef World
              -> IORef OCCamera
              -> EventM UIEvent self ()
rememberStateT posStateRef worldref camRef = do
    preventOthers
    e <- event
    liftIO $ do
        ts <- getTouches e
--        printToDiv . intercalate "<br/>" $ map (\(Vector2 x y) -> show (x,y)) ts
        t <- getTime
        i' <- case ts of
            [] -> return (0,0)
            Vector2 x y:_ -> readIORef worldref >>= getSelection (round x, round y)
--        printToDiv (i', ts)
        writeIORef posStateRef InterState
            { lastPos  = ts
            , lastTime = t
            , iType    = Touches
            , iContext = case i' of
                (0,_) -> CameraMotion
                (_,0) -> CameraMotion
                i     -> CityChange i
            }
        modifyIORef' camRef (\camera@OCCamera{newState = nstate} -> camera{oldState = nstate})

forgetState :: (IsElement self, IsEvent e)
            => IORef InteractionState
            -> EventM e self ()
forgetState posStateRef = do
    preventOthers
    liftIO $ writeIORef posStateRef InterState
            { lastPos  = []
            , lastTime = 0
            , iType    = toEnum $ -1
            , iContext = Idle
            }

mouseMove :: (IsElement self)
           => IORef InteractionState
           -> IORef OCCamera
           -> IORef City
           -> EventM MouseEvent self Bool
mouseMove posStateRef camRef cityRef = do
    preventOthers
    (x, y) <- mouseClientXY
    liftIO $ do
        istate@InterState
            { lastPos  = opos
            , iType    = button
            , iContext = ictx
            } <- readIORef posStateRef
        let updateCamera move = modifyIORef' camRef (move (head opos) npos) >> return True
            npos = fmap fromIntegral $ Vector2 x y
        mcity <- case ictx of
            CityChange i -> do
                city <- readIORef cityRef
                return $ if i /= activeObj city
                    then Nothing
                    else Just city
            _ -> return Nothing
        case (button, mcity) of
            (MainPointer,Nothing) -> updateCamera dragHorizontal
            (SecondaryPointer, _) -> updateCamera dragVertical
            (AltPointer,Nothing) -> updateCamera rotateCentered
            (MainPointer,Just city) -> do
                cam <- readIORef camRef
                writeIORef cityRef (dragBuilding (head opos) npos cam city)
                writeIORef posStateRef istate{lastPos = [npos]}
                return True
            (AltPointer,Just city) -> do
                cam <- readIORef camRef
                writeIORef cityRef (rotateBuilding (head opos) npos cam city)
                writeIORef posStateRef istate{lastPos = [npos]}
                return True
            (NoInteraction,_) -> return False
            (Touches,_) -> return False

touchMove :: (IsElement self)
           => IORef InteractionState
           -> IORef OCCamera
           -> IORef City
           -> EventM UIEvent self Bool
touchMove posStateRef camRef cityRef = do
    preventOthers
    e <- event
    liftIO $ do
        ts <- getTouches e
        istate@InterState
            { lastPos  = opos
            , iContext = ictx
            } <- readIORef posStateRef
        mcity <- case ictx of
            CityChange i -> do
                city <- readIORef cityRef
                return $ if i /= activeObj city
                    then Nothing
                    else Just city
            _ -> return Nothing
        case (ts, opos, mcity) of
            ([], _, _) -> return False
            (_, [], _) -> return False
            ([n1], o1:_, Nothing) -> modifyIORef' camRef (dragHorizontal o1 n1)
                >> return True
            ([n1,n2], o1:o2:_, Nothing) -> modifyIORef' camRef (twoFingerControl (o1,o2) (n1,n2))
                >> return True
            (n1:_:_:_, o1:_:_:_, Nothing) -> modifyIORef' camRef (rotateCentered o1 n1)
                >> return True
            ([n1], o1:_,Just city) -> do
                cam <- readIORef camRef
                writeIORef cityRef (dragBuilding o1 n1 cam city)
                writeIORef posStateRef istate{lastPos = [n1]}
                return True
            ([n1,n2], o1:o2:_,Just city) -> do
                cam <- readIORef camRef
                writeIORef cityRef (twoFingerBuilding (o1,o2) (n1,n2) cam city)
                writeIORef posStateRef istate{lastPos = [n1,n2]}
                return True
            (_, _,Just _) -> return False
            (_:_:_, [_],Nothing) -> return False
            (_:_:_:_, [_,_],Nothing) -> return False



selectObject :: (IsElement self, IsEvent event)
           => IORef InteractionState
           -> IORef City
           -> EventM event self Bool
selectObject posStateRef cityRef = do
    preventOthers
    (ic, dt) <- liftIO $ do
        t <- getTime
        liftM (iContext &&& ( (t-) . lastTime)) . readIORef $ posStateRef
    if dt > mouseClickTime
    then return False
    else liftIO $ do
        city@City{activeObj = j} <- readIORef cityRef
        case (ic) of
            (CityChange i) -> if i == j then return False
                else writeIORef cityRef (city{activeObj = i}) >> return True
            _ -> if fst j == 0 || snd j == 0 then return False
                else writeIORef cityRef (city{activeObj = (0,0)}) >> return True

preventOthers :: (IsEvent e) => EventM e self ()
preventOthers = do
    preventDefault
    stopPropagation
    stopImmediatePropagation
    returnValue False

getSelection :: (Int,Int) -> World -> IO (Int,Int)
getSelection (x,y) World
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
--    clear gl (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
--    readIORef city >>= selectArea world
--    checkFramebufferStatus gl gl_FRAMEBUFFER >>= \r ->
--        if r == gl_FRAMEBUFFER_COMPLETE then print "ok"
--        else print "this combination of attachments does not work"
    readPixels gl (fromIntegral x) (fromIntegral h - fromIntegral y) 1 1 gl_RGBA gl_UNSIGNED_BYTE pcarr
--    print (x,y)
--    printRef pcarr
--    print "get selection"
    r <- liftM fromIntegral $ getIdx pcarr 0
    g <- liftM fromIntegral $ getIdx pcarr 1
    b <- liftM fromIntegral $ getIdx pcarr 2
    a <- liftM fromIntegral $ getIdx pcarr 3
    let i = r + shift g 8
        j = b + shift a 8
--    print (i,j)
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
