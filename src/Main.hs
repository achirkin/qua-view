{-# LANGUAGE JavaScriptFFI #-}
module Main (
    main
) where

import GHCJS.WebGL
import Control.Monad
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
                         ,elementOnmouseup,elementOnmouseleave,elementOnmouseout
                         ,elementOncontextmenu, elementGetClientWidth, elementGetClientHeight
                         ,elementOntouchstart,elementOntouchmove,elementOntouchend,elementOntouchcancel) -- ,elementOnclick
--import GHCJS.DOM.HTMLParagraphElement (castToHTMLParagraphElement)
--import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.DOM.EventM (EventM,mouseClientXY, mouseButton,event,returnValue
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

import Data.IORef
import Data.Bits (shift)
import Data.Maybe (fromMaybe)
--import qualified Data.Map as Map
--import Data.Time.Clock.POSIX

import Model.City
import Model.CityObject
import Model.Grid

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

    htmlElementSetInnerHTML body $ "<canvas id=\"glcanvas\" "
        ++ "style=\"position: fixed; left: 0; top: 0; padding: 0; margin: 0; width: "
        ++ show vpWidth ++ "px; height: "
        ++ show vpHeight ++ "px; overflow: hidden;\" width=\""
        ++ show vpWidth ++ "px;\" height=\""
        ++ show vpHeight ++ "px;\"></canvas>"
    Just canvas <- documentGetElementById doc "glcanvas"
--    canvas <- elemById . toJSString $ "glcanvas"
    ctx <- getCtx . unElement $ canvas


    clearColor ctx 0 0 0 0
    enable ctx gl_DEPTH_TEST
    depthFunc ctx gl_LEQUAL
    viewport ctx 0 0 vpWidth vpHeight

    -- Setup world
    camera <- newIORef $ initOCCamera (fromIntegral vpWidth) (fromIntegral vpHeight)
        OCCState { viewPoint     = Vector3 0 0 0,
                   viewAngles    = Vector2 (pi/3) (pi/4),
                   viewDist      = 40 }
    iworld <- initWorld ctx camera 0
    worldRef <- newIORef iworld

    -- Create sample objects
--    grid <- liftM wrap $ createGrid iworld 500 100 (Vector3 160 160 200)
--        :: IO (QTransform GLfloat Grid)
--    grid <- liftM wrap $ createGrid iworld 500 100 (Vector4 0.6 0.6 0.8 1)
--        :: IO (QTransform GLfloat Grid)
    grid <- createGrid iworld 500 100 (Vector4 0.6 0.6 0.8 1)
    let numb2 = 8
    cityRef <- buildCity iworld (building1: building2 : building3 : repeat building4)
            (Vector3 8 0 0 : Vector3 (-8) 0 0 : Vector3 (-8) 0 (-6) :
            [Vector3 (-i*4 - 12) 0 (j*4) | i <- [1..numb2], j <- [1..numb2]])
            (pi/3 : 0 : 0 : map ((pi/100)*) [1..])
        >>= newIORef
--    cityRef <- buildCity iworld (repeat building4)
--            [Vector3 (-i*4 - 12) 0 (j*4) | i <- [1..numb2], j <- [1..numb2]]
--            (map ((pi/100)*) [1..])
--        >>= newIORef

    addCityObject' iworld road1 (Vector3 0 0 0) 0 cityRef


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

    --drawCallback <- syncCallback AlwaysRetain False $ displayWhileActive posStateRef worldRef drawSequence
    -- Remember state to start moving
    _ <- elementOnmousedown canvas $ rememberState posStateRef iworld camera
        -- >> liftIO (keepAnimate drawCallback)
    _ <- elementOntouchstart canvas $ rememberStateT posStateRef iworld camera
        -- >> liftIO (keepAnimate drawCallback)

    -- mouse scrolling
    _ <- elementOnwheel canvas $ do
        ev <- event
        liftIO $ toJSRef ev >>= mouseWheelChange
                >>= modifyIORef' camera . scroll
                >> displayOnce ltimeRef worldRef drawSequence
                >> selectOnce worldRef [cityRef]

    -- moving
    _ <- elementOnmousemove canvas $ mouseMove posStateRef camera cityRef -- >>= return . const ()
        >>= flip when (liftIO $ displayOnce ltimeRef worldRef drawSequence)
    _ <- elementOntouchmove canvas $ touchMove posStateRef camera cityRef -- >>= return . const ()
        >>= flip when (liftIO $ displayOnce ltimeRef worldRef drawSequence)

    -- Selecting
    _ <- elementOnmouseup canvas $ -- elementOnclick
        selectObject posStateRef iworld cityRef
        >>= flip when (liftIO $ displayOnce ltimeRef worldRef drawSequence)
        >> forgetState posStateRef >> liftIO (selectOnce worldRef [cityRef])
    _ <- elementOntouchend canvas $
        selectObjectT posStateRef iworld cityRef
        >>= flip when (liftIO $ displayOnce ltimeRef worldRef drawSequence)
        >> forgetState posStateRef >> liftIO (selectOnce worldRef [cityRef])

    -- Forget state by hard!
    let drawAndForget :: (IsElement self, IsEvent e) => EventM e self ()
        drawAndForget = forgetState posStateRef
            >> liftIO (displayOnce ltimeRef worldRef drawSequence >> selectOnce worldRef [cityRef])
    _ <- elementOnmouseleave canvas drawAndForget
    _ <- elementOnmouseout canvas drawAndForget
    _ <- elementOntouchcancel canvas drawAndForget

    -- Do not react on right mouse click
    _ <- elementOncontextmenu canvas preventOthers

    -- Draw world
    delay 1000
    displayOnce ltimeRef worldRef drawSequence
    selectOnce worldRef [cityRef]




displayOnce :: IORef (GLfloat)
            -> IORef World
            -> (World -> IO ())
            -> IO ()
displayOnce luref wref drawCall = do
    lastUpdate <- readIORef luref
    ctime <- getTime
    when (lastUpdate + 0.005 < ctime) $ do
        writeIORef luref (lastUpdate+10)
        oldworld@World{ currentTime = t } <- readIORef wref
        world <- prepareWorldRender oldworld t
--        world <- getTime >>= prepareWorldRender oldworld
        drawCall world
        writeIORef wref world
        getTime >>= writeIORef luref
--        delay 10


selectOnce :: (F.Foldable s, T.Traversable s, Selectable a)
           => IORef World
           -> s (IORef a)
           -> IO ()
selectOnce wref selectables = do
    world <- readIORef wref
    T.forM selectables readIORef >>= applySelector world

----------------------------------------------------------------------------------------------
-- Interaction  ------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

--data AnimationId_
--type AnimationId = JSRef AnimationId_

--foreign import javascript interruptible "window.requestAnimationFrame($1);"
--   animate' :: JSFun (IO ()) -> IO ()
--
--animate :: IO () -> IO ()
--animate f = syncCallback NeverRetain False f >>= animate'

--foreign import javascript safe "var g = $1; var f = function() {var b = g();window.requestAnimationFrame(f);};window.requestAnimationFrame(f);"
--   keepAnimate :: JSFun (IO JSBool) -> IO ()

--foreign import javascript safe "window.cancelAnimationFrame($1);"
--   stopAnimate :: AnimationId -> IO ()

--toCallback :: IO a -> IO (JSFun (IO a))
--toCallback f = asyncCallback AlwaysRetain f

--foreign import javascript interruptible "window.requestAnimationFrame($c);"
--   keepAnimate :: IO ()

--displayWhileActive :: IORef InteractionState
--                   -> IORef World
--                   -> (World -> IO ())
--                   -> IO JSBool
--displayWhileActive posStateRef wref drawCall = do
--    oldworld <- readIORef wref
--    world <- getTime >>= prepareWorldRender oldworld
--    drawCall world
--    writeIORef wref world
--    readIORef posStateRef >>= return . toJSBool . (NoInteraction /=) . iType
--    readIORef posStateRef >>= flip when keepAnimate . (NoInteraction ==) . iType


foreign import javascript interruptible "setTimeout($c, $1);"
    delay :: Int -> IO ()

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

data InteractionContext = Idle | CameraMotion | CityChange Int deriving (Eq)


rememberState :: (IsElement self)
              => IORef InteractionState
              -> World
              -> IORef OCCamera
              -> EventM MouseEvent self ()
rememberState posStateRef world camRef = do
    preventOthers
    (x, y) <- mouseClientXY
    but <- mouseButton
    liftIO $ do
        t <- getTime
        i' <- getSelection world (x,y)
        writeIORef posStateRef InterState
            { lastPos  = [fmap fromIntegral $ Vector2 x y]
            , lastTime = t
            , iType    = toEnum $ fromIntegral but
            , iContext = if i' == 0 then CameraMotion else CityChange i'
            }
        modifyIORef' camRef (\camera@OCCamera{newState = nstate} -> camera{oldState = nstate})

rememberStateT :: (IsElement self)
              => IORef InteractionState
              -> World
              -> IORef OCCamera
              -> EventM UIEvent self ()
rememberStateT posStateRef world camRef = do
    preventOthers
    e <- event
    liftIO $ do
        ts <- getTouches e
        t <- getTime
        i' <- case ts of
            [] -> return 0
            Vector2 x y:_ -> getSelection world (round x, round y)
        writeIORef posStateRef InterState
            { lastPos  = ts
            , lastTime = t
            , iType    = Touches
            , iContext = if i' == 0 then CameraMotion else CityChange i'
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



selectObject :: (IsElement self)
           => IORef InteractionState
           -> World
           -> IORef City
           -> EventM MouseEvent self Bool
selectObject posStateRef world cityRef = do
    preventOthers
    pos <- mouseClientXY
    but <- mouseButton
    case but of
        0 -> liftIO $ do
            InterState{lastTime = otime} <- readIORef posStateRef
            t <- getTime
            if t - otime > mouseClickTime then return False
            else do
              i <- getSelection world pos
              modifyIORef' cityRef $ \city -> city{activeObj = i}
              return True
        _ -> return False

selectObjectT :: (IsElement self)
           => IORef InteractionState
           -> World
           -> IORef City
           -> EventM UIEvent self Bool
selectObjectT posStateRef world cityRef = do
    preventOthers
    e <- event
    liftIO $ do
        ts <- getTouches e
        InterState{lastTime = otime, lastPos = opos} <- readIORef posStateRef
        case (ts, opos) of
            ([], Vector2 x y:_) -> do
                t <- getTime
                if t - otime > mouseClickTime then return False
                else do
                    i <- getSelection world (round x, round y)
                    modifyIORef' cityRef $ \city -> city{activeObj = i}
                    return True
            _ -> return False

preventOthers :: (IsEvent e) => EventM e self ()
preventOthers = do
    preventDefault
    stopPropagation
    stopImmediatePropagation
    returnValue False

getSelection :: World -> (Int,Int) -> IO Int
getSelection World
    { glctx     = gl
    , cameraRef = camRef
    , selector  = SelectorObject
        { sbuffer   = sbuf
        , pixProber = pcarr
        }
    } (x,y) = do
    Vector2 _ h <- liftM viewSize $ readIORef camRef
    bindFramebuffer gl gl_FRAMEBUFFER sbuf
    readPixels gl (fromIntegral x) (fromIntegral h - fromIntegral y) 1 1 gl_RGBA gl_UNSIGNED_BYTE pcarr
    r <- liftM fromIntegral $ getIdx pcarr 0
    g <- liftM fromIntegral $ getIdx pcarr 1
    b <- liftM fromIntegral $ getIdx pcarr 2
    let i = r + shift g 8 + shift b 16 :: Int
    bindFramebuffer gl gl_FRAMEBUFFER jsNull
    return i

----------------------------------------------------------------------------------------------
-- Next goes our test data  ------------------------------------------------------------------
----------------------------------------------------------------------------------------------

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
