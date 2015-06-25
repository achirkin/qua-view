{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GUI.PointerEvent
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module GUI.PointerEvent
    ( PointerMoveEvent (..)
    , PointerClickEvent (..)
    , MouseButton (..)
    , Interaction (..)
    , onMouseWheel
    , onMove
    , onCancel
    , onSimpleClick
    ) where

import Control.Monad.Reader (liftIO)
import Control.Monad

import GHCJS.DOM.Element (IsElement,elementOnmousemove,elementOnwheel, elementOnmousedown
                         ,elementOnmouseup,elementOnmouseleave, elementOncontextmenu -- , elementOnclick
                         ,elementOntouchstart,elementOntouchmove,elementOntouchend,elementOntouchcancel)
import GHCJS.DOM.EventM (EventM,mouseClientXY, mouseButton,event,target,returnValue
                        ,stopPropagation, stopImmediatePropagation,preventDefault)
import GHCJS.DOM.Types (MouseEvent,IsEvent, UIEvent, unUIEvent)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import Data.Maybe (fromMaybe)


import GHCJS.WebGL

import Geometry.Space

-- | Click counts only if a user releases pointer before this time elapses
clickTime :: GLfloat
clickTime = 0.2

-- | Event contains pointer locations at current moment and on last frame
data PointerMoveEvent = Move
    { lastState :: !Interaction -- ^ last position on screen and buttons
    , currState :: !Interaction -- ^ current position on screen and buttons
    , timeDelta :: !GLfloat    -- ^ time passed since last interaction
    } deriving Show

-- | Event contains current pointer state (locations and mouse buttons)
newtype PointerClickEvent = Click Interaction -- ^ current position on screen and buttons
   deriving Show

-- | Supported mouse buttons
data MouseButton = LeftButton | RightButton | MiddleButton deriving (Eq, Show)

instance ToJSRef MouseButton where
    toJSRef = liftM castRef . toJSRef . fromEnum

instance FromJSRef MouseButton where
    fromJSRef = liftM (>>= return . toEnum) . fromJSRef . (castRef :: JSRef MouseButton -> JSRef Int)

instance Enum MouseButton where
    fromEnum LeftButton = 0
    fromEnum MiddleButton = 1
    fromEnum RightButton = 2
    toEnum 0 = LeftButton
    toEnum 1 = MiddleButton
    toEnum 2 = RightButton
    toEnum _ = LeftButton

-- | Possible interaction modes
data Interaction = Mouse !MouseButton !(Vector2 GLfloat)
                 | Touches ![Vector2 GLfloat]
                 | NoInteraction deriving (Eq, Show)

instance ToJSRef Interaction where
    toJSRef (Mouse mb (Vector2 x y)) = liftM castRef $ toJSRef (1::Int, fromEnum mb, [(x,y)]::[(GLfloat,GLfloat)])
    toJSRef (Touches xs) = liftM castRef $ toJSRef (2::Int,0::Int, map (\(Vector2 x y) -> (x,y)) xs ::[(GLfloat,GLfloat)])
    toJSRef NoInteraction = liftM castRef $ toJSRef (0::Int,0::Int,[]::[GLfloat])

instance FromJSRef Interaction where
    fromJSRef = liftM (>>= parse) . fromJSRef . (castRef :: JSRef Interaction -> JSRef (Int,Int,[(GLfloat,GLfloat)]))
        where parse (0,_,_)       = Just NoInteraction
              parse (1,b,(x,y):_) = Just $ Mouse (toEnum b) (Vector2 x y)
              parse (2,_,xs)      = Just . Touches $ map (\(x,y) -> Vector2 x y) xs
              parse _             = Nothing



----------------------------------------------------------------------------------------------------
-- Cancel action
----------------------------------------------------------------------------------------------------

-- | Adds an action when mouse leaves the area or on touchcancel (whatever it is)
onCancel :: IsElement self
         => self -- ^ element to setup event
         -> IO () -> IO ()
onCancel self f = do
    _ <- elementOnmouseleave self $ cancelCallBack f
    _ <- elementOntouchcancel self $ cancelCallBack f
    return ()

cancelCallBack :: (IsElement self, IsEvent ev)
               => IO () -> EventM ev self ()
cancelCallBack f = do
    preventOthers
    liftIO f
    forgetState

----------------------------------------------------------------------------------------------------
-- Move (triggers only when pressed)
----------------------------------------------------------------------------------------------------

-- | Adds on move action that triggered only when pressed (i.e. dragging or panning).
onMove :: IsElement self
       => self -- ^ element to setup event
       -> Bool -- ^ whether to block right click or not
       -> (PointerClickEvent -> IO ()) -- ^ beginning move callback
       -> (PointerMoveEvent -> IO ()) -- ^ in move callback
       -> (PointerClickEvent -> IO ()) -- ^ finish move callback
       -> (PointerClickEvent -> IO ()) -- ^ click callback
       -> IO ()
onMove self replaceOnContextMenu startf movef endf clickf = do
    _ <- elementOnmousemove self $ moveCallBack movef
    _ <- elementOntouchmove self $ moveCallBack movef
    _ <- elementOnmouseup self $ clickCallBacks clickf endf
    _ <- elementOntouchend self $ clickCallBacks clickf endf
    addStateTracking self replaceOnContextMenu startf

moveCallBack :: (IsElement self, IsPointerEvent ev)
             => (PointerMoveEvent -> IO ()) -> EventM ev self ()
moveCallBack f = do
    preventOthers
    el <- target >>= liftIO . toJSRef
    isMoving <- liftIO $ do
        im' <- unsafeGetPropMaybe "movingInProcess" el >>= \mv ->
            case mv of
                Nothing -> return False
                Just v -> liftM (fromMaybe False) $ fromJSRef v :: IO Bool
        if im' == False
        then unsafeSetProp "movingInProcess" jsTrue el
        else return ()
        return im'
    if isMoving then return ()
    else do
      (mostate,ot) <- readState False
      case mostate of
        Nothing -> return ()
        Just NoInteraction -> return ()
        Just ostate -> do
            (cstate,ct) <- updateState
            if cstate == NoInteraction || cstate == ostate then return ()
            else liftIO . f $ Move ostate cstate (ct-ot)
      liftIO $ unsafeSetProp "movingInProcess" jsFalse el

clickCallBacks :: (IsElement self, IsPointerEvent ev)
              => (PointerClickEvent -> IO ()) -- ^ on time
              -> (PointerClickEvent -> IO ()) -- ^ after click time (onmouseup)
              -> EventM ev self ()
clickCallBacks f g = do
    preventOthers
    (mostate,ot) <- readState True
    dt <- liftIO . liftM (+(-ot)) $ getTime
    case (mostate, dt <= clickTime) of
        (Nothing, _)            -> getState >>= tryclick
        (Just NoInteraction, _) -> getState >>= tryclick
        (Just ostate, True)     -> liftIO . f . Click $ ostate
        (Just ostate, False)    -> liftIO . g . Click $ ostate
    forgetState
    where tryclick NoInteraction = return ()
          tryclick interaction   = liftIO . f . Click $ interaction

----------------------------------------------------------------------------------------------------
-- Click (triggers on release)
----------------------------------------------------------------------------------------------------

-- | Adds a click action of the elment
--   Works only if no onMove
onSimpleClick :: IsElement self
              => self -- ^ element to setup event
              -> Bool -- ^ whether to block right click or not
              -> (PointerClickEvent -> IO ()) -- ^ callback
              -> IO ()
onSimpleClick self replaceOnContextMenu f = do
    _ <- elementOnmouseup self $ simpleClickCallBack f
    _ <- elementOntouchend self $ simpleClickCallBack f
    _ <- if replaceOnContextMenu
         then elementOncontextmenu self preventOthers
         else return (return ())
    return ()

simpleClickCallBack :: (IsElement self, IsPointerEvent ev)
                    => (PointerClickEvent -> IO ())
                    -> EventM ev self ()
simpleClickCallBack f = preventOthers >> getState >>= liftIO . f . Click


----------------------------------------------------------------------------------------------------
-- Introducing state to the events
----------------------------------------------------------------------------------------------------

-- | Sets event start on pressing if not set yet
addStateTracking :: (IsElement self) => self -> Bool -> (PointerClickEvent -> IO ()) -> IO ()
addStateTracking el replaceOnContextMenu f = do
    elr <- toJSRef el
    stateTracking <- liftM (fromMaybe False) $ unsafeGetPropMaybe "stateTracking" elr
      >>= \mv -> case mv of
        Nothing -> return Nothing
        Just v -> fromJSRef v
    if stateTracking then return ()
    else do
        unsafeSetProp "stateTracking" jsTrue elr
        _ <- elementOnmousedown  el $ preventOthers >> getState >>= liftIO . f . Click >> void rememberState
        _ <- elementOntouchstart el $ preventOthers >> getState >>= liftIO . f . Click >> void rememberState
        _ <- if replaceOnContextMenu
            then elementOncontextmenu el preventOthers
            else return (return ())
        return ()

-- | Gets starting state of an action
readState :: (IsElement self, IsEvent ev)
          => Bool -> EventM ev self (Maybe Interaction, GLfloat)
readState fromStart = do
    el <- target
    liftIO $ do
        elr <- toJSRef el
        startTime <- unsafeGetPropMaybe (if fromStart then "startTime" else "lastTime") elr >>= \mv ->
            case mv of
                Nothing -> return Nothing
                Just v -> fromJSRef v :: IO (Maybe GLfloat)
        startState <- unsafeGetPropMaybe "lastPointerState" elr >>= \mv ->
            case mv of
                Nothing -> return Nothing
                Just v -> fromJSRef v :: IO (Maybe Interaction)
        case startTime of
            Nothing -> getTime >>= return . (,) startState
            Just t -> return (startState,t)

forgetState :: (IsElement self, IsEvent ev) => EventM ev self ()
forgetState = do
    el <- target
    liftIO $ do
        elr <- toJSRef el
        action <- toJSRef NoInteraction
        unsafeSetProp "lastPointerState" action elr
        ct <- getTime >>= toJSRef
        unsafeSetProp "lastTime" ct elr

-- | Stores the event state in the target object
rememberState :: (IsElement self, IsPointerEvent ev) => EventM ev self (Interaction, GLfloat)
rememberState = do
    st <- getState
    el <- target
    liftIO $ do
        elr <- toJSRef el
        action <- toJSRef st
        unsafeSetProp "lastPointerState" action elr
        ct <- getTime
        ctr <- toJSRef ct
        unsafeSetProp "lastTime" ctr elr
        unsafeSetProp "startTime" ctr elr
        return (st, ct)

updateState :: (IsElement self, IsPointerEvent ev) => EventM ev self (Interaction, GLfloat)
updateState = do
    st' <- getState
    el <- target
    ost <- liftM (fromMaybe st' . fst) $ readState False
    let st = case (st', ost) of
                (Mouse _ v, Mouse b _) -> Mouse b v
                (s, _) -> s
    liftIO $ do
        elr <- toJSRef el
        action <- toJSRef st
        unsafeSetProp "lastPointerState" action elr
        ct <- getTime
        ctr <- toJSRef ct
        unsafeSetProp "lastTime" ctr elr
        return (st, ct)

-- | Generalization for mouse and finger events
class (IsEvent ev) => IsPointerEvent ev where
    getState :: (IsElement self) => EventM ev self (Interaction)

instance IsPointerEvent MouseEvent where
    getState = do
        (x, y) <- mouseClientXY
        but <- mouseButton
        if but < 0 || but > 3
        then return NoInteraction
        else return $ Mouse (toEnum $ fromIntegral but) (fmap fromIntegral $ Vector2 x y)

instance IsPointerEvent UIEvent where
    getState = event >>= liftIO . liftM f . getTouches
        where f [] = NoInteraction
              f xs = Touches xs

----------------------------------------------------------------------------------------------------
-- Mouse wheel
----------------------------------------------------------------------------------------------------

-- | An argument is wheel delta, which is only +1 or -1 for compatibility reasons.
--   This is the only stateless action in the module.
onMouseWheel :: IsElement self => self -> (GLfloat -> IO ()) -> IO ()
onMouseWheel self f = void . elementOnwheel self $ do
    preventOthers
    ev <- event
    liftIO $ toJSRef ev >>= mouseWheelChange >>= f

foreign import javascript unsafe "var e = window.event || $1;$r = e.wheelDelta>0 || e.detail<0 || e['deltaY']<0 ? (1.0) : (-1.0);"
    mouseWheelChange :: JSRef a -> IO GLfloat

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

--foreign import javascript safe "console.log($1)"
--    printRef :: JSRef a -> IO ()

-- | Ensure no other events are invoked
preventOthers :: (IsEvent e) => EventM e self ()
preventOthers = do
    preventDefault
    stopPropagation
    stopImmediatePropagation
    returnValue False

foreign import javascript safe "$r = new Date().getTime()/1000"
    getTime :: IO (GLfloat)

foreign import javascript unsafe "$r=[];for(var i = 0; i < $1['touches'].length; i++){$r.push($1['touches'][i].clientX);$r.push($1['touches'][i].clientY);}"
    getTouches' :: JSRef UIEvent -> JSRef [GLfloat]

getTouches :: UIEvent -> IO [Vector2 GLfloat]
getTouches e = liftM (f . fromMaybe []) . fromJSRefListOf . getTouches' $ unUIEvent e
    where f (a:b:xs) = (Vector2 a b):f xs
          f _ = []
