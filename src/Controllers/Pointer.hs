-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.Pointer
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Mouse-touch events.
-- I really tried hard to make it as simple as possible :)
--
-----------------------------------------------------------------------------

module Controllers.Pointer
    ( InteractionType (..)
    , PointerClickEvent (..), PointerDownEvent (..), PointerMoveEvent (..)
    , PointerUpEvent (..), PointerCancelEvent(..), WheelEvent (..)
    , ClickCallBack, MoveStartCallBack, MoveCallBack
    , MoveEndCallBack, PCancelCallBack, WheelCallBack
    , addEventlisteners
    , onMouseWheel
    , ElementClickEvent (..), ElementClickCallback, elementOnClick
    )
    where

import Control.Monad (liftM)

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.WebGL
import GHCJS.Useful

import Geometry.Space




type ClickCallBack = PointerClickEvent -> IO ()
type MoveStartCallBack = PointerDownEvent -> IO ()
type MoveCallBack = PointerMoveEvent -> IO ()
type MoveEndCallBack = PointerUpEvent -> IO ()
type PCancelCallBack = PointerCancelEvent -> IO ()
type WheelCallBack = WheelEvent -> IO ()

-- | Click or touch
data PointerClickEvent = PClick !InteractionType [Vector2 GLfloat] deriving (Eq, Show)
-- | Start of dragging
data PointerDownEvent = PDown !InteractionType [Vector2 GLfloat] deriving (Eq, Show)
-- | During dragging
data PointerMoveEvent = PMove !InteractionType GLfloat [(Vector2 GLfloat,Vector2 GLfloat)] deriving (Eq, Show)
-- | End of dragging
data PointerUpEvent = PUp !InteractionType GLfloat [(Vector2 GLfloat,Vector2 GLfloat)] deriving (Eq, Show)
-- | Cancel move or leave an area
data PointerCancelEvent = PCancel
-- | Mouse wheel change
newtype WheelEvent = WheelDelta GLfloat deriving (Eq, Show)
-- | Supported mouse buttons or touches
data InteractionType = LeftButton | RightButton | MiddleButton | Touches deriving (Eq, Show)



instance FromJSRef InteractionType where
    fromJSRef = liftM (>>= return . toEnum) . fromJSRef . (castRef :: JSRef InteractionType -> JSRef Int)

instance Enum InteractionType where
    fromEnum LeftButton = 0
    fromEnum MiddleButton = 1
    fromEnum RightButton = 2
    fromEnum Touches = 99
    toEnum 0 = LeftButton
    toEnum 1 = MiddleButton
    toEnum 2 = RightButton
    toEnum 99 = Touches
    toEnum _ = LeftButton

----------------------------------------------------------------------------------------------------
-- All event listeners together
----------------------------------------------------------------------------------------------------

-- | Add event listener on all pointer events at once.
--   This is not very nice solution, but it is easy to implement.
addEventlisteners :: JSElement
                  -> ClickCallBack -- ^ click
                  -> MoveStartCallBack -- ^ down
                  -> MoveCallBack -- ^ move
                  -> MoveEndCallBack -- ^ up
                  -> PCancelCallBack -- ^ cancel
                  -> IO ()
addEventlisteners element clickFun downFun moveFun upFun cancelFun = do
    clickCallBack <- asyncCallback1 AlwaysRetain (\iref -> fromJSRef iref
        >>= maybeCall (\(Interaction t _ ps) -> clickFun $ PClick t (map fst ps)))
    downCallBack <- asyncCallback1 AlwaysRetain (\iref -> fromJSRef iref
        >>= maybeCall (\(Interaction t _ ps) -> downFun $ PDown t (map snd ps)))
    moveCallBack <- asyncCallback1 AlwaysRetain (\iref -> fromJSRef iref
        >>= maybeCall (\(Interaction t dt ps) -> moveFun $ PMove t dt ps))
    upCallBack <- asyncCallback1 AlwaysRetain (\iref -> fromJSRef iref
        >>= maybeCall (\(Interaction t dt ps) -> upFun $ PUp t dt ps))
    cancelCallback <- asyncCallback AlwaysRetain (cancelFun PCancel)
    addEventlisteners' element clickCallBack downCallBack moveCallBack upCallBack cancelCallback
    where maybeCall _ Nothing = print "ups!" >> return ()
          maybeCall f (Just x) = f x
foreign import javascript unsafe "var clickTime = 200; var clickMove = 10; \
    \ $1.addEventListener('contextmenu', function(event) {event.preventDefault();event.stopPropagation();return false;}); \
    \ $1.mousebuttons = 0; \
    \ var toInteraction = function(state,dt,pos){ \
    \     var points = []; \
    \     if (!pos) {return undefined;} \
    \     points = points.concat.apply(points, pos.map(function(p) {return [p.nx,p.ny,p.ox,p.oy];})); \
    \     return [state,dt,points]; \
    \ }; \
    \ var pointerUp = function(event){ \
    \     event.preventDefault(); \
    \     event.stopPropagation(); \
    \     for (i = 0; i < event.target.pointerPos.length; i++) { \
    \         event.target.pointerPos[i].ox = event.target.pointerPos[i].nx; \
    \         event.target.pointerPos[i].oy = event.target.pointerPos[i].ny; \
    \     } \
    \     var arg = toInteraction(event.target.pointerState,event.target.deltaTime,event.target.pointerPos); \
    \     if (arg) {\
    \     if ((new Date()).getTime() - event.target.downTime < clickTime \
    \        && Math.abs(event.target.pointerPos[0].nx - event.target.pointerPos[0].ox) < clickMove \
    \        && Math.abs(event.target.pointerPos[0].ny - event.target.pointerPos[0].oy) < clickMove){ \
    \         $2(arg); \
    \     } else { \
    \         $5(arg); \
    \     }} \
    \     if (!event.touches) { event.target.mousebuttons--;}\
    \     if ((!event.touches && event.target.mousebuttons <= 0) || (event.touches && event.touches.length == 0)) {\
    \         event.target.mousebuttons = 0;\
    \         delete event.target.downTime; \
    \         delete event.target.pointerState; \
    \         delete event.target.pointerPos; \
    \         delete event.target.deltaTime; \
    \     } \
    \     return false; \
    \ }; \
    \ var pointerDown = function(event){ \
    \     event.preventDefault(); \
    \     event.stopPropagation(); \
    \     event.target.downTime = (new Date()).getTime(); \
    \     if (event.touches) { \
    \         event.target.pointerState = 99; \
    \         event.target.pointerPos = Array.prototype.slice.call(event.touches).map(function(t) {return {ox: t.clientX, oy: t.clientY, nx: t.clientX, ny: t.clientY};}); \
    \     } else { \
    \         event.target.mousebuttons++; \
    \         event.target.pointerState = event.button; \
    \         event.target.pointerPos = [{ox: event.clientX, oy: event.clientY, nx: event.clientX, ny: event.clientY}]; \
    \     } \
    \     event.target.deltaTime = 0; \
    \     var arg = toInteraction(event.target.pointerState,event.target.deltaTime,event.target.pointerPos); \
    \     if (arg) {$3(arg);} \
    \     return false; \
    \ }; \
    \ var pointerMove = function(event){ \
    \     event.preventDefault(); \
    \     event.stopPropagation(); \
    \     if (event.target.downTime && event.target.pointerPos) { \
    \         if (event.touches) { \
    \             var npos = Array.prototype.slice.call(event.touches).map(function(t) {return {x: t.clientX, y: t.clientY};}); \
    \             if (npos.length == event.target.pointerPos.length) { \
    \                 for(var i = 0; i < event.target.pointerPos.length; i++) {\
    \                   event.target.pointerPos[i].nx = npos[i].x; \
    \                   event.target.pointerPos[i].ny = npos[i].y; \
    \                 } \
    \             } else { \
    \                 event.target.pointerPos = npos.map(function (np, i) {return { nx: np.x \
    \                                                                             , ny: np.y \
    \                                                                             , ox: np.x \
    \                                                                             , oy: np.y };}); \
    \             } \
    \         } else { \
    \             event.target.pointerPos = [event.target.pointerPos[0]]; \
    \             event.target.pointerPos[0].nx = event.clientX; \
    \             event.target.pointerPos[0].ny = event.clientY; \
    \         } \
    \         event.target.deltaTime = (new Date()).getTime() - event.target.downTime; \
    \         var arg = toInteraction(event.target.pointerState,event.target.deltaTime,event.target.pointerPos); \
    \         if (arg) {$4(arg);} \
    \     } \
    \     return false; \
    \ }; \
    \ var pointerCancel = function(event){ \
    \     event.preventDefault(); \
    \     event.stopPropagation(); \
    \     event.target.mousebuttons = 0; \
    \     delete event.target.downTime; \
    \     delete event.target.pointerState; \
    \     delete event.target.pointerPos; \
    \     delete event.target.deltaTime; \
    \     $6(); \
    \     return false; \
    \ }; \
    \ $1.addEventListener('mouseup', pointerUp); \
    \ $1.addEventListener('touchend', pointerUp); \
    \ $1.addEventListener('mousedown', pointerDown); \
    \ $1.addEventListener('touchstart', pointerDown); \
    \ $1.addEventListener('mousemove', pointerMove); \
    \ $1.addEventListener('touchmove', pointerMove); \
    \ $1.addEventListener('mouseleave', pointerCancel); \
    \ $1.addEventListener('touchcancel', pointerCancel); "
    addEventlisteners' :: JSElement
                       -> JSFun (JSRef Interaction -> IO ()) -- ^ click
                       -> JSFun (JSRef Interaction -> IO ()) -- ^ down
                       -> JSFun (JSRef Interaction -> IO ()) -- ^ move
                       -> JSFun (JSRef Interaction -> IO ()) -- ^ up
                       -> JSFun (IO ()) -- ^ cancel
                       -> IO ()


data Interaction = Interaction !InteractionType !GLfloat ![(Vector2 GLfloat, Vector2 GLfloat)]
    deriving (Eq, Show)

instance FromJSRef Interaction where
    fromJSRef = liftM (>>= parse) . fromJSRef . (castRef :: JSRef Interaction -> JSRef (Int,GLfloat,[GLfloat]))
        where parse (t,dt,arr) = Just $ Interaction (toEnum t) dt (listToCoords arr)
              listToCoords (a:b:c:d:xs) = (Vector2 a b,Vector2 c d) : listToCoords xs
              listToCoords [_,_,_] = []
              listToCoords [_,_] = []
              listToCoords [_] = []
              listToCoords [] = []

----------------------------------------------------------------------------------------------------
-- Mouse wheel
----------------------------------------------------------------------------------------------------

-- | An argument is wheel delta, which is only +1 or -1 for compatibility reasons.
--   This is the only stateless action in the module.
onMouseWheel :: JSElement -> WheelCallBack -> IO ()
onMouseWheel element wheelFun = do
    wheelCallBack <- asyncCallback1 AlwaysRetain (\iref -> fromJSRef iref
        >>= maybeCall (\d -> wheelFun $ WheelDelta d))
    onMouseWheel' element wheelCallBack
    where maybeCall _ Nothing = print "ups!" >> return ()
          maybeCall f (Just x) = f x
foreign import javascript unsafe "\
    \ $1.addEventListener('wheel', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(e['wheelDelta'] > 0 || e['detail'] < 0 || e['deltaY'] < 0 ? (1.0) : (-1.0)); \
    \     return false; \
    \ });"
    onMouseWheel' :: JSElement -> JSFun (JSRef GLfloat -> IO ()) -> IO ()



----------------------------------------------------------------------------------------------------
-- Simple Click on Element
----------------------------------------------------------------------------------------------------

-- | Simple event when JSElement is clicked
elementOnClick :: JSElement -> ElementClickCallback -> IO ()
elementOnClick element clickFun = do
    clickCallBack <- asyncCallback AlwaysRetain (clickFun ElementClick)
    elementOnClick' element clickCallBack
foreign import javascript unsafe "\
    \ $1.addEventListener('click', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(); \
    \     return false; \
    \ });"
    elementOnClick' :: JSElement -> JSFun (IO ()) -> IO ()



-- | Simple click on element
data ElementClickEvent = ElementClick
type ElementClickCallback = ElementClickEvent -> IO ()
