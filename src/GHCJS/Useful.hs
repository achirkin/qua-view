{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHCJS.Useful
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module GHCJS.Useful where

--import qualified Data.Aeson as A
import Data.List (intercalate)
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.WebGL.Types (GLfloat)

-- | Good name to call all html-js elements
type JSElement = JSRef JSElement_
data JSElement_

-- | Shortcut to get the element from the DOM
getElementById :: String -> IO JSElement
getElementById = getElementById' . toJSString
foreign import javascript unsafe "$r = document.getElementById($1)"
    getElementById' :: JSString -> IO JSElement

-- | Get body element of the page
foreign import javascript unsafe "$r = document.body"
    documentBody :: IO JSElement

-- | Do some staff in next animation frame
inNextFrame :: IO () -> IO ()
inNextFrame a = syncCallback NeverRetain False a >>= inNextFrame'
foreign import javascript unsafe "window.requestAnimationFrame($1)"
    inNextFrame' :: JSFun (IO ()) -> IO ()

-- | Log staff
foreign import javascript unsafe "console.log($1)"
    printRef :: JSRef a -> IO ()

-- | Current time in seconds
foreign import javascript unsafe "$r = new Date().getTime()/1000"
    getTime :: IO (GLfloat)


foreign import javascript unsafe "$r = $1.clientWidth"
    getElementWidth :: JSElement -> IO GLfloat

foreign import javascript unsafe "$r = $1.clientHeight"
    getElementHeight :: JSElement -> IO GLfloat


foreign import javascript unsafe "$1.width = $2"
    setElementWidth :: JSElement -> GLfloat -> IO ()

foreign import javascript unsafe "$1.height = $2"
    setElementHeight :: JSElement -> GLfloat -> IO ()

foreign import javascript unsafe "$1.style.width = $2 + 'px'"
    setElementStyleWidth :: JSElement -> GLfloat -> IO ()

foreign import javascript unsafe "$1.style.height = $2 + 'px'"
    setElementStyleHeight :: JSElement -> GLfloat -> IO ()


insertAfterHTML :: JSElement -> String -> IO ()
insertAfterHTML el string = insertAfterHTML' el (toJSString string)

foreign import javascript unsafe "$1.insertAdjacentHTML('afterend', $2);"
    insertAfterHTML' :: JSElement -> JSString -> IO ()


-- | Display loading splash
programInProgress :: IO ()
programInProgress = programInProgress' >> threadDelay 0

foreign import javascript interruptible "document.getElementById('loadingSplash').style.display = 'block';setTimeout($c(), 0);"
    programInProgress' :: IO ()

-- | Hide loading splash
foreign import javascript unsafe "document.getElementById('loadingSplash').style.display = 'none';"
    programIdle :: IO ()



foreign import javascript unsafe "logText($1)"
    logText' :: JSString -> IO ()

-- | Log into in-program console
logText :: String -> IO ()
logText s = logText' (toJSString . intercalate "<br/>" . lines $ s)


setElementInnerHTML :: JSElement -> String -> IO ()
setElementInnerHTML el = setElementInnerHTML' el . toJSString

foreign import javascript unsafe "$1.innerHTML = $2;"
    setElementInnerHTML' :: JSElement -> JSString -> IO ()

foreign import javascript unsafe "$1.style.display = 'none';"
    hideElement :: JSElement -> IO ()

foreign import javascript unsafe "$1.style.display = 'block';"
    showElement :: JSElement -> IO ()

foreign import javascript unsafe "$r = $1.value;"
    getInputValue' :: JSElement -> IO JSString

getInputValue :: JSElement -> IO String
getInputValue = liftM fromJSString . getInputValue'

foreign import javascript unsafe "$r = $1.parentNode;"
    elementParent :: JSElement -> IO JSElement

getHtmlArg :: String -> IO (Maybe String)
getHtmlArg arg = liftM (\s -> if s == Just "" then Nothing else s)
    $ getHtmlArg' (toJSString arg) >>= fromJSRef

foreign import javascript unsafe "$r = httpArgs[$1];"
    getHtmlArg' :: JSString -> IO (JSRef String)


