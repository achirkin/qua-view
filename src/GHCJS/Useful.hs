{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import Prelude hiding (lines)
--import qualified Data.Aeson as A
import Data.JSString
--import Data.List (intercalate)
import Control.Concurrent (threadDelay)
--import Control.Monad (liftM)
import GHCJS.Foreign
import GHCJS.Foreign.Callback (Callback)
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.WebGL.Types (GLfloat)
import JavaScript.Web.Canvas (Canvas)

type Time = Double

-- | Good name to call all html-js elements
newtype JSElement = JSElement JSVal
instance IsJSVal JSElement

-- | Shortcut to get the element from the DOM
foreign import javascript unsafe "$r = document.getElementById($1)"
    getElementById :: JSString -> IO JSElement

-- | Shortcut to get the element from the DOM
foreign import javascript unsafe "$r = document.getElementById($1)"
    getCanvasById :: JSString -> IO Canvas

-- | Get body element of the page
foreign import javascript unsafe "$r = document.body"
    documentBody :: IO JSElement


-- | Log staff
foreign import javascript unsafe "console.log($1)"
    printVal :: JSVal -> IO ()

-- | Current time in seconds
foreign import javascript unsafe "$r = performance.now()"
    getTime :: IO Time

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


foreign import javascript unsafe "$1.insertAdjacentHTML('afterend', $2);"
    insertAfterHTML :: JSElement -> JSString -> IO ()


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
logText s = logText' (intercalate "<br/>" . lines . pack $ s)

logShowable :: Show a => a -> IO ()
logShowable = logText . show


foreign import javascript unsafe "$1.innerHTML = $2;"
    setElementInnerHTML :: JSElement -> JSString -> IO ()

foreign import javascript unsafe "$1.style.display = 'none';"
    hideElement :: JSElement -> IO ()

foreign import javascript unsafe "$1.style.display = 'block';"
    showElement :: JSElement -> IO ()

foreign import javascript unsafe "$r = $1.value;"
    getInputValue :: JSElement -> IO JSString

foreign import javascript unsafe "$r = $1.parentNode;"
    elementParent :: JSElement -> IO JSElement

foreign import javascript unsafe "$r = httpArgs[$1];"
    getHtmlArg :: JSString -> JSString


--class JSNum a where
--    fromJSNum :: JSVal -> a
--    toJSNum :: a -> JSVal
--
-- #define JSNUM(T) \
--foreign import javascript unsafe "$r = $1" js_to/**/T :: JSVal -> T; {-# INLINE js_to/**/T #-};\
--foreign import javascript unsafe "$r = $1" js_from/**/T :: T -> JSVal; {-# INLINE js_from/**/T #-};\
--instance JSNum T where { fromJSNum = js_to/**/T; {-# INLINE fromJSNum #-}; toJSNum = js_from/**/T; {-# INLINE toJSNum #-}}
--
--JSNUM(Int)
--JSNUM(Float)
--JSNUM(Double)

-- Home-made callbacks

foreign import javascript unsafe "h$makeCallback(h$runSyncReturnUnsafe, [false], $1)"
  js_syncCallbackReturnUnsafe :: Any -> IO (Callback (IO JSVal))

foreign import javascript unsafe
  "h$makeCallbackApply($1, h$runSyncReturnUnsafe, [false], $2)"
  js_syncCallbackApplyReturnUnsafe :: Int -> Any -> IO (Callback b)

syncCallbackUnsafe :: IO JSVal
                   -> IO (Callback (IO JSVal))
syncCallbackUnsafe x = js_syncCallbackReturnUnsafe (unsafeCoerce x)

syncCallbackUnsafe1 :: (JSVal -> IO JSVal)
                    -> IO (Callback (JSVal -> IO JSVal))
syncCallbackUnsafe1 x = js_syncCallbackApplyReturnUnsafe 1 (unsafeCoerce x)

syncCallbackUnsafe2 :: (JSVal -> JSVal -> IO JSVal)
                    -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallbackUnsafe2 x = js_syncCallbackApplyReturnUnsafe 2 (unsafeCoerce x)

syncCallbackUnsafe3 :: (JSVal -> JSVal -> JSVal -> IO JSVal)
                    -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallbackUnsafe3 x = js_syncCallbackApplyReturnUnsafe 3 (unsafeCoerce x)
