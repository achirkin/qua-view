{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.GeoJSONFileImport
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.GeoJSONFileImport
    ( GeoJSONImports, GeoJSONLoaded (..)
    , addHandler, registerButton, loadFromLink
    , geoJSONFileImports -- geoJSONImports
    ) where


import Data.Geometry.Structure.Feature (FeatureCollection)
import JsHs.LikeJS.Class
import JsHs.Types
import JsHs.Types.Prim
--import GHCJS.Marshal
import JsHs.Callback
--import JsHs.Debug
-- import GHCJS.Foreign (isTruthy)

import GHCJS.Useful
--import Controllers.GUIEvents
import Data.Coerce (coerce)

import Reactive.Banana.Frameworks
import Reactive.Banana.JsHs
--import Reactive.Banana.Combinators
--import Control.Monad.IO.Class

-- | When valid GeoJSON comes from somewhere
data GeoJSONLoaded = GeoJSONLoaded
    { isDynamic         :: Bool
    , featureCollection :: FeatureCollection
    }
type GeoJSONLoadCallBack = GeoJSONLoaded -> IO ()

data GeoJSONImports = GeoJSONImports
  { addHandler :: AddHandler GeoJSONLoaded
  , registerButton :: HTMLElement -> IO ()
  , loadFromLink :: JSString -> Bool -> IO ()
  }


geoJSONFileImports :: IO (AddHandler GeoJSONLoaded, GeoJSONLoadCallBack)
geoJSONFileImports =  do
    (h,fire) <- newAddHandler
    registerLoadingFile $ f fire
    return (h, fire)
  where
    f _ (Left str) = logText' str
    f fire (Right fc) = fire GeoJSONLoaded
          { isDynamic         = True
          , featureCollection = fc
          }

--geoJSONImports :: IO GeoJSONImports
--geoJSONImports = do
--  (h,fire) <- newAddHandler
--  return GeoJSONImports
--    { addHandler = h
--    , registerButton = flip onGeoJSONFileImport fire
--    , loadFromLink = \s d -> loadGeoJSONFromLink s d fire
--    }


--onGeoJSONFileImport :: HTMLElement -> GeoJSONLoadCallBack -> IO ()
--onGeoJSONFileImport importButton callback = elementOnChange importButton $ do
--    programInProgress
--    logText "Trying to parse GeoJSON FeatureCollection..."
--    gfc <- coerce <$> getElementFiles importButton
----    isBehChecked <- isElementChecked  "dynamicstaticswitcher"
--    logText "GeoJSON FeatureCollection is imported."
--    callback GeoJSONLoaded
--        { isDynamic          = True
--        , featureCollection  = gfc
--        }
--    programIdle

loadGeoJSONFromLink :: JSString -> Bool -> GeoJSONLoadCallBack -> IO ()
loadGeoJSONFromLink url isDyn callback = do
    c <- getUrlJSON url
    if jsIsNullOrUndef $ asJSVal c
    then logText "Could not read geometry"
    else callback GeoJSONLoaded
          { isDynamic         = isDyn
          , featureCollection = c
          }
-- | Register callback on file loaded
registerLoadingFile :: (Either JSString FeatureCollection -> IO ()) -> IO ()
registerLoadingFile callback = do
  callbackSuccess <- asyncCallback1 $ callback . Right . asLikeJS
  callbackFailure <- asyncCallback1 $ callback . Left . asLikeJS
  js_registerLoadingFile callbackSuccess callbackFailure

foreign import javascript safe "registerLoadingFile($1,$2)"
  js_registerLoadingFile :: Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()

--foreign import javascript unsafe "$r = document.getElementById($1).checked;"
--    isElementChecked :: JSString -> IO Bool
--
---- | If checkbox element is checked
--foreign import javascript interruptible "var r = new FileReader(); \
--    \ var load = function() { \
--    \ if (r.readyState != FileReader.EMPTY ) { \
--    \   var json = null; \
--    \   try { \
--    \       json = JSON.parse(r.result); \
--    \   } catch (err) { logText('Your browser does not like JSON file you have chosen: ' + err); } \
--    \   $c(json); }}; \
--    \ var errfun = function() {logText('Your browser cannot open file.'); $c(null);}; \
--    \ r.onloadend = load;  \
--    \ r.onerror = errfun; \
--    \ r.readAsText($1.files[0]);"
--    getElementFiles :: HTMLElement -> IO FeatureCollection



foreign import javascript interruptible "var xmlHttp = new XMLHttpRequest(); \
    \ var json = null; \
    \ var i = 0;\
    \ var loadjson = function() { \
    \   try { \
    \       json = JSON.parse(xmlHttp.responseText); \
    \   } catch (err) { logText('Your browser does not like JSON file you have chosen: ' + err); } \
    \   if(i == 0){i++;$c(json);} \
    \ }; \
    \ var errjson = function() {logText('Your browser cannot execute http request on ' + $1); if(i == 0){i++;$c(null);} }; \
    \ try { \
    \   if ('withCredentials' in xmlHttp) { \
    \       xmlHttp.onload = loadjson; \
    \       xmlHttp.onerror = errjson; \
    \       xmlHttp.open( 'GET', $1, true ); \
    \   } else if (typeof XDomainRequest != 'undefined') { \
    \       xmlHttp = new XDomainRequest(); \
    \       xmlHttp.onload = loadjson; \
    \       xmlHttp.onerror = errjson; \
    \       xmlHttp.open( 'GET', $1); \
    \   } else { \
    \       xmlHttp.onload = loadjson; \
    \       xmlHttp.onerror = errjson; \
    \       xmlHttp.open( 'GET', $1, true ); \
    \   } \
    \   xmlHttp.send( ); \
    \ } catch (err) { logText(err); if(i == 0){i++;$c(null);}} "
    getUrlJSON :: JSString -> IO FeatureCollection


-- | Simple event when HTMLElement is changed (e.g. one picked file in "file" button)
elementOnChange :: HTMLElement -> IO () -> IO ()
elementOnChange element clickFun = do
    clickCallBack <- asyncCallback clickFun
    elementOnChange' element clickCallBack
foreign import javascript unsafe "\
    \ $1.addEventListener('change', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(); \
    \     return false; \
    \ });"
    elementOnChange' :: HTMLElement -> Callback (IO ()) -> IO ()
