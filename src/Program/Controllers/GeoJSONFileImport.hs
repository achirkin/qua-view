{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Controllers.GeoJSONFileImport
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Controllers.GeoJSONFileImport
    ( registerJSONFileImports
    , loadGeoJSONFromLink
    , registerClearGeometry
    ) where


import Data.Geometry.Structure.Feature (FeatureCollection, SomeJSONInput)
import JsHs.LikeJS.Class
import JsHs.Types
import JsHs.Callback
import JsHs.Useful
import Program.Controllers.GUI (registerLoadingFile, registerClearGeometry)


-- | Load GeoJSON file from local system
registerJSONFileImports :: (SomeJSONInput -> IO ()) -> IO ()
registerJSONFileImports = registerLoadingFile . f
  where
    f _ (Left str) = logText' str
    f fire (Right ji) = fire ji

-- | Load GeoJSON file by url
loadGeoJSONFromLink :: JSString -> (SomeJSONInput -> IO ()) -> IO ()
loadGeoJSONFromLink url callback = do
  c <- asyncCallback1 $ callback . asLikeJS
  getUrlJSON url c



foreign import javascript unsafe "var xmlHttp = new XMLHttpRequest(); \
    \ var json = null; \
    \ var loadjson = function() { \
    \   try { \
    \       json = JSON.parse(xmlHttp.responseText);$2(json); \
    \   } catch (err) { logText('Your browser does not like JSON file you have chosen: ' + err); } \
    \ }; \
    \ var errjson = function() {logText('Your browser cannot execute http request on ' + $1); }; \
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
    \ } catch (err) { logText(err); } "
    getUrlJSON :: JSString -> Callback (JSVal -> IO ()) -> IO ()

