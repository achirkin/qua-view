{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.GeoJSONFileImport
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.GeoJSONFileImport
    ( onGeoJSONFileImport
    , loadGeoJSONFromLink
    ) where

import qualified Data.Aeson as A
import Control.Monad (liftM)

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign

import GHCJS.Useful
import Controllers.GUIEvents


onGeoJSONFileImport :: JSElement -> GeoJSONLoadCallBack -> IO ()
onGeoJSONFileImport importButton callback = elementOnChange importButton $ do
    programInProgress
    logText "Trying to parse GeoJSON FeatureCollection..."
    c <- getElementFiles importButton >>= fromJSRef_aeson
    case c of
        Nothing -> logText "Could not read geometry"
        Just gfc -> do
            isBehChecked <- isElementChecked $ toJSString "dynamicstaticswitcher"
            logText "GeoJSON FeatureCollection is imported."
            callback GeoJSONLoaded
                { isDynamic          = isBehChecked
                , featureCollection  = gfc
                }
    programIdle

loadGeoJSONFromLink :: String -> Bool -> GeoJSONLoadCallBack -> IO ()
loadGeoJSONFromLink url isDyn callback = do
    c <- getUrlJSON url >>= fromJSRef_aeson
    case c of
        Nothing -> logText "Could not read geometry"
        Just gfc -> do
            callback GeoJSONLoaded
                { isDynamic          = isDyn
                , featureCollection  = gfc
                }


foreign import javascript unsafe "$r = document.getElementById($1).checked;"
    isElementChecked :: JSString -> IO Bool

-- | If checkbox element is checked
foreign import javascript interruptible "var r = new FileReader(); \
    \ var load = function() { \
    \ if (r.readyState != FileReader.EMPTY ) { \
    \   var json = null; \
    \   try { \
    \       json = JSON.parse(r.result); \
    \   } catch (err) { logText('Your browser does not like JSON file you have chosen: ' + err); } \
    \   $c(json); }}; \
    \ r.onloadend = load;  \
    \ r.readAsText($1.files[0]);"
    getElementFiles :: JSVal -> IO (JSVal)

-- | Convert JSON object in JavaScript back to Haskell data that implements fromJSON a class
fromJSRef_aeson :: A.FromJSON a => JSVal -> IO (Maybe a)
fromJSRef_aeson = liftM (>>= f . A.fromJSON) . fromJSRef . castRef
    where f (A.Error _) = Nothing
          f (A.Success x) = Just x

getUrlJSON :: String -> IO (JSVal)
getUrlJSON = getUrlJSON' . toJSString

foreign import javascript interruptible "var xmlHttp = new XMLHttpRequest(); \
    \ var json = null; \
    \ var i = 0;\
    \ var loadjson = function() { \
    \   try { \
    \       json = JSON.parse(xmlHttp.responseText); \
    \   } catch (err) { logText('Your browser does not like JSON file you have chosen: ' + err); } \
    \   if(i == 0){i++;$c(json);} \
    \ }; \
    \ try { \
    \     xmlHttp.onload = loadjson; \
    \     xmlHttp.open( 'GET', $1, true ); \
    \     xmlHttp.send( ); \
    \ } catch (err) { logText(err); if(i == 0){i++;$c(null);}} "
    getUrlJSON' :: JSString -> IO (JSVal)


-- | Simple event when JSElement is changed (e.g. one picked file in "file" button)
elementOnChange :: JSElement -> IO () -> IO ()
elementOnChange element clickFun = do
    clickCallBack <- asyncCallback AlwaysRetain clickFun
    elementOnChange' element clickCallBack
foreign import javascript unsafe "\
    \ $1.addEventListener('change', function(event){ \
    \     var e = window.event || event; \
    \     e.preventDefault(); \
    \     e.stopPropagation(); \
    \     $2(); \
    \     return false; \
    \ });"
    elementOnChange' :: JSElement -> JSFun (IO ()) -> IO ()
