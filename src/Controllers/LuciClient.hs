{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.LuciClient
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.LuciClient
    ( LuciClient (), hostOf, portOf, localPathOf, connectionString
    , connectToLuci
--    , authenticate
    , getServicesList
    , getServiceInfo
    , LuciScenario (), createLuciScenario
    , scenarioId, scenarioName, scenarioMqttTopic, scenarioTimestamp
    , runLuciService
    , LuciServiceInput (..), LuciServiceOutput (..)
    ) where

import Data.Int (Int64)
import JsHs.JSString (JSString, append,unpack',pack)

---- import GHCJS.Foreign
import JsHs.Types (JSVal)
import Data.Geometry.Structure.Feature (FeatureCollection)
import JsHs.Array (LikeJS (..))

import Control.Arrow (first)

newtype LuciServiceInput = LuciServiceInput JSVal
instance LikeJS "Object" LuciServiceInput

newtype LuciServiceOutput = LuciServiceOutput JSVal
instance LikeJS "Object" LuciServiceOutput

newtype LuciServiceInfo = LuciServiceInfo JSVal
instance LikeJS "Object" LuciServiceInfo
instance Show LuciServiceInfo where
    show (LuciServiceInfo val) = unpack' $ jsonStringify val

-- | Object for Luci connection
newtype LuciClient = LuciClient JSVal
instance LikeJS "Luci.Client" LuciClient

-- | Scenario Object
newtype LuciScenario = LuciScenario JSVal
instance LikeJS "Object" LuciScenario

-- | Full string passed into WebSocket constructor
connectionString :: LuciClient -> JSString
connectionString lc = "ws://" `append` hostOf lc `append` ":" `append` pack (show (portOf lc)) `append` "/" `append` localPathOf lc


-- | Local path of the connection on server
{-# NOINLINE localPathOf #-}
foreign import javascript unsafe "$r = $1.localpath;"
    localPathOf :: LuciClient -> JSString


-- | Port of the connection on server
{-# NOINLINE portOf #-}
foreign import javascript unsafe "$r = $1.port;"
    portOf :: LuciClient -> Int


-- | Connection server
{-# NOINLINE hostOf #-}
foreign import javascript unsafe "$r = $1.host;"
    hostOf :: LuciClient -> JSString


-- | ScID
{-# NOINLINE scenarioId #-}
foreign import javascript unsafe "$r = $1['ScID'];"
    scenarioId :: LuciScenario -> Int


-- | Name of the scenario
{-# NOINLINE scenarioName #-}
foreign import javascript unsafe "$r = $1['name'];"
    scenarioName :: LuciScenario -> JSString

-- | Name of the scenario
{-# NOINLINE scenarioMqttTopic #-}
foreign import javascript unsafe "$r = $1['mqtt_topic'];"
    scenarioMqttTopic :: LuciScenario -> JSString

-- | Timestamp of the scenario
{-# NOINLINE scenarioTimestamp #-}
foreign import javascript unsafe "$r = $1['timestamp'];"
    scenarioTimestamp :: LuciScenario -> Int64


-- | Open WebSocket and connect to Luci
connectToLuci :: JSString -> IO (Either JSString LuciClient)
connectToLuci address = eitherError "LuciClient" <$> connectToLuci' (pack host) port (pack path)
    where splitOn _ [] = ([],[])
          splitOn c (x:xs) | c == x = ([], xs)
                           | otherwise = first (x:) $ splitOn c xs
          (addr, path) = splitOn '/' $ unpack' address
          (host, port') = splitOn ':' addr
          port = case filter (\x -> x >= '0' && x <= '9') port' of
            [] -> 80
            pp -> read pp
foreign import javascript interruptible "try {\
    \   var lc = new Luci.Client($1, $2, $3, QLuciHandler);\
    \   lc.host = $1;lc.port = $2;lc.localpath = $3; \
    \   lc.connect\
    \       ( function(){$c(new LikeHS.Either(lc,true));} \
    \       , function(ev){$c(new LikeHS.Either('Closed with code ' + ev.code, false));} \
    \       , function(){logText('Error occured on the WebSocket side.');} \
    \   ); \
    \ }\
    \ catch(err){$c(new LikeHS.Either('Error occured while trying to create Luci client: ' + err, false))}"
    connectToLuci' :: JSString -> Int -> JSString -> IO JSVal


runLuciService :: LuciClient -> JSString -> LuciServiceInput -> LuciScenario -> IO (Either JSString LuciServiceOutput)
runLuciService lc service inputs scenario = eitherError "service output" <$>
                                            runService' lc service (scenarioId scenario) inputs
foreign import javascript interruptible  "var req = {}; \
    \ req['run'] = $2; \
    \ req['ScID'] = $3; \
    \ req['gridMultiPoint'] = $4; \
    \ var logg = (DEBUG && console.log('Running Luci service:')); \
    \ logg = (DEBUG && console.log(req)); \
    \ $1.sendAndReceive(req, new QLuciHandler($c));"
    runService' :: LuciClient -> JSString -> Int -> LuciServiceInput -> IO JSVal

-- | Get list of names of available services in Luci
getServicesList :: LuciClient -> IO (Either JSString [JSString])
getServicesList lc = eitherError "Luci answer" <$> getServicesList' lc
foreign import javascript interruptible "var req = {}; \
    \ req['run'] = 'ServiceList'; \
    \ $1.sendAndReceive(req, new QLuciHandler($c, ['ServiceList']));"
    getServicesList' :: LuciClient -> IO JSVal


getServiceInfo :: LuciClient -> JSString -> IO (Either JSString LuciServiceInfo)
getServiceInfo lc sname = eitherError "LuciServiceInfo" <$> getServiceInfo' lc sname
foreign import javascript interruptible "var req = {}; \
    \ req['run'] = 'ServiceInfo'; \
    \ req['serviceNames'] = [$2]; \
    \ $1.sendAndReceive(req, new QLuciHandler($c, [$2]));"
    getServiceInfo' :: LuciClient -> JSString -> IO JSVal


createLuciScenario :: LuciClient -> JSString -> FeatureCollection -> IO (Either JSString LuciScenario)
createLuciScenario lc sname geom = eitherError "Luci Scenario" <$> createScenario' lc sname geom
foreign import javascript interruptible "var req = {}; \
    \ req['run'] = 'scenario.geojson.Create'; req['name'] = $2; \
    \ req['geometry_input']= {}; \
    \ req['geometry_input']['name'] = 'geometry_input'; \
    \ req['geometry_input']['format'] = 'GeoJSON'; \
    \ req['geometry_input']['geometry'] = $3; \
    \ $1.sendAndReceive(req,new QLuciHandler($c));"
    createScenario' :: LuciClient -> JSString -> FeatureCollection -> IO JSVal

eitherError :: LikeJS ta a => JSString -> JSVal -> Either JSString a
eitherError s val = case asLikeJS val of
                      Just x -> x
                      Nothing -> Left $ "Something bad has just happened: " `append` s `append` " is null or undefined."


foreign import javascript unsafe "JSON.stringify($1)"
    jsonStringify :: JSVal -> JSString
