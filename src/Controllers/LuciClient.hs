{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.LuciClient
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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
    , authenticate
    , getServicesList
    , getServiceInfo
    , Scenario (), createLuciScenario
    , scenarioId, scenarioName, scenarioMqttTopic, scenarioTimestamp
    , runLuciService
    , LuciServiceInput (), LuciServiceOutput ()
    ) where

import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import Data.Int (Int64)
import Data.Coerce (Coercible (), coerce)
import Data.JSString (JSString (..), append,unpack',pack)

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Marshal.Pure (PFromJSVal (), pFromJSVal)
import GHCJS.Types
import Data.Geometry (JSNum (..))

import Control.Arrow (first)

import Controllers.GUIEvents (Scenario)

newtype LuciServiceInput = LuciServiceInput JSVal

newtype LuciServiceOutput = LuciServiceOutput JSVal

newtype LuciServiceInfo = LuciServiceInfo JSVal
instance Show LuciServiceInfo where
    show (LuciServiceInfo val) = unpack' $ jsonStringify val

-- | Object for Luci connection
newtype LuciClient = LuciClient JSVal
--instance IsJSVal LuciClient

-- | Scenario Object
newtype LuciScenario = LuciScenario JSVal
--instance IsJSVal LuciScenario

-- | Full string passed into WebSocket constructor
connectionString :: LuciClient -> JSString
connectionString lc = "ws://" `append` hostOf lc `append` ":" `append` pack (show (portOf lc)) `append` "/" `append` localPathOf lc


-- | Local path of the connection on server
{-# NOINLINE localPathOf #-}
foreign import javascript unsafe "$r = $1['localpath'];"
    localPathOf :: LuciClient -> JSString


-- | Port of the connection on server
{-# NOINLINE portOf #-}
foreign import javascript unsafe "$r = $1['port'];"
    portOf :: LuciClient -> Int


-- | Connection server
{-# NOINLINE hostOf #-}
foreign import javascript unsafe "$r = $1['host'];"
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
foreign import javascript interruptible "try{\
    \   var lc = new LuciClient($1, $2, $3, function(){$c({right: lc});} \
    \       , function(ev){$c({left: 'Closed with code ' + ev.code});} \
    \       , function(){logText(document.getElementById('consolecontent'),'Error occured on the WebSocket side.');} \
    \   ); \
    \   lc.host = $1;lc.port = $2;lc.localpath = $3; \
    \ }\
    \ catch(err){$c({left: 'Error occured while trying to create Luci client: ' + JSON.stringify(err)});}"
    connectToLuci' :: JSString -> Int -> JSString -> IO JSVal


runLuciService :: LuciClient -> JSString -> LuciServiceInput -> LuciScenario -> IO (Either JSString LuciServiceOutput)
runLuciService lc service inputs scenario = eitherError "service output" <$>
                                            runService' lc service (scenarioId scenario) inputs
foreign import javascript interruptible  "var req = {}; \
    \ req['action'] = 'run'; req['service'] = {}; \
    \ req['service']['classname'] = $2; \
    \ req['service']['ScID'] = $3; \
    \ req['service']['inputs'] = $4; \
    \ $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result']) {$c({right: m['result']});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither a result nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    runService' :: LuciClient -> JSString -> Int -> LuciServiceInput -> IO JSVal

-- | Authenticate in Luci
authenticate :: LuciClient
             -> JSString -- ^ login
             -> JSString -- ^ password
             -> IO (Either JSString JSString)
authenticate lc login pass = eitherErrorJSString "Luci answer" <$> authenticate' lc  login pass
foreign import javascript interruptible "$1.authenticate($2,$3, function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result']) {$c({right: m['result']});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither result message nor an error. MSG: \" + JSON.stringify(m)});}});"
    authenticate' :: LuciClient -> JSString -> JSString -> IO JSVal


-- | Get list of names of available services in Luci
getServicesList :: LuciClient -> IO (Either JSString [JSString])
getServicesList lc = eitherErrorAny "Luci answer" <$> getServicesList' lc
foreign import javascript interruptible "var req = {}; req['action'] = 'get_list'; req['of'] = ['services']; $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result'] && m['result']['services']) {$c({right: h$fromArray(m['result']['services'])});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither services list nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    getServicesList' :: LuciClient -> IO JSVal


getServiceInfo :: LuciClient -> JSString -> IO (Either JSString LuciServiceInfo)
getServiceInfo lc sname = eitherError "LuciServiceInfo" <$> getServiceInfo' lc sname
foreign import javascript interruptible "var req = {}; req['action'] = 'get_infos_about'; req['servicename'] = $2; $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result'] && m['result'][$2]) {$c({right: m['result'][$2]});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither service info nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    getServiceInfo' :: LuciClient -> JSString -> IO JSVal


createLuciScenario :: LuciClient -> JSString -> Scenario -> IO (Either JSString LuciScenario)
createLuciScenario lc sname geom = eitherError "Luci Scenario" <$>  createScenario' lc sname geom
foreign import javascript interruptible "var req = {}; \
    \ req['action'] = 'create_scenario'; req['name'] = $2; \
    \ req['geometry']= {}; \
    \ req['geometry']['ghcjs-modeler-scenario'] = {}; \
    \ req['geometry']['ghcjs-modeler-scenario']['format'] = 'GeoJSON'; \
    \ req['geometry']['ghcjs-modeler-scenario']['geometry'] = $3; \
    \ $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result']) {$c({right: m['result']});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither a result nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    createScenario' :: LuciClient -> JSString -> Scenario -> IO JSVal

-- | Either treatment
eitherError :: (Coercible JSVal a) => JSString -> JSVal -> Either JSString a
eitherError = eitherError' coerce
eitherErrorPF :: (PFromJSVal a) => JSString -> JSVal -> Either JSString a
eitherErrorPF = eitherError' pFromJSVal
eitherErrorJSString :: JSString -> JSVal -> Either JSString JSString
eitherErrorJSString = eitherError' val2str
eitherErrorJSNum :: (JSNum a) => JSString -> JSVal -> Either JSString a
eitherErrorJSNum = eitherError' toNum
eitherErrorAny :: JSString -> JSVal -> Either JSString a
eitherErrorAny = eitherError' (unsafeCoerce . val2Any)

eitherError' :: (JSVal -> a) -> JSString -> JSVal -> Either JSString a
eitherError' f s val = let rval = rightOrNullVal val
                           lval = leftOrErrVal val $ "Something bad has just happened: " `append` s `append` "is falsy."
                       in if isTruthy rval
                          then Right $ f rval
                          else Left $ lval

foreign import javascript unsafe "if($1 && $1.right){$r = $1.right;} else {$r = null;}"
    rightOrNullVal :: JSVal -> JSVal
foreign import javascript unsafe "$r = $1;"
    val2Any :: JSVal -> Any

foreign import javascript unsafe "$r = $1;"
    val2str :: JSVal -> JSString

foreign import javascript unsafe "if($1 && $1.left){$r = $1.left;} else {$r = $2;}"
    leftOrErrVal :: JSVal -> JSString -> JSString

foreign import javascript unsafe "JSON.stringify($1)"
    jsonStringify :: JSVal -> JSString
