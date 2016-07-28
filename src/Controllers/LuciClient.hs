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
    ( -- * Client
      LuciClient, luciHandler, connectionString
      -- * Messages
    , LuciMessage (..), sendMessage
    , msgHeaderValue, toLuciMessage
    , MessageHeader (..)
--    , LuciScenario ()
    --, runLuciService
--     LuciClient (), hostOf, portOf, localPathOf, connectionString
--    , connectToLuci
----    , authenticate
--    , getServicesList
--    , getServiceInfo
--    , LuciScenario (), createLuciScenario
--    , scenarioId, scenarioName, scenarioMqttTopic, scenarioTimestamp
--    , runLuciService
--    , LuciServiceInput (..), LuciServiceOutput (..)
    ) where

--import Data.Int (Int64)
--import JsHs.JSString (JSString, append,unpack',pack)

import Data.List (foldl')

---- import GHCJS.Foreign
import JsHs.Types
import JsHs.LikeJS.Class
--import Data.Geometry.Structure.Feature (FeatureCollection)
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import qualified JsHs.Callback as JS (Callback, asyncCallback2)

--import Control.Arrow (first)
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators

----------------------------------------------------------------------------------------------------
-- * Client
----------------------------------------------------------------------------------------------------

-- | Object for Luci connection
newtype LuciClient = LuciClient JSVal
instance LikeJS "Luci.Client" LuciClient



-- | Luci messages - send and receive
data LuciMessage = LuciMessage
  { header      :: !JSString
  , attachments :: !(JS.Array JSTA.ArrayBuffer)
  }

-- | get message header as using JSON.parse()
msgHeaderValue :: LikeJS s a => LuciMessage -> a
msgHeaderValue (LuciMessage h _) = asLikeJS $ jsonParse h

-- | create a message using JSON.stringify()
toLuciMessage :: LikeJS s a => a -> [JSTA.ArrayBuffer] -> LuciMessage
toLuciMessage h bs = LuciMessage (jsonStringify $ asJSVal h) (JS.fromList bs)


-- | Create LuciClient and register events on message receive
luciHandler :: JSString -> MomentIO (LuciClient, Event LuciMessage)
luciHandler str = do
  (ah, luci) <- liftIO $ do
    (ah', fire) <- newAddHandler
    (,) ah' <$> newLuciClient str fire
  luciMsgs <- fromAddHandler ah
  return (luci, luciMsgs)


foreign import javascript interruptible "new Luci.Client($1,$2,$c);"
  js_newLuciClient :: JSString -- ^ Connection string
                   -> JS.Callback (JSVal -> JSVal -> IO ()) -- ^ onmessage
                   -> IO LuciClient

-- | Create a new luci instance and connect
newLuciClient :: JSString  -- ^ connection string (i.e. ws://localhost:8080/luci)
              -> (LuciMessage -> IO ()) -- ^ onmessage callback
              -> IO LuciClient
newLuciClient connStr callback = do
  jsCall <- JS.asyncCallback2 (\h d -> callback $ LuciMessage (asLikeJS h) (asLikeJS d))
  js_newLuciClient connStr jsCall

-- | Full string passed into WebSocket constructor
foreign import javascript safe "$1.connectionString"
  connectionString :: LuciClient -> JSString

-- | Send Luci message
sendMessage :: LuciClient -> LuciMessage -> IO ()
sendMessage luci (LuciMessage h a) = js_sendMessage luci h a

foreign import javascript safe "$1.sendMessage($2,$3)"
  js_sendMessage :: LuciClient -> JSString -> JS.Array JSTA.ArrayBuffer -> IO ()



----------------------------------------------------------------------------------------------------
-- * Messages
----------------------------------------------------------------------------------------------------

-- | All possible message headers
data MessageHeader
  = MsgRun JSString [(JSString, JSVal)]
    -- ^ run service message, e.g. {'run': 'ServiceList'};
    -- params: 'run', [(name, value)]
  | MsgCancel Int
    -- ^ cancel service message, e.g. {'cancel': 25};
    -- params: 'callID'
  | MsgNewCallID Int
    -- ^ Luci call id, { newCallID: 57 };
    -- params: 'newCallID'
  | MsgResult Int Double JSString Int JSVal
    -- ^ result of a service execution,
    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result'
  | MsgProgress Int Double JSString Int Double JSVal
    -- ^ result of a service execution,
    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, percentage: 0, progress: null};
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'percentage', 'progress'
  | MsgError JSString
    -- ^ error message, e.g. {'error': 'We are in trouble!'};
    -- params: 'error'
  | MsgUnknown JSVal
    -- ^ unknown type of message; passed as-is

instance LikeJS "Object" MessageHeader where
  asLikeJS jsv | Just r <- getProp "result"    jsv = maybeUnknown jsv $ MsgResult
                                                   <$> getProp "callID" jsv
                                                   <*> getProp "duration" jsv
                                                   <*> getProp "serviceName" jsv
                                                   <*> getProp "taskID" jsv
                                                   <*> Just r
               | Just e <- getProp "error"     jsv = MsgError e
               | Just i <- getProp "newCallID" jsv = MsgNewCallID i
               | Just r <- getProp "progress"  jsv = maybeUnknown jsv $ MsgProgress
                                                   <$> getProp "callID" jsv
                                                   <*> getProp "duration" jsv
                                                   <*> getProp "serviceName" jsv
                                                   <*> getProp "taskID" jsv
                                                   <*> getProp "percentage" jsv
                                                   <*> Just r
               | Just i <- getProp "cancel"    jsv = MsgCancel i
               | Just n <- getProp "run"       jsv = MsgRun n [] -- TODO: use .getOwnPropertyNames()
               | otherwise = MsgUnknown jsv
    where
      maybeUnknown j Nothing  = MsgUnknown j
      maybeUnknown _ (Just v) = v
  asJSVal (MsgRun run props) = flip (foldl' (\x (n,v) -> setProp n v x)) props
                             $ setProp "run" run newObj
  asJSVal (MsgCancel callID) = setProp "callID" callID newObj
  asJSVal (MsgNewCallID newCallID) = setProp "newCallID" newCallID newObj
  asJSVal (MsgResult callID duration serviceName taskID result) =
          setProp "callID" callID
        . setProp "duration" duration . setProp "taskID" taskID
        . setProp "serviceName" serviceName $ setProp "result" result newObj
  asJSVal (MsgProgress callID duration serviceName taskID percentage result) =
          setProp "callID" callID
        . setProp "duration" duration . setProp "taskID" taskID . setProp "percentage" percentage
        . setProp "serviceName" serviceName $ setProp "result" result newObj
  asJSVal (MsgError err) = setProp "error" err newObj
  asJSVal (MsgUnknown j) = j

----------------------------------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------------------------------

foreign import javascript unsafe "JSON.stringify($1)"
    jsonStringify :: JSVal -> JSString

foreign import javascript unsafe "JSON.parse($1)"
    jsonParse :: JSString -> JSVal

foreign import javascript unsafe "$r = {};"
    newObj :: JSVal


{-# INLINE getProp #-}
getProp :: LikeJS s a => JSString -> JSVal -> Maybe a
getProp name = asLikeJS . js_getProp name

foreign import javascript unsafe "$2[$1]"
    js_getProp :: JSString -> JSVal -> JSVal

{-# INLINE setProp #-}
setProp :: LikeJS s a => JSString -> a -> JSVal -> JSVal
setProp name = js_setProp name . asJSVal

foreign import javascript unsafe "$3[$1] = $2; $r = $3;"
    js_setProp :: JSString -> JSVal -> JSVal -> JSVal


---- | Scenario Object
--newtype LuciScenario = LuciScenario JSVal
--instance LikeJS "Object" LuciScenario
--
---- | Full string passed into WebSocket constructor
--connectionString :: LuciClient -> JSString
--connectionString lc = "ws://" `append` hostOf lc `append` ":" `append` pack (show (portOf lc)) `append` "/" `append` localPathOf lc
--
--
---- | Local path of the connection on server
--{-# NOINLINE localPathOf #-}
--foreign import javascript unsafe "$r = $1.localpath;"
--    localPathOf :: LuciClient -> JSString
--
--
---- | Port of the connection on server
--{-# NOINLINE portOf #-}
--foreign import javascript unsafe "$r = $1.port;"
--    portOf :: LuciClient -> Int
--
--
---- | Connection server
--{-# NOINLINE hostOf #-}
--foreign import javascript unsafe "$r = $1.host;"
--    hostOf :: LuciClient -> JSString
--
--
---- | ScID
--{-# NOINLINE scenarioId #-}
--foreign import javascript unsafe "$r = $1['ScID'];"
--    scenarioId :: LuciScenario -> Int
--
--
---- | Name of the scenario
--{-# NOINLINE scenarioName #-}
--foreign import javascript unsafe "$r = $1['name'];"
--    scenarioName :: LuciScenario -> JSString
--
---- | Name of the scenario
--{-# NOINLINE scenarioMqttTopic #-}
--foreign import javascript unsafe "$r = $1['mqtt_topic'];"
--    scenarioMqttTopic :: LuciScenario -> JSString
--
---- | Timestamp of the scenario
--{-# NOINLINE scenarioTimestamp #-}
--foreign import javascript unsafe "$r = $1['timestamp'];"
--    scenarioTimestamp :: LuciScenario -> Int64
--

--
--
--runLuciService :: LuciClient -> JSString -> LuciServiceInput -> LuciScenario -> IO (Either JSString LuciServiceOutput)
--runLuciService lc service inputs scenario = eitherError "service output" <$>
--                                            runService' lc service (scenarioId scenario) inputs
--foreign import javascript interruptible  "var req = {}; \
--    \ req['run'] = $2; \
--    \ req['ScID'] = $3; \
--    \ req['gridMultiPoint'] = $4; \
--    \ var logg = (DEBUG && console.log('Running Luci service:')); \
--    \ logg = (DEBUG && console.log(req)); \
--    \ $1.sendAndReceive(req, new QLuciHandler($c));"
--    runService' :: LuciClient -> JSString -> Int -> LuciServiceInput -> IO JSVal
--
---- | Get list of names of available services in Luci
--getServicesList :: LuciClient -> IO (Either JSString [JSString])
--getServicesList lc = eitherError "Luci answer" <$> getServicesList' lc
--foreign import javascript interruptible "var req = {}; \
--    \ req['run'] = 'ServiceList'; \
--    \ $1.sendAndReceive(req, new QLuciHandler($c, ['ServiceList']));"
--    getServicesList' :: LuciClient -> IO JSVal
--
--
--getServiceInfo :: LuciClient -> JSString -> IO (Either JSString LuciServiceInfo)
--getServiceInfo lc sname = eitherError "LuciServiceInfo" <$> getServiceInfo' lc sname
--foreign import javascript interruptible "var req = {}; \
--    \ req['run'] = 'ServiceInfo'; \
--    \ req['serviceNames'] = [$2]; \
--    \ $1.sendAndReceive(req, new QLuciHandler($c, [$2]));"
--    getServiceInfo' :: LuciClient -> JSString -> IO JSVal
--
--
--createLuciScenario :: LuciClient -> JSString -> FeatureCollection -> IO (Either JSString LuciScenario)
--createLuciScenario lc sname geom = eitherError "Luci Scenario" <$> createScenario' lc sname geom
--foreign import javascript interruptible "var req = {}; \
--    \ req['run'] = 'scenario.geojson.Create'; req['name'] = $2; \
--    \ req['geometry_input']= {}; \
--    \ req['geometry_input']['name'] = 'geometry_input'; \
--    \ req['geometry_input']['format'] = 'GeoJSON'; \
--    \ req['geometry_input']['geometry'] = $3; \
--    \ $1.sendAndReceive(req,new QLuciHandler($c));"
--    createScenario' :: LuciClient -> JSString -> FeatureCollection -> IO JSVal
--
--eitherError :: LikeJS ta a => JSString -> JSVal -> Either JSString a
--eitherError s val = case asLikeJS val of
--                      Just x -> x
--                      Nothing -> Left $ "Something bad has just happened: " `append` s `append` " is null or undefined."
