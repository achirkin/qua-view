{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Controllers.LuciClient
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Controllers.LuciClient
    ( -- * Client
      LuciClient (..), luciHandler, connectionString
      -- * Core message types
    , LuciMessage (..) -- , sendMessage
    , msgHeaderValue, toLuciMessage
    , MessageHeader (..), getMsgCallId
    , ServiceResult (..)
    , ServiceName (..), unServiceName
      -- * Specific messages
    , LuciResultServiceList (..), runServiceList
--    , LuciResultTestFibonacci, runTestFibonacci
    , LuciScenario (..), runScenarioGet, runScenarioUpdate, runScenarioCreate, runScenarioSubscribe
    , LuciResultScenarioList (..), ScenarioDescription (..), runScenarioList
    , GUI.registerAskLuciForScenario, displayScenarios, GUI.registerGetScenarioList
    , LuciScenarioCreated (..)
    , MessageAttachment (..), makeAttDesc
    , CallId (..), TaskId (..)
    , runQuaServiceList
    , ServiceInvocation
    , ServiceResponse (..), catResponses
    ) where



import qualified Program.Controllers.GUI as GUI
--import Data.String (IsString)
import JsHs.Useful

import JsHs
import JsHs.Types.Prim (jsNull)
import JsHs.JSString (unpack')
import Data.Geometry.Structure.Feature (FeatureCollection)
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import qualified JsHs.Callback as JS (Callback, asyncCallback2, asyncCallback1, asyncCallback)

--import Control.Arrow (first)
import Program.Settings
import Program.Types
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs.Types (Time)
import Data.Maybe (fromMaybe)

import Data.IORef
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)


import qualified Data.Map.Strict as Map


--import JsHs.Debug

----------------------------------------------------------------------------------------------------
-- * Client
----------------------------------------------------------------------------------------------------


-- | Object for Luci connection
data LuciClient
  = LuciClient JSVal -- ^ ready state
  | LuciClientOpening -- ^ opening connection
  | LuciClientClosed  -- ^ websocket connection closed
  | LuciClientError JSString -- ^ error occured

instance LikeJS "Luci.Client" LuciClient where
  asLikeJS jsv = case asLikeJS $ js_Luci jsv of
                  Nothing -> LuciClientError "Not a valid Luci.Client object"
                  Just l -> LuciClient l
  asJSVal (LuciClient jsv) = jsv
  asJSVal _ = jsNull

foreign import javascript unsafe "($1 && $1.objectName == 'LuciClient') \
                                 \ ? $1 : null" js_Luci :: JSVal -> JSVal


type ServiceInvocation = ServiceName -> [(JSString, JSVal)] -> [JSTA.ArrayBuffer] -> MomentIO (Event (ServiceResponse ServiceResult))
type ServiceEvent = Event (ServiceResponse ServiceResult)

-- | Run arbitrary Luci service and wait for responses
runLuciService :: LuciClient
               -> ServiceName
               -> [(JSString, JSVal)] -- ^ service JSON parameters
               -> [JSTA.ArrayBuffer] -- ^ attachments to send
               -> IO CallId
runLuciService luci serviceName params atts = do
  newCallId <-  genCallId luci
  sendMessage luci $ toLuciMessage (
      MsgRun serviceName $ ("callID", JS.asJSVal newCallId) : params
    ) atts
  return newCallId


-- TODO: have couple issues:
--   1. I am using now a dirty hack to not terminate "scenario.SubscribeTo" receiving
--   2. Need to expose a list of pending task to be able to terminate them
--       a) on scenario change
--       b) on GUI event cancel service execution
parseLuciMessages :: Behavior LuciClient
                  -> Event (MessageHeader, JS.Array JSTA.ArrayBuffer)
                  -> MomentIO ServiceInvocation
parseLuciMessages luciB incomings = do
    subscribeToHack <- liftIO $ newIORef (const $ return ())
    sessionMapRef <- liftIO $ newIORef Map.empty
    reactimate $ parseResponses sessionMapRef subscribeToHack <$> responsesE
    reactimate $ (\(msg, _) -> print $ "Ignoring message: " ++ (unpack' . jsonStringify $ JS.asJSVal msg)) <$> otherMsgs
    return $ invoke sessionMapRef subscribeToHack
  where
    (otherMsgs, responsesE) = split $ toServiceResponse <$> incomings
    -- another dirty hack to make luci working since subscribeTo results in "callID":0,"serviceName":"scenario.geojson.Get"
    parseResponses _ subscribeToHack r@(SRResult 0 _ _) = readIORef subscribeToHack >>= \a -> a r
    parseResponses sessionMapRef _ r@(SRResult callId _ _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
          Nothing -> logText' "Got a result for non-existing callID."
          Just a  -> a r >> modifyIORef' sessionMapRef (Map.delete callId)
    parseResponses sessionMapRef _ r@(SRError callId _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
          Nothing -> logText' "Got an error for non-existing callID."
          Just a  -> a r >> modifyIORef' sessionMapRef (Map.delete callId)
    parseResponses sessionMapRef _ r@(SRProgress callId _ _ _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
          Nothing -> logText' "Got a progress message for non-existing callID."
          Just a  -> a r
    invoke :: IORef (Map.Map CallId (Handler (ServiceResponse ServiceResult)))
           -> IORef (Handler (ServiceResponse ServiceResult))
           -> ServiceName -> [(JSString, JSVal)] -> [JSTA.ArrayBuffer] -> MomentIO (Event (ServiceResponse ServiceResult))
    invoke sessionMapRef subscribeToHack "scenario.SubscribeTo" pams atts = do
      luci <- valueB luciB
      newCallId <- liftIO $ runLuciService luci "scenario.SubscribeTo" pams atts
      (responseE, responseFire) <- newEvent
      liftIO $ modifyIORef' sessionMapRef (Map.insert newCallId responseFire)
      liftIO $ writeIORef subscribeToHack responseFire
      return responseE
    invoke sessionMapRef _ sname pams atts = do
      luci <- valueB luciB
      newCallId <- liftIO $ runLuciService luci sname pams atts
      (responseE, responseFire) <- newEvent
      liftIO $ modifyIORef' sessionMapRef (Map.insert newCallId responseFire)
      return responseE


--parseLuciMessages :: Behavior LuciClient
--                  -> Event (MessageHeader, JS.Array JSTA.ArrayBuffer)
--                  -> MomentIO ServiceInvocation
--parseLuciMessages luciB incomings = do
--    sessionMapRef <- liftIO $ newIORef Map.empty
--    reactimate $ parseResponses sessionMapRef <$> responsesE
--    reactimate $ (\(msg, _) -> print $ "Ignoring message: " ++ (unpack' . jsonStringify $ JS.asJSVal msg)) <$> otherMsgs
--    return $ invoke sessionMapRef
--  where
--    (otherMsgs, responsesE) = split $ toServiceResponse <$> incomings
--    parseResponses sessionMapRef r@(SRResult callId _ _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
--          Nothing -> logText' "Got a result for non-existing callID."
--          Just a  -> a r >> modifyIORef' sessionMapRef (Map.delete callId)
--    parseResponses sessionMapRef r@(SRError callId _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
--          Nothing -> logText' "Got an error for non-existing callID."
--          Just a  -> a r >> modifyIORef' sessionMapRef (Map.delete callId)
--    parseResponses sessionMapRef r@(SRProgress callId _ _ _) = readIORef sessionMapRef >>= \m -> case Map.lookup callId m of
--          Nothing -> logText' "Got a progress message for non-existing callID."
--          Just a  -> a r
--    invoke :: IORef (Map.Map CallId (Handler ServiceResponse))
--           -> ServiceName -> [(JSString, JSVal)] -> [JSTA.ArrayBuffer] -> MomentIO (Event ServiceResponse)
--    invoke sessionMapRef sname pams atts = do
--      luci <- valueB luciB
--      newCallId <- liftIO $ runLuciService luci sname pams atts
--      (responseE, responseFire) <- newEvent
--      liftIO $ modifyIORef' sessionMapRef (Map.insert newCallId responseFire)
--      return responseE


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
luciHandler :: JSString -> MomentIO (Behavior LuciClient, Event LuciClient, ServiceInvocation)
luciHandler str = do
  -- create all handlers
  (connectH, (onMessageH, onMessageFire)
           , (onOpenH, onOpenFire)
           , (onCloseH, onCloseFire)
           , (onErrorH, onErrorFire)
           ) <- liftIO $ do
    -- setup connection form
    GUI.showLuciConnectForm str
    (userClickH, userClickFire) <- newAddHandler
    -- register user click
    GUI.registerUserConnectToLuci userClickFire
    (,,,,) userClickH
            <$> newAddHandler
            <*> newAddHandler
            <*> newAddHandler
            <*> newAddHandler
  -- when user clicked on connect button
  connectE <- fromAddHandler connectH
  -- create Luci Client
  eLuciClientE <- flip mapEventIO connectE $ \url -> newLuciClient url onMessageFire
                          (onOpenFire ())
                          (onCloseFire LuciClientClosed)
                          (onErrorFire . LuciClientError)
  luciMsgs <- fromAddHandler onMessageH

  luciOpenEM <- fmap filterJust
      . flip mapEventIO eLuciClientE
      $ \eLuciClient -> case eLuciClient of
    Left err -> do
      onErrorFire (LuciClientError err)
      return Nothing
    Right lc -> return . Just $ (lc <$) <$> fromAddHandler onOpenH
  luciOpenE <- execute luciOpenEM >>= switchE
  luciCloseE <- fromAddHandler onCloseH
  luciErrorE <- fromAddHandler onErrorH
  let luciE = luciOpenE +*+ luciCloseE +*+ luciErrorE
      (+*+) = unionWith (const id)
  luciB <- stepper LuciClientOpening luciE
  lastUrlB <- stepper str connectE
  reactimate $ GUI.showLuciConnectForm <$> lastUrlB <@ luciCloseE
  reactimate $ GUI.showLuciConnectForm <$> lastUrlB <@ luciErrorE
  reactimate $ GUI.showLuciConnecting <$> lastUrlB <@ luciOpenE
  -- At this moment luciE says "opened" if websockets are open;
  -- however, we should wait until success message received from luci connection.
  let eitherWsOrLuci (MsgWebSocketState e, _) = Left e
      eitherWsOrLuci e = Right e
      (wsMsgs, luciMsgs') = split $ (\m -> eitherWsOrLuci (msgHeaderValue m, attachments m)) <$> luciMsgs
      wsChangeConn (LuciClient c) (WsSuccess _) _ = LuciClient c
      wsChangeConn  _ (WsSuccess _) s = s
      wsChangeConn _ (WsError e) _ = LuciClientError e
      wsChangeConn _ (WsTerminate _) _ = LuciClientClosed
      wsChangeE = wsChangeConn <$> luciB <@> wsMsgs
      isOpen LuciClient{} = True
      isOpen _ = False
  luciE' <- accumE LuciClientOpening wsChangeE
  luciB' <- stepper LuciClientOpening luciE'
  reactimate $ GUI.showLuciConnected <$> lastUrlB <@ filterE isOpen luciE'
  -- construct a session invoker function
  invokeH <- parseLuciMessages luciB' luciMsgs'
  return (luciB', luciE', invokeH)


foreign import javascript interruptible "try{$c(new LikeHS.Either(new Luci.Client($1,$2,$3,$4,$5),true));}\
                                        \catch(e){$c(new LikeHS.Either(e['message'] ? e['message'] : 'Luci client initialization error.',false));}"
  js_newLuciClient :: JSString -- ^ Connection string
                   -> JS.Callback (JSVal -> JSVal -> IO ()) -- ^ onmessage
                   -> JS.Callback (IO ()) -- ^ onopen
                   -> JS.Callback (IO ()) -- ^ onclose
                   -> JS.Callback (JSVal -> IO ()) -- ^ onerror
                   -> IO JSVal

-- | Create a new luci instance and connect
newLuciClient :: JSString  -- ^ connection string (i.e. ws://localhost:8080/luci)
              -> (LuciMessage -> IO ()) -- ^ onmessage callback
              -> IO () -- ^ onopen callback
              -> IO () -- ^ onclose callback
              -> (JSString -> IO ()) -- ^ onerror callback
              -> IO (Either JSString LuciClient)
newLuciClient connStr onMsgCall onOpenCall onCloseCall onErrorCall = do
  jsOnMessage <- JS.asyncCallback2 (\h d -> onMsgCall $ LuciMessage (asLikeJS h) (asLikeJS d))
  jsOnOpen <- JS.asyncCallback onOpenCall
  jsOnClose <- JS.asyncCallback onCloseCall
  jsOnError <- JS.asyncCallback1 (onErrorCall . asLikeJS)
  asLikeJS <$> js_newLuciClient connStr jsOnMessage jsOnOpen jsOnClose jsOnError

-- | Full string passed into WebSocket constructor
connectionString :: LuciClient -> JSString
connectionString (LuciClient c) = js_connectionString c
connectionString _ = ""
foreign import javascript safe "$1.connectionString"
  js_connectionString :: JSVal -> JSString

-- | Send Luci message
sendMessage :: LuciClient -> LuciMessage -> IO ()
sendMessage (LuciClient luci) (LuciMessage h a) = js_sendMessage luci h a
sendMessage _ _ = return ()

foreign import javascript safe "$1.sendMessage($2,$3)"
  js_sendMessage :: JSVal -> JSString -> JS.Array JSTA.ArrayBuffer -> IO ()

-- | Generate a new callId in sequential manner
genCallId :: LuciClient -> IO CallId
genCallId (LuciClient c) = js_genCallId c
genCallId _ = return $ -1

foreign import javascript "$1.genCallId()"
  js_genCallId :: JSVal -> IO CallId


----------------------------------------------------------------------------------------------------
-- * Message core types
----------------------------------------------------------------------------------------------------

-- | JSON Value representing result of a luci service work
newtype ServiceResult = ServiceResult { srVal :: JSVal }
instance LikeJS "Object" ServiceResult

-- | Luci callID is used to reference client's calls to luci and services
newtype CallId = CallId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" CallId where
  asLikeJS = CallId . asLikeJS
  asJSVal (CallId v) = asJSVal v

-- | Luci taskID is used in the context of luci workflows to refer to tasks
newtype TaskId = TaskId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" TaskId where
  asLikeJS = TaskId . asLikeJS
  asJSVal (TaskId v) = asJSVal v

-- | Percentage [0..100]%; used in luci messages to indicate state of a service computation
newtype Percentage = Percentage Double
  deriving (Eq,Ord,Num,Real,RealFrac,RealFloat,Fractional,Floating)
instance LikeJS "Number" Percentage where
  asLikeJS = Percentage . asLikeJS
  asJSVal (Percentage v) = asJSVal v
instance Show Percentage where
  show (Percentage x) = show (fromIntegral (round $ x*100 :: Int) / 100 :: Double) ++ "%"

-- | Cleaned up service response
data ServiceResponse a
  = SRResult !CallId !a !(JS.Array JSTA.ArrayBuffer)
  | SRProgress !CallId !Percentage !(Maybe a) !(JS.Array JSTA.ArrayBuffer)
  | SRError !CallId !JSString


-- | Categorize service responses into three different event types:
--     1. Error -- string message
--     2. Progress
--     3. Result
catResponses :: Event (ServiceResponse a) -> ( Event JSString
                                             , Event (Percentage, Maybe a, JS.Array JSTA.ArrayBuffer)
                                             , Event (a, JS.Array JSTA.ArrayBuffer)
                                             )
catResponses ev = (filterJust $ fromErr <$> ev, filterJust $ fromProg <$> ev, filterJust $ fromRes <$> ev)
  where
    fromErr (SRError _ s) = Just s
    fromErr _ = Nothing
    fromProg (SRProgress _ p mv a) = Just (p,mv,a)
    fromProg _ = Nothing
    fromRes (SRResult _ v a) = Just (v, a)
    fromRes _ = Nothing


instance Functor ServiceResponse where
  fmap f (SRResult i v a) = SRResult i (f v) a
  fmap f (SRProgress i p (Just v) a) = SRProgress i p (Just $ f v) a
  fmap _ (SRProgress i p Nothing a) = SRProgress i p Nothing a
  fmap _ (SRError i e) = SRError i e

toServiceResponse :: (MessageHeader, JS.Array JSTA.ArrayBuffer) -> Either (MessageHeader, JS.Array JSTA.ArrayBuffer) (ServiceResponse ServiceResult)
toServiceResponse (MsgResult cId _ _ _ sr, atts) = Right $ SRResult cId sr atts
toServiceResponse (MsgProgress cId _ _ _ p msr, atts) = Right $ SRProgress cId p msr atts
toServiceResponse (MsgError (Just cId) e, _) = Right $ SRError cId e
toServiceResponse x = Left x

responseCallId :: ServiceResponse a -> CallId
responseCallId (SRResult i _ _) = i
responseCallId (SRProgress i _ _ _) = i
responseCallId (SRError i _) = i


-- | All possible message headers
data MessageHeader
  = MsgRun !ServiceName ![(JSString, JSVal)]
    -- ^ run service message, e.g. {'run': 'ServiceList'};
    -- params: 'run', [(name, value)]
  | MsgCancel !CallId
    -- ^ cancel service message, e.g. {'cancel': 25};
    -- params: 'callID'
  | MsgNewCallID !CallId
    -- ^ Luci call id, { newCallID: 57 };
    -- params: 'newCallID'
  | MsgResult !CallId !Time !ServiceName !TaskId !ServiceResult
    -- ^ result of a service execution,
    -- e.g. { callID: 57, duration: 0, serviceName: "ServiceList", taskID: 0, result: Object };
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'result'
  | MsgProgress !CallId !Time !ServiceName !TaskId !Percentage !(Maybe ServiceResult)
    -- ^ result of a service execution,
    -- e.g. { callID: 57, duration: 0, serviceName: "St", taskID: 0, progress: 0, intermediateResult: null};
    -- params: 'callID', 'duration', 'serviceName', 'taskID', 'progress', 'intermediateResult'
  | MsgError !(Maybe CallId) !JSString
    -- ^ error message, e.g. {'error': 'We are in trouble!'};
    -- params: 'error'
  | MsgPanic !JSString
    -- ^ Initiate the panic recovery procedure
  | MsgUnknown !JSVal
    -- ^ unknown type of message; passed as-is
  | MsgWebSocketState !WebSocketMessage

getMsgCallId :: MessageHeader -> Maybe CallId
getMsgCallId MsgRun{} = Nothing
getMsgCallId (MsgCancel i) = Just i
getMsgCallId (MsgNewCallID i) = Just i
getMsgCallId (MsgResult i _ _ _ _) = Just i
getMsgCallId (MsgProgress i _ _ _ _ _) = Just i
getMsgCallId (MsgError mi _) = mi
getMsgCallId MsgPanic{} = Nothing
getMsgCallId MsgUnknown{} = Nothing
getMsgCallId MsgWebSocketState{} = Nothing




data WebSocketMessage
  = WsSuccess !JSString
    -- ^ All good so far
  | WsError !JSString
    -- ^ Some error on a server web-socket side
  | WsTerminate !JSString
    -- ^ Connection with luci is terminated

instance Show WebSocketMessage where
  show (WsSuccess msg) = "websocket success: " ++ unpack' msg
  show (WsError msg) = "websocket error: " ++ unpack' msg
  show (WsTerminate msg) = "websocket close: " ++ unpack' msg

instance LikeJS "Object" MessageHeader where
  asLikeJS jsv | Just r <- getProp "result"    jsv = maybeUnknown jsv $ MsgResult
                                                   <$> getProp "callID" jsv
                                                   <*> getProp "duration" jsv
                                                   <*> getProp "serviceName" jsv
                                                   <*> getProp "taskID" jsv
                                                   <*> Just r
               | Just e <- getProp "error"     jsv = MsgError (getProp "callID" jsv) e
               | Just i <- getProp "newCallID" jsv = MsgNewCallID i
               | Just r <- getProp "progress"  jsv = maybeUnknown jsv $ MsgProgress
                                                   <$> getProp "callID" jsv
                                                   <*> getProp "duration" jsv
                                                   <*> getProp "serviceName" jsv
                                                   <*> getProp "taskID" jsv
                                                   <*> Just r
                                                   <*> Just (getProp "intermediateResult" jsv)
               | Just i <- getProp "cancel"    jsv = MsgCancel i
               | Just p <- getProp "panic"     jsv = MsgPanic p
               | Just n <- getProp "run"       jsv = MsgRun n [] -- TODO: use .getOwnPropertyNames()
               | Just s <- getProp "wsSuccess" jsv   = MsgWebSocketState $ WsSuccess s
               | Just s <- getProp "wsError" jsv     = MsgWebSocketState $ WsError s
               | Just s <- getProp "wsTerminate" jsv = MsgWebSocketState $ WsTerminate s
               | otherwise = MsgUnknown jsv
    where
      maybeUnknown j Nothing  = MsgUnknown j
      maybeUnknown _ (Just v) = v
  asJSVal (MsgRun r props) = fromProps $ ("run", JS.asJSVal r):props
  asJSVal (MsgCancel callID) = setProp "callID" callID newObj
  asJSVal (MsgNewCallID newCallID) = setProp "newCallID" newCallID newObj
  asJSVal (MsgResult callID duration serviceName taskID result) =
          setProp "callID" callID
        . setProp "duration" duration . setProp "taskID" taskID
        . setProp "serviceName" serviceName $ setProp "result" result newObj
  asJSVal (MsgProgress callID duration serviceName taskID percentage result) =
          setProp "callID" callID
        . setProp "duration" duration . setProp "taskID" taskID . setProp "progress" percentage
        . setProp "serviceName" serviceName $ setProp "intermediateResult" result newObj
  asJSVal (MsgError _ err) = setProp "error" err newObj
  asJSVal (MsgPanic panic) = setProp "panic" panic newObj
  asJSVal (MsgUnknown j) = j
  asJSVal (MsgWebSocketState (WsSuccess s))   = setProp "wsSuccess" s newObj
  asJSVal (MsgWebSocketState (WsError s))     = setProp "wsError" s newObj
  asJSVal (MsgWebSocketState (WsTerminate s)) = setProp "wsTerminate" s newObj


data MessageAttachment = MessageAttachment
  { maFormat   :: !JSString
  , maLength   :: !Int
  , maMD5      :: !JSString
  , maPosition :: !Int
  } deriving (Show, Eq)
instance LikeJS "Object" MessageAttachment where
  asLikeJS jsv = MessageAttachment
    { maFormat   = fromMaybe "binary" $ getProp "format" jsv
    , maLength   = fromMaybe 0  $ att >>= getProp "length"
    , maMD5      = fromMaybe "" $ att >>= getProp "checksum"
    , maPosition = fromMaybe 0  $ att >>= getProp "position"
    } where att = getProp "attachment" jsv
  asJSVal MessageAttachment { .. } = fromProps
    [ ( "format", JS.asJSVal maFormat )
    , ( "attachment", fromProps
        [ ("length", JS.asJSVal maLength)
        , ("checksum", JS.asJSVal maMD5)
        , ("position", JS.asJSVal maPosition)
        ]
      )
    ]

makeAttDesc :: Int -> JSString -> JSTA.ArrayBuffer -> MessageAttachment
makeAttDesc pos tt ab = MessageAttachment
  { maFormat   = tt
  , maLength   = JSTA.byteLength ab
  , maMD5      = js_md5 ab
  , maPosition = pos
  }

foreign import javascript unsafe "var cr = new goog.crypt.Md5(); cr.update(new Uint8Array($1)); $r = cr.digest().reduce(function(s,e){return s + ('0' + e.toString(16)).slice(-2);}, '');"
    js_md5 :: JSTA.ArrayBuffer -> JSString

----------------------------------------------------------------------------------------------------
-- * Pre-defined messages
----------------------------------------------------------------------------------------------------

--runHelper :: JS.LikeJS s a => ServiceName -> [(JSString, JSVal)] -> ServiceInvocation -> MomentIO (Event (Either JSString a))
--runHelper sname pams run = filterJust . fmap f <$> run sname pams []
--  where
--    f (SRResult _ (ServiceResult res) _) = Just . Right $ JS.asLikeJS res
--    f SRProgress{} = Nothing
--    f (SRError _ s) = Just $ Left s

-- | A message to get list of available services from luci
runServiceList :: ServiceInvocation -> MomentIO (Event (ServiceResponse LuciResultServiceList))
runServiceList run = fmap (fmap $ JS.asLikeJS . srVal) <$> run "ServiceList" [] []

-- | A message to get list of available services from luci;
--   list only qua-view-compliant services
runQuaServiceList :: ServiceInvocation -> MomentIO (Event (ServiceResponse LuciResultServiceList))
runQuaServiceList run = fmap (fmap $ JS.asLikeJS . srVal) <$> run "FilterServices"
    [ ("rcrLevel", JS.asJSVal (1::Int))
    , ("keys", JS.asJSVal ["qua-view-compliant"::JSString])
    ] []


newtype LuciResultServiceList = ServiceList (JS.Array JSString)
  deriving (Show)
instance LikeJS "Object" LuciResultServiceList where
  asLikeJS b = case getProp "serviceNames" b of
                 Just x  -> ServiceList x
                 Nothing -> ServiceList JS.emptyArray
  asJSVal (ServiceList v) = setProp "serviceNames" v newObj

---- | run a testing service test.Fibonacci
--runTestFibonacci :: Int -> LuciMessage
--runTestFibonacci n = toLuciMessage (MsgRun "test.Fibonacci" [("amount", JS.asJSVal n)]) []

--newtype LuciResultTestFibonacci = TestFibonacci [Int]
--  deriving (Show, Eq)
--instance LikeJS "Object" LuciResultTestFibonacci where
--  asLikeJS b = case getProp "fibonacci_sequence" b of
--                 Just x  -> TestFibonacci $ JS.asLikeJS x
--                 Nothing -> TestFibonacci []
--  asJSVal (TestFibonacci xs) = setProp "fibonacci_sequence" xs newObj



-- | Luci scenario
data LuciScenario = LuciResultScenario ScenarioId FeatureCollection UTCTime
instance LikeJS "Object" LuciScenario where
  asLikeJS jsv = case (,) <$> getProp "ScID" jsv <*> getProp "FeatureCollection" jsv of
                  Just (scId, fc) -> LuciResultScenario scId fc t
                  Nothing -> anotherTry
     where
       t = posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0 $ getProp "lastmodified" jsv
       anotherTry = LuciResultScenario (fromMaybe 0 $ getProp "ScID" jsv)
              (fromMaybe (JS.fromJSArray JS.emptyArray) (getProp "geometry_output" jsv >>= getProp "geometry")) t
  asJSVal (LuciResultScenario scId fc _) =
            setProp "ScID"  (JS.asJSVal scId)
          $ setProp "FeatureCollection" fc newObj

-- | Luci scenario
data LuciScenarioCreated = LuciResultScenarioCreated ScenarioId UTCTime
instance LikeJS "Object" LuciScenarioCreated where
  asLikeJS jsv = LuciResultScenarioCreated (fromMaybe 0 $ getProp "ScID" jsv)
                                     (posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0 $ getProp "lastmodified" jsv)
  asJSVal (LuciResultScenarioCreated scId lm) =
            setProp "ScID"  (JS.asJSVal scId)
          $ setProp "lastmodified" (round $ utcTimeToPOSIXSeconds lm :: Int) newObj

-- | Pass the name of the scenario and a feature collection with geometry
runScenarioCreate :: ServiceInvocation
                  -> ScenarioName -- ^ name of the scenario
                  -> FeatureCollection -- ^ content of the scenario
                  -> MomentIO (Event (ServiceResponse LuciScenarioCreated))
runScenarioCreate run name collection = fmap (fmap $ JS.asLikeJS . srVal) <$> run  "scenario.geojson.Create"
      [ ("name", JS.asJSVal name)
      , ("geometry_input"
        ,   setProp "format"  ("GeoJSON" :: JSString)
          $ setProp "geometry" collection newObj
        )
      ] []
-- returns: "{"created":1470932237,"lastmodified":1470932237,"name":"dgdsfg","ScID":4}"


runScenarioUpdate :: ServiceInvocation
                  -> ScenarioId -- ^ id of the scenario
                  -> FeatureCollection -- ^ content of the scenario update
                  -> MomentIO (Event (ServiceResponse JSVal))
runScenarioUpdate run scId collection = fmap (fmap $ JS.asLikeJS . srVal) <$> run
      "scenario.geojson.Update"
      [ ("ScID", JS.asJSVal scId)
      , ("geometry_input"
        ,   setProp "format"  ("GeoJSON" :: JSString)
          $ setProp "geometry" collection newObj
        )
      ] []


runScenarioGet :: ServiceInvocation
               -> ScenarioId -- ^ id of the scenario
               -> MomentIO (Event (ServiceResponse LuciScenario))
runScenarioGet run scId = fmap (fmap $ JS.asLikeJS . srVal) <$> run
      "scenario.geojson.Get"
      [ ("ScID", JS.asJSVal scId)
      ] []

-- returns: "{"lastmodified":1470932237,"ScID":4}"

runScenarioSubscribe :: ServiceInvocation
                     -> ScenarioId -- ^ id of the scenario
                     -> MomentIO (Event (ServiceResponse LuciScenario))
runScenarioSubscribe run scId = fmap (fmap $ JS.asLikeJS . srVal) <$> run
      "scenario.SubscribeTo"
      [ ("ScIDs", JS.asJSVal [scId])
      , ("format", JS.asJSVal ("geojson" :: JSString))
      ] []


runScenarioList :: ServiceInvocation -> MomentIO (Event (ServiceResponse ServiceResult))
runScenarioList run = fmap (fmap $ JS.asLikeJS . srVal) <$> run "scenario.GetList" [] []


newtype LuciResultScenarioList = ScenarioList [ScenarioDescription]
  deriving (Show)
instance LikeJS "Object" LuciResultScenarioList where
  asLikeJS b = case getProp "scenarios" b of
                 Just x  -> ScenarioList x
                 Nothing -> ScenarioList []
  asJSVal (ScenarioList v) = setProp "scenarios" v newObj


data ScenarioDescription = ScenarioDescription
  { scCreated  :: UTCTime
  , scModified :: UTCTime
  , scName     :: ScenarioName
  , sscId      :: ScenarioId
  }
  deriving (Eq,Ord,Show)
instance LikeJS "Object" ScenarioDescription where
  asLikeJS jsv = ScenarioDescription
    { scCreated  = f $ getProp "created" jsv
    , scModified = f $ getProp "lastmodified" jsv
    , scName     = fromMaybe "" $ getProp "name" jsv
    , sscId      = fromMaybe (-1) $ getProp "ScID" jsv
    }
      where
        f = posixSecondsToUTCTime . realToFrac . secondsToDiffTime . fromMaybe 0
  asJSVal scd =
          setProp "ScID" (sscId scd) . setProp "name" (scName scd)
        . setProp "lastmodified" (f $ scModified scd :: Int) $ setProp "created" (f $ scCreated scd :: Int) newObj
      where
        f = round . utcTimeToPOSIXSeconds


----------------------------------------------------------------------------------------------------
-- * Qua-server integration
----------------------------------------------------------------------------------------------------

displayScenarios ::  ServiceResult -> IO ()
displayScenarios = GUI.displayScenarios . asJSVal

