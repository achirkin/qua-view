{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface,  JavaScriptFFI, GHCForeignImportPrim, UnliftedFFITypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
      LuciClient, luciHandler, connectionString
    , LCState (..)
    , lcState, lcCalls
    , LuciSession, lsServiceName, lsHandler
      -- * Core message types
    , LuciMessage (..) -- , sendMessage
    , msgHeaderValue, toLuciMessage
    , MessageHeader (..), getMsgCallId
    , ServiceResult (..)
    , ServiceName (..), unServiceName
      -- * GUI effects
    , GUI.registerAskLuciForScenario, displayScenarios, GUI.registerGetScenarioList
      -- * message parts
    , MessageAttachment (..), makeAttDesc
    , CallId (..), TaskId (..)
    , ServiceResponse (..), catResponses
    , runService
    , LuciResultServiceList (..), runQuaServiceList
    , responseCallId
    ) where



import qualified Program.Controllers.GUI as GUI
--import Data.String (IsString)
import JsHs.Useful

import JsHs
import JsHs.Types.Prim (jsNull)
import JsHs.JSString (unpack')
--import Data.Geometry.Structure.Feature (FeatureCollection)
import qualified JsHs.Array as JS
import qualified JsHs.TypedArray as JSTA
import qualified JsHs.Callback as JS (Callback, asyncCallback2, asyncCallback1, asyncCallback)

--import Control.Arrow (first)
import Program.Settings
import Program.Types
import Reactive.Banana.Frameworks
import Reactive.Banana.Combinators
import Reactive.Banana.JsHs.Types (Time)
import Control.Monad ((>=>))
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Monoid ((<>))
import Data.Default.Class


import qualified Data.Map.Strict as Map


--import JsHs.Debug

----------------------------------------------------------------------------------------------------
-- * Data types
----------------------------------------------------------------------------------------------------

-- | Luci user session state: which service is called and what to do on message received
data LuciSession = LuciSession
  { _lsServiceName :: !ServiceName
    -- ^ Which Luci service was called
  , _lsHandler     :: !(Handler (ServiceResponse ServiceResult))
    -- ^ What to do when a message has been received
  }

-- | Object for Luci connection
data LuciClient = LuciClient
  { _lcState :: !LCState
    -- ^ state of Luci connection
  , _lcRef   :: !JSVal
    -- ^ reference to JS luci object
  , _lcCalls :: !(Map.Map CallId LuciSession)
    -- ^ map of current sessions
  , _lcInvoker :: !ServiceInvocation
  }

--instance Default LuciClient where
--  def = LuciClient def jsNull Map.empty noInvocation

-- | LuciClient State - websockets and communication status
data LCState
  = LCSOpen
    -- ^ Live connection
  | LCSOpening
    -- ^ opening connection
  | LCSClosed
    -- ^ websocket connection closed
  | LCSError JSString
    -- ^ error occured
  deriving Eq

instance Default LCState where
  def = LCSOpen


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


type ServiceInvocation = ServiceName -> [(JSString, JSVal)] -> [JSTA.ArrayBuffer] -> MomentIO (Event (ServiceResponse ServiceResult))
--type ServiceEvent = Event (ServiceResponse ServiceResult)

noInvocation :: ServiceInvocation
noInvocation (ServiceName sname) _ _ = do
  liftIO . logText' $ "Failed to call " <> sname <> ": luci session map is not ready."
  return never


----------------------------------------------------------------------------------------------------
-- * Java Script
----------------------------------------------------------------------------------------------------

--foreign import javascript unsafe "($1 && $1.objectName == 'LuciClient') \
--                                 \ ? $1 : null" js_Luci :: JSVal -> JSVal

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
              -> IO (Either JSString JSVal)
newLuciClient connStr onMsgCall onOpenCall onCloseCall onErrorCall = do
  jsOnMessage <- JS.asyncCallback2 (\h d -> onMsgCall $ LuciMessage (asLikeJS h) (asLikeJS d))
  jsOnOpen <- JS.asyncCallback onOpenCall
  jsOnClose <- JS.asyncCallback onCloseCall
  jsOnError <- JS.asyncCallback1 (onErrorCall . asLikeJS)
  JS.asLikeJS <$> js_newLuciClient connStr jsOnMessage jsOnOpen jsOnClose jsOnError

-- | Full string passed into WebSocket constructor
connectionString :: LuciClient -> JSString
connectionString (LuciClient LCSOpen c _ _) = js_connectionString c
connectionString (LuciClient LCSOpening c _ _) = js_connectionString c
connectionString _ = ""
foreign import javascript unsafe "$1.connectionString"
  js_connectionString :: JSVal -> JSString

-- | Send Luci message
sendMessage :: LuciClient -> LuciMessage -> IO ()
sendMessage (LuciClient LCSOpen luci _ _) (LuciMessage h a) = js_sendMessage luci h a
sendMessage _ _ = logText' "Tried to send a message while being not in ready state."

foreign import javascript unsafe "$1.sendMessage($2,$3)"
  js_sendMessage :: JSVal -> JSString -> JS.Array JSTA.ArrayBuffer -> IO ()

-- | Generate a new callId in sequential manner
genCallId :: LuciClient -> IO CallId
genCallId (LuciClient LCSOpen c _ _) = js_genCallId c
genCallId _ = return $ -1

foreign import javascript unsafe "$1.genCallId()"
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

instance Show a => Show (ServiceResponse a) where
  show (SRResult cid a _)   = "Result message [" ++ show cid ++ "]: " ++ show a
  show (SRProgress cid p a _) = "Progress message (" ++ show p ++ ") [" ++ show cid ++ "]: " ++ show a
  show (SRError cid e) = "Error message [" ++ show cid ++ "]: " ++ show e


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
-- * Qua-server integration
----------------------------------------------------------------------------------------------------

displayScenarios ::  ServiceResult -> IO ()
displayScenarios = GUI.displayScenarios . asJSVal




----------------------------------------------------------------------------------------------------
-- * Lenses
----------------------------------------------------------------------------------------------------

--makeLenses ''LuciSession
--makeLenses ''LuciClient


lsServiceName :: Lens' LuciSession ServiceName
lsServiceName m t = (\x -> t{_lsServiceName = x }) <$> m (_lsServiceName t)

lsHandler :: Lens' LuciSession (Handler (ServiceResponse ServiceResult))
lsHandler m t = (\x -> t{_lsHandler = x }) <$> m (_lsHandler t)


lcState :: Lens' LuciClient LCState
lcState m t = (\x -> t{_lcState = x }) <$> m (_lcState t)

--lcRef :: Lens' LuciClient JSVal
--lcRef m t = (\x -> t{_lcRef = x }) <$> m (_lcRef t)

lcCalls :: Lens' LuciClient (Map.Map CallId LuciSession)
lcCalls m t = (\x -> t{_lcCalls = x }) <$> m (_lcCalls t)

----------------------------------------------------------------------------------------------------
-- * Reactive-banana
----------------------------------------------------------------------------------------------------


-- | Run arbitrary Luci service and get session callID
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

-- | Run luci service with parameters
runService :: JS.LikeJS s a
           => Behavior LuciClient
           -> Event (ServiceName, [(JSString, JSVal)], [JSTA.ArrayBuffer])
           -> MomentIO (Event (ServiceResponse a))
runService lcB callE = fmap (fmap . fmap $ JS.asLikeJS . srVal) $ execute ev >>= switchE
  where
    ev = (\luci (sname, pams, atts) -> _lcInvoker luci sname pams atts) <$> lcB <@> callE



-- | React on all messages related to started sessions and ignore (print to console) unrelated messages.
parseLuciMessages :: Behavior JSVal
                  -> Event (LCState -> LCState)
                  -> Handler ServiceInvocation
                     -- ^ A handler that allows to start a session and wait for related responses in a form of events
                  -> Event (MessageHeader, JS.Array JSTA.ArrayBuffer)
                  -> Handler (MessageHeader, JS.Array JSTA.ArrayBuffer)
                  -> Handler (ServiceResponse ServiceResult)
                  -> MomentIO (Behavior (Map.Map CallId LuciSession))
parseLuciMessages lcRefB lstateE invocationH incomings unknownMsgFire noCallIdMsgFire = do
    callMap <- liftIO $ newIORef Map.empty
    -- replace invoker on each LCSOpen event
    reactimate $ invocationH . invoke callMap <$> luciNoCallsE
    -- react on unparsed messages
    reactimate $ unknownMsgFire <$> otherMsgs
    -- react on good messages
    reactimate $ (parseResponses callMap >=> modifyIORef' callMap) <$> responsesE
    fromPoll $ readIORef callMap
  where
    luciNoCallsE = filterJust $ overwriteState <$> lcRefB <@> lstateE
    overwriteState l f = case f LCSOpening of
          LCSOpen -> Just $ LuciClient LCSOpen l Map.empty noInvocation
          _       -> Nothing
    (otherMsgs, responsesE) = split $ toServiceResponse <$> incomings
    parseResponses :: IORef (Map.Map CallId LuciSession) -> ServiceResponse ServiceResult -> IO (Map.Map CallId LuciSession -> Map.Map CallId LuciSession)
    parseResponses mr r@(SRResult callId _ _) = readIORef mr >>= \m -> case Map.lookup callId m of
          Nothing -> id <$ noCallIdMsgFire r
          Just a  -> Map.delete callId <$ _lsHandler a r
    parseResponses mr r@(SRError callId _   ) = readIORef mr >>= \m -> case Map.lookup callId m of
          Nothing -> id <$ noCallIdMsgFire r
          Just a  -> Map.delete callId <$ _lsHandler a r
    parseResponses mr r@(SRProgress callId _ _ _) = readIORef mr >>= \m -> case Map.lookup callId m of
          Nothing -> id <$ noCallIdMsgFire r
          Just a  -> id <$ _lsHandler a r
    invoke :: IORef (Map.Map CallId LuciSession) -> LuciClient -> ServiceInvocation
    invoke callMap luci sname pams atts = do
      ah <- liftIO $ do
        newCallId <- runLuciService luci sname pams atts
        (addHandler, fire) <- newAddHandler
        modifyIORef' callMap (Map.insert newCallId (LuciSession sname fire))
        return addHandler
      fromAddHandler ah


-- | Create LuciClient and register events on message receive
luciHandler :: JSString -> MomentIO
                            ( Behavior LuciClient
                            , Event (ServiceResponse ServiceResult)
                            , Event (MessageHeader, JS.Array JSTA.ArrayBuffer)
                            , Event LCState
                            )
luciHandler str = do
    -- create all handlers
    (askConnectH, (onMessageH, onMessageFire)
                , (onOpeningH, onOpeningFire)
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
    -- state of luci JS reference
    (luciRefE, luciRefFire) <- newEvent
    -- when user clicked on connect button
    askConnectE <- fromAddHandler askConnectH
    lastUrlB <- stepper str askConnectE
    -- create Luci Client
    let openingF LCSClosed    = LCSOpening
        openingF LCSOpening   = LCSOpening
        openingF (LCSError e) = LCSError e
        openingF LCSOpen      = LCSError "Tried to open connection when it was already open."
    eErrorLcRefE' <- flip mapEventIO askConnectE $ \url -> newLuciClient url onMessageFire
                            (onOpeningFire openingF)
                            (onCloseFire $ const LCSClosed)
                            (onErrorFire . const . LCSError)
    -- all luci messages in an Event
    luciMsgsE' <- fromAddHandler onMessageH

    -- When user has asked for connecting, we need to check if it is not in an error,
    --  and fire the error if something is wrong
    luciOpeningE2 <- fmap filterJust
        . flip mapEventIO eErrorLcRefE'
        $ \elc -> case elc of
      Left e -> do
        onErrorFire (const $ LCSError e)
        return Nothing
      Right r -> do
        luciRefFire r
        return . Just $ fromAddHandler onOpeningH
    luciOpeningE <- execute luciOpeningE2 >>= switchE
    luciCloseE   <- fromAddHandler onCloseH
    luciErrorE   <- fromAddHandler onErrorH

    -- Catch websockets messages before they go to client
    --  in order to track qua-server <-> luci/helen connection
    let eitherWsOrLuci (MsgWebSocketState e, _) = Left e
        eitherWsOrLuci e = Right e
        (wsMsgs, luciMsgs) = split $ (\m -> eitherWsOrLuci (msgHeaderValue m, attachments m)) <$> luciMsgsE'
        wsChangeConn (WsSuccess _)    LCSOpening   = LCSOpen
        wsChangeConn (WsSuccess _)    s            = s
        wsChangeConn (WsError e)     (LCSError e0) = LCSError (e0 <> "\n" <> e)
        wsChangeConn (WsError e)      _            = LCSError e
        wsChangeConn (WsTerminate _) (LCSError e0) = LCSError e0
        wsChangeConn (WsTerminate _)  _            = LCSClosed
        wsChangeE = wsChangeConn <$> wsMsgs
        wsIsSuccess (WsSuccess _) = True
        wsIsSuccess _ = False
        luciOpenE = filterE wsIsSuccess wsMsgs

    -- All client-state-changing events
    let luciStateTE = unions [luciErrorE, luciCloseE, luciOpeningE, wsChangeE] :: Event (LCState -> LCState)
    luciStateE <- accumE LCSClosed luciStateTE
    luciStateB <- stepper LCSClosed luciStateE
    luciRefB <- stepper jsNull luciRefE
    (luciInvokerE, luciInvokerFire) <- newEvent
    luciInvokerB <- stepper noInvocation luciInvokerE


    (unknownMsgE, unknownMsgFire) <- newEvent
    (noCallIdMsgE, noCallIdFire) <- newEvent
    -- Now we need to parse luci messages and change session map according to them
    luciCallsB <- parseLuciMessages luciRefB wsChangeE luciInvokerFire luciMsgs unknownMsgFire noCallIdFire

    -- Combine all behaviors and we are done!
    let luciClientB = LuciClient <$> luciStateB <*> luciRefB <*> luciCallsB <*> luciInvokerB

    -- Viewer behavior
    reactimate $ GUI.showLuciConnectForm <$> lastUrlB <@ luciCloseE
    reactimate $ GUI.showLuciConnectForm <$> lastUrlB <@ luciErrorE
    reactimate $ GUI.showLuciConnecting  <$> lastUrlB <@ luciOpeningE
    reactimate $ GUI.showLuciConnected   <$> lastUrlB <@ luciOpenE

    reactimate $ logText' "Opening connection." <$ luciOpeningE
    reactimate $ logText' "LuciClient WebSocket connection closed." <$ luciCloseE
    reactimate $ logText' "Some error occurred." <$ luciErrorE
    reactimate $ logText' "Luci is ready." <$ luciOpenE

    return (luciClientB,noCallIdMsgE,unknownMsgE, luciStateE)



----------------------------------------------------------------------------------------------------
-- * Pre-defined messages
----------------------------------------------------------------------------------------------------


-- | A message to get list of available services from luci;
--   list only qua-view-compliant services
runQuaServiceList :: Behavior LuciClient -> Event () -> MomentIO (Event (ServiceResponse LuciResultServiceList))
runQuaServiceList lcB e = runService lcB $
    ( "FilterServices"
    , [ ("rcrLevel", JS.asJSVal (1::Int))
      , ("keys", JS.asJSVal ["qua-view-compliant"::JSString])
      ]
    , []
    ) <$ e


newtype LuciResultServiceList = ServiceList (JS.Array JSString)
  deriving (Show)
instance LikeJS "Object" LuciResultServiceList where
  asLikeJS b = case getProp "serviceNames" b of
                 Just x  -> ServiceList x
                 Nothing -> ServiceList JS.emptyArray
  asJSVal (ServiceList v) = setProp "serviceNames" v newObj
