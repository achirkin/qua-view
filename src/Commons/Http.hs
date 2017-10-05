{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Commons.Http
    ( httpGet
    , httpGetNow
    , httpPost
    ) where

import Foreign.JavaScript.TH
import GHCJS.DOM.Types hiding (Event, Text)
import JavaScript.JSON.Types.Internal as JSON
import JavaScript.JSON.Types.Instances
import Reflex.Dom

import Commons.Import
import Commons.Local
import Control.Monad (join)
import Data.JSString (pack)
import Data.JSString.Text (textFromJSString, textToJSString)
import GHCJS.DOM.JSFFI.XMLHttpRequest (sendString)


instance IsXhrPayload JSString where
  sendXhrPayload = sendString

-- | HTTP GET upon `Event a`
httpGet :: forall t b m
         . ( FromJSON b, TriggerEvent t m, PerformEvent t m
           , HasJSContext (Performable m), MonadIO (Performable m)
           , Reflex t )
        => Event t JSString -> m (Event t (Either JSError b))
httpGet event = do
  (resultE, resultTrigger) <- newTriggerEvent
  performEvent_ $ flip doHttp resultTrigger . getReqConfig <$> event
  return resultE

-- | HTTP POST `ToJSON a` upon `Event a`
httpPost :: forall t a b m
        . ( ToJSON a, FromJSON b, TriggerEvent t m, PerformEvent t m
          , HasJSContext (Performable m), MonadIO (Performable m)
          , Reflex t )
         => Event t (JSString, a) -> m (Event t (Either JSError b))
httpPost event = do
  (resultE, resultTrigger) <- newTriggerEvent
  performEvent_ $ flip doHttp resultTrigger . uncurry postJsonReqConfig <$> event
  return resultE

-- | HTTP GET immediately
httpGetNow :: forall t b m
            . ( FromJSON b, TriggerEvent t m
              , Reflex t, HasJSContext m, MonadJSM m )
           => JSString -> m (Event t (Either JSError b))
httpGetNow s = do
  (e, t) <- newTriggerEvent
  doHttp (getReqConfig s) t
  return e

-- | make HTTP request immediately
doHttp :: forall t a b m
        . ( FromJSON b, IsXhrPayload a
          , Reflex t, HasJSContext m, MonadJSM m )
       => XhrRequest a -> (Either JSError b -> IO ()) -> m ()
doHttp reqConfig cb = void $ newXMLHttpRequestWithError reqConfig cb'
   where
      cb' :: Either XhrException XhrResponse -> IO ()
      cb' = (>>= cb) . parseResp . handleErr
      parseResp =
        let parseJson (Just t) = parseJSONValue $ toJSString t
            parseJson Nothing  = return $ Left mempty
            go val = case fromJSON val of
                       JSON.Success v -> Right v
                       JSON.Error str -> Left $ JSError $ pack str
        in fmap ((>>= go) . join) . sequence
         . fmap (parseJson . _xhrResponse_responseText)
      handleErr (Right res) =
        let status = fromIntegral $ _xhrResponse_status res
            fromMay (Just err) = textToJSString err
            fromMay Nothing    = toJSString $ show status
        in  if status >= 200 && status < (300::Int)
            then Right res
            else Left $ JSError $ fromMay $ _xhrResponse_responseText res
      handleErr (Left XhrException_Error)   = Left "XHR Error"
      handleErr (Left XhrException_Aborted) = Left "XHR Aborted"

getReqConfig :: JSString -> XhrRequest ()
getReqConfig url = XhrRequest "GET" (textFromJSString url) def

-- | Create a "POST" request from an URL and thing with a JSON representation
--   based on Reflex.Dom.Xhr (postJson)
postJsonReqConfig :: (ToJSON a) => JSString -> a -> XhrRequest JSString
postJsonReqConfig url payload =
  XhrRequest "POST" (textFromJSString url) $ def {
                        _xhrRequestConfig_headers  = headerUrlEnc
                      , _xhrRequestConfig_sendData = body
                      }
  where headerUrlEnc = "Content-type" =: "application/json"
        body = encode $ toJSON payload
