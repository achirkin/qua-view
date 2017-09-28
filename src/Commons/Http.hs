{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import Data.JSString.Text (textFromJSString)


-- | HTTP GET upon `Event a`
httpGet :: forall t a b m
         . ( FromJSON b, TriggerEvent t (Performable m), PerformEvent t m
           , MonadIO (Performable m), HasJSContext (Performable m)
           , MonadHold t m, Reflex t )
        => Text -> (Event t a) -> m (Event t (Either JSError b))
httpGet url event = do
  nestedE <- performEvent $ (const $ doHttp $ XhrRequest "GET" url def) <$> event
  switchPromptly never nestedE

-- | HTTP POST `ToJSON a` upon `Event a`
httpPost :: forall t a b m
        . ( ToJSON a, FromJSON b, TriggerEvent t (Performable m), PerformEvent t m
          , MonadIO (Performable m), HasJSContext (Performable m)
          , MonadHold t m, Reflex t )
         => Text -> (Event t a) -> m (Event t (Either JSError b))
httpPost url event = do
  nestedE <- performEvent $ doHttp . postJsonReqConfig url <$> event
  switchPromptly never nestedE

-- | HTTP GET immediately
httpGetNow :: forall t b m
            . ( FromJSON b, TriggerEvent t m
              , Reflex t, HasJSContext m, MonadIO m )
           => Text -> m (Event t (Either JSError b))
httpGetNow url = do
  doHttp $ XhrRequest "GET" url def

-- | make HTTP request immediately
doHttp :: forall t a b m
        . ( FromJSON b, IsXhrPayload a, TriggerEvent t m
          , Reflex t, HasJSContext m, MonadIO m )
       => XhrRequest a -> m (Event t (Either JSError b))
doHttp reqConfig = do
  (resE, cb) <- newTriggerEvent
  let parseResp (Just t) = parseJSONValue $ toJSString t
      parseResp Nothing  = return $ Left mempty
      go val = case fromJSON val of
                 JSON.Success v -> Right v
                 JSON.Error str -> Left $ JSError $ pack str
      xhrEtoJSString XhrException_Error   = "XHR Error"
      xhrEtoJSString XhrException_Aborted = "XHR Aborted"
      cb' :: Either XhrException XhrResponse -> IO ()
      cb' = (>>= cb)
          . fmap ((>>= go) . join)
          . sequence
          . fmap (parseResp . _xhrResponse_responseText)
          . either (Left . xhrEtoJSString) Right
  _ <- newXMLHttpRequestWithError reqConfig cb'
  return resE

-- | Create a "POST" request from an URL and thing with a JSON representation
--   based on Reflex.Dom.Xhr (postJson)
postJsonReqConfig :: (ToJSON a) => Text -> a -> XhrRequest Text
postJsonReqConfig url payload =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = body
                              }
  where headerUrlEnc = "Content-type" =: "application/json"
        body = textFromJSString $ encode $ toJSON payload
