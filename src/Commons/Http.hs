{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Commons.Http
    ( httpPost
    ) where

import Foreign.JavaScript.TH
import GHCJS.DOM.Types hiding (Event, Text)
import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import Reflex.Dom.Xhr
import Reflex.PerformEvent.Class
import Reflex.TriggerEvent.Class

import Commons.Import
import Commons.Local
import Data.JSString.Text (textFromJSString)


-- | HTTP POST `ToJSON a` upon `Event a`
httpPost :: (MonadJSM (Performable m), HasJSContext (Performable m)
              , PerformEvent t m, TriggerEvent t m, ToJSON a, FromJSON b)
         => Text -> (Event t a) -> m (Event t (Maybe b))
httpPost url event = do
  resE  <- performRequestAsync $ fmap (postJsonReq url) event
  let goParse (Just t) = parseJSONValue $ toJSString t
      goParse Nothing = return $ Left mempty
  jsonE <- performEvent $ goParse . _xhrResponse_responseText <$> resE
  let go (Right val) = case fromJSON val of
                         Success v -> Just v
                         _         -> Nothing
      go (Left _)    = Nothing
  return $ fmap go jsonE

-- | Create a "POST" request from an URL and thing with a JSON representation
--   based on Reflex.Dom.Xhr (postJson)
postJsonReq :: (ToJSON a) => Text -> a -> XhrRequest Text --JSString
postJsonReq url a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                              , _xhrRequestConfig_sendData = body
                              }
  where headerUrlEnc = "Content-type" =: "application/json"
        body = textFromJSString $ encode $ toJSON a
