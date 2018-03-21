{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | Common definitions used only in worker executables.
--   Do not import it in qua-view!
module Workers.Commons
    ( getUrlSync
    , module Data.Conduit
    ) where


import Control.Monad.Except
import Commons.NoReflex
import Data.Conduit





-- | Execute GET request and wait for a result synchronously.
--   Try to catch errors on the way.
getUrlSync :: (MonadError JSError m, MonadIO m)
           => JSString -> m LoadedTextContent
getUrlSync url = do
    (mltc, merr, rs, st) <- liftIO $ js_getUrlSync url
    case (rs, st, nullableToMaybe mltc, nullableToMaybe merr) of
      (4, s, Just v, Nothing)
         | 200 <= s && s < 300     -> return v
         | otherwise               -> throwError . JSError $
                                     "Response error " <> showStatus 4 s <> ": " <> getTextContent v
      (r, 0,  _ , _)               -> throwError . JSError $
                                     "Could not send an http request " <> showStatus r 0 <>
                                     "); likely a CORS problem."
      (r, s, _, Just (JSError "")) -> throwError . JSError $ noInfo r s
      (r, s, _, Just (JSError e )) -> throwError . JSError $
                                     "Got an error " <> showStatus r s <>  ": " <> e
      (r, s, _, Nothing)           -> throwError . JSError $ noInfo r s
  where
    readyState :: Int -> JSString
    readyState 0 = "readyState = 0 (UNSENT)"
    readyState 1 = "readyState = 1 (OPENED)"
    readyState 2 = "readyState = 2 (HEADERS_RECEIVED)"
    readyState 3 = "readyState = 3 (LOADING)"
    readyState 4 = "readyState = 4 (DONE)"
    readyState n = toJSString (show n) <> " (unknown!)"
    responseStatus :: Int -> JSString
    responseStatus n = "responseStatus = " <> toJSString (show n)
    showStatus :: Int -> Int -> JSString
    showStatus rs st = "(url = " <> url <> ", " <> readyState rs <> ", " <> responseStatus st <> ")"
    noInfo :: Int -> Int -> JSString
    noInfo rs st = "Could not perform an http request, but also could not get error message "
                <> showStatus rs st <> "."


foreign import javascript interruptible
    "var xh = new XMLHttpRequest(); \
    \ xh.addEventListener(\"load\", function(){$c(xh['responseText'], null, xh['readyState'], xh['status']);}); \
    \ xh.addEventListener(\"error\", function(){$c(null, xh['statusText'], xh['readyState'], xh['status']);}); \
    \ xh.addEventListener(\"abort\", function(){$c(null, xh['statusText'], xh['readyState'], xh['status']);}); \
    \ xh.open( 'GET', $1, true ); \
    \ xh.setRequestHeader(\"Accept\", \"application/json,text/plain;q=0.9\"); \
    \ xh.send( ); "
    js_getUrlSync :: JSString -> IO (Nullable LoadedTextContent, Nullable JSError, Int, Int)
