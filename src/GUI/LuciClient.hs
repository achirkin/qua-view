{-# LANGUAGE JavaScriptFFI #-}
-----------------------------------------------------------------------------
--
-- Module      :  GUI.LuciClient
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module GUI.LuciClient where

import GHCJS.Foreign
--import GHCJS.Marshal
import GHCJS.Types

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)


type LuciClient = JSRef LuciClient_
data LuciClient_


--var lc = new LuciClient("localhost", 8080, "ws/");
-- //#		lc.authenticate("lukas","1234", function(lc){console.log(lc.getMessage().result)})


foreign import javascript interruptible "var lc = new LuciClient($1, $2, $3, function(){$c(lc);});"
    connectToLucy' :: JSString -> Int -> JSString -> IO LuciClient

connectToLucy :: String -> IO (LuciClient, String)
connectToLucy address = liftM (flip (,) host) $ connectToLucy' (toJSString host) port (toJSString path)
    where splitOn _ [] = ([],[])
          splitOn c (x:xs) | c == x = ([], xs)
                           | otherwise = first (x:) $ splitOn c xs
          (addr, path) = splitOn '/' address
          (host, port') = splitOn ':' addr
          port = case filter (\x -> x >= '0' && x <= '9') port' of
            [] -> 80
            pp -> read pp


foreign import javascript interruptible "$1.authenticate($2,$3, function(){$c($1.getMessage().result);});"
    authenticate' :: LuciClient -> JSString -> JSString -> IO JSString


authenticate :: LuciClient -> String -> String -> IO String
authenticate lc login pass = liftM fromJSString $ authenticate' lc  (toJSString login) (toJSString pass)




foreign import javascript interruptible "document.getElementById('loadingSplash').style.display = 'block';$c();"
    programInProgress' :: IO ()

programInProgress :: IO ()
programInProgress = programInProgress' >> threadDelay 0

foreign import javascript unsafe "document.getElementById('loadingSplash').style.display = 'none';"
    programIdle :: IO ()
