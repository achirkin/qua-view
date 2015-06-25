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

module GUI.LuciClient
    ( LuciClient (), hostOf, portOf, localPathOf, connectionString
    , connectToLuci
    , authenticate
    , programInProgress
    , programIdle
    ) where

import GHCJS.Foreign
--import GHCJS.Marshal
import GHCJS.Types

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)


-- | Object for Luci connection
type LuciClient = JSRef LuciClient_
data LuciClient_


-- | Full string passed into WebSocket constructor
connectionString :: LuciClient -> String
connectionString lc = "ws://" ++ hostOf lc ++ ":" ++ show (portOf lc) ++ "/" ++ localPathOf lc

-- | Local path of the connection on server
localPathOf :: LuciClient -> String
{-# NOINLINE localPathOf #-}
localPathOf lc = unsafePerformIO (liftM fromJSString $ localPathOf' lc)

foreign import javascript unsafe "$r = $1.localpath;"
    localPathOf' :: LuciClient -> IO JSString

-- | Port of the connection on server
portOf :: LuciClient -> Int
{-# NOINLINE portOf #-}
portOf lc = unsafePerformIO (portOf' lc)

foreign import javascript unsafe "$r = $1.port;"
    portOf' :: LuciClient -> IO Int

-- | Connection server
hostOf :: LuciClient -> String
{-# NOINLINE hostOf #-}
hostOf lc = unsafePerformIO (liftM fromJSString $ hostOf' lc)

foreign import javascript unsafe "$r = $1.host;"
    hostOf' :: LuciClient -> IO JSString


-- | Open WebSocket and connect to Luci
connectToLuci :: String -> IO LuciClient
connectToLuci address = connectToLuci' (toJSString host) port (toJSString path)
    where splitOn _ [] = ([],[])
          splitOn c (x:xs) | c == x = ([], xs)
                           | otherwise = first (x:) $ splitOn c xs
          (addr, path) = splitOn '/' address
          (host, port') = splitOn ':' addr
          port = case filter (\x -> x >= '0' && x <= '9') port' of
            [] -> 80
            pp -> read pp

foreign import javascript interruptible "var lc = new LuciClient($1, $2, $3, function(){$c(lc);});lc.host = $1;lc.port = $2;lc.localpath = $3;"
    connectToLuci' :: JSString -> Int -> JSString -> IO LuciClient


-- | Authenticate in Luci
authenticate :: LuciClient
             -> String -- ^ login
             -> String -- ^ password
             -> IO String
authenticate lc login pass = liftM fromJSString $ authenticate' lc  (toJSString login) (toJSString pass)

foreign import javascript interruptible "$1.authenticate($2,$3, function(){$c($1.getMessage().result);});"
    authenticate' :: LuciClient -> JSString -> JSString -> IO JSString



-- | Display loading splash
programInProgress :: IO ()
programInProgress = programInProgress' >> threadDelay 0

foreign import javascript interruptible "document.getElementById('loadingSplash').style.display = 'block';$c();"
    programInProgress' :: IO ()

-- | Hide loading splash
foreign import javascript unsafe "document.getElementById('loadingSplash').style.display = 'none';"
    programIdle :: IO ()
