{-# LANGUAGE JavaScriptFFI #-}
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
    ) where

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import Control.Arrow (first)
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (Value)


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
connectToLuci :: String -> IO (Either String LuciClient)
connectToLuci address = connectToLuci' (toJSString host) port (toJSString path) >>= liftEitherRef
    where splitOn _ [] = ([],[])
          splitOn c (x:xs) | c == x = ([], xs)
                           | otherwise = first (x:) $ splitOn c xs
          (addr, path) = splitOn '/' address
          (host, port') = splitOn ':' addr
          port = case filter (\x -> x >= '0' && x <= '9') port' of
            [] -> 80
            pp -> read pp
foreign import javascript interruptible "try{var lc = new LuciClient($1, $2, $3, function(){$c({right: lc});} \
    \ , function(ev){$c({left: 'Closed with code ' + ev.code});} \
    \ , function(){logText(document.getElementById('consolecontent'),'Error occured on the WebSocket side.');} \
    \ ); \
    \ lc.host = $1;lc.port = $2;lc.localpath = $3;}catch(err){$c({left: 'Error occured while trying to create Luci client: ' + JSON.stringify(err)});}"
    connectToLuci' :: JSString -> Int -> JSString -> IO (JSRef (Either String LuciClient_))


-- | Authenticate in Luci
authenticate :: LuciClient
             -> String -- ^ login
             -> String -- ^ password
             -> IO (Either String String)
authenticate lc login pass = liftM eitherJustOrError $ authenticate' lc  (toJSString login) (toJSString pass) >>= fromJSRef
foreign import javascript interruptible "$1.authenticate($2,$3, function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result']) {$c({right: m['result']});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither result message nor an error. MSG: \" + JSON.stringify(m)});}});"
    authenticate' :: LuciClient -> JSString -> JSString -> IO (JSRef (Either String String))


-- | Get list of names of available services in Luci
getServicesList :: LuciClient -> IO (Either String [String])
getServicesList lc = liftM eitherJustOrError $ getServicesList' lc >>= fromJSRef
foreign import javascript interruptible "var req = {}; req['action'] = 'get_list'; req['of'] = ['services']; $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result'] && m['result']['services']) {$c({right: m['result']['services']});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither services list nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    getServicesList' :: LuciClient -> IO (JSRef (Either String [String]))


getServiceInfo :: LuciClient -> String -> IO (Either String Value)
getServiceInfo lc sname = liftM eitherJustOrError $ getServiceInfo' lc (toJSString sname) >>= fromJSRef
foreign import javascript interruptible "var req = {}; req['action'] = 'get_infos_about'; req['servicename'] = $2; $1.sendAndReceive(req,[function(){ \
    \ var m = $1.getMessage(); \
    \ if(m['result'] && m['result'][$2]) {$c({right: m['result'][$2]});} \
    \ else if(m['error']) {$c({left: 'Luci says: ' + m['error']});} \
    \ else {$c({left: \"Message contains neither service info nor an error. MSG: \" + JSON.stringify(m)});}}]);"
    getServiceInfo' :: LuciClient -> JSString -> IO (JSRef (Either String Value))




--- EITHER instance


foreign import javascript unsafe "if($1.right){$r = $1.right;} else {$r = null;}"
    rightOrNullRef :: JSRef (Either a b) -> IO (JSRef b)

foreign import javascript unsafe "if($1.left){$r = $1.left;} else {$r = null;}"
    leftOrNullRef :: JSRef (Either a b) -> IO (JSRef a)


instance (FromJSRef a, FromJSRef b) => FromJSRef (Either a b) where
    fromJSRef ref = do
        mrr <- rightOrNullRef ref
        if isNull mrr
        then leftR
        else do
            mr <- fromJSRef $ mrr
            case mr of
                Nothing -> leftR
                Just r -> return . Just $ Right r
        where leftR = leftOrNullRef ref >>= liftM (liftM Left) . fromJSRef

liftEitherRef :: JSRef (Either String b) -> IO (Either String (JSRef b))
liftEitherRef ref = do
    mrr <- rightOrNullRef ref
    if isNull mrr
    then leftOrNullRef ref >>= liftM (eitherJustOrError . liftM Left) . fromJSRef
    else return $ Right mrr


eitherJustOrError :: Maybe (Either String a) -> Either String a
eitherJustOrError Nothing = Left "Could not read javascript object."
eitherJustOrError (Just x) = x
