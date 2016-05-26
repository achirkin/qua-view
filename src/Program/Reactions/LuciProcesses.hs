{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.LuciProcesses
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Program.Reactions.LuciProcesses () where

import Prelude hiding (unwords)

import GHCJS.Useful
import JsHs.JSString (append)

import Reactive
import Controllers.GUIEvents
import Controllers.LuciClient

import Program
import System.IO.Unsafe (unsafeInterleaveIO)

import Control.Concurrent
--import Control.Concurrent.MVar

--import JsHs.Debug

instance Reaction Program PView LuciConnect "Connecting to Luci" 1 where
    response _ _ LuciConnect{..} _ pview = do
      mmlc <- newEmptyMVar
      _ <- forkIO $ do
        finishEProcess <- logExternalProcess "Connecting to Luci..."
        elc <- connectToLuci cHost
        r <- case elc of
            Left err -> logText' err >> return Nothing
            Right lc -> do
                logText' $ "Connected to Luci on " `append` cHost
--                ans <- authenticate lc cUser cPass
                getElementById "ipaddressinfo" >>= flip setElementInnerHTML (hostOf lc)
--                logText $ show ans
                getElementById "loginform" >>= hideElement
                getElementById "logondiv" >>= showElement
                logText "Getting list of services"
                getServicesList lc >>= \r -> case r of
                          Right l -> do
                            logText' $ "List of services in LUCI: "
                            mapM_ logText' l
                          Left e -> logText' e
                logText "Getting info about service test.Fibonacci"
                getServiceInfo lc "test.Fibonacci" >>= \r -> case r of
                                                          Right l -> logText $ "Info about test.Fibonacci: "  ++ show l
                                                          Left e -> logText' e
                return $ Just lc
        finishEProcess
        putMVar mmlc r
      mlc <- unsafeInterleaveIO $ readMVar mmlc
      return pview{luciClient = mlc}

