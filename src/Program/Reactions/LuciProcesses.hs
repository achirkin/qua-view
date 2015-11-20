{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions.LuciProcesses
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
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
import Data.JSString (append, unwords)

import Reactive
import Controllers.GUIEvents
import Controllers.LuciClient

import Program

instance Reaction Program PView LuciConnect "Connecting to Luci" 1 where
    response _ _ LuciConnect{..} _ pview = do
        programInProgress
        elc <- connectToLuci cHost
        mlc <- case elc of
            Left err -> logText' err >> return Nothing
            Right lc -> do
                logText' $ "Connected to Luci on " `append` cHost
                ans <- authenticate lc cUser cPass
                getElementById "ipaddressinfo" >>= flip setElementInnerHTML (hostOf lc)
                logText $ show ans
                getElementById "loginform" >>= hideElement
                getElementById "logondiv" >>= showElement
                logText "Getting list of services"
                fmap (fmap unwords) (getServicesList lc) >>= \r -> case r of
                                                                    Right l -> logText' $ "List of services in LUCI: " `append` l
                                                                    Left e -> logText' e
                logText "Getting info about service fibonacci"
                getServiceInfo lc "fibonacci" >>= \r -> case r of
                                                          Right l -> logText $ "Info about fibonacci: "  ++ show l
                                                          Left e -> logText' e
                return $ Just lc
        programIdle
        return pview{luciClient = mlc}

