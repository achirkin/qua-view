{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
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

import GHCJS.Useful

import Reactive
import Controllers.GUIEvents
import Controllers.LuciClient

import Program

instance Reaction Program PView LuciConnect "Connecting to Luci" 1 where
--    response _ LuciConnect{..} _ _ pview = do
--        programInProgress
--        elc <- connectToLuci cHost
--        mlc <- case elc of
--            Left err -> logText err >> return Nothing
--            Right lc -> do
--                logText $ "Connected to Luci on " ++ cHost
--                ans <- authenticate lc cUser cPass
--                getElementById "ipaddressinfo" >>= flip setElementInnerHTML (hostOf lc)
--                logText $ show ans
--                getElementById "loginform" >>= hideElement
--                getElementById "logondiv" >>= showElement
--                logText "Getting list of services"
--                liftM (liftM unwords) (getServicesList lc) >>= logText . show
--                logText "Getting info about service fibonacci"
--                getServiceInfo lc "fibonacci" >>= logText . show
--                return $ Just lc
--        programIdle
--        return $ Left pview{luciClient = mlc}

