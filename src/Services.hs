{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Services
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services where

import Controllers.LuciClient
import Program.Model.ScalarField


--data ServiceInput = GridInput ScalarField
--
--data ServiceOuput = GridOutput

data ServiceBox = forall s . ComputingService s => ServiceBox s


class (Show s) => ComputingService s where
    runService :: s -> Maybe LuciClient -> Maybe Scenario -> ScalarField -> IO (Maybe ScalarField)
