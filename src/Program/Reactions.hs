{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Reactions
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Import all modules containing Reaction instances to create EvenSenses
--
-----------------------------------------------------------------------------

module Program.Reactions () where


import Reactive (createAllEventSenses)


import Program.Reactions.ViewRendering ()
import Program.Reactions.CameraBehavior ()
import Program.Reactions.SelectorRendering ()
import Program.Reactions.CitySelection ()
import Program.Reactions.ServiceRun ()
import Program.Reactions.ServiceFinish ()
import Program.Reactions.GeometryChanges ()
import Program.Reactions.LuciProcesses ()
import Program.Reactions.ViewSubmitPopup ()

-- this TH function generates instances of EventSense for all reactions available from this module
$(createAllEventSenses)
