-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Settings
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Program.Settings where

import JsHs.WebGL
--import Data.Geometry
--
--import Services
--import qualified Services.Isovist as Services
--import qualified Services.Radius as Services


data Settings = Settings
    { -- activeService     :: !ServiceBox
     --, availableServices :: ![ServiceBox]
     objectScale       :: !(Maybe GLfloat)
    }

defaultSettings :: Settings
defaultSettings = Settings
        { -- activeService = isovistService -- radService
        -- , availableServices = [radService, isovistService]
        -- ,
        objectScale = Nothing
        } --where radService = ServiceBox . Services.Radius $ vector3 0 3 5
        --        isovistService = ServiceBox (Services.Isovist Services.Area)

