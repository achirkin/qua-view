-- {-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.GUIEvents
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.GUIEvents where

import Data.Geospatial
import qualified Data.Aeson as A
--import GHCJS.Types

-- | Reaction on this event should be starting evaluation of current service
data ServiceRunBegin = ServiceRunBegin

-- | This indicates removal of all geometry from the city
data ClearingGeometry = ClearingGeometry

data ClearServiceResults = ClearServiceResults

-- | When valid GeoJSON comes from somewhere
data GeoJSONLoaded = GeoJSONLoaded
    { isDynamic          :: Bool
    , featureCollection  :: GeoFeatureCollection A.Value
    }
type GeoJSONLoadCallBack = GeoJSONLoaded -> IO ()

-- | When trying to connect to Luci
data LuciConnect = LuciConnect
    { cHost :: String
    , cUser :: String
    , cPass :: String
    }



type SubmitURL = String
-- | Get the geometry and its preview image
newtype SubmitScenario = SubmitScenario SubmitURL
