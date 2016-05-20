-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.GUIEvents
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Controllers.GUIEvents where

import GHCJS.Types
import Data.Geometry.Structure.Feature (FeatureCollection)

-- | Reaction on this event should be starting evaluation of current service
data ServiceRunBegin = ServiceRunBegin

-- | This indicates removal of all geometry from the city
data ClearingGeometry = ClearingGeometry

data ClearServiceResults = ClearServiceResults

-- | When valid GeoJSON comes from somewhere
data GeoJSONLoaded = GeoJSONLoaded
    { isDynamic         :: Bool
    , featureCollection :: FeatureCollection
    }
type GeoJSONLoadCallBack = GeoJSONLoaded -> IO ()

-- | When trying to connect to Luci
data LuciConnect = LuciConnect
    { cHost :: JSString
    , cUser :: JSString
    , cPass :: JSString
    }



type SubmitURL = JSString
-- | Get the geometry and its preview image
newtype SubmitScenario = SubmitScenario SubmitURL
