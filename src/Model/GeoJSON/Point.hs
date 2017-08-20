{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.GeoJSON.Point
    ( Point (..)
    ) where


import Numeric.DataFrame
import Commons
--import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
--import JavaScript.Array
--import JavaScript.Array.Internal
--import Unsafe.Coerce

import Model.GeoJSON.Coordinates

data Point
  = Point
  { _pCoordinates :: Vec3f
  } deriving (Eq, Show)



instance FromJSON (Point, PaddedZeros) where
    parseJSON val = flip (withObject "GeoJSON point") val $ \point ->
        first Point <$> point .: "coordinates"


