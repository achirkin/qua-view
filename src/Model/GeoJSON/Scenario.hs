{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FromJSON and ToJSON instances for `Model.Scenario`.
--
--   Should not be included into `qua-view`!
--
module Model.GeoJSON.Scenario
    () where


import Control.Lens hiding (indices)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Internal
import Numeric.DataFrame
import Numeric.DataFrame.IO

import Commons.NoReflex
import qualified Model.Scenario as Scenario
import qualified Model.Scenario.Object as Object
import qualified Model.Scenario.Object.Geometry as Geometry
import Model.GeoJSON.Coordinates




instance FromJSON (Object.Object' 'Object.NotReady) where
    parseJSON v = flip (withObject "Feature") v $ \fObj -> do
        let _renderingData = Object.ORDN
        _properties <- fObj .:? "properties" .!= def
        (_geometry, PaddedZeros was2D) <- fObj .: "geometry"
        let _center = unsafePerformIO $ do
               SomeIODataFrame mdf <- Geometry.allData _geometry
               df <- unsafeFreezeDataFrame mdf
               return $ let t = ewfoldl (\a x -> let (x1,x2,x3,x4) = unpackV4 x
                                                 in if abs x4 < 0.0001
                                                    then a
                                                    else a + vec4 (x1/x4) (x2/x4) (x3/x4) 1
                                        )
                                        (vec4 0 0 0 0) df
                        in t / fromScalar (4 !. t)


        pure Object.Object {..}


instance ToJSON (Object.Object' m) where
    toJSON o = objectValue $ object
      [ ("type", stringValue "Feature")
      , ("geometry", toJSON $ o ^. Object.geometry )
      , ("properties", toJSON $ o ^. Object.properties )
      ]

instance ToJSON (Scenario.Scenario' m) where
    toJSON sc = objectValue $ object
        $  ("name", sc^?Scenario.name._Just.to stringValue)
        ?: ("lon",  sc^?Scenario.geoLoc._Just._1.to doubleValue)
        ?: ("lat",  sc^?Scenario.geoLoc._Just._2.to doubleValue)
        ?: ("alt",  sc^?Scenario.geoLoc._Just._3.to doubleValue)
        ?: ("srid", sc^?Scenario.srid._Just.to (doubleValue.fromIntegral))
        ?: [ ("properties", toJSON $ sc ^. Scenario.properties )
           , ("geometry"
             , objectValue $ object
               [ ("type", stringValue "FeatureCollection")
               , ("features", toJSON . Map.elems $ sc ^. Scenario.objects)
               ] -- TODO: do we need to store objects' geomIDs here?
             )
           ]
      where
        infixr 5 ?:
        (_, Nothing) ?: xs = xs
        (s, Just v)  ?: xs = (s,v):xs



