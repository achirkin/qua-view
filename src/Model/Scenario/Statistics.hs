{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information about scenarios
--
--   Read various info about objects count and positions
--
module Model.Scenario.Statistics
    ( ScenarioStatistics (..)
    ) where


import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import Numeric.DataFrame
import Commons

-- | This information is used to derive strategies of building WebGL scene
data ScenarioStatistics = ScenarioStatistics
    { lowerCorner :: !Vec2f
    , upperCorner :: !Vec2f
    , objNumber   :: !Int
    , centerPoint :: !Vec2f
    }

instance Semigroup ScenarioStatistics where
    sca@ScenarioStatistics{objNumber = n} <> scb@ScenarioStatistics{objNumber = m}
        = ScenarioStatistics
        { lowerCorner = min (lowerCorner sca) (lowerCorner scb)
        , upperCorner = max (upperCorner sca) (upperCorner scb)
        , objNumber   = n + m
        , centerPoint = if n == 0 && m == 0
                        then 0
                        else let nm = fromIntegral $ n + m
                                 a = fromIntegral n / nm
                                 b = fromIntegral m / nm
                             in fromScalar a * centerPoint sca
                              + fromScalar b * centerPoint scb
        }

instance Monoid ScenarioStatistics where
    mempty = ScenarioStatistics
        { lowerCorner = fromScalar inf
        , upperCorner = fromScalar $ negate inf
        , objNumber   = 0
        , centerPoint = 0
        }
      where
        inf = scalar $ read "Infinity"
    mappend = (<>)


instance PToJSVal ScenarioStatistics where
    pToJSVal ScenarioStatistics {..} = coerce . objectValue $ object
      [ ("lowerCorner", toJSON lowerCorner)
      , ("upperCorner", toJSON upperCorner)
      , ("objNumber"  , doubleValue $ fromIntegral objNumber)
      , ("centerPoint", toJSON centerPoint)
      ]

instance ToJSVal ScenarioStatistics where
  toJSVal = pure . pToJSVal

instance FromJSON ScenarioStatistics where
    parseJSON val = flip (withObject "ScenarioStatistics") val $ \obj ->
        ScenarioStatistics
          <$> obj .:? "lowerCorner" .!= lowerCorner d
          <*> obj .:? "upperCorner" .!= upperCorner d
          <*> obj .:? "objNumber"   .!= objNumber d
          <*> obj .:? "centerPoint" .!= centerPoint d
      where
       d = mempty


instance FromJSVal ScenarioStatistics where
    fromJSVal jsv = pure $ case fromJSON (SomeValue jsv) of
      Error _   -> Nothing
      Success x -> Just x





