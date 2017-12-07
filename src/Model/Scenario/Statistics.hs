{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
-- | Information about scenarios
--
--   Read various info about objects count and positions
--
module Model.Scenario.Statistics
    ( ScenarioStatistics (..)
    , inferViewDistance, inferCameraLookAt
    ) where


import JavaScript.JSON.Types.Internal
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Generic ()
import GHC.Generics
import Numeric.DataFrame
import Commons.NoReflex

-- | This information is used to derive strategies of building WebGL scene
data ScenarioStatistics = ScenarioStatistics
    { lowerCorner :: !Vec2f
    , upperCorner :: !Vec2f
    , objNumber   :: !Int
    , centerPoint :: !Vec2f
    } deriving Generic

instance FromJSON ScenarioStatistics
instance ToJSON  ScenarioStatistics
instance FromJSVal ScenarioStatistics where
    fromJSVal v = pure $ case fromJSON (SomeValue v) of
        Error _ -> Nothing
        Success r -> Just r
instance ToJSVal ScenarioStatistics where
    toJSVal  = pure . pToJSVal
instance PToJSVal ScenarioStatistics where
    pToJSVal = coerce . toJSON

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


inferViewDistance :: ScenarioStatistics -> Float
inferViewDistance ScenarioStatistics {..} = trunc $ 20 * r / sqrt n
  where
    -- assume minimum possible radius is 100 m
    minRad = 1000
    -- assume maximum possible radius is 10 km
    maxRad = 10000

    trunc = max minRad . min maxRad
    -- characteristic size in terms of number of blocks
    n = fromIntegral $ max 100 objNumber
    -- characteristic size in terms of radius
    r = unScalar
      $ min (normL2 (lowerCorner - centerPoint))
            (normL2 (upperCorner - centerPoint))


inferCameraLookAt :: ScenarioStatistics -> (Vec3f, Vec3f)
inferCameraLookAt s@ScenarioStatistics {..} = (v+dv, v)
  where
    r = inferViewDistance s
    v = centerPoint <+:> 0
    dv = vec3 (-0.3*r) (-0.2*r) (0.7*r)



