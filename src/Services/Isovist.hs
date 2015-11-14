{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Services.Isovist
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services.Isovist where

--import Control.Monad (mzero)

--import Data.List (foldl')
--import Data.Foldable (toList)
--import Data.Text.Encoding
--import Data.Aeson
--import Data.Geospatial (GeoMultiPoint (..))

import GHCJS.WebGL
--import GHCJS.Types
--import GHCJS.Foreign
--import GHCJS.Marshal
import GHCJS.Useful

--import Geometry.Space

--import Controllers.LuciClient
--import Program.Model.GeoJSON
--import Program.Model.ScalarField

import Services

-- | Declare various regimes of Isovist calculation
data IsovistMode = Area
                 | Compactness
                 | MaxRadial
                 | MinRadial
                 | Occlusivity
                 | Perimeter
    deriving Show

-- | Declare Isovist Service with its parameters
data Isovist = Isovist IsovistMode

-- | How Isovist service is presented to a user
instance Show Isovist where
    show (Isovist mode) = "Isovist Service (" ++ show mode ++ ")"

-- | Results that we get from luci (after parsing JSON)
data IsovistResults = IR [IsovistPoint]
-- | Results that we get from luci (after parsing JSON)
data IsovistPoint = IsovistPoint
    { ipArea        :: !GLfloat
    , ipCompactness :: !GLfloat
    , ipMaxRadial   :: !GLfloat
    , ipMinRadial   :: !GLfloat
    , ipOcclusivity :: !GLfloat
    , ipPerimeter   :: !GLfloat
    } deriving Show

-- | Get desired grid values from Isovist results
getIsovistValues :: IsovistResults -> IsovistMode -> [GLfloat]
getIsovistValues (IR irs) Area = map ipArea irs
getIsovistValues (IR irs) Compactness = map ipCompactness irs
getIsovistValues (IR irs) MaxRadial = map ipMaxRadial irs
getIsovistValues (IR irs) MinRadial = map ipMinRadial irs
getIsovistValues (IR irs) Occlusivity = map ipOcclusivity irs
getIsovistValues (IR irs) Perimeter = map ipPerimeter irs

---- | Helper to convert JSON to Haskell values
--instance FromJSON IsovistResults where
--    parseJSON (Object o) = o .: "outputs" >>= (.: "outputVals") >>= parseText >>= parseList
--        where parseText (String text) = case decodeStrict $ encodeUtf8 text of
--                    Just (Object v) -> v .: "Results"
--                    Just _ -> mzero
--                    Nothing -> mzero
--              parseText _ = mzero
--              parseList (Array ar) = liftM (IR . toList) $ mapM parseJSON ar
--              parseList _ = mzero
--    parseJSON _ = mzero
--
---- | Helper to convert JSON to Haskell values
--instance FromJSON IsovistPoint where
--    parseJSON (Object o) = IsovistPoint <$>
--        o .: "Area" <*>
--        o .: "Compactness" <*>
--        o .: "MaxRadial" <*>
--        o .: "MinRadial" <*>
--        o .: "Occlusivity" <*>
--        o .: "Perimeter"
--    parseJSON _ = mzero

-- | Send request to Luci and get the data.
instance ComputingService Isovist where
    runService _ Nothing _ _ = logText "Can not run Isovist without Luci" >> return Nothing
    runService _ _ Nothing _ = logText "Can not run Isovist without Scenario on Luci" >> return Nothing
    runService _ _ _ _ = return Nothing
--    runService (Isovist mode) (Just luci) (Just scenario) sf@ScalarField{ sfPoints = pnts} = do
--      mresult <- runLuciService luci "Isovist" inputs scenario
--      return Nothing
--      case mresult of
--        Left err -> logText err >> return Nothing
--        Right result -> case fromJSON result of
--            Error jerr -> logText jerr >> return Nothing
--            Success isovist -> return (Just nsf)
--                where nsf = sf{sfValues = vals, sfRange = vrange }
--                      vals = map (\x -> log (1 + x)) $ getIsovistValues isovist mode
--                      vrange = foldl' (\(Vector2 xmin xmax) t
--                        -> Vector2 (min xmin t) (max xmax t)) (pure $ head vals) vals
--      where inputs = object . (:[]) . (.=) "inputVals" . toJSON . GeoMultiPoint
--                     $ map (\(Vector3 x _ z) -> map realToFrac [x,-z]) pnts

