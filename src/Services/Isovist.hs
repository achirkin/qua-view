{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Services.Isovist
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services.Isovist where

--import Data.Foldable (foldl')


import JsHs.WebGL
import JsHs.Types
import JsHs.LikeJS.Class
import qualified JsHs.Array as JS
import GHCJS.Useful

import Data.Geometry
import qualified Data.Geometry.Structure.Point as Point
import qualified Data.Geometry.Structure.Feature as Feature

import Controllers.LuciClient
import Program.Model.ScalarField

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
newtype IsovistResults = IR JSVal
instance LikeJS "Array" IsovistResults where
    asLikeJS = IR . asLikeJS
    asJSVal (IR x) = asJSVal x
instance JS.LikeJSArray "Object" IsovistResults where
    type ArrayElem IsovistResults = IsovistPoint
    toJSArray x = js_getResults x
    fromJSArray = undefined

foreign import javascript unsafe  "$r = $1['outputs']['outputVals']['Results'];" js_getResults :: IsovistResults -> JS.Array IsovistPoint

-- | Results that we get from luci
newtype IsovistPoint = IsovistPoint JSVal
instance LikeJS "Object" IsovistPoint
--instance LikeJS IsovistPoint

foreign import javascript unsafe  "$1['Area']" ipArea :: IsovistPoint -> GLfloat
foreign import javascript unsafe  "$1['Area']" ipCompactness :: IsovistPoint -> GLfloat
foreign import javascript unsafe  "$1['Area']" ipMaxRadial :: IsovistPoint -> GLfloat
foreign import javascript unsafe  "$1['Area']" ipMinRadial :: IsovistPoint -> GLfloat
foreign import javascript unsafe  "$1['Area']" ipOcclusivity :: IsovistPoint -> GLfloat
foreign import javascript unsafe  "$1['Area']" ipPerimeter :: IsovistPoint -> GLfloat


-- | Get desired grid values from Isovist results
getIsovistValues :: IsovistResults -> IsovistMode -> JS.Array GLfloat
getIsovistValues irs Area = JS.map ipArea irs
getIsovistValues irs Compactness = JS.map ipCompactness irs
getIsovistValues irs MaxRadial = JS.map ipMaxRadial irs
getIsovistValues irs MinRadial = JS.map ipMinRadial irs
getIsovistValues irs Occlusivity = JS.map ipOcclusivity irs
getIsovistValues irs Perimeter = JS.map ipPerimeter irs

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
    runService (Isovist mode) (Just luci) (Just scenario) sf@ScalarField{ sfPoints = pnts} = do
      mresult <- runLuciService luci "Isovist" inputs scenario
      case mresult of
        Left err -> logText' err >> return Nothing
        Right (LuciServiceOutput isovist) -> return (Just nsf)
          where nsf = sf{sfValues = vals, sfRange = unpackV2 vrange }
                vals = JS.map (\x -> log (1 + x)) $ getIsovistValues (asLikeJS isovist) mode
                vrange = JS.foldl (\v t -> let (xmin, xmax) = unpackV2 v
                            in vector2 (min xmin t) (max xmax t)) (vector2 (1/0) (-1/0)) vals
      where inputs = LuciServiceInput
                 . asJSVal
                 . Feature.GeoMultiPoint
                 . (JS.fromJSArray :: JS.Array (Vector2 GLfloat) -> Point.MultiPoint 2 GLfloat)
                 $ JS.map resizeVector pnts
                 -- JS.map (\v -> let (x,_,z) = unpackV3 v in vector2 x (-z)) pnts
