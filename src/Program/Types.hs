-----------------------------------------------------------------------------
--
-- Module      :  Program.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
module Program.Types where

import JsHs


newtype RequireViewUpdate a = RequireViewUpdate a


-- | Luci ScID is used to reference luci scenarios
newtype ScenarioId = ScenarioId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" ScenarioId where
  asLikeJS = ScenarioId . asLikeJS
  asJSVal (ScenarioId v) = asJSVal v


data ScenarioSync
  = CityNotBound
  | CityUpdated ScenarioId JSString [Int]
  | CitySynced ScenarioId JSString

--data CityNotBoundDetails
--  = CityEmpty
--  | LuciDisconnected
--  | ScenarioNotSynced



-- | Luci geomID is used to reference scenario objects
newtype GeomId = GeomId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" GeomId where
  asLikeJS = GeomId . asLikeJS
  asJSVal (GeomId v) = asJSVal v
