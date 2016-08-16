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

import Data.Time (UTCTime)
import Data.Hashable
import Data.String (IsString())
import JsHs
import JsHs.JSString (unpack')

newtype RequireViewUpdate a = RequireViewUpdate a


-- | Luci ScID is used to reference luci scenarios
newtype ScenarioId = ScenarioId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" ScenarioId where
  asLikeJS = ScenarioId . asLikeJS
  asJSVal (ScenarioId v) = asJSVal v

-- | Represent synchronization state with a remote server (luci)
data ScenarioSync
  = SSEmpty
    -- ^ There is no geometry loaded
  | SSNotBound
    -- ^ Not synchronized with luci
  | SSPendingCreate ScenarioName
    -- ^ Run "scenario.geojson.Create", but not received result yet
  | SSPendingGet ScenarioId ScenarioName
    -- ^ Run "scenario.geojson.Get", but not received result yet
  | SSSynced ScenarioId ScenarioName UTCTime
    -- ^ Last synchronized at a given time



-- | Luci geomID is used to reference scenario objects
newtype GeomId = GeomId Int
  deriving (Eq,Ord,Show,Enum,Num,Real,Integral)
instance LikeJS "Number" GeomId where
  asLikeJS = GeomId . asLikeJS
  asJSVal (GeomId v) = asJSVal v


-- | Luci scenario name
newtype ScenarioName = ScenarioName JSString
  deriving (Eq,Ord,Show,IsString, Monoid)
instance LikeJS "String" ScenarioName where
  asLikeJS = ScenarioName . asLikeJS
  asJSVal (ScenarioName v) = asJSVal v

-- | Luci service name
newtype ServiceName = ServiceName JSString
  deriving (Eq,Ord,Show,IsString, Monoid)
instance LikeJS "String" ServiceName where
  asLikeJS = ServiceName . asLikeJS
  asJSVal (ServiceName v) = asJSVal v

unServiceName :: ServiceName -> String
unServiceName (ServiceName a) = unpack' a

instance Hashable ServiceName where
  hash = hash . unServiceName
  hashWithSalt i = hashWithSalt i . unServiceName
