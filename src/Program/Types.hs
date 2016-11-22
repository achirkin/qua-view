-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Types
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
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
import GHC.Exts (IsString(..))

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



data WantPicture = WantPicture

newtype PictureVal = PictureVal JSVal
instance LikeJS "Object" PictureVal


data RangedInt = RangedInt
  { riMin :: !Int
  , riMax :: !Int
  , riVal :: !Int
  }
  deriving Show

instance Eq RangedInt where
  a == b = riVal a == riVal b

instance Ord RangedInt where
  a `compare` b = riVal a `compare` riVal b
  a < b = riVal a < riVal b
  a <= b = riVal a <= riVal b
  a > b = riVal a > riVal b
  a >= b = riVal a >= riVal b


putInt :: Int -> RangedInt -> RangedInt
putInt x (RangedInt l h _) = RangedInt l h (min h . max l $ x)


putFloat :: Float -> RangedFloat -> RangedFloat
putFloat x (RangedFloat l h _) = RangedFloat l h (min h . max l $ x)

{-# INLINE riCombineSpreading #-}
riCombineSpreading :: (Int -> Int -> Int) -> RangedInt -> RangedInt -> RangedInt
riCombineSpreading f (RangedInt al ah a)  (RangedInt bl bh b) = RangedInt l h (min h . max l $ f a b)
  where
    l = min al bl
    h = max ah bh

instance Num RangedInt where
  (+) = riCombineSpreading (+)
  (-) = riCombineSpreading (-)
  (*) = riCombineSpreading (*)
  abs (RangedInt l h x) = RangedInt l h (min h . max l $ abs x)
  negate (RangedInt l h x) = RangedInt l h (min h . max l $ negate x)
  signum (RangedInt l h x) = RangedInt l h (min h . max l $ signum x)
  fromInteger i = RangedInt minBound maxBound (fromInteger i)


data RangedFloat = RangedFloat
  { rfMin :: !Float
  , rfMax :: !Float
  , rfVal :: !Float
  }
  deriving Show

instance Eq RangedFloat where
  a == b = rfVal a == rfVal b

instance Ord RangedFloat where
  a `compare` b = rfVal a `compare` rfVal b
  a < b = rfVal a < rfVal b
  a <= b = rfVal a <= rfVal b
  a > b = rfVal a > rfVal b
  a >= b = rfVal a >= rfVal b

{-# INLINE rfCombineSpreading #-}
rfCombineSpreading :: (Float -> Float -> Float) -> RangedFloat -> RangedFloat -> RangedFloat
rfCombineSpreading f (RangedFloat al ah a)  (RangedFloat bl bh b) = RangedFloat l h (min h . max l $ f a b)
  where
    l = min al bl
    h = max ah bh

instance Num RangedFloat where
  (+) = rfCombineSpreading (+)
  (-) = rfCombineSpreading (-)
  (*) = rfCombineSpreading (*)
  abs (RangedFloat l h x) = RangedFloat l h (min h . max l $ abs x)
  negate (RangedFloat l h x) = RangedFloat l h (min h . max l $ negate x)
  signum (RangedFloat l h x) = RangedFloat l h (min h . max l $ signum x)
  fromInteger i = RangedFloat ((2^^) . (+1) . fst $ floatRange (0::Float)) ((2^) . (+(-1)) . snd $ floatRange (0::Float)) (fromInteger i)


data StringEnum = StringEnum
  { seOptions :: [JSString]
  , seVal :: JSString
  }
  deriving Show

instance IsString StringEnum where
  fromString s = let t = fromString s in StringEnum [t] t



