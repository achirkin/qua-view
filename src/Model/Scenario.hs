{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Geometry Scenario
--
--   The main structure in qua-view!
--
module Model.Scenario
    ( Scenario (..)
    ) where


--import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Internal
import Commons

data Scenario
  = Scenario
  { name       :: !(Maybe JSString)
    -- ^ Friendly name for a scenario
  , geoLoc     :: !(Maybe (Double, Double, Double))
    -- ^ Longitude, Latitude, and Altitude of scenario reference point
  , properties :: !(Map JSString Value)
  }

instance ToJSVal Scenario where
    toJSVal = pure . pToJSVal

instance PToJSVal Scenario where
    pToJSVal Scenario{..} = coerce . objectValue . object
       $ "name" =:? name
      <> "lon"  =:? (fst3 <$> geoLoc)
      <> "lat"  =:? (snd3 <$> geoLoc)
      <> "alt"  =:? (thd3 <$> geoLoc)
      <> "properties" =:: properties

(=:?) :: PToJSVal a => JSString -> Maybe a -> [(JSString, Value)]
(=:?) _ Nothing = []
(=:?) n (Just v) = [(n, coerce $ pToJSVal v)]
infixr 7 =:?

(=::) :: PToJSVal a => JSString -> a -> [(JSString, Value)]
(=::) n v = [(n, coerce $ pToJSVal v)]
infixr 7 =::

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

