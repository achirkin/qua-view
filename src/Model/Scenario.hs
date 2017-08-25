{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Geometry Scenario
--
--   The main structure in qua-view!
--
module Model.Scenario
    ( Scenario (..), getTransferables
    , name, geoLoc, properties
    , defaultActiveColor, defaultStaticColor, defaultBlockColor, defaultLineColor
    , defaultObjectHeight
    ) where


--import qualified Data.Map.Strict as Map
--import Control.Lens
import Data.Foldable (toList)
import GHC.Generics
import Commons
import Model.Scenario.Properties
import qualified Model.Scenario.Object as Object

data Scenario
  = Scenario
  { _name       :: !(Maybe JSString)
    -- ^ Friendly name for a scenario
  , _geoLoc     :: !(Maybe (Double, Double, Double))
    -- ^ Longitude, Latitude, and Altitude of scenario reference point
  , _properties :: !Properties
    -- ^ key-value of arbitrary JSON properties
  , _objects    :: ![Object.Object]
  } deriving Generic

instance FromJSVal Scenario
instance ToJSVal   Scenario

-- | Get transferable content of each scenario object
getTransferables :: Scenario -> IO [Transferable]
getTransferables = mapM Object.getTransferable . toList . _objects


-- * Lenses

name :: Functor f
     => (Maybe JSString -> f (Maybe JSString))
     -> Scenario -> f Scenario
name f s = (\x -> s{_name = x}) <$> f (_name s)

geoLoc :: Functor f
       => (Maybe (Double, Double, Double) -> f (Maybe (Double, Double, Double)))
       -> Scenario -> f Scenario
geoLoc f s = (\x -> s{_geoLoc = x}) <$> f (_geoLoc s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Scenario -> f Scenario
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)







-- * Special properties

defaultActiveColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario -> f Scenario
defaultActiveColor f = properties $ property "defaultActiveColor" g
   where
     g Nothing  = Just <$> f "#FF8888FF"
     g (Just c) = Just <$> f c

defaultStaticColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario -> f Scenario
defaultStaticColor f = properties $ property "defaultStaticColor" g
   where
     g Nothing  = Just <$> f "#808088FF"
     g (Just c) = Just <$> f c

defaultBlockColor :: Functor f
                  => (HexColor -> f HexColor) -> Scenario -> f Scenario
defaultBlockColor f = properties $ property "defaultBlockColor" g
   where
     g Nothing  = Just <$> f "#C0C082FF"
     g (Just c) = Just <$> f c

defaultLineColor :: Functor f
                  => (HexColor -> f HexColor) -> Scenario -> f Scenario
defaultLineColor f = properties $ property "defaultLineColor" g
   where
     g Nothing  = Just <$> f "#CC6666FF"
     g (Just c) = Just <$> f c

defaultObjectHeight :: Functor f
                    => (Double -> f Double) -> Scenario -> f Scenario
defaultObjectHeight f = properties $ property "defaultObjectHeight" g
   where
     g Nothing  = Just <$> f 3.5
     g (Just c) = Just <$> f c




