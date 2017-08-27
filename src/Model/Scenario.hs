{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Geometry Scenario
--
--   The main structure in qua-view!
--
module Model.Scenario
    ( Scenario, Scenario' (..), getTransferables
    , name, geoLoc, properties, objects, objIdSeq, withoutObjects
    , defaultActiveColor, defaultStaticColor
    , defaultBlockColor, defaultLineColor, defaultPointColor
    , defaultObjectHeight
    ) where


--import qualified Data.Map.Strict as Map
import Control.Lens (set)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Semigroup (stimesIdempotentMonoid)
import GHC.Generics
import Commons
import Model.Scenario.Properties
--import Model.Scenario.Statistics
import qualified Model.Scenario.Object as Object


type Scenario = Scenario' 'Object.Renderable

data Scenario' s
  = Scenario
  { _name       :: !(Maybe JSString)
    -- ^ Friendly name for a scenario
  , _geoLoc     :: !(Maybe (Double, Double, Double))
    -- ^ Longitude, Latitude, and Altitude of scenario reference point
  , _properties :: !Properties
    -- ^ key-value of arbitrary JSON properties
  , _objects    :: !(Object.Collection' s)
    -- ^ Map with scenario content
  , _objIdSeq   :: !Object.ObjectId
  } deriving Generic

instance FromJSVal (Scenario' 'Object.Prepared)
instance ToJSVal   (Scenario' 'Object.Prepared)

-- | Get transferable content of each scenario object
getTransferables :: Scenario' s -> IO [Transferable]
getTransferables = mapM Object.getTransferable . toList . _objects

-- | Get scenario with no Objects inside.
--   Used to send scenario information to a geometry loader,
--   so that geometry loader has scenario context.
withoutObjects :: Scenario' s -> Scenario' 'Object.Prepared
withoutObjects = set objects mempty


-- * Lenses

name :: Functor f
     => (Maybe JSString -> f (Maybe JSString))
     -> Scenario' s -> f (Scenario' s)
name f s = (\x -> s{_name = x}) <$> f (_name s)

geoLoc :: Functor f
       => (Maybe (Double, Double, Double) -> f (Maybe (Double, Double, Double)))
       -> Scenario' s -> f (Scenario' s)
geoLoc f s = (\x -> s{_geoLoc = x}) <$> f (_geoLoc s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Scenario' s -> f (Scenario' s)
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)

objects :: Functor f
        => (Object.Collection' s -> f (Object.Collection' t))
        -> Scenario' s -> f (Scenario' t)
objects f s = (\x -> s{_objects = x}) <$> f (_objects s)

objIdSeq :: Functor f
         => (Object.ObjectId -> f (Object.ObjectId))
         -> Scenario' s -> f (Scenario' s)
objIdSeq f s = (\x -> s{_objIdSeq = x}) <$> f (_objIdSeq s)






-- * Special properties

defaultActiveColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultActiveColor f = properties $ property "defaultActiveColor" g
   where
     g Nothing  = Just <$> f "#FF8888FF"
     g (Just c) = Just <$> f c

defaultStaticColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultStaticColor f = properties $ property "defaultStaticColor" g
   where
     g Nothing  = Just <$> f "#808088FF"
     g (Just c) = Just <$> f c

defaultBlockColor :: Functor f
                  => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultBlockColor f = properties $ property "defaultBlockColor" g
   where
     g Nothing  = Just <$> f "#C0C082FF"
     g (Just c) = Just <$> f c

defaultLineColor :: Functor f
                  => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultLineColor f = properties $ property "defaultLineColor" g
   where
     g Nothing  = Just <$> f "#CC6666FF"
     g (Just c) = Just <$> f c

defaultPointColor :: Functor f
                  => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultPointColor f = properties $ property "defaultPointColor" g
   where
     g Nothing  = Just <$> f "#006666FF"
     g (Just c) = Just <$> f c


defaultObjectHeight :: Functor f
                    => (Double -> f Double) -> Scenario' s -> f (Scenario' s)
defaultObjectHeight f = properties $ property "defaultObjectHeight" g
   where
     g Nothing  = Just <$> f 3.5
     g (Just c) = Just <$> f c



instance Semigroup (Scenario' s) where
  scOld <> scNew =  Scenario
    {               -- trying to update scenario name if it has changed
      _name       = _name scNew <|> _name scOld
                    -- keeping GeoLocation from older version
    , _geoLoc     = _geoLoc scOld <|> _geoLoc scNew
                    -- prefer duplicate properties from a new version
    , _properties = _properties scNew <> _properties scOld
                    -- replace older objects with newer ones
    , _objects    = _objects scNew <> _objects scOld
                    -- get maximum of objId counters to make sure
                    -- no objects could have the same object Id
    , _objIdSeq   = max (_objIdSeq scOld) (_objIdSeq scNew)
    }
  stimes = stimesIdempotentMonoid

instance Monoid (Scenario' s) where
  mempty = Scenario
    { _name       = Nothing
    , _geoLoc     = Nothing
    , _properties = mempty
    , _objects    = mempty
    , _objIdSeq   = Object.ObjectId 0
    }
  mappend = (<>)

instance Default (Scenario' s) where
  def = mempty
