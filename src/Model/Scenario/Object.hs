{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | All sorts of objects in the scene.
--   Meant to be imported qualified
--
--   > import qualified Model.Scenario.Object as Object
--
module Model.Scenario.Object
    ( Object (..), ObjectId (..), ObjectBehavior (..)
    , getTransferable
    , renderingId, center, geometry, properties
    , height, viewColor, objectBehavior
    ) where

import JavaScript.JSON.Types.Instances
import GHC.Generics
import Numeric.DataFrame
import Commons
import SmallGL.Types
import Model.Scenario.Properties
import Model.Scenario.Object.Geometry (Geometry)
import qualified Model.Scenario.Object.Geometry as Geometry

-- | Refernce to object in a scenario.
--
--   It corresponds to @properties.geomID@ value of every feature in luci scenario
--
newtype ObjectId = ObjectId { _unObjectId :: Int }
  deriving (PToJSVal, ToJSVal, ToJSON, PFromJSVal, FromJSVal, FromJSON, Eq, Ord)

data Object
  = Object
  { _renderingId :: !RenderedObjectId
  , _center      :: !Vec4f
  , _geometry    :: !Geometry
  , _properties  :: !Properties
  } deriving Generic

instance FromJSVal Object
instance ToJSVal   Object

-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)


-- | One object has one geometry,
--   and one geometry must have only one Transferable
getTransferable :: Object -> IO Transferable
getTransferable = Geometry.getTransferable . _geometry


renderingId :: Functor f
            => (RenderedObjectId -> f RenderedObjectId)
            -> Object -> f Object
renderingId f s = (\x -> s{_renderingId = x}) <$> f (_renderingId s)


center :: Functor f
       => (Vec4f -> f Vec4f)
       -> Object -> f Object
center f s = (\x -> s{_center = x}) <$> f (_center s)


geometry :: Functor f
         => (Geometry -> f Geometry)
         -> Object -> f Object
geometry f s = (\x -> s{_geometry = x}) <$> f (_geometry s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Object -> f Object
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)


-- * Special properties

height :: Functor f => (Maybe Double -> f (Maybe Double)) -> Object -> f Object
height = properties . propertyWithParsing "height"


viewColor :: Functor f
          => (Maybe HexColor -> f (Maybe HexColor)) -> Object -> f Object
viewColor = properties . property "viewColor"

objectBehavior :: Functor f
               => (ObjectBehavior -> f ObjectBehavior) -> Object -> f Object
objectBehavior f = properties $ propertyWithParsing "static" g
  where
    g (Just True) = Just . (Static ==) <$> f Static
    g _           = Just . (Static ==) <$> f Dynamic




