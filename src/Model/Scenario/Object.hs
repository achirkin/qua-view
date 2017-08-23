{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | All sorts of objects in the scene.
--   Meant to be imported qualified
--
--   > import qualified Model.Scenario.Object as Object
--
module Model.Scenario.Object
    ( Object (..), ObjectId (..)
    , renderingId, center, geometry, properties
    ) where

import JavaScript.JSON.Types.Instances
import Numeric.DataFrame
import Commons
import SmallGL.Types
import Model.Scenario.Properties

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
  , _geometry    :: !Int -- Geometry
  , _properties  :: !Properties
  }

renderingId :: Functor f
            => (RenderedObjectId -> f RenderedObjectId)
            -> Object -> f Object
renderingId f s = (\x -> s{_renderingId = x}) <$> f (_renderingId s)


center :: Functor f
       => (Vec4f -> f Vec4f)
       -> Object -> f Object
center f s = (\x -> s{_center = x}) <$> f (_center s)


geometry :: Functor f
         => (Int -> f Int)
         -> Object -> f Object
geometry f s = (\x -> s{_geometry = x}) <$> f (_geometry s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Object -> f Object
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)
