{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
-- | All sorts of objects in the scene.
--   Meant to be imported qualified
--
--   > import qualified Model.Scenario.Object as Object
--
module Model.Scenario.Object
    ( Object (..), ObjectId (..), ObjectBehavior (..), Collection
    , getTransferable
    , renderingId, center, geometry, properties
    , height, viewColor, objectBehavior
    ) where


import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Instances (ToJSON, FromJSON)
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

data ObjectRenderable
   = Renderable
     -- ^ Object has its rendering id
   | Prepared
     -- ^ Rendering data for object is prepared, but not uploaded to SmallGL yet
   | NotReady
     -- ^ Object just have been imported and not ready to be rendered yet.

data ObjectRenderingData (s :: ObjectRenderable) where
  ORDR :: {_renderingId :: !RenderedObjectId } -> ObjectRenderingData 'Renderable
  ORDP :: {_preparedData :: !PreparedData }    -> ObjectRenderingData 'Prepared
  ORDN :: ObjectRenderingData 'NotReady


data PreparedData
  = PreparedPoints !ColoredPointData
  | PraparedLines !ColoredLineData
  | PreparedPolys !ColoredData


--instance FromJSVal (ObjectRenderingData Renderable) where
--

data Object' (s :: ObjectRenderable) = Object'
  { _renderingData :: !(ObjectRenderingData s)
  , _center        :: !Vec4f
  , _geometry      :: !Geometry
  , _properties    :: !Properties
  } deriving Generic

instance FromJSVal (Object' s)
instance ToJSVal   (Object' s)


-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)


-- | One object has one geometry,
--   and one geometry must have only one Transferable
getTransferable :: Object' s -> IO Transferable
getTransferable = Geometry.getTransferable . _geometry


renderingId :: Functor f
            => (RenderedObjectId -> f RenderedObjectId)
            -> Object' 'Renderable -> f (Object' 'Renderable)
renderingId f s = (\x -> s{_renderingData = ORDR x}) <$> f (_renderingId $ _renderingData s)


center :: Functor f
       => (Vec4f -> f Vec4f)
       -> Object' s -> f (Object' s)
center f s = (\x -> s{_center = x}) <$> f (_center s)


geometry :: Functor f
         => (Geometry -> f Geometry)
         -> Object' s -> f (Object' s)
geometry f s = (\x -> s{_geometry = x}) <$> f (_geometry s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Object' s -> f (Object' s)
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)


-- * Special properties

height :: Functor f => (Maybe Double -> f (Maybe Double)) -> Object' s -> f (Object' s)
height = properties . propertyWithParsing "height"


viewColor :: Functor f
          => (Maybe HexColor -> f (Maybe HexColor)) -> Object' s -> f (Object' s)
viewColor = properties . property "viewColor"

objectBehavior :: Functor f
               => (ObjectBehavior -> f ObjectBehavior) -> Object' s -> f (Object' s)
objectBehavior f = properties $ propertyWithParsing "static" g
  where
    g (Just True) = Just . (Static ==) <$> f Static
    g _           = Just . (Static ==) <$> f Dynamic




-- | Alias for a map of objects
type Collection s = Map ObjectId (Object' s)

instance FromJSVal (Map ObjectId (Object' s)) where
    fromJSVal = fmap (fmap Map.fromAscList) . fromJSVal
    fromJSValUnchecked = fmap Map.fromAscList . fromJSValUnchecked


instance ToJSVal (Map ObjectId (Object' s)) where
    toJSVal m = do
        j <- js_createMap
        _ <- Map.traverseWithKey (f j) m
        return j
     where
       f :: JSVal -> ObjectId -> Object' s -> IO ()
       f j i o = toJSVal o >>= js_addKeyVal j i


foreign import javascript unsafe
  "$r = [];"
  js_createMap :: IO JSVal

foreign import javascript unsafe
  "$1.push([$2,$3]);"
  js_addKeyVal :: JSVal -> ObjectId -> JSVal -> IO ()

