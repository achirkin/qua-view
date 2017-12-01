{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | All sorts of objects in the scene.
--   Meant to be imported qualified
--
--   > import qualified Model.Scenario.Object as Object
--
module Model.Scenario.Object
    ( Object, Object' (..), ObjectId (..), GroupId (..), ObjectBehavior (..)
    , Collection, Collection'
    , ObjectRenderable (..), Renderable, Prepared, NotReady
    , ObjectRenderingData (..), SpecialObject (..)
    , getTransferable, registerRender
    , renderingData, renderingId, center, geometry, properties
    , height, viewColor, objectBehavior
    , geomID, groupID
    , selectable, visible, special
    ) where

import Control.Lens
import qualified Data.Map.Strict as Map
import JavaScript.JSON.Types.Instances (ToJSON (..), FromJSON (..))
import JavaScript.WebGL (GLuint)
import GHC.Generics
import Numeric.DataFrame
import Commons.NoReflex
import SmallGL.Types
import Model.Scenario.Properties
import Model.Scenario.Object.Geometry (Geometry)
import qualified Model.Scenario.Object.Geometry as Geometry

-- | Reference to object in a scenario.
--
--   It corresponds to @properties.geomID@ value of every feature in luci scenario
--
newtype ObjectId = ObjectId { _unObjectId :: GLuint }
  deriving (PToJSVal, ToJSVal, ToJSON, PFromJSVal, FromJSVal, FromJSON, Eq, Ord, Show)


newtype GroupId = GroupId { _unGroupId :: GLuint }
  deriving (PToJSVal, ToJSVal, ToJSON, PFromJSVal, FromJSVal, FromJSON, Eq, Ord, Show)

instance FromJSONOrString ObjectId where
    parseJSONOrString = fmap (ObjectId . (fromIntegral :: Word -> GLuint)). parseJSONOrString

instance FromJSONOrString GroupId where
    parseJSONOrString = fmap (GroupId . (fromIntegral :: Word -> GLuint)) . parseJSONOrString


data ObjectRenderable
   = Renderable
     -- ^ Object has its rendering id
   | Prepared
     -- ^ Rendering data for object is prepared, but not uploaded to SmallGL yet
   | NotReady
     -- ^ Object just have been imported and not ready to be rendered yet.

-- | Object has its rendering id
type Renderable = 'Renderable
-- | Rendering data for object is prepared, but not uploaded to SmallGL yet
type Prepared   = 'Prepared
-- | Object just have been imported and not ready to be rendered yet
type NotReady   = 'NotReady

data ObjectRenderingData (s :: ObjectRenderable) where
  ORDR :: {_renderingId :: !RenderedObjectId } -> ObjectRenderingData 'Renderable
  ORDP :: forall m . !(ObjRenderingData m)   -> ObjectRenderingData 'Prepared
  ORDN :: ObjectRenderingData 'NotReady


instance FromJSVal (ObjectRenderingData 'Prepared) where
    fromJSValUnchecked jsv = ORDP <$> fromJSValUnchecked jsv
    fromJSVal jsv          = fmap ORDP <$> fromJSVal jsv

instance ToJSVal (ObjectRenderingData 'Prepared) where
    toJSVal (ORDP pd) = toJSVal pd


type Object = Object' 'Renderable

data Object' (s :: ObjectRenderable) = Object
  { _renderingData :: !(ObjectRenderingData s)
  , _center        :: !Vec4f
  , _geometry      :: !Geometry
  , _properties    :: !Properties
  , was2D          :: !Bool
  } deriving Generic

instance FromJSVal (Object' 'Prepared)
instance ToJSVal   (Object' 'Prepared)


-- | Whether one could interact with an object or not
data ObjectBehavior = Static | Dynamic deriving (Eq,Show)


-- | One object has one geometry,
--   and one geometry must have only one Transferable
getTransferable :: Object' s -> IO Transferable
getTransferable = Geometry.getTransferable . _geometry

registerRender :: Functor f
               => (forall m . ObjRenderingData m -> f RenderedObjectId)
               -> Object' 'Prepared -> f (Object' 'Renderable)
registerRender f s = (\x -> s{_renderingData = ORDR x}) <$> g (_renderingData s)
    where
      g (ORDP d) = f d

renderingData :: Functor f
              => (ObjectRenderingData s -> f (ObjectRenderingData t))
              -> Object' s -> f (Object' t)
renderingData f s = (\x -> s{_renderingData = x}) <$> f (_renderingData s)

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

-- | Alias for a map of objects
type Collection' s = Map ObjectId (Object' s)
type Collection    = Map ObjectId (Object' 'Renderable)

instance FromJSVal (Map ObjectId (Object' 'Prepared)) where
    fromJSVal = fmap (fmap Map.fromAscList) . fromJSVal
    fromJSValUnchecked = fmap Map.fromAscList . fromJSValUnchecked


instance ToJSVal (Map ObjectId (Object' 'Prepared)) where
    toJSVal m = do
        j <- js_createMap
        _ <- Map.traverseWithKey (f j) m
        return j
     where
       f :: JSVal -> ObjectId -> Object' 'Prepared -> IO ()
       f j i o = toJSVal o >>= js_addKeyVal j (_unObjectId i)


foreign import javascript unsafe
  "$r = [];"
  js_createMap :: IO JSVal

foreign import javascript unsafe
  "$1.push([$2,$3]);"
  js_addKeyVal :: JSVal -> GLuint -> JSVal -> IO ()



instance FromJSVal (Map.Map GroupId [ObjectId]) where
    fromJSVal = fmap (fmap Map.fromAscList) . fromJSVal
    fromJSValUnchecked = fmap Map.fromAscList . fromJSValUnchecked


instance ToJSVal (Map.Map GroupId [ObjectId]) where
    toJSVal m = do
        j <- js_createMap
        _ <- Map.traverseWithKey (f j) m
        return j
     where
       f :: JSVal -> GroupId -> [ObjectId] -> IO ()
       f j i o = toJSVal o >>= js_addKeyVal j (_unGroupId i)




-- * Special properties (with no scenario-based resolving)

-- | Property "special" of objects; lets scenario customize qua-view presentation.
data SpecialObject
  = SpecialCamera
  | SpecialForcedArea
  | SpecialTemplate
  deriving Eq

instance Show SpecialObject where
  show SpecialCamera     = "camera"
  show SpecialForcedArea = "forcedArea"
  show SpecialTemplate   = "template"

instance FromJSON SpecialObject where
  parseJSON v = parseJSON v >>= \s -> case (s :: JSString) of
    "camera"     -> return SpecialCamera
    "forcedArea" -> return SpecialForcedArea
    "template"   -> return SpecialTemplate
    _            -> fail $ "Unknown special object: " ++ show s

instance ToJSON SpecialObject where
  toJSON SpecialCamera     = toJSON ("camera" :: JSString)
  toJSON SpecialForcedArea = toJSON ("forcedArea" :: JSString)
  toJSON SpecialTemplate   = toJSON ("template" :: JSString)

geomID :: Functor f => (Maybe ObjectId -> f (Maybe ObjectId)) -> Object' s -> f (Object' s)
geomID = properties . propertyWithParsing "geomID"

groupID :: Functor f => (Maybe GroupId -> f (Maybe GroupId)) -> Object' s -> f (Object' s)
groupID = properties . propertyWithParsing "groupID"

height :: Functor f => (Maybe Double -> f (Maybe Double)) -> Object' s -> f (Object' s)
height = properties . propertyWithParsing "height"

viewColor :: Functor f
          => (Maybe HexColor -> f (Maybe HexColor)) -> Object' s -> f (Object' s)
viewColor f o = properties (property "viewColor" g) o
  where
    g (Just c) = f (Just c)
    g Nothing  = f dueSpecial
    dueSpecial = case o ^. special of
      Just SpecialCamera     -> Nothing
      Just SpecialForcedArea -> Just "#FFFFFF99"
      Just SpecialTemplate   -> Nothing
      Nothing                -> Nothing

objectBehavior :: Functor f
               => (ObjectBehavior -> f ObjectBehavior) -> Object' s -> f (Object' s)
objectBehavior f o = properties (propertyWithParsing "static" g) o
  where
    g (Just True)  = Just . (Static ==) <$> f Static
    g (Just False) = Just . (Static ==) <$> f Dynamic
    g Nothing
       | not (o ^. selectable) = Just . (Static ==) <$> f Static
       | otherwise             = Just . (Static ==) <$> f Dynamic

selectable :: Functor f
           => (Bool -> f Bool) -> Object' s -> f (Object' s)
selectable f o = properties (propertyWithParsing "selectable" g) o
   where
     g (Just x) = Just <$> f x
     g Nothing  = Just <$> f (dueSpecial && o ^. visible)
     dueSpecial = case o^. special of
       Just SpecialCamera     -> False
       Just SpecialForcedArea -> False
       Just SpecialTemplate   -> True
       Nothing                -> True

visible :: Functor f
        => (Bool -> f Bool) -> Object' s -> f (Object' s)
visible f o = properties (propertyWithParsing "visible" g) o
  where
    g (Just x)  = Just <$> f x
    g Nothing   = Just <$> f dueSpecial
    dueSpecial = case o ^. special of
      Just SpecialCamera     -> False
      Just SpecialForcedArea -> True
      Just SpecialTemplate   -> True
      Nothing                -> True

special :: Functor f
        => (Maybe SpecialObject -> f (Maybe SpecialObject))
        -> Object' s -> f (Object' s)
special = properties . property "special"

