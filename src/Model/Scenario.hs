{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Strict               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Geometry Scenario
--
--   The main structure in qua-view!
--
module Model.Scenario
    ( Scenario, Scenario' (..), getTransferables
    , name, geoLoc, srid, properties, objects, objIdSeq, viewState, withoutObjects
    , selectedDynamicColor, selectedStaticColor, selectedGroupColor
    , defaultStaticColor
    , defaultBlockColor, defaultLineColor, defaultPointColor
    , defaultObjectHeight
    , viewDistance, evaluationCellSize
    , mapZoomLevel, mapOpacity, useMapLayer, mapUrl
    , hiddenProperties
    , resolvedObjectHeight, resolvedObjectColor
    , resolvedObjectColorIgnoreVisible
    , ScenarioState (..)
    , cameraPos, cameraLoc, cameraLookAt, objectGroups, clippingDist
    , templates, forcedAreaObjId, groupIdSeq
    ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (set,(^.),non, _1, _2)
import Control.Applicative ((<|>))
import Numeric.DataFrame hiding (toList)
import Data.Foldable (toList)
import Data.Semigroup (stimesIdempotentMonoid)
import GHC.Generics
import Commons.NoReflex
import Model.Scenario.Properties
import           Model.Scenario.Object ( GroupId, ObjectId)
import qualified Model.Scenario.Object as Object
import qualified Model.Scenario.Object.Geometry as Geometry


type Scenario = Scenario' 'Object.Renderable

data Scenario' s
  = Scenario
  { _name       :: !(Maybe JSString)
    -- ^ Friendly name for a scenario
  , _geoLoc     :: !(Maybe (Double, Double, Double))
    -- ^ Longitude, Latitude, and Altitude of scenario reference point
  , _srid       :: !(Maybe Int)
    -- ^ We can explicitly specify srid
  , _properties :: !Properties
    -- ^ key-value of arbitrary JSON properties
  , _objects    :: !(Object.Collection' s)
    -- ^ Map with scenario content
  , _objIdSeq   :: !ObjectId
    -- ^ Keep track of highest ObjectId to be able to generate more
  , _viewState  :: !ScenarioState
    -- ^ Some necessary information for viewing and interacting with scenario
  } deriving Generic

instance FromJSVal (Scenario' Object.Prepared)
instance ToJSVal   (Scenario' Object.Prepared)

-- | Get transferable content of each scenario object
getTransferables :: Scenario' s -> IO [Transferable]
getTransferables = mapM Object.getTransferable . toList . _objects

-- | Get scenario with no Objects inside.
--   Used to send scenario information to a geometry loader,
--   so that geometry loader has scenario context.
withoutObjects :: Scenario' s -> Scenario' Object.Prepared
withoutObjects = set objects mempty
               . set (viewState.objectGroups) mempty

-- * Lenses

name :: Functor f
     => (Maybe JSString -> f (Maybe JSString))
     -> Scenario' s -> f (Scenario' s)
name f s = (\x -> s{_name = x}) <$> f (_name s)

geoLoc :: Functor f
       => (Maybe (Double, Double, Double) -> f (Maybe (Double, Double, Double)))
       -> Scenario' s -> f (Scenario' s)
geoLoc f s = (\x -> s{_geoLoc = x}) <$> f (_geoLoc s)

srid :: Functor f
     => (Maybe Int -> f (Maybe Int))
     -> Scenario' s -> f (Scenario' s)
srid f s = (\x -> s{_srid = x}) <$> f (_srid s)


properties :: Functor f
           => (Properties -> f Properties)
           -> Scenario' s -> f (Scenario' s)
properties f s = (\x -> s{_properties = x}) <$> f (_properties s)

objects :: Functor f
        => (Object.Collection' s -> f (Object.Collection' t))
        -> Scenario' s -> f (Scenario' t)
objects f s = (\x -> s{_objects = x}) <$> f (_objects s)

objIdSeq :: Functor f
         => (ObjectId -> f ObjectId)
         -> Scenario' s -> f (Scenario' s)
objIdSeq f s = (\x -> s{_objIdSeq = x}) <$> f (_objIdSeq s)

viewState :: Functor f
          => (ScenarioState -> f ScenarioState)
          -> Scenario' s -> f (Scenario' s)
viewState f s = (\x -> s{_viewState = x}) <$> f (_viewState s)


instance Semigroup (Scenario' s) where
  scOld <> scNew =  Scenario
    {               -- trying to update scenario name if it has changed
      _name       = _name scNew <|> _name scOld
                    -- keeping GeoLocation from older version
    , _geoLoc     = _geoLoc scOld <|> _geoLoc scNew
                    -- keeping SRID from older version
    , _srid       = _srid scOld <|> _srid scNew
                    -- prefer duplicate properties from a new version
    , _properties = _properties scNew <> _properties scOld
                    -- replace older objects with newer ones
    , _objects    = _objects scNew <> _objects scOld
                    -- get maximum of objId counters to make sure
                    -- no objects could have the same object Id
    , _objIdSeq   = max (_objIdSeq scOld) (_objIdSeq scNew)
                    -- just get a new view state
    , _viewState  = _viewState scNew
    }
  stimes = stimesIdempotentMonoid

instance Monoid (Scenario' s) where
  mempty = Scenario
    { _name       = Nothing
    , _geoLoc     = Nothing
    , _srid       = Nothing
    , _properties = mempty
    , _objects    = mempty
    , _objIdSeq   = Object.ObjectId 0
    , _viewState  = def
    }
  mappend = (<>)

instance Default (Scenario' s) where
  def = mempty




-- * Special properties

defaultObjectHeight :: Functor f
                    => (Double -> f Double) -> Scenario' s -> f (Scenario' s)
defaultObjectHeight f = properties $ property "defaultObjectHeight" g
   where
     g Nothing  = Just <$> f 3.5
     g (Just c) = Just <$> f c

selectedDynamicColor :: Functor f
                     => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
selectedDynamicColor f = properties $ property "selectedDynamicColor" g
   where
     g Nothing  = Just <$> f "#FF6060FF"
     g (Just c) = Just <$> f c

selectedGroupColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
selectedGroupColor f = properties $ property "selectedGroupColor" g
   where
     g Nothing  = Just <$> f "#CC8888E0"
     g (Just c) = Just <$> f c

selectedStaticColor :: Functor f
                     => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
selectedStaticColor f = properties $ property "selectedStaticColor" g
   where
     g Nothing  = Just <$> f "#BB8888FF"
     g (Just c) = Just <$> f c

defaultStaticColor :: Functor f
                   => (HexColor -> f HexColor) -> Scenario' s -> f (Scenario' s)
defaultStaticColor f = properties $ property "defaultStaticColor" g
   where
     g Nothing  = Just <$> f "#808088E0"
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

viewDistance :: Functor f
             => (Maybe Float -> f (Maybe Float)) -> Scenario' s -> f (Scenario' s)
viewDistance = properties . property "viewDistance"

mapZoomLevel :: Functor f
             => (Int -> f Int) -> Scenario' s -> f (Scenario' s)
mapZoomLevel f = properties $ property "mapZoomLevel" g
   where
     g Nothing  = Just <$> f 15
     g (Just c) = Just <$> f c

mapOpacity :: Functor f
           => (Double -> f Double) -> Scenario' s -> f (Scenario' s)
mapOpacity f = properties $ property "mapOpacity" g
   where
     g Nothing  = Just <$> f 0.8
     g (Just c) = Just <$> f c

useMapLayer :: Functor f
            => (Bool -> f Bool) -> Scenario' s -> f (Scenario' s)
useMapLayer f = properties $ property "useMapLayer" g
   where
     g Nothing  = Just <$> f False
     g (Just c) = Just <$> f c

mapUrl :: Functor f
       => (JSString -> f JSString) -> Scenario' s -> f (Scenario' s)
mapUrl f = properties $ property "mapUrl" g
   where
     g Nothing  = Just <$> f "https://a.tile.openstreetmap.org/${z}/${x}/${y}.png"
     g (Just c) = Just <$> f c

hiddenProperties :: Functor f
                 => ([JSString] -> f [JSString]) -> Scenario' s -> f (Scenario' s)
hiddenProperties f = properties $ property "hiddenProperties" g
   where
     g Nothing  = Just <$> f [ "geomID", "groupID"
                             , "hiddenProperties", "viewColor"
                             , "height", "static", "selectable"
                             , "visible", "special"]
     g (Just c) = Just <$> f c

evaluationCellSize :: Functor f
                   => (Double -> f Double) -> Scenario' s -> f (Scenario' s)
evaluationCellSize f = properties $ property "evaluationCellSize" g
   where
     g Nothing  = Just <$> f 5.0
     g (Just c) = Just <$> f c



-- * Resolved properties

resolvedObjectColorIgnoreVisible :: Scenario' s -> Object.Object' t -> HexColor
resolvedObjectColorIgnoreVisible s o = o^.Object.viewColor.non sdef
  where
    sdef = case o^.Object.geometry of
      Geometry.Points _ -> s^.defaultPointColor
      Geometry.Lines  _ -> s^.defaultLineColor
      Geometry.Polygons  _ -> case o^.Object.objectBehavior of
        Object.Static  -> s^.defaultStaticColor
        Object.Dynamic -> s^.defaultBlockColor

-- | Resolve view color of object based on object and scenario properties.
resolvedObjectColor :: Scenario' s -> Object.Object' t -> HexColor
resolvedObjectColor s o
    = if o^.Object.selectable
      then resolvedObjectColorIgnoreVisible s o
      else "#00000000"

-- | Resolve object height to extrude it if necessary
resolvedObjectHeight :: Scenario' s -> Object.Object' t -> Double
resolvedObjectHeight s o = o^.Object.height.non (s^.defaultObjectHeight)



-- * Computed and updateable attributes

-- | Parsed settings for qua-view
data ScenarioState
  = ScenarioState
  { _cameraPos       :: !(Vec3f, Vec3f)
  , _objectGroups    :: !(Map.Map GroupId [ObjectId])
  , _clippingDist    :: !Float
  , _templates       :: !(Set.Set (Either ObjectId GroupId))
  , _forcedAreaObjId :: !(Maybe ObjectId)
  , _groupIdSeq      :: !GroupId
  } deriving Generic

instance FromJSVal ScenarioState
instance ToJSVal   ScenarioState
instance FromJSVal (Either ObjectId GroupId)
instance ToJSVal   (Either ObjectId GroupId)
instance FromJSVal (Set.Set (Either ObjectId GroupId)) where
  fromJSVal = fmap (fmap Set.fromDistinctAscList) . fromJSVal
instance ToJSVal   (Set.Set (Either ObjectId GroupId)) where
  toJSVal = toJSVal . Set.toAscList


instance Default ScenarioState where
  def = ScenarioState
    { _cameraPos       = (vec3 100 150 500, 0)
    , _objectGroups    = mempty
    , _clippingDist    = 2000
    , _templates       = mempty
    , _forcedAreaObjId = Nothing
    , _groupIdSeq      = 0
    }



cameraPos :: Functor f
          => ((Vec3f, Vec3f) -> f (Vec3f, Vec3f))
          -> ScenarioState -> f ScenarioState
cameraPos f s = (\x -> s{_cameraPos = x}) <$> f (_cameraPos s)

cameraLoc :: Functor f
          => (Vec3f -> f Vec3f)
          -> ScenarioState -> f ScenarioState
cameraLoc = cameraPos . _1

cameraLookAt :: Functor f
             => (Vec3f -> f Vec3f)
             -> ScenarioState -> f ScenarioState
cameraLookAt = cameraPos . _2

objectGroups :: Functor f
             => (Map.Map GroupId [ObjectId] -> f (Map.Map GroupId [ObjectId]))
             -> ScenarioState -> f ScenarioState
objectGroups f s = (\x -> s{_objectGroups = x}) <$> f (_objectGroups s)

clippingDist :: Functor f
             => (Float -> f Float)
             -> ScenarioState -> f ScenarioState
clippingDist f s = (\x -> s{_clippingDist = x}) <$> f (_clippingDist s)

templates :: Functor f
          => (Set.Set (Either ObjectId GroupId) -> f (Set.Set (Either ObjectId GroupId) ))
          -> ScenarioState -> f ScenarioState
templates f s = (\x -> s{_templates = x}) <$> f (_templates s)

forcedAreaObjId :: Functor f
                => (Maybe ObjectId -> f (Maybe ObjectId))
                -> ScenarioState -> f ScenarioState
forcedAreaObjId f s = (\x -> s{_forcedAreaObjId = x}) <$> f (_forcedAreaObjId s)


groupIdSeq :: Functor f
           => (GroupId -> f GroupId)
           -> ScenarioState -> f ScenarioState
groupIdSeq f s = (\x -> s{_groupIdSeq = x}) <$> f (_groupIdSeq s)

