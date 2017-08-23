{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.Scenario.Properties
    ( Properties, PropName(..), PropValue
    , toPropValue, fromPropValue, propValue
    , property, propertyWithParsing
    , HexColor, colorVeci, colorVecf
    ) where


import qualified Data.Map.Strict as Map
import Data.String (IsString(..))
import Data.Word
import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Internal
import Numeric.DataFrame
import Unsafe.Coerce

import Commons

-- | Properties is a map JSString -> JSON-like Value
type Properties = Map PropName PropValue

-- | Property name is a plane JSString
newtype PropName = PropName { _unPropName :: JSString }
  deriving ( PFromJSVal, PToJSVal, ToJSVal
           , FromJSON, ToJSON, ToJSString, IsString, Show, Eq, Ord)

-- | Property can be anything, but not all operations are allowed on all properties.
newtype PropValue = PropValue { _unPropValue :: JSVal }
  deriving ( PFromJSVal, PToJSVal, ToJSVal, FromJSON)

-- | We assume that only "proper" JSON values are stored in PropValue,
--   because they come from parsed JSON.
instance ToJSON PropValue where
  toJSON = SomeValue . _unPropValue


toPropValue :: ToJSON a => a -> PropValue
toPropValue = coerce . toJSON

fromPropValue :: FromJSON a => PropValue -> Maybe a
fromPropValue p = case fromJSON (coerce p) of
    Error   _ -> Nothing
    Success r -> Just r


-- | Lens into property value being converted from JSON.
propValue :: (Functor f, FromJSON a, ToJSON a)
          => (Maybe a -> f a) -> PropValue -> f PropValue
propValue f v =  toPropValue <$> f (fromPropValue v)


-- | Lens into peroperty value by its name inside a Properties map.
property :: (Functor f, FromJSON a, ToJSON a)
         => PropName
         -> (Maybe a -> f (Maybe a))
         -> Properties -> f Properties
property pName f m = g <$> f (Map.lookup pName m >>= fromPropValue)
    where
      g Nothing  = m
      g (Just v) = Map.insert pName (toPropValue v) m

propertyWithParsing :: (Functor f, FromJSONOrString a, ToJSON a)
                    => PropName
                    -> (Maybe a -> f (Maybe a))
                    -> Properties -> f Properties
propertyWithParsing pName f m = g <$> f (Map.lookup pName m >>= parseJSONOrString . coerce)
    where
      g Nothing  = m
      g (Just v) = Map.insert pName (toPropValue v) m

class FromJSONOrString a where
    parseJSONOrString :: Value -> Maybe a

instance FromJSONOrString Bool where
    parseJSONOrString v = case match v of
        Bool b -> pure b
        String "true"  -> pure True
        String "false" -> pure False
        _ -> Nothing

instance FromJSONOrString Double where
    parseJSONOrString v = case match v of
        Number x -> Just x
        String s -> nullableToMaybe $ js_parseDouble s
        _ -> Nothing

foreign import javascript unsafe
    "var a = parseFloat($1); $r = isNaN(a) ? null : a;"
    js_parseDouble :: JSString -> Nullable Double



instance PFromJSVal Properties where
    pFromJSVal v = case fromJSON $ SomeValue v of
      Error   _ -> mempty
      Success m -> m
instance PToJSVal Properties where
    pToJSVal = coerce . objectValue . object . fmap (_unPropName *** coerce) . Map.assocs

instance FromJSON Properties where
    parseJSON = withObject "Properties object"
       (pure . Map.fromList . fmap (PropName *** coerce) . objectAssocs)
instance ToJSON Properties where
    toJSON = objectValue . object . fmap (_unPropName *** coerce) . Map.assocs



-- | #RGBA Color represented as hex string
newtype HexColor = HexColor { _unHexColor :: JSString }
    deriving (PToJSVal, ToJSVal, ToJSON, ToJSString, IsString, Show, Eq, Ord)

instance FromJSON HexColor where
    parseJSON = withJSString "Hex-encoded color"
        (\s -> if js_isHexColor s then pure (HexColor s) else fail "Not a valid hex #RGBA string.")

instance FromJSVal HexColor where
    fromJSVal v = pure $ case fromJSON (SomeValue v) of
        Error   _ -> Nothing
        Success c -> Just c

-- | View hex color as a 4D vector of word components (0..255)
colorVeci :: Functor f => (Vector Word8 4 -> f (Vector Word8 4)) -> HexColor -> f HexColor
colorVeci f c = js_convertRGBAToHex . unsafeCoerce <$> f (unsafeCoerce $ js_convertHexToRGBA c)


-- | View hex color as a 4D vector of float components (0..1)
colorVecf :: Functor f => (Vec4f -> f Vec4f) -> HexColor -> f HexColor
colorVecf f = colorVeci g
    where
      g = fmap (ewmap (scalar . round . min 255 . max 0 . (255*) . unScalar))
        . f
        . ewmap (scalar . (/255) . fromIntegral . unScalar)


foreign import javascript unsafe "($1 && ($1.match(/^(#[A-Fa-f0-9]{3,8})$/) !== null))"
    js_isHexColor ::  JSString -> Bool

foreign import javascript unsafe
    "var a = [0,0,0,255];\
    \if($1.length > 5) { \
    \   $r = new Uint8Array(a.map(function(e,i){ return parseInt($1.substr(i*2+1,2),16); }));\
    \} else { \
    \   $r = new Uint8Array(a.map(function(e,i){ return parseInt($1.substr(i+1,1),16) * 17; }));\
    \}"
    js_convertHexToRGBA :: HexColor -> JSVal

foreign import javascript unsafe
    "($1).reduce(function(a, x)\
      \{return a.concat(('00').concat(x.toString(16)).substr(-2));}, '#')"
    js_convertRGBAToHex :: JSVal -> HexColor

