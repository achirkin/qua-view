{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE RecordWildCards            #-}
-- | Run third-party analysis webservices
module Model.Scenario.ServicePlugin
    ( ServicePlugin, SPView (..)
    , spName, spURL, spIcon, spIconBgColor, spView
    ) where

import JavaScript.JSON.Types.Instances
import JavaScript.JSON.Types.Internal
import Commons.NoReflex
import Model.Scenario.Properties



data ServicePlugin = ServicePlugin
  { _spName        :: JSString
  , _spURL         :: JSString
  , _spIcon        :: JSString
  , _spIconBgColor :: HexColor
  , _spView        :: SPView
  } deriving Show

instance FromJSON ServicePlugin where
  parseJSON = withObject "ServicePlugin" $ \v -> ServicePlugin
    <$> v .:  "name"
    <*> v .:  "url"
    <*> v .:  "icon"
    <*> v .:? "iconBgColor" .!= "#FFFFFFFF"
    <*> v .:? "view" .!= SPVAuto

instance ToJSON ServicePlugin where
  toJSON ServicePlugin {..} = objectValue $ object
    [ "name"        .= _spName
    , "url"         .= _spURL
    , "icon"        .= _spIcon
    , "iconBgColor" .= _spIconBgColor
    , "view"        .= _spView
    ]

instance FromJSVal ServicePlugin where
    fromJSVal v = pure $ case fromJSON (SomeValue v) of
        Error   _ -> Nothing
        Success c -> Just c
    fromJSValUnchecked v = case fromJSON (SomeValue v) of
        Error   e -> error $ "fromJSValUnchecked ServicePlugin: " <> e
        Success c -> pure c
instance PToJSVal ServicePlugin where
    pToJSVal = pToJSVal . toJSON
instance ToJSVal ServicePlugin where
    toJSVal = pure . pToJSVal



data SPView
  = SPVModal
    -- ^ Show service in a modal window
  | SPVNewTab
    -- ^ Show service in a new tab
  | SPVAuto
    -- ^ Determine from the editor mode
  deriving (Eq, Show)

instance FromJSON SPView where
  parseJSON v = parseJSON v >>= \s -> case (s :: JSString) of
    "modal"  -> return SPVModal
    "newtab" -> return SPVNewTab
    "auto"   -> return SPVAuto
    _        -> fail $ "Unknown ServicePlugin view: " ++ show s

instance ToJSON SPView where
  toJSON SPVModal  = toJSON ("modal" :: JSString)
  toJSON SPVNewTab = toJSON ("newtab" :: JSString)
  toJSON SPVAuto   = toJSON ("auto" :: JSString)

instance PFromJSVal SPView where
    pFromJSVal v = case fromJSON $ SomeValue v of
      Error   _ -> SPVAuto
      Success m -> m
instance FromJSVal SPView where
    fromJSVal v = pure $ case fromJSON (SomeValue v) of
        Error   _ -> Nothing
        Success c -> Just c
    fromJSValUnchecked = pure . pFromJSVal
instance PToJSVal SPView where
    pToJSVal = pToJSVal . toJSON
instance ToJSVal SPView where
    toJSVal = pure . pToJSVal


spName :: Functor f
       => (JSString -> f JSString) -> ServicePlugin -> f ServicePlugin
spName f s = (\x -> s { _spName = x }) <$> f (_spName s)

spURL :: Functor f
      => (JSString -> f JSString) -> ServicePlugin -> f ServicePlugin
spURL f s = (\x -> s { _spURL = x }) <$> f (_spURL s)

spIcon :: Functor f
       => (JSString -> f JSString) -> ServicePlugin -> f ServicePlugin
spIcon f s = (\x -> s { _spIcon = x }) <$> f (_spIcon s)

spIconBgColor :: Functor f
               => (HexColor -> f HexColor) -> ServicePlugin -> f ServicePlugin
spIconBgColor f s = (\x -> s { _spIconBgColor = x }) <$> f (_spIconBgColor s)

spView :: Functor f
       => (SPView -> f SPView) -> ServicePlugin -> f ServicePlugin
spView f s = (\x -> s { _spView = x }) <$> f (_spView s)
