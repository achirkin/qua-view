{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Settings
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Program.Settings
  ( Settings (..), defaultSettings, loadSettings
  , Profile (..)
  -- * JSON helpers
  , getProp, setProp, newObj, jsonParse, jsonStringify, fromProps
  ) where

import JsHs (JSVal, JSString, LikeJS (..))
import JsHs.WebGL (GLfloat)
import qualified JsHs.Array as JS
--import Data.Maybe (fromMaybe)


data Profile = Full | ExternalEditor | ExternalViewer deriving (Show, Eq)


data Settings = Settings
    { -- activeService     :: !ServiceBox
      --, availableServices :: ![ServiceBox]
      objectScale :: !(Maybe GLfloat)
      -- ^ default scale of city geometry
    , viewRoute   :: !(Maybe JSString)
      -- ^ url of the application
    , luciRoute   :: !(Maybe JSString)
      -- ^ url of luci websocket proxy
    , scenarioUrl :: !(Maybe JSString)
      -- ^ url of default scenario to load
    , profile :: !Profile
      -- ^ user profile -- defines editor capabilities
    , submitUrl :: !(Maybe JSString)
      -- ^ url to save scenario
    , loggingUrl :: !(Maybe JSString)
      -- ^ url to log actions
    } deriving (Show, Eq)

defaultSettings :: Settings
defaultSettings = Settings
        { -- activeService = isovistService -- radService
        -- , availableServices = [radService, isovistService]
        -- ,
          objectScale = Nothing
        , viewRoute   = Nothing
        , luciRoute   = Nothing
        , scenarioUrl = Nothing
        , profile     = Full
        , submitUrl   = Nothing
        , loggingUrl  = Nothing
        } --where radService = ServiceBox . Services.Radius $ vector3 0 3 5
        --        isovistService = ServiceBox (Services.Isovist Services.Area)

-- | Load settings from auxilary javascript
loadSettings :: IO Settings
loadSettings = asLikeJS <$> js_getQuaViewSettings

instance LikeJS "Object" Settings where
  asJSVal = undefined
  asLikeJS jsv = Settings
   { objectScale = getProp "objectScale" jsv
   , viewRoute   = getProp "viewRoute" jsv
   , luciRoute   = getProp "luciRoute" jsv
   , scenarioUrl = getProp "scenarioUrl" jsv
   , profile     = case getProp "profile" jsv :: Maybe JSString of
                    Just "edit" -> ExternalEditor
                    Just "view" -> ExternalViewer
                    _ -> Full
   , submitUrl   = getProp "submitUrl" jsv
   , loggingUrl  = getProp "loggingUrl" jsv
   }


foreign import javascript interruptible "getQuaViewSettings($c);"
  js_getQuaViewSettings :: IO JSVal



foreign import javascript unsafe "JSON.stringify($1)"
    jsonStringify :: JSVal -> JSString

foreign import javascript unsafe "JSON.parse($1)"
    jsonParse :: JSString -> JSVal

foreign import javascript unsafe "$r = {};"
    newObj :: JSVal


{-# INLINE getProp #-}
getProp :: LikeJS s a => JSString -> JSVal -> Maybe a
getProp name = asLikeJS . js_getProp name

foreign import javascript unsafe "$2[$1]"
    js_getProp :: JSString -> JSVal -> JSVal

{-# INLINE setProp #-}
setProp :: LikeJS s a => JSString -> a -> JSVal -> JSVal
setProp name = js_setProp name . asJSVal

foreign import javascript unsafe "$3[$1] = $2; $r = $3;"
    js_setProp :: JSString -> JSVal -> JSVal -> JSVal

fromProps :: [(JSString, JSVal)] -> JSVal
fromProps xs = js_fromProps (JS.fromList keys) (JS.fromList vals)
  where
    (keys, vals) = unzip xs

foreign import javascript unsafe "var r = {}; $1.forEach(function(e,i){ r[e] = $2[i];}); $r = r;"
  js_fromProps :: JS.Array JSString -> JS.Array JSVal -> JSVal

