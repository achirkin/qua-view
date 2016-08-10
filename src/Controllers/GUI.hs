-----------------------------------------------------------------------------
-- |
-- Module      :  Controllers.GUI
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Foreign imports for all GUI html elements on qua-server side
--
-----------------------------------------------------------------------------

module Controllers.GUI
  ( registerLoadingFile
  , registerClearGeometry
  , displayScenarios
  , registerAskLuciForScenario
  , registerGetScenarioList
  , registerUserConnectToLuci
  , showLuciConnected
  , showLuciConnectForm
  ) where


import JsHs (JSString, JSVal, LikeJS(..))
import JsHs.Callback
import Data.Geometry.Structure.Feature



-- | Registers two callbacks; comes from Handler.Home.PanelGeometry.
--   onSuccess :: JSON -> IO ()
--   onFailure :: JSString -> IO ()
--   return :: IO ()
registerLoadingFile :: (Either JSString FeatureCollection -> IO ()) -> IO ()
registerLoadingFile callback = do
  callbackSuccess <- asyncCallback1 $ callback . Right . asLikeJS
  callbackFailure <- asyncCallback1 $ callback . Left . asLikeJS
  js_registerLoadingFile callbackSuccess callbackFailure
foreign import javascript safe "registerLoadingFile($1,$2)"
  js_registerLoadingFile :: Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelGeometry.
--   onClick :: IO ()
--   return :: IO ()
registerClearGeometry :: (() -> IO ()) -> IO ()
registerClearGeometry callback = do
  call <- asyncCallback $ callback ()
  js_registerClearGeometry call
foreign import javascript safe "registerClearGeometry($1)"
  js_registerClearGeometry :: Callback (IO ()) -> IO ()


-- | Call this when scenarios are parsed; comes from Handler.Home.PanelGeometry.
--   xs :: [{ScenarioDescription, as-is}]
--   return :: IO ()
foreign import javascript safe "displayScenarios($1['scenarios'])" displayScenarios :: JSVal -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelGeometry.
--   h :: ScID -> IO ()
--   return :: IO ()
registerAskLuciForScenario :: (Int -> IO ()) -> IO ()
registerAskLuciForScenario c = asyncCallback1 (c . asLikeJS) >>= js_registerAskLuciForScenario
foreign import javascript safe "registerAskLuciForScenario($1)" js_registerAskLuciForScenario :: Callback (JSVal -> IO ()) -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelGeometry.
--   onClick :: IO ()
--   return :: IO ()
registerGetScenarioList :: (() -> IO ()) -> IO ()
registerGetScenarioList c = asyncCallback (c ()) >>= js_registerGetScenarioList
foreign import javascript safe "registerGetScenarioList($1)" js_registerGetScenarioList :: Callback (IO ()) -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelServices.
--   onClick :: JSString -> IO () -- address of websocket host
--   return :: IO ()
registerUserConnectToLuci :: (JSString -> IO ()) -> IO ()
registerUserConnectToLuci c = asyncCallback1 (c . asLikeJS) >>= js_registerUserConnectToLuci
foreign import javascript safe "registerUserConnectToLuci($1)" js_registerUserConnectToLuci :: Callback (JSVal -> IO ()) -> IO ()


-- | Display "luci connected message"; comes from Handler.Home.PanelServices.
--   connectedHost :: JSString -- address of websocket host
--   return :: IO ()
foreign import javascript safe "showLuciConnected($1)" showLuciConnected :: JSString -> IO ()


-- | Display "connect to luci" form; comes from Handler.Home.PanelServices.
--   defaultHost :: JSString -- default address of websocket host
--   return :: IO ()
foreign import javascript safe "showLuciConnectForm($1)" showLuciConnectForm :: JSString -> IO ()


