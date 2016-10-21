-----------------------------------------------------------------------------
-- |
-- Module      :  Program.Controllers.GUI
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
-- Foreign imports for all GUI html elements on qua-server side
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Program.Controllers.GUI
  ( registerLoadingFile
  , registerClearGeometry
  , displayScenarios
  , registerAskLuciForScenario
  , registerGetScenarioList
  , registerUserConnectToLuci
  , showLuciConnected
  , showLuciConnecting
  , showLuciConnectForm
  , registerSaveScenario
  , toggleSaveScenarioButton
  , registerServiceClear
  , registerServiceRun
  , toggleServiceClear
  , registerColorizeProperty
  , showInfo
  , registerSubmit
  , registerResetCamera
  ) where

--import Control.Concurrent.MVar
import JsHs (JSString, JSVal, LikeJS(..))
import JsHs.Callback
import Data.Geometry.Structure.Feature
import Program.Types



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
registerAskLuciForScenario :: (ScenarioId -> ScenarioName -> IO ()) -> IO ()
registerAskLuciForScenario c = asyncCallback2 (\i s -> c (asLikeJS i) (asLikeJS s)) >>= js_registerAskLuciForScenario
foreign import javascript safe "registerAskLuciForScenario($1)" js_registerAskLuciForScenario :: Callback (JSVal -> JSVal ->  IO ()) -> IO ()


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


-- | Display "luci connecting message"; comes from Handler.Home.PanelServices.
--   connectedHost :: JSString -- address of websocket host
--   return :: IO ()
foreign import javascript safe "showLuciConnecting($1)" showLuciConnecting :: JSString -> IO ()


-- | Display "connect to luci" form; comes from Handler.Home.PanelServices.
--   defaultHost :: JSString -- default address of websocket host
--   return :: IO ()
foreign import javascript safe "showLuciConnectForm($1)" showLuciConnectForm :: JSString -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelGeometry.
--   sendMsg :: JSString -> IO ()
--   return :: IO ()
registerSaveScenario :: (ScenarioName -> IO ()) -> IO ()
registerSaveScenario c = asyncCallback1 (c . asLikeJS) >>= js_registerSaveScenario
foreign import javascript safe "registerSaveScenario($1)" js_registerSaveScenario :: Callback (JSVal -> IO ()) -> IO ()


-- | call it to setup scenario buttons state; comes from Handler.Home.PanelGeometry.
--   showButton :: Bool -- whether to show "save scenario" button
--   scName :: JSString -- name of the scenario displayed on a panel
--   return :: IO ()
foreign import javascript safe "toggleSaveScenarioButton($1, $2)" toggleSaveScenarioButton :: Bool -> ScenarioName -> IO ()


-- | Registers one callback; comes from Handler.Home.UIButtons.
--   onClick :: IO ()
--   return :: IO ()
registerServiceClear :: (() -> IO ())  -> IO ()
registerServiceClear c = asyncCallback (c ()) >>= js_registerServiceClear
foreign import javascript safe "registerServiceClear($1)" js_registerServiceClear  :: Callback (IO ()) -> IO ()

-- | Registers one callback; comes from Handler.Home.UIButtons.
--   onClick :: IO ()
--   return :: IO ()
registerServiceRun :: (() -> IO ()) -> IO ()
registerServiceRun c = asyncCallback (c ()) >>= js_registerServiceRun
foreign import javascript safe "registerServiceRun($1)" js_registerServiceRun :: Callback (IO ()) -> IO ()

-- | Shows or hides button "clear"; comes from Handler.Home.UIButtons.
--   state :: Bool
--   return :: IO ()
foreign import javascript safe "toggleServiceClear($1)" toggleServiceClear :: Bool -> IO ()


-- | Registers one callback; comes from Handler.Home.PanelInfo.
--   f :: JSString -> IO ()
--   return :: IO ()
registerColorizeProperty :: (Maybe JSString -> IO ()) -> IO ()
registerColorizeProperty c = asyncCallback1 (c . f . asLikeJS) >>= js_registerColorizeProperty
  where
    f (Just "") = Nothing
    f x = x
foreign import javascript safe "registerColorizeProperty($1)" js_registerColorizeProperty :: Callback (JSVal -> IO ()) -> IO ()

-- | Show info (pairs of key-value); comes from Handler.Home.PanelInfo.
--   obj :: Object -- all property names and values inside an object
--   return :: IO ()
foreign import javascript safe "showInfo($1)" showInfo :: JSVal -> IO ()


-- | Registers one callback; comes from Handler.Home.UIButtons.
--   onClick ::  (submitUrl -> FeatureCollection -> Image -> IO ()) -> IO ()
--   return :: IO ()
registerSubmit :: (((JSString, FeatureCollection, JSVal) -> IO ()) -> IO ()) -> IO ()
registerSubmit c =  asyncCallback1  (c . (\f (u,d,i) -> f u d i) . js_uncallback3) >>= js_registerSubmit
foreign import javascript safe "registerSubmit($1)" js_registerSubmit :: Callback (JSVal -> IO ()) -> IO ()
foreign import javascript safe "$1($2,$3,$4)"
  js_uncallback3 :: JSVal -> JSString -> FeatureCollection -> JSVal -> IO ()


-- | Registers one callback; comes from Handler.Home.UIButtons.
--   onClick :: IO ()
--   return :: IO ()
registerResetCamera :: (() -> IO ())  -> IO ()
registerResetCamera c = asyncCallback (c ()) >>= js_registerResetCamera
foreign import javascript safe "registerResetCamera($1)" js_registerResetCamera  :: Callback (IO ()) -> IO ()
