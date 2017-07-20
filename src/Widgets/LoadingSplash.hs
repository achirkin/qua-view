{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Widgets.LoadingSplash
    ( loadingSplash
    ) where

import Data.Semigroup
import Language.Haskell.TH
import Reflex.Dom

import Types

--( DOM.IsElement el, MonadIO m, MonadIO (Performable m), Reflex t
--                   , MonadHold t m, TriggerEvent t m, PerformEvent t m
--                   )
-- | When the incoming event is Busy, it show loading splash.
--   It hides as soon as Idle event is coming.
loadingSplash :: Reflex t => Event t (IsBusy "program") -> Widget x ()
loadingSplash isBusyE = do
    let elementId = $(LitE . StringL . show <$> newName "qc")
        classIdle = elementId <> "-idle"
        classBusy = elementId <> "-busy"
        toClass Busy = classBusy
        toClass Idle = classIdle
    classDyn <- holdDyn classIdle (toClass <$> isBusyE)
    elDynClass "div" classDyn (text $ "Hello world! " <> elementId)


---- | Display loading splash
--programInProgress :: IO ()
--programInProgress = programInProgress' >> threadDelay 0
--
--foreign import javascript interruptible "document.getElementById('loadingSplash').style.display = 'block';setTimeout($c(), 0);"
--    programInProgress' :: IO ()
--
---- | Hide loading splash
--foreign import javascript unsafe "document.getElementById('loadingSplash').style.display = 'none';"
--    programIdle :: IO ()
