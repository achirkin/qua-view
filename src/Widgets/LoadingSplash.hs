{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.LoadingSplash
    ( loadingSplash
    ) where

import Data.Semigroup
import Language.Haskell.TH
import Reflex.Dom

import Types

import qualified GHCJS.DOM.Element as Element
import Text.Hamlet
import Data.Text (Text)
import Control.Monad.IO.Class
import Text.Blaze.Html.Renderer.String

import Data.JSString (JSString)

import Widgets.Generation

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
    (e, _) <- elDynClass' "div" classDyn (text $ "Hello world! " <> elementId)
    setInnerHTML e
      $(qhtml
        [shamlet|
          <div>
            Hello
            <p># {felementId}
            <p>World!
          <svg id="loadingSplash" fill="none" xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 72 72">
            <style>
                #redCircle {
                  position: relative;
                  transform-origin: 36px 36px;
                  -ms-transform-origin: 36px 36px;
                  -moz-transform-origin: 36px 36px;
                  -webkit-transform-origin: 36px 36px;
                  -webkit-animation: rotRed 3s linear infinite;
                  -moz-animation: rotRed 3s linear infinite;
                  -ms-animation: rotRed 3s linear infinite;
                  -o-animation: rotRed 3s linear infinite;
                  animation: rotRed 3s linear infinite;
                }
                #greyCircle {
                  position: relative;
                  transform-origin: 36px 36px;
                  -ms-transform-origin: 36px 36px;
                  -moz-transform-origin: 36px 36px;
                  -webkit-transform-origin: 36px 36px;
                  -webkit-animation: rotGrey 8s linear infinite;
                  -moz-animation: rotGrey 8s linear infinite;
                  -ms-animation: rotGrey 8s linear infinite;
                  -o-animation: rotGrey 8s linear infinite;
                  animation: rotGrey 8s linear infinite;
                }

                @-webkit-keyframes rotRed /* Safari and Chrome */ {
                  from {
                    -ms-transform: rotate(0deg);
                    -moz-transform: rotate(0deg);
                    -webkit-transform: rotate(0deg);
                    -o-transform: rotate(0deg);
                    transform: rotate(0deg);
                  }
                  to {
                    -ms-transform: rotate(360deg);
                    -moz-transform: rotate(360deg);
                    -webkit-transform: rotate(360deg);
                    -o-transform: rotate(360deg);
                    transform: rotate(360deg);
                  }
                }
                @keyframes rotRed {
                  from {
                    -ms-transform: rotate(0deg);
                    -moz-transform: rotate(0deg);
                    -webkit-transform: rotate(0deg);
                    -o-transform: rotate(0deg);
                    transform: rotate(0deg);
                  }
                  to {
                    -ms-transform: rotate(360deg);
                    -moz-transform: rotate(360deg);
                    -webkit-transform: rotate(360deg);
                    -o-transform: rotate(360deg);
                    transform: rotate(360deg);
                  }
                }
                @-webkit-keyframes rotGrey /* Safari and Chrome */ {
                  from {
                    -ms-transform: rotate(360deg);
                    -moz-transform: rotate(360deg);
                    -webkit-transform: rotate(360deg);
                    -o-transform: rotate(360deg);
                    transform: rotate(360deg);
                  }
                  to {
                    -ms-transform: rotate(0deg);
                    -moz-transform: rotate(0deg);
                    -webkit-transform: rotate(0deg);
                    -o-transform: rotate(0deg);
                    transform: rotate(0deg);
                  }
                }
                @keyframes rotGrey {
                  from {
                    -ms-transform: rotate(360deg);
                    -moz-transform: rotate(360deg);
                    -webkit-transform: rotate(360deg);
                    -o-transform: rotate(360deg);
                    transform: rotate(360deg);
                  }
                  to {
                    -ms-transform: rotate(0deg);
                    -moz-transform: rotate(0deg);
                    -webkit-transform: rotate(0deg);
                    -o-transform: rotate(0deg);
                    transform: rotate(0deg);
                  }
                }

            <circle id="redCircle" stroke-opacity="1" cx="36" cy="36" stroke="#FF5722" r="28" stroke-dasharray="10, 5, 50, 40, 30.929188601, 40" stroke-width="16">
            <circle id="greyCircle" stroke-width="8" stroke-opacity=".2" stroke="#BF360C" cy="36" cx="36" stroke-dasharray="38, 14, 8, 14, 65.929188601, 14, 8, 14" r="28">
        |]
       )



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

elHtml :: MonadWidget t m => Text -> Text -> m ()
elHtml elementTag html = do
    (e, _) <- el' elementTag blank
    liftIO $ Element.setInnerHTML (_element_raw e) html
