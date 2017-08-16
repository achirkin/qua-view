{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Widgets.Logger
    ( loggerWidget
    ) where


import Reflex.Dom
import Control.Monad.Reader
import qualified Data.JSString as JSString (append)

import qualified GHCJS.DOM.JSFFI.Generated.Document as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.Node as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.HTMLElement as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.Element as JSFFI
import qualified JavaScript.JSON.Types.Internal as JSON

import Commons
import Widgets.Generation


-- | This widget shows message log on control panel;
--   It returns a logger function that can be used to create `MonadLoger` instance using `ReaderT`.
loggerWidget :: Reflex t => Widget x LoggerFunc
loggerWidget = do
    (msgE, msgCallback) <- newTriggerEvent
    (consoleEl, ()) <- elAttr "div" ("id" =: consoleDivId) $
       elAttr' "div" ("id" =: consoleDivContentId) blank
    doc <- askDocument
    void $ accumM @_ @(Event _)
                  (\msgs m -> do
                        msgs' <- updateMessageNodes (_element_raw consoleEl) msgs
                        m' <- createMessageNode doc (_element_raw consoleEl) m
                        return $ m' : msgs'
                  ) [] msgE
    return . makeWidgetLogger $ \ll str -> msgCallback . encodeMessage ll str
  where
    encodeMessage :: LogLevel -> JSString -> Maybe JSVal -> JSString
    encodeMessage _ str Nothing = str
    encodeMessage _ str (Just jv) = str `JSString.append` "; " `JSString.append` JSON.encode (JSON.SomeValue jv)
    colors = fmap (\c -> "color: " `JSString.append` c `JSString.append` ";")
      [ "#3E2723", "#4E342E", "#5D4037", "#6D4C41", "#795548"
      , "#8D6E63", "#A1887F", "#BCAAA4", "#D7CCC8", "#EFEBE9"
      ] :: [JSString]
    createMessageNode doc parent msg = do
        e <- JSFFI.HTMLElement . JSFFI.unElement <$> JSFFI.createElement doc ("div" :: JSString)
        JSFFI.setAttribute e ("style" :: JSString) (head colors)
        JSFFI.setInnerText e (msg :: JSString)
        JSFFI.appendChild_ parent e
        return (tail colors, e)
    updateMessageNode parent ([], e) = Nothing <$ JSFFI.removeChild_ parent e
    updateMessageNode _ (c:cs, e) = do
        JSFFI.setAttribute e ("style" :: JSString) c
        return $ Just (cs, e)
    updateMessageNodes _ [] = pure []
    updateMessageNodes parent (n:ns) = do
        mr <- updateMessageNode parent n
        case mr of
          Nothing -> updateMessageNodes parent ns
          Just r  -> (r:) <$> updateMessageNodes parent ns
    (consoleDivId, consoleDivContentId) = $(do
        consolediv <- newVar
        consoledivcontent <- newVar
        qcss
          [cassius|
            ##{consolediv}
                z-index: 3
                width: 100%
                text-align: justify
                line-height: 100%
                font-size: 9pt
                position: absolute
                bottom: 0
                left: 0
                overflow: hidden
                padding: 0
                margin: 0

            ##{consoledivcontent}
                z-index: 3
                height: 100%
                overflow: hidden
                padding: 0
                margin: 0 5px 3px 32px

            ##{consoledivcontent} div
                z-index: 4
                padding: 0
                margin: 0 0 5px 0
          |]

        returnVars [consolediv, consoledivcontent]
      )



