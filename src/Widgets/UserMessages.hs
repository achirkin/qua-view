{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Widgets.UserMessages
    ( userMessageWidget
    , UserProgressCallback (..), UserMessage (..)
    ) where


import Reflex.Dom
import Control.Monad.Reader
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import qualified Data.JSString as JSString (append)

import qualified GHCJS.DOM.JSFFI.Generated.Document as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.Node as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.HTMLElement as JSFFI
import qualified GHCJS.DOM.JSFFI.Generated.Element as JSFFI

import Commons
import Widgets.Generation




-- | This widget shows message log on control panel;
--   It returns a logger function that can be used to create `MonadLoger` instance using `ReaderT`.
userMessageWidget :: Reflex t => QuaWidget t x UserMessageCallback
userMessageWidget = do
    (msgE, msgCallback) <- newTriggerEvent
    (consoleEl, ()) <- elAttr "div" ("id" =: consoleDivId) $
       elAttr' "div" ("id" =: consoleDivContentId) blank
    doc <- askDocument
    evs <- accumM @_ @(Event _)
                  (\msgs (m, mgetCbks) -> do
                        msgs' <- updateMessageNodes (_element_raw consoleEl) msgs
                        m' <- createMessageNode doc (_element_raw consoleEl) m mgetCbks
                        return $ m' : msgs'
                  ) [] msgE
    -- TODO: currently, I have to do this weird perform event to make sure accumM executes well.
    performEvent_ $ flip seq (pure ()) <$> evs
    return $ UserMessageCallback (makeUMsgCallback msgCallback)
  where

    makeUMsgCallback :: ((JSString, Maybe (UserProgressCallback -> IO ())) -> IO ())
                     -> UserMessage s -> IO s
    makeUMsgCallback f (SingleMsg msg) = f (msg, Nothing)
    makeUMsgCallback f (ProgressMsg msg) = do
        cbksVar <- newEmptyMVar
        f (msg, Just (putMVar cbksVar))
        unsafeInterleaveIO $ takeMVar cbksVar

    colors = fmap (\c -> "color: " `JSString.append` c `JSString.append` ";")
      [ "#3E2723", "#4E342E", "#5D4037", "#6D4C41", "#795548"
      , "#8D6E63", "#A1887F", "#BCAAA4", "#D7CCC8", "#EFEBE9"
      ] :: [JSString]

    createMessageNode doc parent msg mbCbks = do
        e <- JSFFI.HTMLElement . JSFFI.unElement <$> JSFFI.createElement doc ("div" :: JSString)
        JSFFI.setAttribute e ("style" :: JSString) (head colors)
        JSFFI.setInnerText e (msg :: JSString)
        JSFFI.appendChild_ parent e
        isInProgressRef <- liftIO $ case mbCbks of
          Nothing -> newIORef False
          Just getCbks -> do
            isipr <- newIORef True
            getCbks UserProgressCallback
              { uMsgProgress = whenRef isipr . JSFFI.setInnerText e
              , uMsgComplete = \m -> whenRef isipr $ do
                    writeIORef isipr False
                    JSFFI.setInnerText e m
              } :: IO ()
            return isipr
        return (tail colors, e, isInProgressRef)
    -- update state of one log record given
    --   parentNode (listOfColors, record, isInProgress)
    -- when list of colors is empty, record is deleted from parent
    updateMessageNode parent ([], e, isInProgressRef) =
       liftIO (readIORef isInProgressRef) >>= \isInProgress ->
         if isInProgress
         then return $ Just ([], e, isInProgressRef)
         else Nothing <$ JSFFI.removeChild_ parent e
    updateMessageNode _ (c:cs, e, isInProgressRef) = do
        JSFFI.setAttribute e ("style" :: JSString) c
        return $ Just (cs, e, isInProgressRef)
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
                font-size: 9pt
                line-height: 1
                box-shadow: 0 -3px 5px #e5e5e5
                z-index: 0

            ##{consoledivcontent}
                margin: 5px 5px 5px 32px

            ##{consoledivcontent} div
                margin: 0 0 5px 0
          |]

        returnVars [consolediv, consoledivcontent]
      )

whenRef :: IORef Bool -> IO () -> IO ()
whenRef r a = readIORef r >>= \b -> when b a
