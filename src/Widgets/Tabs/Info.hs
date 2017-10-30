{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.JSString.Text (textFromJSString, textToJSString)
import Data.Text
import Data.Time.Format
import Reflex.Dom
import Widgets.Commons
import Widgets.Generation


panelInfo :: Reflex t
          => ReaderT (Dynamic t Settings) (WidgetWithLogs x) ()
panelInfo = do
  settingsD <- ask
  eitherReviewSettingsE <- httpGetNowOrOnUpdate $ reviewSettingsUrl
                                               <$> settingsD
  reviewSettingsE <- lift $ renderError eitherReviewSettingsE
  reviewSettingsD <- holdDyn Nothing $ Just <$> reviewSettingsE
  void $ lift $ dyn $ renderPanelInfo <$> reviewSettingsD

renderPanelInfo :: Reflex t
                => Maybe ReviewSettings -> WidgetWithLogs x ()
renderPanelInfo Nothing = blank
renderPanelInfo (Just reviewSettings) = do
    responseE <- renderWriteReview reviewSettings
    reviewsD  <- accum accumRevs (reviews reviewSettings) responseE
    let crits = criterions reviewSettings
    void . dyn $ mapM_ (renderReview crits) <$> reviewsD
  where
    accumRevs revs (Right newRev) = newRev:revs
    accumRevs revs (Left _)       = revs

-- draws the write-review-text-entry component if ReviewSettings contains `Just reviewsUrl`.
-- Returns event of posted review, or error on unsuccessful post.
renderWriteReview :: Reflex t
                  => ReviewSettings
                  -> WidgetWithLogs x (Event t (Either JSError TReview))
renderWriteReview (ReviewSettings crits _ (Just revsUrl)) =
  elClass "div" ("card " <> writeReviewClass) $
    elClass "div" "form-group form-group-label" $ mdo
      textD  <- renderTextArea resetTextE "Write a review"
      critD  <- renderCriterions crits
      thumbD <- renderThumbs

      let hideStateD = hideState <$> critD <*> thumbD
      clickE <- buttonFlatHideDyn hideStateD "Send" mempty

      let postDataOnClickE = ReviewPost <$> fromJust <$> current critD
                                        <*> current thumbD
                                        <*> current textD
                                        <@ clickE
      responseE <- httpPost $ (,) revsUrl <$> postDataOnClickE
      let reset (Right _) = Just ""
          reset _         = Nothing
      let resetTextE = fmapMaybe reset responseE

      _ <- renderError responseE
      return responseE
  where
    writeReviewClass = $(do
        reviewCls <- newVar
        qcss
          [cassius|
            .#{reviewCls}
                padding: 10px
                margin: 0 10px 10px 10px
              .form-group
                  margin: 8px 0 0 0
                  width: 100%
              textarea
                  resize: vertical
              .btn-flat
                  padding: 0
                  line-height: 14px
          |]
        returnVars [reviewCls]
      )
renderWriteReview _ = return never

renderReview :: Reflex t
             => [TCriterion] -> TReview -> WidgetWithLogs x ()
renderReview crits r = elClass "div" ("card " <> reviewClass) $
  el "div" $ do
    renderCrit $ tReviewCriterionId r
    elClass "span" "icon" $ text $ showThumb $ tReviewThumb r
    text $ pack $ ' ' : (formatTime defaultTimeLocale "%F, %R - " $ tReviewTimestamp r)
    text $ textFromJSString $ tReviewUserName r <> ": "
    el "p" $ text $ textFromJSString $ tReviewComment r
  where
    renderCrit critId =
      void $ for [ c | c <- crits, critId == tCriterionId c] $ \c -> do
        (spanEl, ()) <- elClass' "span" critClass $ return ()
        setInnerHTML spanEl $ tCriterionIcon c
    (reviewClass, critClass) = $(do
        reviewCls <- newVar
        critCls   <- newVar
        qcss
          [cassius|
            .#{reviewCls}
                padding: 10px
                margin-right: 10px
                margin-left:  10px
              .#{critCls}
                  position: relative
                  top: 5px
          |]
        returnVars [reviewCls, critCls]
      )

-- either renders the `JSError` or fires the returned event which contains `a`
renderError :: Reflex t
            => Event t (Either JSError a)
            -> WidgetWithLogs x (Event t a)
renderError event = do
  let (errE, resultE) = fanEither event
  holdDyn Nothing (Just <$> errE) >>= void . dyn . fmap renderErr
  return resultE
  where
    renderErr (Just err) = el "div" $ text $ textFromJSString $ getJSError err
    renderErr Nothing    = blank

-- render bootstrapified textarea and return dynamic of text it contains
renderTextArea :: Reflex t
               => Event t Text
               -> Text
               -> WidgetWithLogs x (Dynamic t JSString)
renderTextArea setValE label = do
  elAttr "label" ("class" =: "floating-label") $ text label
  let config = TextAreaConfig "" setValE $ constDyn ("class" =: "form-control")
  t <- textArea config
  return $ textToJSString <$> _textArea_value t

-- render supplied criterios and return dynamic with criterionId of selected one
renderCriterions :: Reflex t
                 => [TCriterion] -> WidgetWithLogs x (Dynamic t (Maybe Int))
renderCriterions crits = elClass "span" critsClass $ mdo
    let critE = leftmost critEs
    critEs <- for crits $ \c -> do
      let critId        = tCriterionId c
          chooseStyle mi _
            | Just i <- mi, i == critId = activeStyle
            | otherwise                 = inactiveStyle
      critAttrD <- foldDyn chooseStyle inactiveStyle critE
      (spanEl, ()) <- elDynAttr' "span" critAttrD $ return ()
      setInnerHTML spanEl $ tCriterionIcon c
      return $ (Just critId) <$ domEvent Click spanEl
    holdDyn Nothing critE
  where
    critsClass = $(do
        critsCls <- newVar
        qcss
          [cassius|
            .#{critsCls}
                position: relative
                top: 5px
                margin-right: 15px
              span
                  cursor: pointer
                  &:hover
                      opacity: 1 !important
          |]
        returnVars [critsCls]
      )

-- render thumb-up and -down buttons and return dynamic of their state
renderThumbs :: Reflex t
             => WidgetWithLogs x (Dynamic t ThumbState)
renderThumbs = elClass "span" thumbsClass $ mdo
    let thumbE = leftmost [
            ThumbUp   <$ domEvent Click upEl
          , ThumbDown <$ domEvent Click dnEl
          ]
    let makeThumb upOrDown = do
          let chooseStyle th _
                | th == upOrDown = activeStyle
                | otherwise      = inactiveStyle
          thumbAttrD <- foldDyn chooseStyle inactiveStyle thumbE
          fst <$> elDynAttr' "span" (fmap (<> "class" =: "icon") thumbAttrD)
                    (text $ showThumb upOrDown)
    upEl <- makeThumb ThumbUp
    dnEl <- makeThumb ThumbDown
    holdDyn None thumbE
  where
    thumbsClass = $(do
        thumbsCls <- newVar
        qcss
          [cassius|
            .#{thumbsCls}
                margin-right: 10px
              >span
                  margin-right: 5px
                  cursor: pointer
                  &:hover
                      opacity: 1 !important
          |]
        returnVars [thumbsCls]
      )

showThumb :: ThumbState -> Text
showThumb ThumbUp   = "thumb_up"
showThumb ThumbDown = "thumb_down"
showThumb None      = "none"

hideState :: Maybe a -> ThumbState -> ComponentState s
hideState (Just _) ThumbUp   = Active
hideState (Just _) ThumbDown = Active
hideState _        _         = Inactive

inactiveStyle :: Map Text Text
inactiveStyle = "style" =: "opacity: 0.3"

activeStyle :: Map Text Text
activeStyle   = "style" =: "opacity: 1"
