{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import Control.Monad.Trans.Reader
import Data.JSString.Text (textFromJSString, textToJSString)
import Reflex.Dom
import Widgets.Commons


panelInfo :: Reflex t
          => ReaderT (Dynamic t Settings) (WidgetWithLogs x) ()
panelInfo = do
  settingsD <- ask
  reviewSettingsE <- httpGet $ fmapMaybe id $
                       reviewSettingsUrl <$> updated settingsD

  let rightToMaybe = either (const Nothing) Just
  reviewSettingsD <- holdDyn Nothing $ rightToMaybe <$> reviewSettingsE

  let w = writeReview <$> reviewSettingsD
  void $ lift $ dyn w

writeReview :: Reflex t
            => Maybe ReviewSettings -> WidgetWithLogs x ()
writeReview Nothing = blank
writeReview (Just reviewSettings) =
  elClass "div" "card-comment form-group form-group-label" $ do
    let wrIdent = "writeReview"
    elAttr "label" ("class" =: "floating-label" <> "for" =: wrIdent)
      $ text "Write a review"
    let attrs = constDyn ("class" =: "form-control")
    t <- textArea $ def & textAreaConfig_attributes .~ attrs
    let textD = textToJSString <$> _textArea_value t
    (upEl,   ()) <- elClass' "span" "icon" $ text "thumb_up"
    (downEl, ()) <- elClass' "span" "icon" $ text "thumb_down"
    thumbD <- holdDyn None $ leftmost [
        ThumbUp   <$ domEvent Click upEl
      , ThumbDown <$ domEvent Click downEl ]
    let thumbToHideState None = Inactive
        thumbToHideState _    = Active
    clickE <- buttonFlatHideDyn (fmap thumbToHideState thumbD) "Send" mempty
    let dataOnClickE = (\textVal thumbVal -> ReviewPost thumbVal textVal)
                         <$> current textD
                         <*> current thumbD
                         <@ clickE
    responseE <- httpPost $ (,) (postReviewUrl reviewSettings) <$> dataOnClickE
    let rightToMaybe = either (const Nothing) Just
    responseD <- holdDyn Nothing $ rightToMaybe <$> responseE
    renderReview responseD
    return ()

renderReview :: Reflex t
             => Dynamic t (Maybe ReviewPost) -> WidgetWithLogs x ()
renderReview mreviewD = elClass "div" "" $ do
  let dispReview (Just r) = reviewPostComment r
      dispReview Nothing  = ""
  dynText $ textFromJSString <$> dispReview <$> mreviewD
