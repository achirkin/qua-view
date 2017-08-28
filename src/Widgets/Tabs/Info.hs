{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import Widgets.Commons
import Reflex.Dom

panelInfo :: (Reflex t) => WidgetWithLogs x ()
panelInfo = writeReview


data ThumbState = None | ThumbUp | ThumbDown deriving Show


writeReview :: (Reflex t) => WidgetWithLogs x ()
writeReview = elClass "div" "card-comment form-group form-group-label" $ do
  let wrIdent = "writeReview"
  elAttr "label" ("class" =: "floating-label" <> "for" =: wrIdent)
    $ text "Write a review"
  let attrs = constDyn ("class" =: "form-control")
  t <- textArea $ def & textAreaConfig_attributes .~ attrs
  dynText $ _textArea_value t
  (upEl,   ()) <- elClass' "span" "icon" $ text "thumb_up"
  (downEl, ()) <- elClass' "span" "icon" $ text "thumb_down"
  stateD <- holdDyn None $ leftmost [
      ThumbUp   <$ domEvent Click upEl
    , ThumbDown <$ domEvent Click downEl ]
  let thumbToHideState None = Inactive
      thumbToHideState _    = Active
  clickE <- buttonFlatHideDyn (fmap thumbToHideState stateD) "Send" mempty
  httpPost "/mooc/proposals/review/#ScenarioId" clickE
  return ()
