{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import GHC.Generics
import Reflex.Dom
import Widgets.Commons


panelInfo :: (Reflex t) => ReaderT (Dynamic t Settings) (WidgetWithLogs x) ()
panelInfo = writeReview

writeReview :: forall t x . Reflex t => ReaderT (Dynamic t Settings) (WidgetWithLogs x) ()
writeReview = elClass "div" "card-comment form-group form-group-label" $ do
  let wrIdent = "writeReview"
  elAttr "label" ("class" =: "floating-label" <> "for" =: wrIdent)
    $ text "Write a review"
  let attrs = constDyn ("class" =: "form-control")
  t <- textArea $ def & textAreaConfig_attributes .~ attrs
  let textD = _textArea_value t
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
  responseE <- httpPost "/mooc/proposals/review/#ScenarioId" dataOnClickE
    -- :: WidgetWithLogs x (Event t (Maybe Review))
  responseD <- holdDyn Nothing responseE
  lift $ renderReview responseD
  return ()

renderReview :: Reflex t => Dynamic t (Maybe ReviewPost) -> WidgetWithLogs x ()
renderReview mreviewD = elClass "div" "" $ do
  let dispReview (Just r) = reviewPostComment r
      dispReview Nothing  = "nope"
  dynText $ dispReview <$> mreviewD
