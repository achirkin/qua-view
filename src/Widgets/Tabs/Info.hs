{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.JSString.Text (textFromJSString, textToJSString)
import Reflex.Dom
import Widgets.Commons
import Widgets.Generation


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
writeReview Nothing = text "Loading..."
writeReview (Just reviewSettings) = elClass "div" "card" $
  elClass "div" "card-comment form-group form-group-label" $ do

    let wrIdent = "writeReview"
    elAttr "label" ("class" =: "floating-label" <> "for" =: wrIdent)
      $ text "Write a review"
    let attrs = constDyn ("class" =: "form-control")
    t <- textArea $ def & textAreaConfig_attributes .~ attrs
    let textD = textToJSString <$> _textArea_value t

    let inactiveStyle = "style" =: "opacity: 0.3"
        activeStyle   = "style" =: "opacity: 1"

    rec
      let critE = leftmost critEs
      critEs <- for (criterions reviewSettings) $ \c -> do
        let critId        = tCriterionId c
            chooseStyle mi _
              | Just i <- mi, i == critId = activeStyle
              | otherwise                 = inactiveStyle
        critAttrD <- foldDyn chooseStyle inactiveStyle critE
        (spanEl, ()) <- elDynAttr' "span" critAttrD $ return ()
        setInnerHTML spanEl $ tCriterionIcon c
        return $ (Just critId) <$ domEvent Click spanEl
    critD <- holdDyn Nothing critE

    rec
      let thumbE = leftmost [
              ThumbUp   <$ domEvent Click upEl
            , ThumbDown <$ domEvent Click dnEl
            ]
      let showThumb ThumbUp   = "thumb_up" :: Text
          showThumb ThumbDown = "thumb_down"
          showThumb None      = "none"
          makeThumb upOrDown = do
            let chooseStyle th _
                  | th == upOrDown = activeStyle
                  | otherwise      = inactiveStyle
            thumbAttrD <- foldDyn chooseStyle inactiveStyle thumbE
            fst <$> elDynAttr' "span" (fmap (<> "class" =: "icon") thumbAttrD)
                      (text $ showThumb upOrDown)
      upEl <- makeThumb ThumbUp
      dnEl <- makeThumb ThumbDown
    thumbD <- holdDyn None thumbE

    let toHideState (Just _) ThumbUp   = Active
        toHideState (Just _) ThumbDown = Active
        toHideState _        _         = Inactive
    let hideStateD = toHideState <$> critD <*> thumbD
    clickE <- buttonFlatHideDyn hideStateD "Send" mempty

    let postDataOnClickE = ReviewPost <$> fromJust <$> current critD
                                      <*> current thumbD
                                      <*> current textD
                                      <@ clickE
    responseE <- httpPost $ (,) (reviewsUrl reviewSettings) <$> postDataOnClickE
    let doAccum revs (Right newRev) = newRev:revs
        doAccum revs (Left _)       = revs
    reviewsD <- accum doAccum (reviews reviewSettings) responseE

    let renderErr (Just err) = text $ textFromJSString err
        renderErr Nothing    = blank
        filterNonErr (Left (JSError err)) = Just err
        filterNonErr _ = Nothing
    errD <- holdDyn Nothing $ filterNonErr <$> responseE
    _ <- dyn $ renderErr <$> errD

    void . dyn $ mapM_ renderReview <$> reviewsD

renderReview :: Reflex t
             => TReview -> WidgetWithLogs x ()
renderReview r = elClass "div" "card" $ do
  text $ textFromJSString $ tReviewUserName r <> ": "
  text $ textFromJSString $ tReviewComment r
