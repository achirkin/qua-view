{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Reviews
    ( panelReviews
    ) where

import Commons
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text
import Data.Time.Format
import QuaTypes.Review
import Reflex.Dom
import Widgets.Commons
import Widgets.Generation


panelReviews :: Reflex t
             => Dynamic t (Either JSError ReviewSettings) -> QuaWidget t x ()
panelReviews reviewSettingsD = do
  void $ dyn $ renderPanelReviews <$> reviewSettingsD

renderPanelReviews :: Reflex t
                   => Either JSError ReviewSettings -> QuaWidget t x ()
renderPanelReviews (Left _) = blank
renderPanelReviews (Right reviewSettings) = do
    rec let renderExp = renderWriteExpertReview reviewSettings
            showExpE = isLeft <$> expertRespE
        expertRespE <- if isJust $ expertReviewsUrl reviewSettings
                       then switchPromptlyDyn <$>
                         widgetHold (renderExp True) (renderExp <$> showExpE)
                       else return never
    respE <- renderWriteReview reviewSettings
    reviewsD <- accum accumRevs (reviews reviewSettings) $
                  leftmost [respE, expertRespE]
    let crits = criterions reviewSettings
    void . dyn $ mapM_ (renderReview crits) <$> reviewsD
  where
    accumRevs revs (Right newRev) = newRev:revs
    accumRevs revs (Left _)       = revs

-- draws the write-review-text-entry component if ReviewSettings contains `Just reviewsUrl`.
-- Returns event of posted review, or error on unsuccessful post.
renderWriteReview :: Reflex t
                  => ReviewSettings
                  -> QuaWidget t x (Event t (Either JSError Review))
renderWriteReview (ReviewSettings crits _ (Just revsUrl) _) =
  elClass "div" ("card " <> writeReviewClass) $
    elClass "div" "form-group form-group-label" $ mdo
      textD  <- renderTextArea resetTextE "Write a review"
      critD  <- renderCriterions crits
      thumbD <- renderThumbs

      let hideStateD = hideState <$> critD <*> thumbD
      clickE <- buttonFlatDyn hideStateD "Send" mempty

      let postDataOnClickE = (\th te c -> ReviewPost c th te)
                                         <$> current thumbD
                                         <*> current textD
                                         <@> fmapMaybe id (current critD <@ clickE)
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
             => [Criterion] -> Review -> QuaWidget t x ()
renderReview crits r = elClass "div" ("card " <> reviewClass) $
  el "div" $ do
    case reviewRating r of
      UserRating critId thumb -> do
        renderCrit critId
        elClass "span" "icon" $ text $ showThumb thumb
      ExpertRating grade -> renderStars grade
    text $ pack $ ' ' : formatTime defaultTimeLocale "%F, %R - " (reviewTimestamp r)
    text $ textFromJSString $ reviewUserName r <> ": "
    el "p" $ text $ textFromJSString $ reviewComment r
  where
    renderCrit critId =
      void $ for [ c | c <- crits, critId == criterionId c] $ \c -> do
        (spanEl, ()) <- elClass' "span" critClass $ return ()
        setInnerHTML spanEl $ criterionIcon c
    renderStars grade =
      let star t = elClass "span" "icon icon-lg" $ text t
      in sequence_ $
        (Prelude.replicate grade $ star "star") ++
         Prelude.replicate (5 - grade) (star "star_border")
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

-- draws the write-expert-review-text-entry component
-- if ReviewSettings contains `Just expertReviewsUrl`.
-- Returns event of posted review, or error on unsuccessful post.
renderWriteExpertReview :: Reflex t
                        => ReviewSettings
                        -> Bool -- ^ draw panel
                        -> QuaWidget t x (Event t (Either JSError Review))
renderWriteExpertReview (ReviewSettings _ _ _ (Just revsUrl)) True =
  elClass "div" "card " $
    elClass "div" "form-group form-group-label" $ mdo
      textD  <- renderTextArea never "Write an expert review"
      gradeD <- renderGrade 0
      clickE <- buttonFlatDyn (hideZeroState <$> gradeD) "Grade" mempty

      let postDataOnClickE = (\txt grade -> ExpertReviewPost grade txt)
                               <$> current textD
                               <@> ffilter (> 0) (current gradeD <@ clickE)
      responseE <- httpPost $ (,) revsUrl <$> postDataOnClickE
      _ <- renderError responseE
      return responseE
renderWriteExpertReview _ _ = return never

-- either renders the `JSError` or fires the returned event which contains `a`
renderError :: Reflex t
            => Event t (Either JSError a)
            -> QuaWidget t x (Event t a)
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
               -> QuaWidget t x (Dynamic t JSString)
renderTextArea setValE label = do
  elAttr "label" ("class" =: "floating-label") $ text label
  let config = TextAreaConfig "" setValE $ constDyn ("class" =: "form-control")
  t <- textArea config
  return $ textToJSString <$> _textArea_value t

-- | Render supplied criterios and return dynamic with criterionId of selected one
renderCriterions :: Reflex t
                 => [Criterion] -> QuaWidget t x (Dynamic t (Maybe Int))
renderCriterions crits = elClass "span" critsClass $ mdo
    let critE = leftmost critEs
    critEs <- for crits $ \c -> do
      let critId        = criterionId c
          chooseStyle mi _
            | Just i <- mi, i == critId = activeStyle
            | otherwise                 = inactiveStyle
      critAttrD <- foldDyn chooseStyle inactiveStyle critE
      (spanEl, ()) <- elDynAttr' "span" critAttrD $ return ()
      setInnerHTML spanEl $ criterionIcon c
      return $ Just critId <$ domEvent Click spanEl
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

-- render grading stars and return dynamic of their state (0 is unselected)
renderGrade :: Reflex t => Int -> QuaWidget t x (Dynamic t Int)
renderGrade nrStartStars = mdo
    nrStarsD  <- holdDyn nrStartStars nrStarsE
    nrStarsEE <- dyn (renderStars <$> nrStarsD)
    nrStarsE  <- switchPromptly never nrStarsEE
    return nrStarsD
  where
    renderStars nr = do
      els <- sequence $ (Prelude.replicate nr       $ renderStar "star") ++
                        (Prelude.replicate (5 - nr) $ renderStar "star_border")
      return $ leftmost $ (\(i, elm) -> i <$ domEvent Click elm) <$> Prelude.zip [1..] els
    renderStar t = fmap fst $ elClass' "span" "icon icon-lg" $ text t

-- render thumb-up and -down buttons and return dynamic of their state
renderThumbs :: Reflex t => QuaWidget t x (Dynamic t ThumbState)
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

hideZeroState :: Int -> ComponentState s
hideZeroState 0 = Inactive
hideZeroState _ = Active

inactiveStyle :: Map Text Text
inactiveStyle = "style" =: "opacity: 0.3"

activeStyle :: Map Text Text
activeStyle   = "style" =: "opacity: 1"
