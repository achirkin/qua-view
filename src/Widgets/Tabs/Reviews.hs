{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Widgets.Tabs.Reviews
    ( panelReviews
    ) where

import Control.Monad (join)
import Commons
import qualified Data.Text as T
import Data.List (sortOn)
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
    expertRespE <- renderWriteExpertReview reviewSettings
    respE <- renderWriteReview reviewSettings
    let revs = reverse . sortOn reviewTimestamp $ reviews reviewSettings
    reviewsE <- accum (flip ($)) revs $ leftmost [accumRevs <$> respE, onlyOneExpRev <$> expertRespE]
    let renderRevs = mapM_ $ renderReview $ criterions reviewSettings
    void $ widgetHold (renderRevs revs) (renderRevs <$> reviewsE)
  where
    accumRevs (Right newRev) revs = newRev:revs
    accumRevs (Left _)       revs = revs
    onlyOneExpRev :: Either JSError Review -> [Review] -> [Review]
    onlyOneExpRev (Right newRev) revs = newRev : (filter (not . isMyExpertReview) revs)
    onlyOneExpRev (Left _) revs = revs

-- draws the write-review-text-entry component if ReviewSettings contains `Just reviewsUrl`.
-- Returns event of posted review, or error on unsuccessful post.
renderWriteReview :: Reflex t
                  => ReviewSettings
                  -> QuaWidget t x (Event t (Either JSError Review))
renderWriteReview (ReviewSettings crits _ (Just revsUrl) _)
  = elAttr "div" (  "class" =: "card"
                 <> "style" =: "padding: 0px; margin: 10px 0px 10px 0px"
                 )
    $ elClass "div" (T.unwords ["card-main", spaces2px, writeReviewClass])
      $ mdo
        textD <- elClass "div" (T.unwords ["card-inner", spaces2px])
          $ elClass "div" "form-group form-group-label"
            $ renderTextArea resetTextE "Write a review"

        (critD, thumbD, clickE)
          <- elClass "div" (T.unwords ["card-action", spaces2px])
            $ do
              cD <- renderCriterions crits
              tD <- renderThumbs
              cE <- el "p" $ do
                void $ dyn $ renderTxt <$> tD <*> cD
                buttonFlatDyn (hideState <$> critD <*> thumbD) "Send" mempty
              return (cD, tD, cE)

        let postDataOnClickE = (\th te c -> ReviewPost (criterionId c) th te)
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
    WidgetCSSClasses {..} = widgetCSS
    renderTxt _ Nothing = blank
    renderTxt None _    = blank
    renderTxt thumb (Just crit) =
      text $ toTxt thumb <> (textFromJSString $ criterionName crit) <> " "
      where
        toTxt ThumbUp   = "Upvote "
        toTxt ThumbDown = "Downvote "
        toTxt None      = ""
renderWriteReview _ = return never

renderReview :: Reflex t
             => [Criterion] -> Review -> QuaWidget t x ()
renderReview crits r
  = elClass "div" (T.unwords ["card", spaces0px])
    $ do
      case reviewRating r of
        ExpertRating _ -> pure ()
        UserRating critId thumb ->
          elAttr "aside" (  "class" =: "card-side pull-left"
                         <> "style" =:
                               "background-color: #ffffff; width: 24px; margin: 2px; padding: 0px"
                         )
            $ do
              elClass "span" (T.unwords ["icon", icon24px, "text-brand-accent"])
                $ text $ showThumb thumb
              renderCrit critId

      elClass "div" (T.unwords ["card-main", spaces0px])
        $ elClass "div" (T.unwords ["card-inner", spaces2px])
          $ do
            elClass "p" (T.unwords ["text-brand-accent", smallP])
              $ do
                text $ T.pack $ ' ' : formatTime defaultTimeLocale "%F, %R - " (reviewTimestamp r)
                text $ textFromJSString $ reviewUserName r

            case reviewRating r of
              ExpertRating grade -> renderStars grade
              UserRating _ _ -> pure ()

            elClass "p" smallP
              $ text $ textFromJSString $ reviewComment r
  where
    WidgetCSSClasses {..} = widgetCSS
    renderCrit critId =
      void $ for [ c | c <- crits, critId == criterionId c] $ \c -> do
        (spanEl, ()) <- elAttr' "span" ("style" =: "position: relative; top: 5px;") blank
        setInnerHTML spanEl $ criterionIcon c
    renderStars grade =
      let star t = elClass "span" "icon icon-lg" $ text t
      in sequence_ $
        (Prelude.replicate grade $ star "star") ++
         Prelude.replicate (5 - grade) (star "star_border")


-- draws the write-expert-review-text-entry component
-- if ReviewSettings contains `Just expertReviewsUrl`.
-- Returns event of posted review, or error on unsuccessful post.
renderWriteExpertReview :: Reflex t
                        => ReviewSettings
                        -> QuaWidget t x (Event t (Either JSError Review))
renderWriteExpertReview (ReviewSettings _ revs _ (Just revsUrl))
  = elAttr "div" (  "class" =: "card"
                 <> "style" =: "padding: 0px; margin: 10px 0px 10px 0px"
                 )
    $ elClass "div" (T.unwords ["card-main", spaces2px, writeReviewClass])
      $ mdo
        textD <- elClass "div" (T.unwords ["card-inner", spaces2px])
          $ elClass "div" "form-group form-group-label"
            $ join <$> widgetHold
                (renderTextArea resetTextE initLabel)
                (renderTextArea resetTextE <$> (updateReviewTxt <$ resetTextE))

        (gradeD, clickE)
          <- elClass "div" (T.unwords ["card-action", spaces2px])
            $ do
              gD <- renderGrade 0
              cE <- buttonFlatDyn (hideZeroState <$> gradeD) "Grade" mempty
              return (gD, cE)

        let postDataOnClickE = (\txt grade -> ExpertReviewPost grade txt)
                                 <$> current textD
                                 <@> ffilter (> 0) (current gradeD <@ clickE)

        responseE <- httpPost $ (,) revsUrl <$> postDataOnClickE
        let reset (Right _) = Just ""
            reset _         = Nothing
        let resetTextE = fmapMaybe reset responseE
        _ <- renderError responseE
        return responseE
  where
    updateReviewTxt = "Update your expert review"
    writeReviewTxt  = "Write an expert review"
    initLabel = if any isMyExpertReview revs
                then updateReviewTxt
                else writeReviewTxt
    WidgetCSSClasses {..} = widgetCSS
renderWriteExpertReview _ = return never

writeReviewClass :: Text
writeReviewClass = $(do
    reviewCls <- newVar
    qcss
      [cassius|
        .#{reviewCls}
          .card-inner
            .form-group
              margin: 8px 0 0 0
              width: 100%
            textarea
              resize: vertical
          .card-action
            min-height: 32px
            .btn-flat
              padding: 0
              line-height: 14px
      |]
    returnVars [reviewCls]
  )


-- either renders the `JSError` or fires the returned event which contains `a`
renderError :: Reflex t
            => Event t (Either JSError a)
            -> QuaWidget t x (Event t a)
renderError event = do
  let (errE, resultE) = fanEither event
  performEvent_ $ liftIO . print <$> errE
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
                 => [Criterion] -> QuaWidget t x (Dynamic t (Maybe Criterion))
renderCriterions crits = elClass "span" critsClass $ mdo
    let critE = leftmost critEs
    critEs <- for crits $ \c -> do
      let cTitle         = "title" =: textFromJSString (criterionName c)
          activeStyle'   = activeStyle   <> cTitle
          inactiveStyle' = inactiveStyle <> cTitle
          chooseStyle mc _
            | Just c' <- mc, criterionId c' == criterionId c = activeStyle'
            | otherwise                 = inactiveStyle'
      critAttrD <- foldDyn chooseStyle inactiveStyle' critE
      (spanEl, ()) <- elDynAttr' "span" critAttrD $ return ()
      setInnerHTML spanEl $ criterionIcon c
      return $ Just c <$ domEvent Click spanEl
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
    renderStar t = fmap fst $ elClass' "span" (T.unwords ["icon","icon-lg", starsClass])
                 $ text t
    starsClass = $(do
          starsCls <- newVar
          qcss
            [cassius|
              .#{starsCls}
                cursor: pointer
                &:hover
                    color: #ff6f00
            |]
          returnVars [starsCls]
        )

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

isMyExpertReview :: Review -> Bool
isMyExpertReview rev = reviewIsMine rev && isExpertRating (reviewRating rev)
  where
    isExpertRating (ExpertRating _) = True
    isExpertRating _ = False
