{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.LoadingSplash
    ( loadingSplash, loadingSplashD
    ) where

import Data.Semigroup
import qualified Data.Text as Text
import qualified Reflex.Dom as Dom

import CommonTypes
import Widgets.Generation

-- | When the incoming event is Busy, it shows the loading splash.
--   It hides as soon as Idle event is coming.
loadingSplashD :: Reflex t => Event t (IsBusy "program") -> Widget x ()
loadingSplashD isBusyE = do
    classDyn <- Dom.holdDyn classBusy (toClass <$> isBusyE)
    (e, _) <- Dom.elDynClass' "div" classDyn Dom.blank
    setInnerHTML e htmlCode
  where
    toClass Busy = classBusy
    toClass Idle = classIdle
    (htmlCode, (classIdle, classBusy)) = loadingSplashCode

-- | Show the loading splash, no events processed.
loadingSplash :: Widget x ()
loadingSplash = do
    (e, _) <- Dom.elClass' "div" classBusy Dom.blank
    setInnerHTML e htmlCode
  where
    (htmlCode, (_, classBusy)) = loadingSplashCode

-- | This function is fully evaluated at compile time.
--   At runtime this is just a tuple containing string literals.
loadingSplashCode :: (JSString, (Text,Text))
loadingSplashCode = $(do
      -- create unique css identifiers
      svgId <- newVar
      classBase' <- newVar
      redCircle <- newVar
      greyCircle <- newVar
      rotRed <- newVar
      rotGrey <- newVar
      fadeElId <- newVar
      let classIdle' = classBase' <> "-idle"
          classBusy' = classBase' <> "-busy"
      -- generate a chunk of css in qua-view.css
      qcss
        [cassius|
          .#{classBase'}
            z-index: 775
            height: 0
            width: 0
            top: 0
            left: 0
            padding: 0
            margin: 0

          .#{classIdle'}
            display: none

          .#{classBusy'}
            display: block

          ##{svgId}
            z-index: 777
            position: fixed
            height: 40%
            padding: 0
            top: 50%
            left: 50%
            margin: 0 -50% 0 0
            transform: translate(-50%, -50%)

          ##{fadeElId}
            z-index: 776
            position: fixed
            height: 100%
            width: 100%
            padding: 0
            margin: 0
            top: 0
            left: 0
            background-color: rgba(255,255,255,0.8)

          ##{redCircle}
            position: relative
            transform-origin: 36px 36px
            -ms-transform-origin: 36px 36px
            -moz-transform-origin: 36px 36px
            -webkit-transform-origin: 36px 36px
            -webkit-animation: #{rotRed} 3s linear infinite
            -moz-animation: #{rotRed} 3s linear infinite
            -ms-animation: #{rotRed} 3s linear infinite
            -o-animation: #{rotRed} 3s linear infinite
            animation: #{rotRed} 3s linear infinite

          ##{greyCircle}
            position: relative
            transform-origin: 36px 36px
            -ms-transform-origin: 36px 36px
            -moz-transform-origin: 36px 36px
            -webkit-transform-origin: 36px 36px
            -webkit-animation: #{rotGrey} 8s linear infinite
            -moz-animation: #{rotGrey} 8s linear infinite
            -ms-animation: #{rotGrey} 8s linear infinite
            -o-animation: #{rotGrey} 8s linear infinite
            animation: #{rotGrey} 8s linear infinite

          @-webkit-keyframes #{rotRed}
            from
              -ms-transform: rotate(0deg)
              -moz-transform: rotate(0deg)
              -webkit-transform: rotate(0deg)
              -o-transform: rotate(0deg)
              transform: rotate(0deg)
            to
              -ms-transform: rotate(360deg)
              -moz-transform: rotate(360deg)
              -webkit-transform: rotate(360deg)
              -o-transform: rotate(360deg)
              transform: rotate(360deg)

          @keyframes #{rotRed}
            from
              -ms-transform: rotate(0deg)
              -moz-transform: rotate(0deg)
              -webkit-transform: rotate(0deg)
              -o-transform: rotate(0deg)
              transform: rotate(0deg)
            to
              -ms-transform: rotate(360deg)
              -moz-transform: rotate(360deg)
              -webkit-transform: rotate(360deg)
              -o-transform: rotate(360deg)
              transform: rotate(360deg)

          @-webkit-keyframes #{rotGrey}
            from
              -ms-transform: rotate(360deg)
              -moz-transform: rotate(360deg)
              -webkit-transform: rotate(360deg)
              -o-transform: rotate(360deg)
              transform: rotate(360deg)
            to
              -ms-transform: rotate(0deg)
              -moz-transform: rotate(0deg)
              -webkit-transform: rotate(0deg)
              -o-transform: rotate(0deg)
              transform: rotate(0deg)

          @keyframes #{rotGrey}
            from
              -ms-transform: rotate(360deg)
              -moz-transform: rotate(360deg)
              -webkit-transform: rotate(360deg)
              -o-transform: rotate(360deg)
              transform: rotate(360deg)
            to
              -ms-transform: rotate(0deg)
              -moz-transform: rotate(0deg)
              -webkit-transform: rotate(0deg)
              -o-transform: rotate(0deg)
              transform: rotate(0deg)
        |]
      -- generate a chunk of html as plain JS string
      content <- qhtml
        [hamlet|
          <div ##{fadeElId}>
          <svg fill="none" ##{svgId} version="1.1" viewBox="0 0 72 72" xmlns="http://www.w3.org/2000/svg">
            <circle cx="36" cy="36" ##{redCircle} r="28" stroke="#FF5722" stroke-dasharray="10, 5, 50, 40, 30.929188601, 40" stroke-opacity="1" stroke-width="16">
            <circle cx="36" cy="36" ##{greyCircle} r="28" stroke="#BF360C" stroke-dasharray="38, 14, 8, 14, 65.929188601, 14, 8, 14" stroke-opacity=".2" stroke-width="8">
        |]
      -- We need to return several variables to use them at runtime.
      -- That's why I put this splice here
      [| ($(pure content), $(returnVars [ Text.unwords [classBase', classIdle']
                                        , Text.unwords [classBase', classBusy']]) ) |]
     )


