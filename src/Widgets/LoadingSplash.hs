{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.LoadingSplash
    ( loadingSplash, loadingSplashD
    ) where

import qualified Reflex.Dom as Dom
import Control.Lens ((.~), (&))

import Commons
import Widgets.Generation

-- | When the incoming event is Busy, it shows the loading splash.
--   It hides as soon as Idle event is coming.
--
--   Note, loadingSplash is a special widget.
--   We need it load as soon as possible to show it during page loading.
--   Therefore, I added its html code dirctly to qua-view.html, so it starts spinning even before
--   the javascript code is loaded.
--   Also note, that our static css is placed in a separate file (not in place of generated JS code),
--   so we can safely write required css code below in a splice.
loadingSplashD :: Reflex t => Event t (IsBusy "program") -> Widget x ()
loadingSplashD isBusyE = do
    let cfg = def & Dom.modifyAttributes .~ fmap setClass isBusyE
    void $ getElementById cfg "qua-view-loading-div"
  where
    setClass Busy = "class" =: Just "qua-view-loading-busy"
    setClass Idle = "class" =: Just "qua-view-loading-idle"

-- | Show the loading splash, no events processed.
loadingSplash :: Widget x ()
loadingSplash = void $ getElementById def "qua-view-loading-div"

-- | This function is fully evaluated at compile time.
--   It generates css code for existing Dom elements
$(do
  -- create unique css identifiers
  rotRed <- newVar
  rotGrey <- newVar
  -- generate a chunk of css in qua-view.css
  qcss
    [cassius|
      #qua-view-loading-div
        z-index: 775
        height: 0
        width: 0
        top: 0
        left: 0
        padding: 0
        margin: 0

      .qua-view-loading-idle
        display: none

      .qua-view-loading-busy
        display: block

      #qua-view-loading-splash
        z-index: 777
        position: fixed
        height: 40%
        padding: 0
        top: 50%
        left: 50%
        margin: 0 -50% 0 0
        transform: translate(-50%, -50%)

      #qua-view-loading-background
        z-index: 776
        position: fixed
        height: 100%
        width: 100%
        padding: 0
        margin: 0
        top: 0
        left: 0
        background-color: rgba(255,255,255,0.8)

      #qua-view-loading-splash-red-circle
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

      #qua-view-loading-splash-grey-circle
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
  return []
 )


