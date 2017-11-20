{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.Share
    ( popupShare
    ) where

import Data.JSString.Text (textFromJSString)
import Reflex.Dom

import Commons
import Widgets.Commons
import Widgets.Generation
import Widgets.Modal

popupShare :: Reflex t
           => Event t (ElementClick shareButton)
           -> JSString -- ^ share link
           -> QuaWidget t x ()
popupShare sharePopupE shareLink
  = void $ createSmallModalWithClicks' sharePopupE Inactive $ popupShareContent shareLink


popupShareContent :: Reflex t
                  => JSString -- ^ share link
                  -> QuaWidget t x (Event t (ElementClick "close share popup"))
popupShareContent shareLink = do
    let linkTxt = textFromJSString shareLink
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Share this design with others"
    elClass "div" ("modal-inner " <> socialCls) $ do
      elClass "p" "text" $ text "You can share the following link that refers to the last saved version of this design:"
      el "code" $ text linkTxt
      elClass "p" "text" $ text "Alternatively, use your favourite button:"
      void $ elAttr "a" (
           "href"    =: ("https://www.facebook.com/sharer/sharer.php?u=" <> linkTxt)
        <> "onclick" =: ("window.open('https://www.facebook.com/sharer/sharer.php?u=" <> linkTxt <> "','_blank','width=555,height=328'); return false;")
        ) $ makeElementFromHtml def $(qhtml
          [hamlet|
            <svg xmlns="http://www.w3.org/2000/svg" x="0px" y="0px" width="40" height="40" viewBox="0 0 266.893 266.895" enable-background="new 0 0 266.893 266.895" xml:space="preserve">
              <path fill="#3C5A99" d="M248.082,262.307c7.854,0,14.223-6.369,14.223-14.225V18.812 c0-7.857-6.368-14.224-14.223-14.224H18.812c-7.857,0-14.224,6.367-14.224,14.224v229.27c0,7.855,6.366,14.225,14.224,14.225 H248.082z"/>
              <path id="f" fill="#FFFFFF" d="M182.409,262.307v-99.803h33.499l5.016-38.895h-38.515V98.777c0-11.261,3.127-18.935,19.275-18.935 l20.596-0.009V45.045c-3.562-0.474-15.788-1.533-30.012-1.533c-29.695,0-50.025,18.126-50.025,51.413v28.684h-33.585v38.895h33.585 v99.803H182.409z"/>
          |])
      void $ elAttr "a" (
           "href"    =: ("http://vk.com/share.php?url=" <> linkTxt)
        <> "onclick" =: "window.open(this.href, 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;"
        ) $ makeElementFromHtml def $(qhtml
          [hamlet|
            <svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="0 0 192 192">
              <path fill="#5181B8" d="M66.56,0 C120.32,0 71.68,0 125.44,0 C179.2,0 192,12.8 192,66.56 C192,120.32 192,71.68 192,125.44 C192,179.2 179.2,192 125.44,192 C71.68,192 120.32,192 66.56,192 C12.8,192 0,179.2 0,125.44 C0,71.68 0,96.580329 0,66.56 C0,12.8 12.8,0 66.56,0 Z"/>
              <path fill="#FFFFFF" d="M157.233993,66.1462211 C158.123557,63.1797719 157.233994,61 153.000244,61 L139.000244,61 C135.440505,61 133.799415,62.8830035 132.909356,64.9593945 C132.909356,64.9593945 125.789878,82.3129373 115.704198,93.5851974 C112.441227,96.8481681 110.957879,97.8863636 109.178009,97.8863636 C108.288198,97.8863636 107,96.8481681 107,93.8819658 L107,66.1462211 C107,62.586482 105.96694,61 103.000244,61 L81.0002441,61 C78.7757158,61 77.4378669,62.6521562 77.4378669,64.2179674 C77.4378669,67.5925348 82.4804603,68.3707494 83.0002441,77.8633869 L83.0002441,98.4799003 C83.0002441,103 82.1839388,103.819509 80.4040693,103.819509 C75.6579974,103.819509 64.1131647,86.388441 57.2660122,66.4427426 C55.9241353,62.5659897 54.5782535,61 51.0002441,61 L37.0002441,61 C33.0002441,61 32.2001953,62.8830035 32.2001953,64.9593945 C32.2001953,68.6675178 36.9465141,87.059256 54.2998099,111.383646 C65.8685915,127.995268 82.1682449,137 97.0002441,137 C105.899345,137 107.000244,135 107.000244,131.555007 L107.000244,119 C107.000244,115 107.843292,114.201711 110.661357,114.201711 C112.737749,114.201711 116.297488,115.239906 124.603545,123.249196 C134.095936,132.741586 135.660882,137 141.000244,137 L155.000244,137 C159.000244,137 161.000244,135 159.846475,131.053112 C158.583906,127.119411 154.051802,121.412135 148.038124,114.646617 C144.774906,110.790356 139.88045,106.637574 138.397102,104.560689 C136.320711,101.891255 136.914001,100.704429 138.397102,98.3315162 C138.397102,98.3315162 155.454123,74.3036478 157.233993,66.1462211 Z"/>
          |])
      void $ elAttr "a" (
           "href"    =: ("https://twitter.com/share?url=_&text=" <> linkTxt)
        <> "onclick" =: ("window.open('https://twitter.com/intent/tweet?url=" <> linkTxt <> "&text=' + encodeURIComponent('Check out this design on #quakit!') + '&hashtags=mooc,edx,ethz,chairia,urbandesign', 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;")
        ) $ makeElementFromHtml def $(qhtml
          [hamlet|
            <svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="0 0 300.00006 244.18703">
             <g transform="translate(-539.18 -568.86)">
              <path d="m633.9 812.04c112.46 0 173.96-93.168 173.96-173.96 0-2.6463-0.0539-5.2806-0.1726-7.903 11.938-8.6302 22.314-19.4 30.498-31.66-10.955 4.8694-22.744 8.1474-35.111 9.6255 12.623-7.5693 22.314-19.543 26.886-33.817-11.813 7.0031-24.895 12.093-38.824 14.841-11.157-11.884-27.041-19.317-44.629-19.317-33.764 0-61.144 27.381-61.144 61.132 0 4.7978 0.5364 9.4646 1.5854 13.941-50.815-2.5569-95.874-26.886-126.03-63.88-5.2508 9.0354-8.2785 19.531-8.2785 30.73 0 21.212 10.794 39.938 27.208 50.893-10.031-0.30992-19.454-3.0635-27.69-7.6468-0.009 0.25652-0.009 0.50661-0.009 0.78077 0 29.61 21.075 54.332 49.051 59.934-5.1376 1.4006-10.543 2.1516-16.122 2.1516-3.9336 0-7.766-0.38716-11.491-1.1026 7.7838 24.293 30.355 41.971 57.115 42.465-20.926 16.402-47.287 26.171-75.937 26.171-4.929 0-9.7983-0.28036-14.584-0.84634 27.059 17.344 59.189 27.464 93.722 27.464" fill="#1da1f2"/>
          |])
      void $ elAttr "a" (
           "href"    =: ("https://plus.google.com/share?url=" <> linkTxt)
        <> "onclick" =: ("window.open(this.href,'popUpWindow','menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;")
        ) $ makeElementFromHtml def $(qhtml
          [hamlet|
            <svg width="40" height="40" enable-background="new 0 0 128 128" viewBox="0 0 128 128" xml:space="preserve" xmlns="http://www.w3.org/2000/svg">
              <circle cx="64" cy="64" fill="#D95032" r="64"/>
              <path d="M49.424,97.875c-19.018,0-34.491-15.193-34.491-33.874c0-18.68,15.473-33.875,34.491-33.875     c8.318,0,16.354,2.952,22.624,8.309l-8.771,9.899c-3.838-3.279-8.758-5.086-13.853-5.086c-11.652,0-21.13,9.31-21.13,20.752     c0,11.441,9.479,20.75,21.13,20.75c9.858,0,16.311-4.723,18.407-13.197H49.587V58.432h32.347v6.562     C81.934,84.659,68.869,97.875,49.424,97.875z" fill="#FFFFFF"/>
              <polygon fill="#FFFFFF" points="117.934,58.438 107.934,58.438 107.934,48.438 99.934,48.438 99.934,58.438 89.934,58.438     89.934,66.438 99.934,66.438 99.934,76.438 107.934,76.438 107.934,66.438 117.934,66.438"/>
          |])
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $
        buttonFlat "Close" def
    where
      (socialCls) = $(do
        social <- newVar
        qcss
          [cassius|
            .#{social} > a
              margin-right: 10px
          |]
        returnVars [social]
        )
