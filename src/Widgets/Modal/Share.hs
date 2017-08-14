-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.Share
    ( popupShare
    ) where

import Reflex.Dom

import Commons
import Widgets.Commons
--import Widgets.Generation
import Widgets.Modal

popupShare :: Reflex t
           => Event t (ElementClick shareButton)
           -> Text -- ^ share link
           -> Widget x ()
popupShare sharePopupE shareLink = void $ createSmallModalWithClicks' sharePopupE Inactive $ popupShareContent shareLink


popupShareContent :: Reflex t
                  => Text -- ^ share link
                  -> Widget x (Event t (ElementClick "close share popup"))
popupShareContent shareLink = do
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Share this design with others"
    elClass "div" "modal-inner" $ do
      elClass "p" "text" $ text "You can share the following link that refers to the last saved version of this design:"
      el "code" $ text shareLink
      elClass "p" "text" $ text "Alternatively, use your favourite button:"
      -- TODO: the share buttons
      -- void $ makeElementFromHtml def $(do
      --     -- define html code (this is converted into a static JSString)
      --     -- always make sure it has only one root element if you use it in makeElementFromHtml function
      --     qhtml
      --       [hamlet|
      --           <div style="text-align:center">
      --             <a.shareButton onclick="FB.ui({method: 'share',mobile_iframe: true, href: '#{shareLink}'}, function(response){});">
      --               <img src="@{StaticR img_fbIcon_png}" style="width:40px;height:40px;" title="Share on Facebook" alt="Share on Facebook">
      --             <a.shareButton href="http://vk.com/share.php?url=#{shareLink}" onclick="javascript:window.open(this.href, 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
      --               <img src="@{StaticR img_vkIcon_png}" style="width:40px;height:40px;" title="Share on Vkontakte" alt="Share on Vkontakte">
      --             <a.shareButton onclick="window.open('https://twitter.com/intent/tweet?url=' + encodeURIComponent('#{shareLink}') + '&text=' + encodeURIComponent('Check out this design on #quakit!') + '&hashtags=mooc,edx,ethz,chairia,urbandesign', 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
      --               <img src="@{StaticR img_twitterIcon_png}" style="width:40px;height:40px;" title="Tweet the link" alt="Tweet the link">
      --             <a.shareButton href="https://plus.google.com/share?url=#{shareLink}" onclick="javascript:window.open(this.href, 'popUpWindow', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=700,width=500,left=20,top=10');return false;">
      --               <img src="@{StaticR img_gIcon_png}" style="width:40px;height:40px;" title="Share on Google" alt="Share on Google">
      --       |]
      --   )
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $
        flatButton "Close"

