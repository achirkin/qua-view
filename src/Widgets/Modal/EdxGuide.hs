{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}

module Widgets.Modal.EdxGuide
    ( popupEdxGuide
    ) where

import Control.Monad (void)
import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.CommonWidget
import Widgets.Generation
import Widgets.Modal


-- | EdX help page is shown only at startup, only to edX students, and only if requested.
popupEdxGuide :: Reflex t
              => ComponentState "EdxGuide"
              -> Widget x ()
popupEdxGuide showAtStartUp = void $ createModalWithClicks' never showAtStartUp popupEdxGuideContent


popupEdxGuideContent :: Reflex t => Widget x (Event t (ElementClick "Close edX guide"))
popupEdxGuideContent = do
    elClass "div" "modal-heading" $
      elClass "p" "modal-title" $ text "Welcome to Quick Urban Analysis kit - the web geometry viewer"
    elClass "div" "modal-inner" $ do
      text "Dear student,"
      el "br" blank
      text $ "Your task is to develop a reasonable design proposal in a context "
          <> "you have been introduced previously at edX platform. "
          <> "You can work on a design by moving and rotating objects. "
          <> "Here are a few things you should note before proceeding with the exercise:"
      el "br" blank
      return addCss -- Not sure if this is the right way
      void $ makeElementFromHtml def $(do
          helpIcons <- newVar
          qhtml
            [hamlet|
              <table>

                <tr>
                  <td.table-cell>
                    <a.fbtn.fbtn-lg.fbtn-red style="display: inline-block">
                      <span class="fbtn-ori icon">apps
                  <td.table-cell>
                    <a.fbtn.fbtn-brand style="display: inline-block">
                      <span class="icon icon-lg">save
                  <td.table-cell>
                    Use <b>Save</b> button #
                    to submit your proposal to the system. #
                    You can save your design several times and continue work on it later; #
                    your latest submission is always visible in the gallery.

                <tr>
                  <td.table-cell>
                  <td.table-cell>
                    <a.fbtn style="display: inline-block">
                      edX
                  <td.table-cell>
                    <b>You are logged in #
                    our system via edX platform. #
                    We are using a special anonymized edX id to identify you as edX student #
                    and grade your submissions.

                <tr>
                  <td.table-cell>
                  <td.table-cell>
                    <a.fbtn.fbtn-orange style="display: inline-block">
                      <span class="icon icon-lg">grade
                  <td.table-cell>
                    You will get #
                    <b>
                      60% #
                    of the exercise score as long as you submit your proposal.
                    You will get
                    <b>
                      remaining 0-40% #
                    of the exercise score according to the peer-reviewing rating of your proposal #
                    at the end of the course. #
                    We will tell you more about submissions peer-reviewing later in the course.

                <tr>
                  <td.table-cell>
                  <td.table-cell>
                    <a.fbtn style="display: inline-block">
                      <span class="icon icon-lg">vpn_key
                  <td.table-cell>
                    <b> You can always come back to your last submitted design #
                    by following the exercise link at edX. #
                    Or you can setup a full password-protected acount here to login #
                    directly on our site. #
                    You will be able to do so after you submit your first design proposal.

                <tr>
                  <td.table-cell>
                  <td.table-cell>
                    <a.fbtn style="display: inline-block">
                      <span class="icon icon-lg">subject
                  <td.table-cell>
                    For research purposes, some of your actions (e.g. moving and rotating geometry objects) #
                    on this page may be anonymously recorded and sent to our servers; #
                    by proceeding you agree to share these data.
            |]
        )
    elClass "div" "modal-footer" $
      elClass "p" "text-right" $
        flatButton "Ok, let's go"
  where
    addCss = $(do
        qcss
          [cassius|
            .table-cell
              padding: 0px 4px
              text-align: justify
          |]
        returnVars []
      )

