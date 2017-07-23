{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Widgets.ControlButtons
    ( resetCameraButton
    , helpButton
    , toggleFullScreenButton
    , controlPanelButton
    , submitProposalButton
    , serviceClearButton
    , serviceRunButton
    ) where

import qualified Reflex.Dom as Dom

import CommonTypes
import Widgets.Generation

resetCameraButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
resetCameraButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect fbtn-brand-accent" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Reset camera position
          <span class="icon icon-lg" style="font-size: 2em;margin-left:-8px;vertical-align:-32%;margin-top:-3px;">fullscreen
          <span class="icon icon" style="margin-left: -24px;font-size: 1em;line-height: 1em;">videocam
        |])
    return e

helpButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
helpButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">How-to: mouse & finger controls
          <span class="icon icon-lg">help_outline
        |])
    return e

toggleFullScreenButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
toggleFullScreenButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Toggle fullscreen
                    <span class="icon icon-lg" #fullscreenbicon>fullscreen
        |])
    return e

controlPanelButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
controlPanelButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Control panel
          <span class="icon icon-lg">settings
        |])
    return e

submitProposalButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
submitProposalButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect fbtn-brand" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Submit proposal
                    <span class="icon icon-lg">save
        |])
    return e

serviceClearButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
serviceClearButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect waves-light fbtn-brand-accent" Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Clear service results
          <span class="icon icon-lg">visibility_off
        |])
    return e

serviceRunButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
serviceRunButton = do
    (e, _) <- Dom.elClass' "a" "fbtn waves-attach waves-circle waves-effect fbtn-green " Dom.blank
    setInnerHTML e $(qhtml
        [hamlet|
          <span class="fbtn-text fbtn-text-left">Run evaluation service
          <span class="icon icon-lg">play_arrow
        |])
    return e

