{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
module Widgets.ControlButtons
    ( controlButtonGroup
      -- the module below should expose only controlButtonGroup widget and related data types.
      -- buttons below are not implemented yet...
    , resetCameraButton
    , submitProposalButton
    , serviceClearButton
    , serviceRunButton
    ) where

import qualified Reflex.Class as Reflex
import qualified Reflex.Dom as Dom
import Text.Julius (julius)

import Commons
import Widgets.Generation
import Widgets.Modal.Help

-- | Control button group is a column of colourfull buttons in the bottom-right corner of the screen.
--   It defines the most useful functions of qua-kit.
controlButtonGroup :: Reflex t =>  Widget x (Event t (ElementClick "Reset Camera"), Dynamic t (ComponentState "ControlPanel"))
controlButtonGroup = mdo
    (toggleGroupD, resetCameraE, cpStateD) <-
        Dom.elDynClass "div" (toPanelClass <$> cpStateD) $
          Dom.elDynClass "div" toggleGroupD $ do
            -- toggle visibility of buttons
            toggleGroupD'  <- expandCtrlGroupButton
            -- show all buttons
            (resetCameraE', groupContents) <- Dom.elClass "div" "fbtn-dropup" $ do
                resetCamE <- resetCameraButton
                helpButton
                toggleFullScreenButton
                groupContents' <- controlPanelButton
                return (resetCamE, groupContents')
            return (toggleGroupD', resetCameraE', groupContents)
    return (resetCameraE, cpStateD)
  where
    toPanelClass Active   = openPanelState
    toPanelClass Inactive = closedPanelState
    -- Styles for the panel are generated statically.
    -- newVar guarantees that the class name is unique.
    (openPanelState, closedPanelState) = $(do
        placeholder <- newVar
        let ostate = placeholder <> "-open"
            cstate = placeholder <> "-closed"
        qcss
          [cassius|
            .#{placeholder}
                position: absolute
                bottom: 0
                padding: 0
                margin: 0
                z-index: 4
                overflow: visible
                width: 64px
                -webkit-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -moz-transition: width 300ms ease-in-out, left 300ms ease-in-out
                -o-transition: width 300ms ease-in-out, left 300ms ease-in-out
                transition: width 300ms ease-in-out, left 300ms ease-in-out

            .#{ostate}
                left: -32px

            .#{cstate}
                left: -64px

          |]
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ fmap ((placeholder <> " ") <>) [ostate, cstate]
      )



-- | Main control group button that toggles the control group on or off.
--   Returns the state of css class that controls the state.
expandCtrlGroupButton :: Reflex t => Widget x (Dynamic t Text)
expandCtrlGroupButton = do
    e <- makeElementFromHtml def $(qhtml
        [hamlet|
          <a aria-expanded="true" .fbtn .fbtn-lg .fbtn-red .waves-attach .waves-circle .waves-light .waves-effect">
            <span .fbtn-text .fbtn-text-left>Tools
            <span .fbtn-ori .icon>apps
            <span .fbtn-sub .icon>close
        |])
    Reflex.accum toggleGroup "fbtn-inner open" $ Dom.domEvent Dom.Click e
  where
    toggleGroup "fbtn-inner" = const "fbtn-inner open"
    toggleGroup _            = const "fbtn-inner"



-- | Open or close control panel
controlPanelButton :: Reflex t => Widget x (Dynamic t (ComponentState "ControlPanel"))
controlPanelButton = do
    e <- makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect>
            <span .fbtn-text .fbtn-text-left>Control panel
            <span .icon .icon-lg>settings
        |])
    Reflex.accum flipPanel Inactive $ Dom.domEvent Dom.Click e
  where
    flipPanel Active   = const Inactive
    flipPanel Inactive = const Active


-- | Fullscreen button is quite independent.
--   It does not require any input events, does its own action, end returns no events.
--
--   Note: never do such weird JS scripting anymore!
--         I had to put this script into hamlet splice; otherwise borwser's security does not allow
--         enabling fullscreen!
toggleFullScreenButton :: Widget x ()
toggleFullScreenButton = do
    runCode
    void $ makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect onclick="document.q$toggleFullScreen(this)">
            <span .fbtn-text .fbtn-text-left>Toggle fullscreen
            <span .icon .icon-lg #fullscreenIcon>fullscreen
        |])
  where
    runCode = $(qjs
        [julius|
            document.q$toggleFullScreen = function(el) {
                if (!document['fullscreenElement'] && !document['mozFullScreenElement']
                 && !document['webkitFullscreenElement'] && !document['msFullscreenElement'] && !document['fullScreen']) {
                  if (document.documentElement['requestFullscreen']) {
                    document.documentElement.requestFullscreen();
                    el.querySelector('#fullscreenIcon').innerText = 'fullscreen_exit';
                 } else if (document.documentElement['msRequestFullscreen']) {
                    document.documentElement.msRequestFullscreen();
                    el.querySelector('#fullscreenIcon').innerText = 'fullscreen_exit';
                  } else if (document.documentElement['mozRequestFullScreen']) {
                    document.documentElement.mozRequestFullScreen();
                    el.querySelector('#fullscreenIcon').innerText = 'fullscreen_exit';
                  } else if (document.documentElement['webkitRequestFullscreen']) {
                    document.documentElement.webkitRequestFullscreen(Element.ALLOW_KEYBOARD_INPUT);
                    el.querySelector('#fullscreenIcon').innerText = 'fullscreen_exit';
                  }
                } else {
                  if (document['exitFullscreen']) {
                    document.exitFullscreen();
                  } else if (document['msExitFullscreen']) {
                    document.msExitFullscreen();
                  } else if (document['mozCancelFullScreen']) {
                    document.mozCancelFullScreen();
                  } else if (document['webkitExitFullscreen']) {
                    document.webkitExitFullscreen();
                  } else if (document['cancelFullscreen']) {
                    document.cancelFullScreen();
                  }
                  el.querySelector('#fullscreenIcon').innerText = 'fullscreen';
                }
            };
        |])


-- | Show help popup on click event
helpButton :: Reflex t => Widget x ()
helpButton = do
    e <- makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect>
            <span .fbtn-text .fbtn-text-left>
              How-to: mouse & finger controls
            <span .icon .icon-lg>
              help_outline
        |])
    popupHelp (ElementClick <$ Dom.domEvent Dom.Click e)

resetCameraButton :: Reflex t => Widget x (Event t (ElementClick s))
resetCameraButton = do
    e <- makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect .fbtn-brand-accent>
            <span .fbtn-text .fbtn-text-left>
              Reset camera position
            <span .icon .icon-lg style="font-size: 2em;margin-left:-8px;vertical-align:-32%;margin-top:-3px;">
              fullscreen
            <span .icon style="margin-left: -24px;font-size: 1em;line-height: 1em;">
              videocam
        |])
    return (ElementClick <$ Dom.domEvent Dom.Click e)

----------------------------------------------------------------------------------------------------
-- below are drafts: buttons that not implemented yet
----------------------------------------------------------------------------------------------------

submitProposalButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
submitProposalButton = makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect .fbtn-brand>
            <span .fbtn-text .fbtn-text-left>
              Submit proposal
            <span .icon .icon-lg>
              save
        |])


serviceClearButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
serviceClearButton = makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect .waves-light .fbtn-brand-accent>
            <span .fbtn-text .fbtn-text-left>
              Clear service results
            <span .icon .icon-lg>
              visibility_off
        |])


serviceRunButton :: Reflex t => Widget x (Element Dom.EventResult Dom.GhcjsDomSpace t)
serviceRunButton = makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect .fbtn-green>
            <span class="fbtn-text fbtn-text-left">
              Run evaluation service
            <span class="icon icon-lg">
              play_arrow
        |])


