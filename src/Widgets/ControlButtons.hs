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

import Control.Monad (join)
import qualified Reflex.Class as Reflex
import qualified Reflex.Dom as Dom
import Reflex.Dynamic
import Text.Julius (julius)

import Commons
import SmallGL (RenderingApi)
import QuaTypes
import Model.Scenario (Scenario)
import Widgets.Generation
import Widgets.Modal.DownloadScenario
import Widgets.Modal.Help
import Widgets.Modal.Share
import Widgets.Modal.SubmitProposal

-- | Control button group is a column of colourfull buttons in the bottom-right corner of the screen.
--   It defines the most useful functions of qua-kit.
controlButtonGroup :: Reflex t
                   => RenderingApi
                   -> Behavior t Scenario
                   -> QuaWidget t x ( Dynamic t (ComponentState "ControlPanel"))
controlButtonGroup rApi scenarioB = mdo
    (toggleGroupD, cpStateD) <-
        Dom.elDynClass "div" (toPanelClass <$> cpStateD) $
          Dom.elDynClass "div" toggleGroupD $ do
            -- toggle visibility of buttons
            toggleGroupD'  <- expandCtrlGroupButton
            -- show all buttons
            groupContents <- Dom.elClass "div" "fbtn-dropup" $ do
                downloadScenarioButton scenarioB
                shareButton
                resetCameraButton
                helpButton
                toggleFullScreenButton
                groupContents' <- controlPanelButton
                _serviceStateD <- serviceButtons $ constDyn Inactive -- TODO: For running service
                _ <- submitProposalButton rApi scenarioB
                return groupContents'
            return (toggleGroupD', groupContents)
    return cpStateD
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
expandCtrlGroupButton :: Reflex t => QuaWidget t x (Dynamic t Text)
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
controlPanelButton :: Reflex t => QuaWidget t x (Dynamic t (ComponentState "ControlPanel"))
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
toggleFullScreenButton :: Reflex t => QuaWidget t x ()
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
helpButton :: Reflex t => QuaWidget t x ()
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


-- | Registers an event `AskResetCamera`.
resetCameraButton :: Reflex t => QuaWidget t x ()
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
    registerEvent (UserRequest AskResetCamera) (() <$ Dom.domEvent Dom.Click e)

serviceButtons :: Reflex t
               => Dynamic t (ComponentState "LuciConnect")
               -> QuaWidget t x (Dynamic t (ComponentState "Service"))
serviceButtons luciConD = do
  serviceStateE <- Dom.dyn $ renderBtn <$> luciConD
  serviceStateDD <- holdDyn (constDyn Inactive) serviceStateE
  return $ join serviceStateDD
  where
    renderBtn Inactive = return $ constDyn Inactive
    renderBtn Active = mdo
      serviceStateD <- holdDyn Inactive $ Reflex.leftmost [Active <$ serviceRunE, Inactive <$ serviceClearE]
      serviceRunE   <- serviceRunButton serviceStateD
      serviceClearE <- serviceClearButton serviceStateD
      return serviceStateD


serviceClearButton :: Reflex t
                   => Dynamic t (ComponentState "Service")
                   -> QuaWidget t x (Event t (ElementClick s))
serviceClearButton stateD = do
    (e, _) <- Dom.elDynAttr' "a" (attrs <$> stateD) $ do
                Dom.elClass "span" "fbtn-text fbtn-text-left" $ Dom.text "Clear service results"
                Dom.elClass "span" "icon icon-lg" $ Dom.text "visibility_off"
    return (ElementClick <$ Dom.domEvent Dom.Click e)
  where
    attrs s = ("class" =: "fbtn waves-attach waves-circle waves-effect waves-light fbtn-brand-accent")
              <> displayButton s
    displayButton Inactive = "style" =: "display: none"
    displayButton Active   = mempty

serviceRunButton :: Reflex t
                 => Dynamic t (ComponentState "Service")
                 -> QuaWidget t x (Event t (ElementClick s))
serviceRunButton stateD = do
    (e, _) <- Dom.elDynAttr' "a" (attrs <$> stateD) $ do
                Dom.elClass "span" "fbtn-text fbtn-text-left" $ Dom.text "Run evaluation service"
                Dom.elClass "span" "icon icon-lg" $ Dom.text "play_arrow"
    return (ElementClick <$ Dom.domEvent Dom.Click e)
  where
    attrs s = ("class" =: "fbtn waves-attach waves-circle waves-effect fbtn-green")
              <> displayButton s
    displayButton Active   = "style" =: "display: none"
    displayButton Inactive = mempty

shareButton :: Reflex t
            => QuaWidget t x ()
shareButton = do
  settingsD <- quaSettings
  link <- Dom.sample $ current $ viewUrl <$> settingsD
  e <- makeElementFromHtml def $(qhtml
        [hamlet|
          <a .fbtn .waves-attach .waves-circle .waves-effect .fbtn-brand>
            <span .fbtn-text .fbtn-text-left>
              Share proposal
            <span .icon .icon-lg>
              share
        |])
  popupShare (ElementClick <$ Dom.domEvent Dom.Click e) link

submitProposalButton :: Reflex t
                     => RenderingApi
                     -> Behavior t Scenario -> QuaWidget t x ()
submitProposalButton rApi scenarioB = do
    e <- makeElementFromHtml def $(qhtml
          [hamlet|
            <a .fbtn .waves-attach .waves-circle .waves-effect .fbtn-brand>
              <span .fbtn-text .fbtn-text-left>
                Submit proposal
              <span .icon .icon-lg>
                save
          |])
    popupSubmitProposal rApi scenarioB (ElementClick <$ Dom.domEvent Dom.Click e)


downloadScenarioButton :: Reflex t => Behavior t Scenario -> QuaWidget t x ()
downloadScenarioButton scB = do
    e <- makeElementFromHtml def $(qhtml
          [hamlet|
            <a .fbtn .waves-attach .waves-circle .waves-effect>
              <span .fbtn-text .fbtn-text-left>
                Download scenario
              <span .icon .icon-lg>
                file_download
          |])
    popupDownloadScenario scB (ElementClick <$ Dom.domEvent Dom.Click e)


