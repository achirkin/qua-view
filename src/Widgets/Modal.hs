{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TypeApplications #-}


module Widgets.Modal
    ( createModal
    , createModalWithClicks
    , createModalWithClicks'
    , createSmallModalWithClicks
    , createSmallModalWithClicks'
    ) where


import qualified GHCJS.DOM.Element as Element

import Control.Lens ((%~))
import Reflex.Dom

import Commons
import Widgets.Generation

-- | Create a modal window ("popup") with arbitrary content inside.
createModal :: Reflex t
            => Event t (ComponentState modalName) -- ^ get open or close command from outside
            -> ComponentState modalName -- ^ initial state (open or closed)
            -> Widget x a -- ^ content of the widget
            -> Text -- ^ Modal size
            -> Widget x (Dynamic t (ComponentState modalName), a)
createModal outsideStateE defaultState contentWidget size = fmap resultOrError . appendElementToAnotherById "qua-view-modals" $ mdo
    let stateEvs = leftmost [Inactive <$ domEvent Click backpane, outsideStateE]
        changeState Active = liftIO $ js_showModal (_element_raw backpane)
        changeState Inactive = liftIO $ js_hideModal (_element_raw backpane)
    modalActive <- holdDyn defaultState stateEvs
    (backpane, contentReturn) <- elAttr' "div"
                                     (  ("class" =: "modal modal-va-middle fade")
                                     <> ("aria-hidden" =: "true")
                                     <> ("role" =: "dialog")
                                     <> ("tabindex" =: "-1")
                                     <> ("style" =: "display: none")
                                     ) $
      fmap snd $
       element "div" ( def & initialAttributes .~ ("class" =: ("modal-dialog" <> modalSize size))
                           & elementConfig_eventSpec @_ @_ @GhcjsDomSpace @_ @GhcjsDomSpace
                                               %~ addEventSpecFlags (Proxy @GhcjsDomSpace)
                                                                    Click
                                                                    (const $ preventDefault <> stopPropagation)
                     ) $
         elClass "div" "modal-content" contentWidget
    performEvent_ $ changeState <$> stateEvs
    return (backpane, (modalActive, contentReturn))
  where
    resultOrError Nothing = error "Could not add modal to qua-view page!"
    resultOrError (Just (_,a)) = a
    modalSize "small" = " modal-xs"
    modalSize _       = mempty
    

-- | Create a modal window ("popup") with arbitrary content inside.
--   Provides a typical use case: outside button opens the modal,
--   modal close button closed the modal.
createModalWithClicks :: Reflex t
                      => Event t (ElementClick linkToActivate) -- ^ event from a link opening the modal
                      -> ComponentState modalName              -- ^ initial state
                      -> Widget x (Event t (ElementClick linkToDeactivate), a) -- ^ Content with an event to close the modal.
                      -> Widget x (Dynamic t (ComponentState modalName), a)
createModalWithClicks openClickE defaultState contentWidget = mdo
    (stateD, (closeClickE, r)) <- createModal (leftmost [Inactive <$ closeClickE, Active <$ openClickE])
                                              defaultState
                                              contentWidget
                                              "standard"
    return (stateD, r)

-- | Create a modal window ("popup") with arbitrary content inside.
--   Provides a typical use case: outside button opens the modal,
--   modal close button closed the modal.
createModalWithClicks' :: Reflex t
                       => Event t (ElementClick linkToActivate) -- ^ event from a link opening the modal
                       -> ComponentState modalName              -- ^ initial state
                       -> Widget x (Event t (ElementClick linkToDeactivate)) -- ^ Content with an event to close the modal.
                       -> Widget x (Dynamic t (ComponentState modalName))
createModalWithClicks' openClickE defaultState contentWidget = mdo
    (stateD, closeClickE) <- createModal (leftmost [Inactive <$ closeClickE, Active <$ openClickE])
                                              defaultState
                                              contentWidget
                                              "standard"
    return stateD

-- | Create a modal window ("popup") with arbitrary content inside.
--   Provides a typical use case: outside button opens the modal,
--   modal close button closed the modal.
createSmallModalWithClicks :: Reflex t
                           => Event t (ElementClick linkToActivate) -- ^ event from a link opening the modal
                           -> ComponentState modalName              -- ^ initial state
                           -> Widget x (Event t (ElementClick linkToDeactivate), a) -- ^ Content with an event to close the modal.
                           -> Widget x (Dynamic t (ComponentState modalName), a)
createSmallModalWithClicks openClickE defaultState contentWidget = mdo
    (stateD, (closeClickE, r)) <- createModal (leftmost [Inactive <$ closeClickE, Active <$ openClickE])
                                              defaultState
                                              contentWidget
                                              "small"
    return (stateD, r)

-- | Create a modal window ("popup") with arbitrary content inside.
--   Provides a typical use case: outside button opens the modal,
--   modal close button closed the modal.
createSmallModalWithClicks' :: Reflex t
                            => Event t (ElementClick linkToActivate) -- ^ event from a link opening the modal
                            -> ComponentState modalName              -- ^ initial state
                            -> Widget x (Event t (ElementClick linkToDeactivate)) -- ^ Content with an event to close the modal.
                            -> Widget x (Dynamic t (ComponentState modalName))
createSmallModalWithClicks' openClickE defaultState contentWidget = mdo
    (stateD, closeClickE) <- createModal (leftmost [Inactive <$ closeClickE, Active <$ openClickE])
                                              defaultState
                                              contentWidget
                                              "small"
    return stateD

foreign import javascript unsafe "$($1)['modal']('show');" js_showModal :: Element.Element -> IO ()
foreign import javascript unsafe "$($1)['modal']('hide');" js_hideModal :: Element.Element -> IO ()
