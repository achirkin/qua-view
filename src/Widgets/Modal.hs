{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Modal
    ( createModal
    ) where

import Data.Semigroup
import Reflex.Dom

createModal :: Reflex t => Event t () -> Bool -> (a -> Event t ()) -> Widget x a -> Widget x a
createModal openModalE defaultOpen getCloseModalE contentWidget = mdo
    modalActive <- holdDyn defaultOpen $ leftmost [False <$ closeModalE, True <$ openModalE]
    let closeModalE = getCloseModalE contentReturn
    contentReturn <- elDynAttr "div" (attrs <$> modalActive) $
      elClass "div" "modal-dialog" $
        elClass "div" "modal-content" contentWidget
    elDynClass "div" (backdropClass <$> modalActive) blank
    return contentReturn
  where
    attrs active = ("class" =: ("modal modal-va-middle fade" <> displayClass active))
                <> ("role" =: "dialog")
                <> ("tabindex" =: "-1")
                <> ("style" =: ("display: " <> displayStyle active))
    displayClass True  = " modal-va-middle-show in"
    displayClass False = ""
    displayStyle True  = "flex"
    displayStyle False = "none"
    backdropClass True  = "modal-backdrop fade in"
    backdropClass False = ""
