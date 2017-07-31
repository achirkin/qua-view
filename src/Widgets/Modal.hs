{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Modal
    ( createModal
    ) where

import Data.Semigroup
import Reflex.Dom

import Widgets.Generation

createModal :: Reflex t => Event t () -> (a -> Event t ()) -> Widget x a -> Widget x a
createModal openModalE getCloseModalE contentWidget = mdo
    modalActive <- holdDyn False $ leftmost [False <$ closeModalE, True <$ openModalE]
    let closeModalE = getCloseModalE contentReturn
    contentReturn <- elDynAttr "div" (attrs <$> modalActive) $ do
      elClass "div" "modal-dialog" $ do
        elClass "div" "modal-content" $ do
          contentWidget
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
