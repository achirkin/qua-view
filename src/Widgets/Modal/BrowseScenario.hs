{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Modal.BrowseScenario
    ( browseScenarioPane
    ) where

import Data.Semigroup
import Reflex.Dom

import Widgets.CommonWidget
import Widgets.Modal

browseScenarioPane :: Reflex t => Event t () -> Widget x (Event t ())
browseScenarioPane browsePopupE = createModal browsePopupE False id browseScenarioContent

browseScenarioContent :: Reflex t => Widget x (Event t ())
browseScenarioContent = do
  elAttr "div" (("class" =: "modal-heading") <> ("style" =: "max-height: 10%")) $ do
    elClass "p" "modal-title" $ text "Select scenario"
  elAttr "div" (("class" =: "modal-inner") <> ("style" =: "max-height: 80%")) $ do
    text "Scenario List Table will be here." -- TODO: Scenario list table
  elAttr "div" (("class" =: "modal-footer") <> ("style" =: "max-height: 10%")) $ do
    elClass "p" "text-right" $ do
      flatButton' "Cancel"