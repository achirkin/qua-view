{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelNavigator
    ( panelNavigator
    ) where

import Data.Default (def)
import Reflex.Dynamic
import Reflex.Dom

import CommonTypes
import Widgets.Generation

panelNavigator :: Reflex t => Widget x (Dynamic t PanelState)
panelNavigator = do
  (geometryEl, infoEl, servicesEl) <- elClass "nav" "tab-nav tab-nav-red margin-top-no" $ do
    elClass "ul" "nav nav-justified" $ do
      geometryEl <- makeElementFromHtml def $(qhtml
        [hamlet|
          <li class="active">
            <a aria-expanded="true" class="waves-attach waves-effect" data-toggle="tab" href="#itabGeometry">
              Geometry
        |])
      infoEl <- makeElementFromHtml def $(qhtml
        [hamlet|
          <li class="">
            <a aria-expanded="false" class="waves-attach waves-effect" data-toggle="tab" href="#itabInfo">
              Info
        |])
      servicesEl <- makeElementFromHtml def $(qhtml
        [hamlet|
          <li class="">
            <a aria-expanded="false" class="waves-attach waves-effect" data-toggle="tab" href="#itabServices">
              Services
        |])
      return (geometryEl, infoEl, servicesEl)
  holdDyn PanelGeometry $ leftmost [PanelGeometry <$ domEvent Click geometryEl, 
                                    PanelInfo <$ domEvent Click infoEl, 
                                    PanelServices <$ domEvent Click servicesEl]
