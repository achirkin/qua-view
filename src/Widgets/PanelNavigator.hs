{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelNavigator
    ( panelNavigator
    ) where

import Control.Monad (void)
import Data.Default (def)

import CommonTypes
import Widgets.Generation

panelNavigator :: Reflex t => Widget x ()
panelNavigator = do
    void $ makeElementFromHtml def $(qhtml
        [hamlet|
          <ul class="nav nav-justified">
            <li class="active">
              <a aria-expanded="true" class="waves-attach waves-effect" data-toggle="tab" href="#itabGeometry">
                Geometry
            <li class="">
              <a aria-expanded="false" class="waves-attach waves-effect" data-toggle="tab" href="#itabInfo">
                Info
            <li class="">
              <a aria-expanded="false" class="waves-attach waves-effect" data-toggle="tab" href="#itabServices">
                Services
          <div style="left: 0px; right: 412px;" class="tab-nav-indicator">
        |])
