{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Widgets.Tabs
    ( panelTabs
    ) where

import Reflex.Dom

import Widgets.Generation
import Widgets.Tabs.Navigator
import Widgets.Tabs.Geometry
import Widgets.Tabs.Info
import Widgets.Tabs.Services

panelTabs :: Reflex t => Widget x (EventSelector t GeometryTabOutE)
panelTabs = do
    elAttr "div" ("style" =: "overflow-y: auto; overflow-x: hidden; height: 100%;") $ do
      elAttr "div" ("style" =: "margin: 0; padding: 0; height: 56px;") blank
      panelStateD <- tabsNavigator
      elClass "div" tabContent $ do
        panelGeometryEvents <- panelGeometry panelStateD
        panelInfo panelStateD
        panelServices panelStateD
        return panelGeometryEvents
  where
    tabContent = $(do
        tabContentClass <- newVar
        qcss
          [cassius|
            .#{tabContentClass}
                padding-left: 20px;
                padding-right: 20px;
          |]
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars [tabContentClass]
      )
