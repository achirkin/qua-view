module Widgets.Tabs
    ( panelTabs
    ) where

import Reflex.Dom

import CommonTypes
import Widgets.Tabs.Navigator
import Widgets.Tabs.Geometry
import Widgets.Tabs.Info
import Widgets.Tabs.Services

panelTabs :: Reflex t => Widget x (EventSelector t GeometryTabOutE)
panelTabs = do
  panelStateD <- tabsNavigator
  panelGeometryEvents <- panelGeometry panelStateD
  panelInfo panelStateD
  panelServices panelStateD
  return panelGeometryEvents
