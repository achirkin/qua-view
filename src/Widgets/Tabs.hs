module Widgets.Tabs
    ( panelTabs
    ) where

import Reflex.Dom

import Widgets.Tabs.Navigator
import Widgets.Tabs.Geometry
import Widgets.Tabs.Info
import Widgets.Tabs.Services

panelTabs :: Reflex t => Widget x (Event t (), Event t ())
panelTabs = do
  panelState <- tabsNavigator
  (browsePopupE, savePopupE) <- panelGeometry panelState
  panelInfo panelState
  panelServices panelState
  return (browsePopupE, savePopupE)
