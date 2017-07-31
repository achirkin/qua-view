{-# LANGUAGE OverloadedStrings #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Reflex.Dom

import Widgets.Tabs.Navigator (PanelState (..))

panelInfo :: Reflex t => Dynamic t PanelState -> Widget x ()
panelInfo pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $
      text "Info pane will be here." -- TODO: Info pane
  where
    toPanelClass PanelInfo = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"