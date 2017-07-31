{-# LANGUAGE OverloadedStrings #-}

module Widgets.ControlPanel.Info
    ( panelInfo
    ) where

import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

panelInfo :: Reflex t => Dynamic t PanelState -> Widget x ()
panelInfo pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $
      text "Info pane will be here." -- TODO: Info pane
  where
    toPanelClass PanelInfo = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"