{-# LANGUAGE OverloadedStrings #-}

module Widgets.Tabs.Info
    ( panelInfo
    ) where

import Commons
import Reflex.Dom

panelInfo :: (Reflex t, DomBuilder t m) => m ()
panelInfo = text "Info pane will be here." -- TODO: Info pane
