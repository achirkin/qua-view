{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.ControlPanel.Services
    ( panelServices
    ) where

import Control.Monad (void)
import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

-- TODO: Add functionality to Panel Services
panelServices :: Reflex t => Dynamic t PanelState -> Widget x ()
panelServices pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $
      void $ makeElementFromHtml def $(qhtml
        [hamlet|
          <div>
            Select a remote service to run
              <table style="width: 95%">
                <tr>
                  <td style="width: 5%">
                    <a.btn.btn-flat.btn-red.waves-attach title="Refresh list of available services">
                      <span.icon.icon-lg>refresh
                  <td style="width: 95%">
                    <select.form-control>
          <div.form-group>
        |])
  where
    toPanelClass PanelServices = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"
