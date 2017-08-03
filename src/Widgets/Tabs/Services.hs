{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Tabs.Services
    ( panelServices
    ) where

import Control.Monad (void)
import Reflex.Dom

import Widgets.Generation
import Widgets.Tabs.Navigator (PanelState (..))

-- TODO: Add functionality to Panel Services
panelServices :: Reflex t => Dynamic t PanelState -> Widget x ()
panelServices pStateD =
    elDynClass "div" (toPanelClass <$> pStateD) $
      void $ makeElementFromHtml def $(qhtml
        [hamlet| -- TODO: this splice must have a single root html element! use setInnerHTML if you want several children
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
