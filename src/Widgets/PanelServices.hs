{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelServices
    ( panelServices
    ) where

import Control.Monad (void)
import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

panelServices :: Reflex t => Dynamic t PanelState -> Widget x ()
panelServices pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $ do
      void $ makeElementFromHtml def $(qhtml
        [hamlet|
          <div>
            Select a remote service to run
              <table style="width: 95%">
                <tr>
                  <td style="width: 5%">
                    <a.btn.btn-flat.btn-red.waves-attach title="Refresh list of available services" #refreshServicesBtn>
                      <span.icon.icon-lg>refresh
                  <td style="width: 95%" onchange="activeVisService($(this).find(':selected').val())">
                    <select.form-control #serviceListControlMenu>

          <div.form-group #guiServiceParams>
        |])
  where
    toPanelClass PanelServices = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"
