{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Widgets.Tabs.Services
    ( panelServices
    ) where

import Reflex.Dom

import Commons
import Widgets.Generation
import Widgets.Tabs.Navigator (PanelState (..))

-- TODO: Add functionality to Panel Services
panelServices :: Reflex t => Dynamic t PanelState -> Widget x ()
panelServices pStateD =
    elDynClass "div" (toPanelClass <$> pStateD) $ do
      el "div" $ do
        text $ "Select a remote service to run"
        void $ makeElementFromHtml def $(do
            qcss
              [cassius|
                .spKey
                  padding: 2px
                  text-align: right
                .spVal
                  padding: 2px
              |]
            qhtml
              [hamlet|
                <table style="width: 95%">
                  <tr>
                    <td style="width: 5%">
                      <a.btn.btn-flat.btn-red.waves-attach title="Refresh list of available services">
                        <span.icon.icon-lg>refresh
                    <td style="width: 95%">
                      <select.form-control>
              |]
          )
      elAttr "div" attrs $
        el "table" blank -- TODO: For service parameters.
  where
    toPanelClass PanelServices = "tab-pane fade active in"
    toPanelClass _ = "tab-pane fade"
    attrs = ("class" =: "form-group")
         <> ("id" =: "guiServiceParams")
