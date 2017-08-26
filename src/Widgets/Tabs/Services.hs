{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Widgets.Tabs.Services
    ( panelServices
    ) where

import Reflex.Dom

import Commons
import Widgets.Generation

-- TODO: Add functionality to Panel Services
panelServices :: ( Reflex t
                 , DomBuilder t m
                 , DomBuilderSpace m ~ GhcjsDomSpace
                 , MonadIO m
                 )
              => m ()
panelServices = do
      el "div" $ do
        text "Select a remote service to run"
        void $ makeElementFromHtml def $(-- do
--            qcss
--              [cassius|
--                .spKey
--                  padding: 2px
--                  text-align: right
--                .spVal
--                  padding: 2px
--              |]
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
    attrs = ("class" =: "form-group")
         <> ("id" =: "guiServiceParams")
