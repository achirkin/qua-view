{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.PanelGeometry
    ( panelGeometry
    ) where

import Data.Semigroup
import Reflex.Dom

import CommonTypes
import Widgets.Generation

panelGeometry :: Reflex t => Dynamic t PanelState -> Widget x ()
panelGeometry pStateD = 
    elDynClass "div" (toPanelClass <$> pStateD) $ do
      text "G"
      -- void $ makeElementFromHtml def $(qhtml
      --   [hamlet|

      --   |])
  where
    toPanelClass PanelGeometry = openPanelState
    toPanelClass _ = closedPanelState
    -- Styles for the panel are generated statically.
    -- newVar guarantees that the class name is unique.
    (openPanelState, closedPanelState) = $(do
        placeholder <- newVar
        let ostate = placeholder <> "-open"
            cstate = placeholder <> "-closed"
        qcss
          [cassius|
            .#{placeholder}
                position: absolute

            .#{ostate}
                visibility: visible

            .#{cstate}
                visibility: hidden

          |]
        -- Combine two classes: {.base .base-open} and {.base .base-closed}
        returnVars $ fmap ((placeholder <> " ") <>) [ostate, cstate]
      )