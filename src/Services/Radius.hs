-----------------------------------------------------------------------------
-- |
-- Module      :  Services.Radius
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services.Radius where

import JsHs.WebGL

import Data.Geometry
import Data.JSArray
import Program.Model.ScalarField

import Services

-- | This service computes simple euclidian distance from one pre-defined point
newtype Radius = Radius (Vector3 GLfloat) deriving Show


instance ComputingService Radius where
    runService (Radius x) _ _ sf@ScalarField{ sfPoints = pnts} = return $
      if jslength pnts == 0 then Nothing
      else Just sf
        { sfRange = ( jsfoldl1 min vals
                    , jsfoldl1 max vals)
        , sfValues = vals
        } where vals = jsmap (\v -> - normL2 (x - v)) pnts
