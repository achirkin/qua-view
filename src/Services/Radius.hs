-----------------------------------------------------------------------------
-- |
-- Module      :  Services.RadianceService
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module Services.Radius where


import Data.List (foldl')

import GHCJS.WebGL

import Data.Geometry
import Program.Model.ScalarField

import Services

-- | This service computes simple euclidian distance from one pre-defined point
newtype RadianceService = RadianceService (Vector3 GLfloat) deriving Show


instance ComputingService RadianceService where
    runService _ _ _ ScalarField{ sfPoints = []} = return $ Nothing
    runService (RadianceService x) _ _ sf@ScalarField{ sfPoints = pnts} = return $ Just sf
        { sfRange = foldl' (\(xmin,xmax) t
            -> (min xmin t, max xmax t)) (head vals, head vals) vals
        , sfValues = vals
        } where vals = map (\v -> - normL2 (x - v)) pnts
