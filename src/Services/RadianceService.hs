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

module Services.RadianceService where


import Data.List (foldl')

import GHCJS.WebGL

import Geometry.Space
import Program.Model.ScalarField

import Services

-- | This service computes simple euclidian distance from one pre-defined point
newtype RadianceService = RadianceService (Vector3 GLfloat) deriving Show


instance ComputingService RadianceService where
    runService _ _ _ ScalarField{ sfPoints = []} = return $ Nothing
    runService (RadianceService x) _ _ sf@ScalarField{ sfPoints = pnts} = return $ Just sf
        { sfRange = foldl' (\(Vector2 xmin xmax) t
            -> Vector2 (min xmin t) (max xmax t)) (pure $ head vals) vals
        , sfValues = vals
        } where vals = map (\v -> 1 / (1 + normL2 (x .- v))) pnts
