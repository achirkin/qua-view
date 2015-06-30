-----------------------------------------------------------------------------
--
-- Module      :  Model.RadianceService
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Model.RadianceService where

import Data.List (foldl')

import GHCJS.WebGL

import Geometry.Space
import Model.ScalarField

distFromPoint :: Vector3 GLfloat -> ScalarField -> ScalarField
distFromPoint _ sf@ScalarField{ sfPoints = []} = sf{sfRange = zeros, sfValues = []}
distFromPoint x sf@ScalarField{ sfPoints = pnts} = sf
    { sfRange = foldl' (\(Vector2 xmin xmax) t -> Vector2 (min xmin t) (max xmax t)) (pure $ head vals) vals
    , sfValues = vals
    } where vals = map (normL2 . (x .-)) pnts
